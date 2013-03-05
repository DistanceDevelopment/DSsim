################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

#' Virtual Class "LT.Design" 
#'
#' Virtual Class \code{"LT.Design"} is an S4 class detailing the type of line transect 
#' design and the co-ordinates of the end points of the transects.
#' @name LT.Design-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' @keywords classes
#' @export

setClass(Class = "LT.Design", 
         representation = representation(design.axis = "numeric",
                                         spacing = "numeric", "VIRTUAL"),
         contains = "Survey.Design"
)

setMethod(
  f="initialize",
  signature="LT.Design",
  definition=function(.Object, region, spacing, design.axis, plus.sampling, path = character(0), ...){
    filenames <- character(0)
    file.index <- numeric(0)
    if(length(path) > 0){
      filenames  <- get.shapefile.names(path)
      file.index <- 1  
    }
    #Set slots
    .Object@region.obj    <- region
    .Object@plus.sampling <- plus.sampling
    .Object@spacing       <- spacing
    .Object@design.axis   <- design.axis  
    .Object@path          <- path
    .Object@filenames     <- filenames
    .Object@file.index    <- file.index
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)

setValidity("LT.Design",
  function(object){
    if(length(object@path) > 1){
      message("You must only specify one path. All transect shapefiles must be in the same folder.")
      return(FALSE)
    }
    return(TRUE)
  }
)


################################################################################
# GENERIC METHODS
################################################################################

setMethod(
  f="generate.transects",
  signature="LT.Design",
  definition=function(object, read.from.file = TRUE, write.to.file = FALSE, region = NULL){
    if(is.null(region) | class(region) != "Region"){
      region <- object@region.obj
      region <- get(region, pos = 1)
    }
    #Input pre-processing
    if(read.from.file){
      shapefile <- read.shapefile(paste(object@path, "/", object@filenames[object@file.index], sep=""))
      #lt.survey <- make.line.transect(region = region, shapefile = shapefile)
      line.transect <- new(Class = "Line.Transect", region = region, shapefile = shapefile)
    }else{
      message("Only pre-generated surveys are currently implemented")
      line.transect <- NULL
    }
    return(line.transect)
  }    
) 


 
#setMethod(
#  f="generate.transects",
#  signature="character",
#  definition=function(object, read.from.file = TRUE, write.to.file = FALSE){
#    design.object <- get(object)
#    transects <- generate.transects(design.object, object)    
#    return(transects)
#  }    
#) 

setMethod(
  f="plot",
  signature="LT.Design",
  definition=function(x, y, col = 1){
    plot.transect <- function(sampler.info, col){
      lines(x = c(sampler.info[["start.X"]],sampler.info[["end.X"]]), y = c(sampler.info[["start.Y"]],sampler.info[["end.Y"]]), col = col)
      invisible(sampler.info)  
    }
    sampler.info <- x@sampler.info
    sampler.info$ID <- as.numeric(sampler.info$ID)
    apply(as.matrix(sampler.info), 1, FUN = plot.transect, col = col)
    invisible()
  }
)

################################################################################
# ASSOCIATED METHODS
################################################################################

get.shapefile.names <- function(path){
  #get all filenames in folder
  all.filenames <- list.files(path)
  #remove file extenstions
  all.filenames.list <- strsplit(all.filenames, split="[.]")
  all.filenames <- lapply(all.filenames.list, function(x){return(x[1])})
  all.filenames <- as.character(all.filenames)
  #find unique shapefiles 
  unique.shapefiles <- unique(all.filenames)
  return(unique.shapefiles)
}


