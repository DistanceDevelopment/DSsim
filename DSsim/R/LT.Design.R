#' @include generic.functions.R
#' @include Survey.Design.R

#' Virtual Class "LT.Design" extends Class "Survey.Design"
#'
#' Virtual Class \code{"LT.Design"} is an S4 class detailing the type of 
#' line transect design.
#' @name LT.Design-class
#' @title S4 Class "LT.Design"
#' @slot design.axis Object of class \code{"numeric"}; the angle of
#'  the design axis.
#' @slot spacing Object of class \code{"numeric"}; the spacing of
#'  the design.
#' @section Methods:
#' \describe{
#'  \item{\code{generate.transects}}{\code{signature=(object = "LT.Design", ...)}:
#'  loads a set of transects from a shapefile.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.design}}
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
      return("You must only specify one path. All transect shapefiles must be in the same folder.")
      #return(FALSE)
    }
    return(TRUE)
  }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname generate.transects-methods
#' @importFrom utils read.table
#' @export
setMethod(
  f="generate.transects",
  signature="LT.Design",
  definition=function(object, read.from.file = TRUE, write.to.file = FALSE, region = NULL, index = NULL){
    if(is.null(region) | class(region) != "Region"){
      region <- object@region.obj
      region <- get(region, pos = 1)
    }
    file.index <- ifelse(is.null(index), object@file.index, index)
    #Input pre-processing
    if(read.from.file){
      #Load the shapefle
      shapefile <- read.shapefile(paste(object@path, "/", object@filenames[file.index], sep=""))
      #Load the meta file if it exists - describes which transects are in which strata
      meta <- suppressWarnings(try(read.table(paste(object@path, "/Meta.txt", sep="")), silent = TRUE))
      if(class(meta) == "try-error"){
        meta <- NULL
      }
      if(!is.null(meta)){
        meta <- meta[meta[,1] == object@filenames[file.index],]
      }
      line.transect <- new(Class = "Line.Transect", region = region, shapefile = shapefile, meta = meta)
    }else{
      stop("Only pre-generated surveys are currently implemented", call. = FALSE)
    }
    return(line.transect)
  }
)







