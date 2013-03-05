################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

##' Class "LTSurvey" containing an instance of a Line Transect Survey design
##'
##' Class \code{"LTSurvey"} is an S4 class detailing the type of line transect 
##' design and the co-ordinates of the end points of the transects.
##' class \code{\link{lm}} with a common model.
##' @name lmList-class
##' @aliases lmList-class show,lmList-method
##' @docType class
##' @section Objects from the Class: Objects can be created by calls of the form
##' \code{new("lmList", ...)} or, more commonly, by a call to
##' \code{\link{lmList}}.
##' @keywords classes
##' @export
setClass("Line.Transect", representation( design.obj = "character", sampler.info = "data.frame"))

setMethod(
  f="initialize",
  signature="Line.Transect",
  definition=function(.Object, region, sampler.info = NULL, shapefile = NULL){
    #Input pre-processing
    if(is.null(sampler.info) & !is.null(shapefile)){      
      sampler.info <- get.sampler.info(shapefile, region)
    }else{      
    }
    #Set slots
    #.Object@design.obj <- design.obj
    .Object@sampler.info <- sampler.info
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)

setValidity("Line.Transect",
  function(object){
    sampler.info.names <- names(object@sampler.info)
    required.var.names <- c("ID", "start.X", "start.Y", "end.X", "end.Y", "length", "region")
    if(length(sampler.info.names) != length(required.var.names)){
      return("The sampler.info data.frame contains an incorect number of variables.")
    }
    for(i in seq(along = sampler.info.names)){
      if(!sampler.info.names[i]%in%required.var.names){
        return("The variable names provided for the sampler.info data.frame do not match those which are required.")
      }
    }
    return(TRUE)
  }
)

################################################################################
# GENERIC METHODS
################################################################################
setMethod(
  f="plot",
  signature="Line.Transect",
  definition=function(x, y, transect.ID = numeric(0), col = 1, ...){
    plot.transect <- function(sampler.info, col, ...){
      lines(x = c(sampler.info[["start.X"]],sampler.info[["end.X"]]), y = c(sampler.info[["start.Y"]],sampler.info[["end.Y"]]), col = col, ...)
      invisible(sampler.info)  
    }
    sampler.info <- x@sampler.info
    sampler.info$ID <- as.numeric(sampler.info$ID)
    if(length(transect.ID) == 0){
      transect.ID <- unique(sampler.info$ID)
    }
    apply(as.matrix(sampler.info[sampler.info$ID%in%transect.ID,]), 1, FUN = plot.transect, col = col, ...)
    invisible()
  }
)

################################################################################
# ASSOCIATED METHODS
################################################################################

get.sampler.info <- function(shapefile, region.obj){
  ID <- start.X <- start.Y <- end.X <- end.Y <- tot.length <- region <- NULL
  for(samp in seq(along = survey.shapefile$shp$shp)){
    #segs <- nrow(survey.shapefile$shp$shp[[samp]]$points)/2
    segs <- survey.shapefile$shp$shp[[samp]]$num.parts
    temp.length <- 0 
    for(seg in seq(1:segs)){
      ID      <- c(ID, survey.shapefile$shp$shp[[samp]]$record)
      start.X <- c(start.X, survey.shapefile$shp$shp[[samp]]$points$X[2*seg-1])
      start.Y <- c(start.Y, survey.shapefile$shp$shp[[samp]]$points$Y[2*seg-1])
      end.X   <- c(end.X, survey.shapefile$shp$shp[[samp]]$points$X[2*seg])
      end.Y   <- c(end.Y, survey.shapefile$shp$shp[[samp]]$points$Y[2*seg])
      temp.length  <- temp.length + sqrt((survey.shapefile$shp$shp[[samp]]$points$X[2*seg] - survey.shapefile$shp$shp[[samp]]$points$X[2*seg-1])^2 + 
                                         (survey.shapefile$shp$shp[[samp]]$points$Y[2*seg] - survey.shapefile$shp$shp[[samp]]$points$Y[2*seg-1])^2   )
    }
    tot.length <- c(tot.length, rep(temp.length, segs))
    region     <- c(region, rep(region.obj@region.name, segs))
  } 
  sampler.info <- data.frame(ID = ID, start.X = start.X, start.Y = start.Y, end.X = end.X, end.Y = end.Y, length = tot.length, region = region)
  return(sampler.info)
}




