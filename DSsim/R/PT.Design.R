#' @include generic.functions.R
#' @include Survey.Design.R

#' @title Virtual Class "PT.Design" extends Class "Survey.Design"
#'
#' @description Virtual Class \code{"PT.Design"} is an S4 class detailing the type of 
#' line transect design.
#' @name PT.Design-class
#' @title S4 Class "PT.Design"

#' @section Methods:
#' \describe{
#'  \item{\code{generate.transects}}{\code{signature=(object = "PT.Design", ...)}:
#'  loads a set of transects from a shapefile.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "PT.Design",
         representation = representation("VIRTUAL"),
         contains = "Survey.Design"
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname generate.transects-methods
#' @importFrom utils read.table
#' @export
setMethod(
  f="generate.transects",
  signature="PT.Design",
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
      if(length(shapefile$shp$shp) == 0){
        warning("Survey transect shapefile has no transects.", call. = FALSE, immediate. = TRUE)
        dd <- data.frame(Id=c(1,1),X=c(1,1),Y=c(1,1))
        dd.table <- data.frame(Id=c(1),Name=c("1"))
        shapefile <- convert.to.shapefile(dd, dd.table, "Id", 3)
        meta <- data.frame(V1 = as.factor(object@filenames[file.index]), V2 = 1, V3 = 1)  
        #Check the shapefile is the correct type
      }else if(!shapefile$shp$header$shape.type %in% c(1,11,21)){
        warning("Survey transect shapefile of wrong shapefile type (not points) cannot load survey.", call. = FALSE, immediate. = TRUE)
        dd <- data.frame(Id=c(1),X=c(-Inf),Y=c(-Inf))
        dd.table <- data.frame(Id=c(1),Name=c("1"))
        shapefile <- convert.to.shapefile(dd, dd.table, "Id", 1)
        meta <- data.frame(V1 = as.factor(object@filenames[file.index]), V2 = 1, V3 = 1)
      }else{
        #Load the meta file if it exists - describes which transects are in which strata
        meta <- suppressWarnings(try(read.table(paste(object@path, "/Meta.txt", sep="")), silent = TRUE))
        if(class(meta) == "try-error"){
          meta <- NULL
        }
        if(!is.null(meta)){
          meta <- meta[meta[,1] == object@filenames[file.index],]
        }  
      }
      point.transect <- new(Class = "Point.Transect", region = region, shapefile = shapefile, meta = meta)
    }else{
      stop("Only pre-generated surveys are currently implemented", call. = FALSE)
    }
    return(point.transect)
  }
)







