#' @include LT.Design.R

setClass(Class = "LT.Systematic.Design", 
         contains = "LT.Design"
) 

setValidity("LT.Systematic.Design",
            function(object){
              if(length(object@path) > 1){
                return("You must only specify one path. All transect shapefiles must be in the same folder.")
              }
              if(any(ifelse(object@design.axis != 0, TRUE, FALSE))){
                warning("Only a design axis of 0 is currently implemented, other values will be ignored at present.", call. = FALSE, immediate. = TRUE)
              }
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname generate.transects-methods
#' @param silent if TRUE does not report warnings about a single value for nested spacing with a multi strata region
#' @importFrom utils read.table
#' @importFrom fields cover.design
#' @export
setMethod(
  f="generate.transects",
  signature="LT.Systematic.Design",
  definition=function(object, read.from.file = FALSE, write.to.file = FALSE, region = NULL, index = NULL, silent = FALSE){
    if(is.null(region) | class(region) != "Region"){
      region <- object@region.obj
      region <- get(region, pos = 1)
      warning("Obtaining region object from the global environment.", call. = FALSE, immediate. = TRUE)
    }
    #Input pre-processing
    if(read.from.file){
      #Go to parent method to read from file
      line.transect <- callNextMethod()
      return(line.transect)
    }else{
      if(write.to.file){
        warning("Write to file not currently implemented", call. = FALSE, immediate. = TRUE)
      }
      stop("About to be implemented!", call. = FALSE, immediate. = TRUE)
      # Find x and y coordinate limits
      # Generate a start point between 0 and the spacing value
      # Generate a sequence of x values with correct spacing till end of study region
      # Generate the lines from min to max y values for each x value
      # Clip the lines to the polygon
      # Calculate transect lengths
      # Generate sampler info
      # Create Line.Transect object
      # Return object
    }
  }
)
