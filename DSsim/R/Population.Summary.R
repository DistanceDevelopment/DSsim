#' Class "Population.Summary" 
#' 
#' Class \code{"Population.Summary"} is an S4 class containing a summary of
#' the survey population. This is returned when \code{summary(Population)}
#' is called. If it is not assigned to a variable the object will be 
#' displayed via the \code{show} method. 
#'
#' @name Population.Summary-class
#' @title S4 Class "Population.Summary"
#' @docType class
#' @keywords classes
#' @section Methods:
#' \describe{
#'  \item{\code{show}}{\code{signature=(object = "Population.Summary")}: prints
#'  the contents of the object in a user friendly format.}
#'  }
setClass("Population.Summary", representation(region.name = "character",
                                              population.parameters = "list"))
setMethod(
  f="initialize",   
  signature="Population.Summary",
  definition=function(.Object, region.name, population.type, population.parameters){
    #Set slots
    .Object@region.name       <- region.name
    .Object@population.parameters <- population.parameters
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
) 

setValidity("Population.Summary",
            function(object){   
              return(TRUE)
            }
)  
