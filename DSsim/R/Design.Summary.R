#' Class "Design.Summary" 
#' 
#' Class \code{"Design.Summary"} is an S4 class containing a summary of
#' the survey design. This is returned when \code{summary(Design)}
#' is called. If it is not assigned to a variable the object will be 
#' displayed via the \code{show} method. 
#'
#' @name Design.Summary-class
#' @title S4 Class "Design.Summary"
#' @docType class
#' @keywords classes
#' @section Methods:
#' \describe{
#'  \item{\code{show}}{\code{signature=(object = "Design.Summary")}: prints
#'  the contents of the object in a user friendly format.}
#'  }
setClass("Design.Summary", representation(design.type = "character",
                                          design.parameters = "list"))
setMethod(
  f="initialize",   
  signature="Design.Summary",
  definition=function(.Object, design.type, design.parameters){
    #Set slots
    .Object@design.type       <- design.type
    .Object@design.parameters <- design.parameters
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
) 

setValidity("Design.Summary",
            function(object){   
              return(TRUE)
            }
)  

#' show
#' 
#' Displays the simulation summary
#' 
#' #@param object object of class Simulation.Summary
#' @rdname show.Simulation.Summary-methods
#' @export
setMethod(
  f="show",   
  signature="Design.Summary",
  definition=function(object){  
    #Display summaries
    cat("\nDesign: ", object@design.type, fill = TRUE)
    for(i in seq(along = object@design.parameters)){
      cat("  ", names(object@design.parameters)[i], " = ", object@design.parameters[[i]], fill = TRUE)
    }
  }
)





