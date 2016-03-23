#' @title Class "Simulation.Summary" 
#' 
#' @description Class \code{"Simulation.Summary"} is an S4 class containing a summary of
#' the simulation results. This is returned when \code{summary(Simulation)}
#' is called. If it is not assigned to a variable the object will be 
#' displayed via the \code{show} method. 
#'
#' @name Simulation.Summary-class
#' @title S4 Class "Simulation.Summary"
#' @docType class
#' @keywords classes
#' @section Methods:
#' \describe{
#'  \item{\code{show}}{\code{signature=(object = "Simulation.Summary")}: prints
#'  the contents of the object in a user friendly format.}
#'  }
setClass("Simulation.Summary", representation(region.name = "character",
                                      total.reps = "numeric",
                                      failures = "numeric",
                                      individuals = "list",
                                      clusters = "list",
                                      expected.size = "data.frame", 
                                      detection = "data.frame",
                                      model.selection = "table",
                                      design.summary = "Design.Summary",
                                      detectability.summary = "list",
                                      analysis.summary = "list",
                                      include.glossary = "logical"))
                                      


setMethod(
  f="initialize",   
  signature="Simulation.Summary",
  definition=function(.Object, region.name, total.reps, failures, individuals, clusters = list(), expected.size = data.frame(NULL), detection, model.selection, design.summary = list(), detectability.summary = list(), analysis.summary = list(), include.glossary = FALSE){
    #Set slots
    .Object@region.name     <- region.name
    .Object@total.reps      <- total.reps
    .Object@failures        <- failures
    .Object@individuals     <- individuals    
    .Object@clusters        <- clusters
    .Object@expected.size   <- expected.size
    .Object@detection       <- detection
    .Object@model.selection <- model.selection
    .Object@design.summary  <- design.summary
    .Object@detectability.summary <- detectability.summary
    .Object@analysis.summary <- analysis.summary
    .Object@include.glossary <- include.glossary
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
) 

setValidity("Simulation.Summary",
  function(object){   
    return(TRUE)
  }
)  

################################################################################
# GENERIC METHODS
################################################################################

#' show
#' 
#' Displays the simulation summary
#' 
#' @param object object of class Simulation.Summary
#' @rdname show.Simulation.Summary-methods
#' @export
setMethod(
  f="show",   
  signature="Simulation.Summary",
  definition=function(object){  
    #Display summaries
    cat("\nRegion: ", object@region.name) 
    cat("\nNo. Repetitions: ", object@total.reps)
    cat("\nNo. Failures: ", object@failures, fill = TRUE)
    show(object@design.summary)
    cat("\nPopulation Detectability Summary:", fill = TRUE)
    for(i in seq(along = object@detectability.summary)){
      if(length(object@detectability.summary[[i]]) > 0){
        cat("   ",names(object@detectability.summary)[i], " = ", object@detectability.summary[[i]], fill = TRUE)  
      }
    }
    cat("\nAnalysis Summary:")  
    for(i in seq(along = object@analysis.summary)){
      if(i == 1){
        cat("\n   Candidate Models:", fill = TRUE)
        for(j in seq(along = object@analysis.summary[[1]])){
          no.times.selected <- object@model.selection[names(object@model.selection) == as.character(j)]
          if(length(no.times.selected) == 0){
            no.times.selected <- 0
          }
          cat("      Model ", j, ": ", as.character(object@analysis.summary[[1]][[j]]), " was selected ", no.times.selected, " time(s).", fill = TRUE)
        }
      }else{
        cat("  ",names(object@analysis.summary)[i], " = ", object@analysis.summary[[i]], fill = TRUE)
      }
    }
    cat("\nSummary for Individuals")      
    cat("\n\nSummary Statistics\n\n")
    print(object@individuals$summary)
    cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\nEstimates of Abundance (N)\n\n")
    print(round(object@individuals$N,2))
    cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\nEstimates of Density (D)\n\n")
    print(object@individuals$D)
    cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    if(length(object@clusters) > 0){
      cat("\n\nSummary for Clusters")      
      cat("\n\nSummary Statistics\n\n")
      print(object@clusters$summary)
      cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\nEstimates of Abundance (N)\n\n")
      print(round(object@clusters$N,2))
      cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\nEstimates of Density (D)\n\n")
      print(object@clusters$D)
      cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~") 
      cat("\nEstimates of Expected Cluster Size\n\n")
      print(round(object@expected.size,2))
      cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    }
    cat("\n\nDetection Function Values\n\n")
    print(round(object@detection,2))
    invisible(NULL)    
  }
)                                  
                                       
