
################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
#' Class "Survey" 
#' 
#' Class \code{"Survey"} is an S4 class containing an instance of a population
#' and a set of transects. 
#'
#' @name Survey-class
#' @docType class
#' @section Objects from the Class: Objects which inherit from this class 
#' are usually created internally, however, an instance is also contained in the 
#' object returned from \code{simulate.survey(simulation)}
#' @keywords classes         
#' @export
setClass("Survey", representation(population = "Population", "VIRTUAL")) 

################################################################################
# GENERIC METHODS
################################################################################


setMethod(
  f="create.region.table",
  signature="Survey",
  definition=function(object, region){
    population <- object@population
    if(length(region@strata.name) > 0){       
      region.table <- data.frame(Region.Label = region@strata.name, Area = get.area(region))
    }else{
      region.table <- data.frame(Region.Label = region@region.name, Area = get.area(region))
    }
    region.table.obj <- new(Class = "Region.Table", data = region.table)
    return(region.table.obj)
  }    
)  





