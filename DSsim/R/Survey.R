
################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY

#' Virtual Class "Survey" 
#' 
#' Class \code{"Survey"} is an S4 class containing an instance of a population. 
#'
#' @name Survey-class
#' @docType class
#' @section Slots: 
#' \describe{
#'  \item{\code{population}}{Object of class \code{"Population"}; an instance of
#'  a population.}
#'  }
#' @section Methods:
#' \describe{
#'  \item{\code{create.region.table}}{\code{signature=(object = "Survey", ...)}: 
#'  creates a region table for \code{dht}.}
#' }
#' @keywords classes         
#' @export
setClass("Survey", representation(population = "Population", "VIRTUAL")) 

################################################################################
# GENERIC METHODS
################################################################################

#' @rdname create.region.table-methods
#' @aliases create.region.table,Survey-method
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





