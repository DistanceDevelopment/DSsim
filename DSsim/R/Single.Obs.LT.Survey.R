################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

#' Class "Single.Obs.LT.Survey" 
#' 
#' An S4 class containing an instance of a population
#' and a set of transects. 
#'
#' @name Single.Obs.LT.Survey-class
#' @docType class
#' @keywords classes         
#' @export

setClass(Class = "Single.Obs.LT.Survey",
         contains = "LT.Survey"
)

setMethod(
  f="initialize",
  signature="Single.Obs.LT.Survey",
  definition=function(.Object, population, line.transect, perp.truncation){
    #Input pre-processing
    #full.data <- calculate.distances(get(population), get(survey))
    #dist.data <- simulate.detection(full.data, pop@detectability)
    #Set slots
    #.Object@distance.data <- dist.data
    #.Object@full.data     <- full.data
    .Object@population    <- population
    .Object@line.transect <- line.transect
    .Object@perpendicular.truncation <- perp.truncation
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("Single.Obs.LT.Survey",
  function(object){
    return(TRUE)
  }
)

################################################################################
# GENERIC METHODS
################################################################################


#' @rdname create.survey.results-methods
#' @aliases create.survey.results,Single.Obs.LT.Survey-method
setMethod(
  f="create.survey.results",
  signature="Single.Obs.LT.Survey",
  definition=function(object, dht.tables = FALSE, ...){
    population <- object@population
    line.transect <- object@line.transect
    poss.distances <- calc.poss.detect.dists(population, line.transect, perp.truncation = object@perpendicular.truncation)
    n.in.covered <- nrow(poss.distances)
    dist.data <- simulate.detections(poss.distances, population@detectability)
    dist.data <- rename.duplicates(dist.data)
    dist.data <- dist.data[,c("object", "transect.ID", "distance", "x", "y")]    
    ddf.data.obj <- new(Class = "Single.Obs.DDF.Data", data = dist.data)
    if(dht.tables){
      region.table <- create.region.table(object, ...)
      sample.table <- create.sample.table(object)
      obs.table <- data.frame(object = dist.data$object, Sample.Label = dist.data$transect.ID)
      obs.table <- merge(obs.table, sample.table@sample.table, by = "Sample.Label")[,c("object","Sample.Label","Region.Label")] 
      obs.table.obj <- new(Class = "Obs.Table", data = obs.table)
      return(list(ddf.data = ddf.data.obj, obs.table = obs.table.obj, sample.table = sample.table, region.table = region.table, n.in.covered = n.in.covered))
    }else{
      return(list(ddf.data = ddf.data.obj, n.in.covered = n.in.covered))
    }
  }    
) 

 









   




