#' @importFrom stats as.formula 
#' @importFrom stats predict
calculate.fitted <- function(ddf.model){
  # Calculates the new fitted values for the additional data points
  # These will have been added to the data and xmat parts of the ddf 
  # object already.
  # Also updates the data matrix (dm) 
  # AUTHOR: L Marshall
  #Extract values
  xmat <- ddf.model$ds$aux$ddfobj$xmat
  no.obs <- nrow(xmat)
  call.names <- names(ddf.model$call$dsmodel[[2]])
  #Find out if there are adjustments
  if("adj.series" %in% call.names){
    adj.terms <- TRUE
  }else{
    adj.terms <- FALSE
  }
  #Find out if there are covariates
  if(ddf.model$call$dsmodel[[2]]$formula == "~1"){
    covar <- FALSE
  }else{
    covar <- TRUE
  }
  key.function <- ddf.model$ds$aux$ddfobj$type
  if(!key.function %in% c("hn", "hr")){
    stop("This key function is not currently supported.", call. = FALSE)
  }
  #All have scale intercept
  no.scale.params <- length(ddf.model$ds$aux$ddfobj$scale$parameters)
  covar.names <- names(ddf.model$ds$aux$ddfobj$scale$parameters)[-1]
  new.dm.scale <- setcov(ddf.model$data, as.formula(ddf.model$ds$aux$ddfobj[["scale"]]$formula))
  ddf.model$ds$aux$ddfobj$scale$dm <- new.dm.scale
  if(key.function == "hr"){
    if(length(ddf.model$ds$aux$ddfobj$shape$parameters) > 1){
      stop("Currently can only deal with an intercept only shape parameter", call. = FALSE)
    }
    new.dm.shape <- matrix(1, nrow = no.obs, ncol = 1, dimnames = list(xmat$object, "(Intercept)")) 
    ddf.model$ds$aux$ddfobj$shape$dm <- new.dm.shape
  }
  #what is the truncation
  truncation <- ddf.model$meta.data$width
  if(is.null(truncation)){
    truncation <- max(ddf.model$data$distance)
  }
  new.fitted <- predict(ddf.model, ddf.model$data)$fitted
  names(new.fitted) <- ddf.model$data$object
  ddf.model$fitted <- new.fitted
  return(ddf.model)
}
