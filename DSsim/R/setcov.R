#' @importFrom stats as.formula
setcov <- function(dmat, model){
  # dmat: data matrix
  # model: model formula
  # returns a design matrix for the specified data and model
  # author: Jeff Laake
  # Null Model
  if(model=="~."){
    n <- 0
    x <- NULL
    # Intercept Model
  }else if(model == "~1"){
    n <- 1
    x <- matrix(rep(1, nrow(dmat)), ncol=1)
    colnames(x) <- "(Intercept)"
    # Covariate Model
  }else{
    x <- stats::model.matrix(as.formula(model), data = dmat)
  }
  return(x)
}