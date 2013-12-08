#' Displays a glossary
#'
#' Displays a glossary of the terms supplied by the Summary.Simulation
#' object. 
#' 
#' @export
#' @author Laura Marshall
#' 
display.glossary <- function(){
  cat("\nSummary Statistics\n\n")
  cat("mean.Cov.Area: mean covered area\n")
  cat("mean.Effort:   mean effort\n")
  cat("mean.n:        mean number of observations\n")
  cat("no.zero.n:     number of simulations when there were no observations\n")
  cat("mean.ER:       mean encounter rate\n")
  cat("mean.se.ER:    mean of the encounter rate standard deviations across\n")
  cat("               replications\n")
  cat("sd.mean.ER:    standard deviation of the mean encounter rates\n")
  cat("\nEstimates of Abundance (N) / Density (D)\n\n")
  cat("Truth:            true value\n")
  cat("mean.Estimate:    mean of the estimates\n")
  cat("percent.bias:     percentage bias\n")
  cat("CI.coverage.prob: proportion of times the true value is captured in \n")
  cat("                  the 95% confidence intervals\n")
  cat("mean.se:          mean standard error\n")
  cat("sd.of.means:      standard deviation of the estimates\n")
  cat("\nDetection Function Values\n\n")
  cat("mean.observed.Pa: mean observed probability of detection when in \n")
  cat("                  covered region (Pa).\n")
  cat("mean.estimate.Pa: mean estimated Pa.\n")
  cat("sd.estimate.Pa:   standard deviation of the estimates of Pa\n")
  cat("mean.ESW:         mean of the estimated strip widths (ESW)\n")
  cat("sd.ESW:           standard deviation of the ESWs\n")
  
}