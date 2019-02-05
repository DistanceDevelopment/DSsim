#' @title check.sim.setup 
#' @description A function which allows the user to check the simulation setup. It
#' displays a panel of 4 diagnostic plots: top left - study region with example 
#' population, top right - study region with transects, bottom left - an example
#' realisation of a survey with detected animals shown in cyan and undetected
#' animals shown in red, bottom right - a histogram of the example distances to 
#' which the detection function would be fitted.
#' @param simulation A simulation object created by a call to \code{make.simulation}
#' @return a invisible copy of the simulation
#' @export
#' @importFrom graphics par
#' @author Eric Rexstad, Laura Marshall
#' @examples 
#' \dontrun{
#' sim <- make.simulation(design.obj = make.design("point"))
#' check.sim.setup(sim)
#' }
check.sim.setup <- function(simulation) {
  #  small function to plot the simulation setup
  #
  #  input:
  #     simsetup - object created by make.simulation()
  #
  #  output:
  #     4-panel plot of study region with popn, study region with transects,
  #     study region with detected objects, and histogram of detection distances
  simulation <- dssim.update(simulation, warn = FALSE)
  eg.survey <- create.survey.results(simulation)  #simulate the survey process of detection
  dist.data <- get.distance.data(eg.survey)  #look at distance data
  oldparams <- par(mfrow = c(2,2))
  on.exit(par(oldparams))
  plot(simulation@population.description@density, style = "blocks", main = "Density Surface with Example Population")
  plot(simulation@region, add = TRUE)
  plot(eg.survey@population)
  plot(simulation@region, main = "Region with Example Transects")
  plot(eg.survey@transects, col = 4, lwd = 2)
  plot(simulation@region, main = "Transects with Example Detections")
  plot(eg.survey@transects)
  plot(eg.survey@ddf.data)
  x.label <- paste("Distance (", simulation@region@units, ")", sep = "")
  hist(dist.data$distance, xlab = x.label, main = "Example Distance Data")
  invisible(eg.survey)
}