library(DSsim)
library(testthat)

context("Constructor Checks")

test_that("Can create object or return correct error messages", {

  #Test region constructor, invoke errors
  coords <- gaps <- list(1)
  coords[[1]] <- list(data.frame(x = c(0,0,100,100,0), y = c(0,100,100,0,0)))
  expect_that(region <- make.region(region.name = "Region", 
                                    strata.name = c("strata"), 
                                    units = "m", coords = coords),
              throws_error("The lengths of the coords and gaps lists differ, these must be the same and equal to the number of strata.\n"))
  
  expect_that(region <- make.region(region.name = "Region", 
                                    strata.name = c("strata"), 
                                    units = "m"),
              throws_error("You must provide either coordinates or a shapefile."))
  
})
          