library(DSsim)
library(testthat)

context("Constructor Checks")

test_that("Can create object or return correct error messages", {

  #Test region constructor, invoke errors
  coords <- gaps <- list(1)
  coords[[1]] <- list(data.frame(x = c(0,0,100,100,0), y = c(0,100,100,0,0)))
  #Commented out as error messages cannot match due to quotes?
  expect_that(make.region(region.name = "Region", 
                                    strata.name = c("strata"), 
                                    units = "m", coords = coords),
              throws_error("The lengths of the coords and gaps lists differ, these must be the same and equal to the number of strata."))
  
  expect_that(make.region(region.name = "Region", 
                                    strata.name = c("strata"), 
                                    units = "m"),
              throws_error("You must provide either coordinates or a shapefile."))
  
  gaps[[1]] <- list()
  
  region <- make.region(region.name = "StudySite",
                        units = "km",
                        coords = coords,
                        gaps = gaps)
  
  #Test area calculator
  expect_that(region@area, equals(100^2))
  
  gaps[[1]] <- list(data.frame(x = c(45,55,55,45,45), y = c(45,45,55,55,45)))
  region <- make.region(region.name = "StudySite",
                        units = "km",
                        coords = coords,
                        gaps = gaps)
  
  expect_that(region@area, equals(100^2-10^2))
  
  # Test density surface creation
  density <- make.density(region, x.space = 2.5, y.space = 2.5, constant = 10)
  expect_that(all(density@density.surface[[1]]$density == 10), equals(TRUE))
  density <- add.hotspot(density, centre = c(20,80), sigma = 20, amplitude = 5)
  density <- add.hotspot(density, centre = c(80,30), sigma = 20, amplitude = -2.5)
  
  # Fit a gam
  ddata <- density@density.surface[[1]]
  fit.gam <- mgcv::gam(density~s(x,y), data = ddata)
  
  # Try creating a density object from the gam results
  density <- make.density(region, x.space = 2, y.space = 2, density.gam = fit.gam)
  density.grid <- density@density.surface
  density2 <- make.density(region, density.surface = density.grid, x.space = 2, y.space = 2)
  #Check these two things are identical
  expect_identical(density, density2)
  
  # Test failure when nothing is supplied
  expect_that(make.density(region, x.space = 2, y.space = 2),
              throws_error("All strata must have some cells with non-zero density. Check that you have correctly specified your density grid. Large grid spacing may also generate this error."))
  
})
          