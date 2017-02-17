library(DSsim)
library(testthat)

context("Systematic Line Transect Checks")

test_that("Check systematic line generation with strata and gaps", {
 
  coords <- gaps <- list()
  poly1 <- data.frame(x = c(0,0,1000,1000,0), y = c(0,500,500,0,0))
  poly2 <- data.frame(x = c(1500,1500,2000,2000,1500), y = c(0,500,500,0,0))
  poly3 <- data.frame(x = c(100,100,1600,1600,100), y = c(600,900,900,600,600))
  gap1 <- data.frame(x = c(400,600,600,400,400), y = c(200,200,300,300,200))
  gap2 <- data.frame(x = c(500,1000,1000,500,500), y = c(700,700,800,800,700))
  coords[[1]] <- list(poly1, poly2)
  coords [[2]] <- list(poly3)
  gaps[[1]] <- list(gap1)
  gaps[[2]] <- list(gap2)
  region <- make.region(coords = coords, gaps = gaps)
  
  expect_that(region@area[1], equals(1000*500 + 500*500 - 200*100))
  expect_that(region@area[2], equals(1500*300 - 500*100))
  
  density <- make.density(region)
  density <- add.hotspot(density, c(1000,400), 500, 0.5)
  
  expect_that(length(density@density.surface), equals(2))
  
  pop <- make.population.description(region,density, N = c(750,150))
  
  sim <- make.simulation(region.obj = region, population.description.obj = pop)
  
  test <- create.survey.results(sim, dht.tables = TRUE)
  
  # Check population size
  population <- test@population
  expect_that(nrow(population@population), equals(sum(population@N)))
  
  # Check transect lengths
  transects <- test@transects
  expect_that(sort(unique(transects@sampler.info$length)), equals(c(200,300,400,500)))

})