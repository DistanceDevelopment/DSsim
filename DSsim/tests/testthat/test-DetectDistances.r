library(DSsim)
library(testthat)

context("Detection distance checks")

test_that("Tests calc.poss.detect.dists.lines functions", {
  
  coords <- gaps <- list()
  coords[[1]] <- list(data.frame(x = c(0,0,1000,1000,0), 
                                 y = c(0,500,500,0,0)))
  coords[[2]] <- list(data.frame(x = c(1000,1000,2000,2000,1000), 
                                 y = c(0,500,500,0,0)))
  gaps[[1]] <- list(data.frame(x = c(400,800,500,200,400), 
                               y = c(100,250,400,120,100)))
  gaps[[2]] <- list()
  
  region <- make.region(region.name = "study.area", units = "m", 
                        coords = coords, gaps = gaps)
  
  popn = make.population.description(region.obj = region, 
                              density.obj = make.density(region.obj = region, x.space = 10), 
                              N = c(2000,2000))
  popn@density <- add.hotspot(popn@density, c(500,500), 700, 1)
  #plot(popn@density, style = "blocks")
  #plot(region, add = TRUE)
  
  sim <- make.simulation(region.obj = region,
                         design.obj = make.design(region.obj = region),
                         population.description.obj = popn)
  
  set.seed(232)
  popn <- generate.population(sim)
  survey <- generate.transects(sim)
  
  #plot(region)
  #plot(popn)
  #plot(survey)
  
  # Test that the new code does the same as the old code
  test1 <- calc.poss.detect.dists.lines.largeN(population = popn, survey = survey, perp.truncation = 20)
  test2 <- calc.poss.detect.dists.lines(population = popn, survey = survey, perp.truncation = 20)
  
  index <- order(test1$object)
  test1 <- test1[index,]
  row.names(test1) <- 1:nrow(test1)
  
  expect_equal(test1, test2)
  
  # Test which points should be retained
  popn@population <- popn@population[1:8,] 
  popn@population$x <- c(1100,1100,290,290,400,392,100,1500)
  popn@population$y <- c(95,105,50,205,305,302,200,100)
  # Expected r, k, k, r, k, r, r, r
  
  survey@sampler.info <- survey@sampler.info[2:6,]
  survey@sampler.info[1,2:6] <- c(1100,100,1400,400,424.2641)
  survey@sampler.info$region[1] <- "B"
  
  test.new <- calc.poss.detect.dists.lines.largeN(population = popn, survey = survey, perp.truncation = 20)
  
  test.orig <- calc.poss.detect.dists.lines(population = popn, survey = survey, perp.truncation = 20)
  
  expect_equal(test.new, test.orig)
  expect_equal(test.new$object, c(2,3,5))
  
  # Test what happens when there are no possible detections
  popn@population <- popn@population[1:4,] 
  popn@population$x <- c(100,600,1500,1001)
  popn@population$y <- c(200,100,100,400)
  
  test.new <- calc.poss.detect.dists.lines.largeN(population = popn, survey = survey, perp.truncation = 20)
  
  test.orig <- calc.poss.detect.dists.lines(population = popn, survey = survey, perp.truncation = 20)
  
  expect_equal(nrow(test.new), nrow(test.orig))
  
})
  