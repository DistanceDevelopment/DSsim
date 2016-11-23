library(DSsim)
library(testthat)

context("Covariate Simulation Checks")

test_that("Checking of Covariate Input", {
  
  covariate.list <- list()
  covariate.list$size <- list(data.frame(level = c(1,2,3), prob = c(0.25,0.5,0.25)))
  covariate.list$height <- list(list("normal", list(mean = 1.8, sd = 0.1)))
  
  covariate.list2 <- check.covariates(covariate.list, 2)
  expect_equal(length(covariate.list2$size), 2)
  covariate.list3 <- check.covariates(covariate.list, 3)
  expect_equal(length(covariate.list3$height), 3)
  
  covariate.list$size <- list(data.frame(level = c(1,2,3), prob = c(0.25,0.5,0.25)), data.frame(level = c(1,2,3), prob = c(0.2,0.6,0.2)))
  
  expect_error(check.covariates(covariate.list, 3), "You have supplied an incorrect number of covariate distributions for cov size the list should either be of length 1 or equal to the number of strata.")
  
  covariate.list <- list()
  covariate.list[[1]] <- list(data.frame(level = c(1,2,3), prob = c(0.25,0.5,0.25)), data.frame(level = c(1,2,3), prob = c(0.2,0.6,0.2)))
  covariate.list$height <- list(list("normal", list(mean = 1.8, sd = 0.1)))
  
  expect_error(check.covariates(covariate.list, 2), "Not all the elements of the covariate list are named. Please name all elements.")
  
  covariate.list <- list()
  covariate.list$size <- list(data.frame(level = c(1,2,3), prob = c(0.25,0.5,0.25)))
  covariate.list$height <- list(list("normal", list(lambda = 0.1)))
  
  expect_error(check.covariates(covariate.list, 1), "The distribution parameters for covariate height and strata 1 should be mean and sd.")
  
  covariate.list$height <- list(list("poisson", list(lambda = 0.1)))
  expect_equal(check.covariates(covariate.list, 1), covariate.list)
  
  covariate.list$height <- list(list("negbinom", list(lambda = 0.1)))
  expect_error(check.covariates(covariate.list, 1), "The distribution for covariate height and strata 1 is not implemented at present. Please select from: normal, lognormal, poisson and ztruncpois.")
  
  covariate.list <- list()
  covariate.list$size <- list(matrix(c(1,2,3, 0.25,0.5,0.25), byrow = FALSE, ncol = 2))
  covariate.list$height <- list(list("normal", list(mean = 1.8, sd = 0.1)))
  
  expect_error(check.covariates(covariate.list, 1), "Element 1 of your covariate list is not an accepted format. Please supply either a data.frame or a list")
  
  covariate.list <- list()
  covariate.list$size <- list(data.frame(level = c(1,2,3), prob = c(0.25,0.5,0.25)))
  covariate.list$height <- list(list("lognormal", list(meanlog = 1.8, sdlog = 1.1)))
  expect_equal(check.covariates(covariate.list, 1), covariate.list)
})


test_that("Check calculate.scale.param function", {
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Simple Test
  
  # Make a basic region
  region <- make.region()
  # Set up covariates
  covariate.list <- list()
  covariate.list$size <- list(data.frame(level = c(1,2,3), prob = c(0.25,0.5,0.25)))
  covariate.list$height <- list(list("normal", list(mean = 1.8, sd = 0.1)))
  covariate.list$sex <- list(data.frame(level = c("male", "female"), prob = c(0.45,0.55)))
  # Create covariate description
  pop.desc <- make.population.description(covariates = covariate.list, N = 10)
  
  # define covariate parameters
  cov.params <- list(size = 0.5, height = 0.2, sex = data.frame(level = c("male", "female"), param = c(0,-0.5)))
  detect <- make.detectability(cov.param = cov.params)
  
  # Generate population
  pop <- generate.population(pop.desc, detect, region)
  # Test calculation of scale parameters
  test <- pop@population
  
  sex.param <- ifelse(test$sex == "female", -0.5, 0)
  expect_equal(as.vector(test$scale.param), exp(log(25) + test$size*0.5 + test$height*0.2 + sex.param))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Multi-strata Test but covariate param the same over strata
  
  # Make a multi strata region
  poly1 <- data.frame(x = c(0,0,100,100,0), y = c(0,100,100,0,0))
  poly2 <- data.frame(x = c(200,200,300,300,200), y = c(10,110,110,10,10))
  coords <- list(list(poly1), list(poly2))
  region <- make.region(coords = coords)
  density <- make.density(region)
  
  # Set up covariates
  covariate.list <- list()
  covariate.list$size <- list(data.frame(level = c(1,2,3), prob = c(0.25,0.5,0.25)))
  covariate.list$height <- list(list("normal", list(mean = 1.8, sd = 0.1)))
  covariate.list$sex <- list(data.frame(level = c("male", "female"), prob = c(0.45,0.55)))
  # Create covariate description
  pop.desc <- make.population.description(region.obj = region, density.obj = density, covariates = covariate.list, N = c(10,10))
  
  # define covariate parameters
  cov.params <- list(size = 0.5, height = 0.2, sex = data.frame(level = c("male", "female"), param = c(0,-0.5)))
  detect <- make.detectability(cov.param = cov.params)
  
  # Generate population
  pop <- generate.population(pop.desc, detect, region)
  # Test calculation of scale parameters
  test <- pop@population
  
  sex.param <- ifelse(test$sex == "female", -0.5, 0)
  expect_equal(as.vector(test$scale.param), exp(log(25) + test$size*0.5 + test$height*0.2 + sex.param))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Multi-strata Test and covariates param vary over strata
  # Also test that the categorical variables work corectly when varying by strata
  
  # Set up covariates
  covariate.list <- list()
  covariate.list$size <- list(data.frame(level = c(1,2,3), prob = c(0.25,0.5,0.25)), data.frame(level = c(2,3), prob = c(0.5,0.5)))
  covariate.list$height <- list(list("normal", list(mean = 1.8, sd = 0.1)))
  covariate.list$sex <- list(data.frame(level = c("male", "female"), prob = c(0.45,0.55)))
  # Create covariate description
  pop.desc <- make.population.description(region.obj = region, density.obj = density, covariates = covariate.list, N = c(10,10))
  
  # define covariate parameters
  cov.params <- list(size = c(0.5,0), sex = data.frame(level = c("male", "female","male", "female"),
                                                       strata = c("A", "A", "B", "B"),
                                                       param = c(0, -0.5, 0, 0)))
  detect <- make.detectability(cov.param = cov.params)
  
  # Generate population
  pop <- generate.population(pop.desc, detect, region)
  
  # Check that there are no clusters of group size 1 in strata B
  temp.B <- pop@population[pop@population$strata == 2,]
  expect_false(any(temp.B$size == 1))
  
  # Check that all animals in strata B have 25 for a scale parameter
  expect_true(all(temp.B$scale.param == exp(log(25))))
  
  # Check that the scale param values in strata A are still correct
  temp.A <- pop@population[pop@population$strata == 1,]
  sex.param <- ifelse(temp.A$sex == "female", -0.5, 0)
  expect_equal(as.vector(temp.A$scale.param), exp(log(25) + temp.A$size*0.5 + sex.param))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Multi-strata Test and covariates param vary over strata
  # Also test different distributions
  
  # Set up covariates
  covariate.list <- list()
  covariate.list$size <- list(list("ztruncpois", list(mean = 3)),
                              list("ztruncpois", list(mean = 10)))
  covariate.list$height <- list(list("normal", list(mean = 1.8, sd = 0.1)))
  covariate.list$sex <- list(data.frame(level = c("male", "female"), prob = c(0.45,0.55)))
  # Create covariate description
  pop.desc <- make.population.description(region.obj = region, density.obj = density, covariates = covariate.list, N = c(10,10))
  
  # define covariate parameters
  cov.params <- list(size = c(0.5,0), sex = data.frame(level = c("male", "female","male", "female"),
                                                       strata = c("A", "A", "B", "B"),
                                                       param = c(0, -0.5, 0, 0.1)))
  detect <- make.detectability(cov.param = cov.params)
  
  # Generate population
  pop <- generate.population(pop.desc, detect, region)
  
  # Check that there are no clusters of group size 1 in strata B
  pop.data <- pop@population
  expect_false(any(pop.data$size == 0))
  
  # Check that all animals in strata B have 25 for a scale parameter
  temp.B <- pop@population[pop@population$strata == 2,]
  expect_true(all(temp.B$scale.param[temp.B$sex == "male"] == exp(log(25))))
  expect_true(all(temp.B$scale.param[temp.B$sex == "female"] == exp(log(25)+0.1)))
  
  # Check that the scale param values in strata A are still correct
  temp.A <- pop@population[pop@population$strata == 1,]
  sex.param <- ifelse(temp.A$sex == "female", -0.5, 0)
  expect_equal(temp.A$scale.param, exp(log(25) + temp.A$size*0.5 + sex.param))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test other covariate distributions
  
  # Set up covariates
  covariate.list <- list()
  covariate.list$size <- list(list("ztruncpois", list(mean = 5)),
                              list("poisson", list(lambda = 30)))
  covariate.list$height <- list(list("lognormal", list(meanlog = log(2), sdlog = log(1.25))))
  covariate.list$sex <- list(data.frame(level = c("male", "female"), prob = c(0.45,0.55)), data.frame(level = c("male", "female"), prob = c(0,1)))
  # Create covariate description
  pop.desc <- make.population.description(region.obj = region, density.obj = density, covariates = covariate.list, N = c(10,10))
  
  # define covariate parameters
  cov.params <- list(size = c(0.5,0), sex = data.frame(level = c("male", "female","male", "female"),
                                                       strata = c("A", "A", "B", "B"),
                                                       param = c(0, -0.5, 0, 0.1)))
  detect <- make.detectability(cov.param = cov.params)
  
  # Generate population
  pop <- generate.population(pop.desc, detect, region)
  
  # Check covairate values are as expected
  temp.A <- pop@population[pop@population$strata == 1,]
  temp.B <- pop@population[pop@population$strata == 2,]
  expect_true(mean(temp.A$size) < mean(temp.B$size))
  expect_true(all(temp.B$sex == "female"))

  # Check that the scale param values in strata A are still correct
  sex.param <- ifelse(temp.A$sex == "female", -0.5, 0)
  expect_equal(temp.A$scale.param, exp(log(25) + temp.A$size*0.5 + sex.param))
  expect_equal(temp.B$scale.param, rep(exp(log(25) + 0.1),10))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test hr 
  
  # Set up covariates
  covariate.list <- list()
  covariate.list$size <- list(list("ztruncpois", list(mean = 5)),
                              list("poisson", list(lambda = 30)))
  covariate.list$height <- list(list("lognormal", list(meanlog = log(2), sdlog = log(1.25))))
  covariate.list$sex <- list(data.frame(level = c("male", "female"), prob = c(0.45,0.55)), data.frame(level = c("male", "female"), prob = c(0,1)))
  # Create covariate description
  pop.desc <- make.population.description(region.obj = region, density.obj = density, covariates = covariate.list, N = c(10,10))
  
  # define covariate parameters
  cov.params <- list(size = c(0.5,0), sex = data.frame(level = c("male", "female","male", "female"),
                                                       strata = c("A", "A", "B", "B"),
                                                       param = c(0, -0.5, 0, 0.1)))
  detect <- make.detectability("hr", shape.param = c(2,3), cov.param = cov.params)
  
  # Generate population
  pop <- generate.population(pop.desc, detect, region)
  pop.data <- pop@population
  
  temp.A <- pop.data[pop.data$strata == 1,]
  temp.B <- pop.data[pop.data$strata == 2,]
  
  expect_true(all(temp.A$shape == 2))
  expect_true(all(temp.B$shape == 3))
})
