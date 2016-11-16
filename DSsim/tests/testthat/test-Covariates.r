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
  
  covariate.list[[1]] <- list(data.frame(level = c(1,2,3), prob = c(0.25,0.5,0.25)), data.frame(level = c(1,2,3), prob = c(0.2,0.6,0.2)))
  
  expect_error(check.covariates(covariate.list, 2), "Not all the elements of the covariate list are named. Please name all elements.")
  
  covariate.list <- list()
  covariate.list$size <- list(data.frame(level = c(1,2,3), prob = c(0.25,0.5,0.25)))
  covariate.list$height <- list(list("normal", list(lambda = 0.1)))
  
  expect_error(check.covariates(covariate.list, 1), "The distribution parameters for covariate height and strata 1 should be mu and sigma.")
  
  covariate.list$height <- list(list("poisson", list(lambda = 0.1)))
  expect_equal(check.covariates(covariate.list, 1), covariate.list)
  
  covariate.list$height <- list(list("negbinom", list(lambda = 0.1)))
  expect_error(check.covariates(covariate.list, 1), "The distribution for covariate height and strata 1 is not implemented at present. Please select from: normal, lognormal and poisson.")
  
  covariate.list <- list()
  covariate.list$size <- list(matrix(c(1,2,3, 0.25,0.5,0.25), byrow = FALSE, ncol = 2))
  covariate.list$height <- list(list("normal", list(mean = 1.8, sd = 0.1)))
  
  expect_error(check.covariates(covariate.list, 1), "Element 1 of your covariate list is not an accepted format. Please supply either a data.frame or a list")
  
  covariate.list <- list()
  covariate.list$size <- list(data.frame(level = c(1,2,3), prob = c(0.25,0.5,0.25)))
  covariate.list$height <- list(list("lognormal", list(logmean = 1.8, logsd = 0.1)))
  expect_equal(check.covariates(covariate.list, 1), covariate.list)
  
})
