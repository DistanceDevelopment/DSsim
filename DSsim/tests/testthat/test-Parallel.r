library(DSsim)
library(testthat)

context("Test Running in Parallel")

test_that("Simulations run in Parallel", {
  
  sim <- make.simulation()
  
  #Check it runs in parallel without issue
  test <- capture.output(simR1 <- run(sim, run.parallel = TRUE))
  expect_s4_class(simR1, "Simulation")
  simR2 <- run(sim, run.parallel = TRUE, counter = FALSE)
  expect_s4_class(simR2, "Simulation")

})
