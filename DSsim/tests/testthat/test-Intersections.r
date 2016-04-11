library(DSsim)
library(testthat)

context("Intersection Checks")

test_that("Correctly identifies intersections", {
  
  #Function for calculating perpendicular distance to transect
  get.perp.dists <- function(transects, individual){
    x.coord <- individual['x']
    y.coord <- individual['y']
    transect.angle <- atan2(transects[["end.Y"]]-transects[["start.Y"]], transects[["end.X"]]-transects[["start.X"]]) 
    animal.angle   <- atan2(y.coord-transects[["start.Y"]], x.coord-transects[["start.X"]])
    delta.angle <- abs(animal.angle-transect.angle)
    delta.angle <- (ifelse(delta.angle > pi, 2*pi - delta.angle, delta.angle))
    hyp         <- sqrt((y.coord-transects[["start.Y"]])^2+(x.coord-transects[["start.X"]])^2)
    all.perp.dists  <- hyp*sin(delta.angle) 
  }
  
  #Create transects
  transects <- data.frame(start.X = c(0,0,0,0,0), start.Y = c(0,0,0,0,0), end.X = c(0,0.005,3,3,3), end.Y = c(3,3,3,0.005,0))
  #Find the transect length
  lengths <- rep(NA,nrow(transects))
  for(i in seq(along = transects$start.X)){
    delta.x <- abs(transects$end.X[i] - transects$start.X[i])
    delta.y <- abs(transects$end.Y[i] - transects$start.Y[i])
    lengths[i] <- sqrt(delta.x^2 + delta.y^2)
  }
  transects$length <- lengths
  
  #Plot transects
#   plot(c(-0.5,3.5,3.5,-0.5,-0.5), c(-0.5,-0.5,3.5,3.5,-0.5), type = "l")
#   lines(c(transects$start.X[1],transects$end.X[1]), c(transects$start.Y[1],transects$end.Y[1]), lwd = 2, col = 1)
#   lines(c(transects$start.X[2],transects$end.X[2]), c(transects$start.Y[2],transects$end.Y[2]), lwd = 2, col = 2)
#   lines(c(transects$start.X[3],transects$end.X[3]), c(transects$start.Y[3],transects$end.Y[3]), lwd = 2, col = 3)
#   lines(c(transects$start.X[4],transects$end.X[4]), c(transects$start.Y[4],transects$end.Y[4]), lwd = 2, col = 4)
#   lines(c(transects$start.X[5],transects$end.X[5]), c(transects$start.Y[5],transects$end.Y[5]), lwd = 2, col = 5)
  
  #Test 1 - all TRUE
  point <- c(x = 1, y = 2)
  p.dist <- get.perp.dists(transects, individual = point)
  test.results <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = p.dist), 1, FUN = check.intersection, point = data.frame(x = point['x'], y = point['y']), display.diagnostics = FALSE)
  expect_that(test.results, is_identical_to(rep(TRUE,5)))
  
  #Test 2 - FALSE, FALSE, TRUE, TRUE, TRUE
  point <- c(x = 1, y = 3.1)
  p.dist <- get.perp.dists(transects, individual = point)
  test.results <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = p.dist), 1, FUN = check.intersection, point = data.frame(x = point['x'], y = point['y']), display.diagnostics = FALSE)
  expect_that(test.results, is_identical_to(c(rep(FALSE,2), rep(TRUE,3))))
  
  #Test 3 - TRUE, FALSE, TRUE, TRUE, TRUE
  point <- c(x = 1, y = 3)
  p.dist <- get.perp.dists(transects, individual = point)
  test.results <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = p.dist), 1, FUN = check.intersection, point = data.frame(x = point['x'], y = point['y']), display.diagnostics = FALSE)
  expect_that(test.results, is_identical_to(c(TRUE, FALSE, rep(TRUE,3))))
  
  #Test 4 - all TRUE
  point <- c(x = 1, y = 0)
  p.dist <- get.perp.dists(transects, individual = point)
  test.results <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = p.dist), 1, FUN = check.intersection, point = data.frame(x = point['x'], y = point['y']), display.diagnostics = FALSE)
  expect_that(test.results, is_identical_to(rep(TRUE,5)))
  
  #Test 5 - all TRUE
  point <- c(x = 0, y = 0)
  p.dist <- get.perp.dists(transects, individual = point)
  test.results <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = p.dist), 1, FUN = check.intersection, point = data.frame(x = point['x'], y = point['y']), display.diagnostics = FALSE)
  expect_that(test.results, is_identical_to(rep(TRUE,5)))
  
  #Test 6 - TRUE, FALSE, TRUE, FALSE, TRUE
  point <- c(x = 3, y = 3)
  p.dist <- get.perp.dists(transects, individual = point)
  test.results <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = p.dist), 1, FUN = check.intersection, point = data.frame(x = point['x'], y = point['y']), display.diagnostics = FALSE)
  expect_that(test.results, is_identical_to(c(TRUE, FALSE, TRUE, FALSE, TRUE)))
  
  #Test 7 - all FALSE
  point <- c(x = 1, y = 1.00000001)
  p.dist <- get.perp.dists(transects, individual = point)
  test.results <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = p.dist), 1, FUN = check.intersection, point = data.frame(x = point['x'], y = point['y']), display.diagnostics = FALSE)
  expect_that(test.results, is_identical_to(rep(TRUE,5)))
  
  #Test 8 - all FALSE
  point <- c(x = -0.00000001, y = -0.00000001)
  p.dist <- get.perp.dists(transects, individual = point)
  test.results <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = p.dist), 1, FUN = check.intersection, point = data.frame(x = point['x'], y = point['y']), display.diagnostics = FALSE)
  expect_that(test.results, is_identical_to(rep(FALSE,5)))
  
  #Create transects
  transects <- data.frame(start.X = c(-1,-3,-1,0,1), start.Y = c(2,-3,0,-2,-1), end.X = c(1,0,-4,2,2), end.Y = c(0,0,1,-1,-2))
  
  #Find the transect length
  lengths <- rep(NA,nrow(transects))
  for(i in seq(along = transects$start.X)){
    delta.x <- abs(transects$end.X[i] - transects$start.X[i])
    delta.y <- abs(transects$end.Y[i] - transects$start.Y[i])
    lengths[i] <- sqrt(delta.x^2 + delta.y^2)
  }
  transects$length <- lengths
  
  #Plot transects
#     plot(c(-4.5,2.5,2.5,-4.5,-4.5), c(-3.5,-3.5,2.5,2.5,-3.5), type = "l")
#     lines(c(transects$start.X[1],transects$end.X[1]), c(transects$start.Y[1],transects$end.Y[1]), lwd = 2, col = 1)
#     lines(c(transects$start.X[2],transects$end.X[2]), c(transects$start.Y[2],transects$end.Y[2]), lwd = 2, col = 2)
#     lines(c(transects$start.X[3],transects$end.X[3]), c(transects$start.Y[3],transects$end.Y[3]), lwd = 2, col = 3)
#     lines(c(transects$start.X[4],transects$end.X[4]), c(transects$start.Y[4],transects$end.Y[4]), lwd = 2, col = 4)
#     lines(c(transects$start.X[5],transects$end.X[5]), c(transects$start.Y[5],transects$end.Y[5]), lwd = 2, col = 5)
  
  #Test 9 - TRUE, TRUE, FALSE, TRUE, FALSE
  point <- c(x = 0, y = 0)
  p.dist <- get.perp.dists(transects, individual = point)
  test.results <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = p.dist), 1, FUN = check.intersection, point = data.frame(x = point['x'], y = point['y']), display.diagnostics = FALSE)
  expect_that(test.results, is_identical_to(c(TRUE, TRUE, FALSE, TRUE, FALSE)))
  
  #Test 10 - FALSE, TRUE, FALSE, TRUE, TRUE
  point <- c(x = 1, y = -1)
  p.dist <- get.perp.dists(transects, individual = point)
  test.results <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = p.dist), 1, FUN = check.intersection, point = data.frame(x = point['x'], y = point['y']), display.diagnostics = FALSE)
  expect_that(test.results, is_identical_to(c(FALSE, TRUE, FALSE, TRUE, TRUE)))
  
  #Test 11 - TRUE, TRUE, FALSE, TRUE, FALSE
  point <- c(x = -0.999999, y = 0)
  p.dist <- get.perp.dists(transects, individual = point)
  test.results <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = p.dist), 1, FUN = check.intersection, point = data.frame(x = point['x'], y = point['y']), display.diagnostics = FALSE)
  expect_that(test.results, is_identical_to(c(TRUE, TRUE, FALSE, TRUE, FALSE)))
  
  #Test 12 - FALSE, TRUE, FALSE, FALSE, FALSE
  point <- c(x = -4, y = 2)
  p.dist <- get.perp.dists(transects, individual = point)
  test.results <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = p.dist), 1, FUN = check.intersection, point = data.frame(x = point['x'], y = point['y']), display.diagnostics = FALSE)
  expect_that(test.results, is_identical_to(c(FALSE, TRUE, FALSE, FALSE, FALSE)))
  
  #Test 13 - TRUE, FALSE, FALSE, TRUE, FALSE
  point <- c(x = 1, y = 0.000001)
  p.dist <- get.perp.dists(transects, individual = point)
  test.results <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = p.dist), 1, FUN = check.intersection, point = data.frame(x = point['x'], y = point['y']), display.diagnostics = FALSE)
  expect_that(test.results, is_identical_to(c(TRUE, FALSE, FALSE, TRUE, FALSE)))
  
})
      