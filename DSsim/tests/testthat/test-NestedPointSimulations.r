library(DSsim)
library(testthat)

context("Nested Point Transect Simulation Checks")

test_that("Can perform nested point transect simulations", {
  
  #Create Region
  coords <- gaps <- list(2)
  coords[[1]] <- list(data.frame(x = c(0,0,5,5,0), y = c(0,5,5,0,0)))
  coords[[2]] <- list(data.frame(x = c(5,5,20,20,5), y = c(0,5,5,0,0)))
  gaps[[1]] <- list()
  gaps[[2]] <- list(data.frame(x = c(10,15,11.5,10), y = c(1,3,4,1)))
  expect_that(region <- make.region(region.name = "Region", 
                                    strata.name = c("A", "B"), 
                                    units = "km", coords = coords, gaps = gaps),
              is_a("Region"))
  
  #Create Density
  expect_that(density <- make.density(region, 
                                      x.space = 0.25, 
                                      y.space = 0.25, 
                                      constant = c(0.5,0.5)),
              is_a("Density"))
  expect_that(density <- add.hotspot(density, c(15,2), 5, 0.5),
              is_a("Density"))
  
  #Create Population Description
  size <- data.frame(size = c(1,2,3), prob = c(0.25,0.5,0.25))
  expect_that(pop.desc <- make.population.description(region, 
                                                      density, 
                                                      #cluster.size.table = size,
                                                      N = c(150,1000), 
                                                      fixed.N = TRUE),
              is_a("Population.Description"))
  
  #Create Detectability
  expect_that(detect <- make.detectability(key.function = "hn", 
                                           scale.param = 0.25,
                                           truncation = 0.75),
              is_a("Detectability"))
  
  #Create Design
  expect_that(design <- make.design(transect.type = "point", 
                                    design.details = "nested", 
                                    spacing = c(0.75,1), 
                                    design.axis = c(0,0),
                                    nested.space = c(2,2)),
              is_a("PT.Nested.Design"))
  
  #Create Analyses
  analyses <- make.ddf.analysis.list(dsmodel = list(~cds(key = "hn", formula = ~1),~cds(key = "hr", formula = ~1)), method = "ds", truncation = 0.75)
  
  expect_that(analyses, is_a("list"))
  expect_that(analyses[[2]], is_a("DDF.Analysis"))
  
  #Create Simulation
  expect_that(my.sim <- make.simulation(reps = 3, 
                                        single.transect.set = FALSE, 
                                        region.obj = region, 
                                        design.obj = design,
                                        population.description.obj = pop.desc, 
                                        detectability.obj = detect, 
                                        ddf.analyses.list = analyses),
              is_a("Simulation"))
  
  #Create a population
  set.seed(752)
  expect_that(pop <- generate.population(pop.desc, detect, region),
              is_a("Population"))
  expect_that(nrow(pop@population), equals(sum(pop.desc@N)))
  set.seed(752)
  expect_that(pop2 <- generate.population(my.sim),
              is_a("Population"))
  expect_that(pop, is_identical_to(pop2))
  
  #Generate transects
  set.seed(747)
  expect_that(point.transect <- generate.transects(design,
                                                   region = region),
              is_a("Point.Transect"))
  set.seed(747)
  expect_that(point.transect2 <- generate.transects(my.sim),
              is_a("Point.Transect"))
  expect_that(point.transect, is_identical_to(point.transect2))
  
  #plot(density, style = "blocks")
  #plot(region, add = T)
  #plot(point.transect)
  
  #Generate detections
  expect_that(survey.results <- create.survey.results(my.sim,
                                                      dht.tables = TRUE),
              is_a("Survey.Results"))
  
  expect_that(ddf.data <- survey.results@ddf.data,
              is_a("DDF.Data"))
  expect_that(length(which(ddf.data@ddf.dat$distance > detect@truncation)), equals(0))
  
  expect_that(obs.table <- survey.results@obs.table,
              is_a("Obs.Table"))
  
  expect_that(sample.table <- survey.results@sample.table,
              is_a("Sample.Table"))
  
  expect_that(region.table <- survey.results@region.table,
              is_a("Region.Table"))

  plot(survey.results)
  
  expect_that(nrow(ddf.data@ddf.dat),equals(427))
              #equals(448))
  
  with.dists <- ddf.data@ddf.dat[!is.na(ddf.data@ddf.dat$distance),]
  
  expect_that(nrow(with.dists), equals(108))
              #equals(119))
  
  
})