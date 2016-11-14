library(DSsim)
library(testthat)

context("Line Transect Simulation Checks")

test_that("Can perform basic simulation", {
  
  #Create Region
  coords <- gaps <- list(2)
  coords[[1]] <- list(data.frame(x = c(0,0,5,5,0), y = c(0,5,5,0,0)))
  coords[[2]] <- list(data.frame(x = c(5,5,15,15,5), y = c(0,5,5,0,0)))
  gaps[[1]] <- list()
  gaps[[2]] <- list()
  expect_that(region <- make.region(region.name = "Region", 
                                    strata.name = c("A", "Total"), 
                                    units = "km", coords = coords, gaps = gaps),
              throws_error("'Total' is not an accepted strata name, please ammend it."))
  expect_that(region <- make.region(region.name = "Region", 
                                    strata.name = c("A", "B"), 
                                    units = "km", coords = coords, gaps = gaps),
              is_a("Region"))
  
  #Create Density
  expect_that(density <- make.density(region, 
                                      x.space = 0.2, 
                                      y.space = 0.2, 
                                      constant = c(0.5,0.5)),
              is_a("Density"))
  expect_that(density <- add.hotspot(density, c(5,2), 5, 0.5),
              is_a("Density"))
  
  #Create Population Description
  expect_that(pop.desc <- make.population.description(region, 
                                                      density, 
                                                      N = c(300,500), 
                                                      fixed.N = TRUE),
              is_a("Population.Description"))
  
  #Create Detectability
  expect_that(detect <- make.detectability(key.function = "hn", 
                                 scale.param = 0.25, 
                                 truncation = 0.5),
              is_a("Detectability"))
  
  #Create Design
  expect_that(design <- make.design(transect.type = "line", 
                                    design.details = c("parallel", "systematic"), 
                                    spacing = 1.5, 
                                    design.axis = 0,
                                    path = "fortest"),
              is_a("LT.Systematic.Design"))
  
  #Create Analyses
  analyses <- make.ddf.analysis.list(dsmodel = list(~cds(key = "hn", formula = ~1)), method = "ds", truncation = 0.5)
  
  expect_that(analyses, is_a("list"))
  expect_that(analyses[[1]], is_a("DDF.Analysis"))
  
  #Create Simulation
  expect_that(my.sim <- make.simulation(reps = 3, 
                                        single.transect.set = TRUE, 
                                        region.obj = region, 
                                        design.obj = design,
                                        population.description.obj = pop.desc, 
                                        detectability.obj = detect, 
                                        ddf.analyses.list = analyses),
              is_a("Simulation"))
  
  #Create a population
  expect_that(pop <- generate.population(pop.desc, detect, region),
              is_a("Population"))
  expect_that(nrow(pop@population), equals(sum(pop.desc@N)))
  
  #Manually make a line transect object
  sampler.info <- data.frame(ID = 1:10,
                             start.X = seq(0.132, 15, by = 1.5),
                             start.Y = rep(0,10),
                             end.X = seq(0.132, 15, by = 1.5),
                             end.Y = rep(5,10),
                             length = rep(5,10),
                             region = c(rep("A",4),rep("B",6)),
                             d7.length = rep(NA,10))
  expect_that(line.transect <- new(Class = "Line.Transect", 
                                   sampler.info = sampler.info),
              is_a("Line.Transect"))
  
  #Manually make a survey object
  expect_that(survey <- new(Class = "Single.Obs.LT.Survey",
                            population = pop,
                            line.transect = line.transect,
                            perp.truncation = detect@truncation),
              is_a("Single.Obs.LT.Survey"))
  
  #Generate detections
  expect_that(survey.results <- create.survey.results(survey,
                                                      dht.tables = TRUE,
                                                      region = region),
              is_a("list"))
  
  expect_that(ddf.data <- survey.results$ddf.data,
              is_a("DDF.Data"))
  expect_that(length(which(ddf.data@ddf.dat$distance > detect@truncation)), equals(0))
  
  expect_that(obs.table <- survey.results$obs.table,
              is_a("Obs.Table"))
  
  expect_that(sample.table <- survey.results$sample.table,
              is_a("Sample.Table"))
  expect_that(nrow(sample.table@sample.table), equals(10))
  
  expect_that(region.table <- survey.results$region.table,
              is_a("Region.Table"))
  expect_that(nrow(region.table@region.table), equals(2))
  expect_that(region.table@region.table$Area, equals(c(25,50)))
  
  expect_that(survey.results <- new(Class = "Survey.Results",
                                    region = region,
                                    population = pop,
                                    transects = line.transect,
                                    ddf.data = ddf.data,
                                    obs.table = obs.table,
                                    sample.table = sample.table,
                                    region.table = region.table),
              is_a("Survey.Results"))
  
  #plot(survey.results)
  
})