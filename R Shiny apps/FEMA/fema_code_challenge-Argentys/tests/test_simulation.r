source("../simulation/deployment_simulation.r", chdir = TRUE)
library(testthat)

test_that("deployments are properly generated", {

  deployments <- generate_deployments(1)
  
  # for that default input and 1 year of simulation we expect at least one deployment
  expect_gt(deployments$depl_num, 0)
  
  # we expect the lenght of arrival times array to equal to the number of deployments
  expect_equal(length(deployments$request_received_time), deployments$depl_num)
  
  
})

test_that("deployments are properly processed", {
  
  deployments <- generate_deployments(5)
  processed_deployments <- process_deployments(responder_num = 30,deployments)
  metrics <- calculate_metrics(processed_deployments)

  # we expect that for 5 years of simulations each responder gets to execute on 
  # at least one deployment 
  expect_equal(length(unique(processed_deployments$responder_id)), 30)
  expect_gt(metrics$util_rate,0.0)
  expect_lt(metrics$util_rate,1.0)
  
})

test_that("simulate_deployments works properly", {
  sim <- simulate_deployments()
  expect_gt(length(sim$data),0)
})
