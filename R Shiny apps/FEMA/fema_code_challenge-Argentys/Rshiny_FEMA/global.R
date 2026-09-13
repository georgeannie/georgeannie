options(warn=-1, message=-1)

list_of_packages = c("shiny","highcharter","shinydashboard","shinyWidgets",
                     "plotly","dplyr","lubridate","scales","shinycssloaders")

lapply(list_of_packages, 
       function(x) if(!require(x,character.only = TRUE)) install.packages(x))

library(highcharter)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(lubridate)
library(scales)
source("ui/dashboardHeader.R")
source("ui/dashboardSidebar.R")
source("ui/dashboardBody.R")
source("server/sim_bench_deployment.R")
source("server/insights_plots_output.R")
source('../simulation/deployment_simulation.r')

#source('dependencies.R')
#load all packages
#lapply(required_packages, require, character.only = TRUE)

# Exploratory Data Analysis
## Load and clean data

# Read in mock data file
fema = read.csv(file = "./data/Attachment 6 mock_data.csv", header = T)

# Truncate deployments that end in 2022
fema <- fema %>% 
  mutate(deployment_end_time = ifelse(deployment_end_time > "2022-01-01",
                                      as.character("2021-12-31T23:59:59Z"),
                                      deployment_end_time))

# Convert time features to time
fema$request_received_time <- as.POSIXct(fema$request_received_time,
                                         format="%Y-%m-%dT%H:%M:%S")
fema$deployment_start_time <- as.POSIXct(fema$deployment_start_time,
                                         format="%Y-%m-%dT%H:%M:%S")
fema$deployment_end_time   <- as.POSIXct(fema$deployment_end_time,
                                         format="%Y-%m-%dT%H:%M:%S")

## Generate features
# 1. Request gap time - all requests (request_time(n+1) - request_time(n))
fema <- fema %>%
  arrange(request_id) %>%
  mutate(gap_time_all = difftime(request_received_time,
                                 lag(request_received_time), units='days'))
# 2. Request gap time - by responder
fema <- fema %>%
  arrange(responder_id, request_received_time) %>%
  group_by(responder_id) %>%
  mutate(gap_time_resp = difftime(request_received_time,
                                  lag(request_received_time), units='days'))
# 3.  Fulfillment time (start_time - request_time)
fema$fulfill_time <- difftime(fema$deployment_start_time,
                              fema$request_received_time, units='days')
# 4. Execution time (end_time - start_time)
fema$execution_time <- difftime(fema$deployment_end_time,
                                fema$deployment_start_time, units='days')
#5.  Idle time (start_time(n+1) - end_time(n))
#fema$responder_id = as.factor(fema$responder_id)
fema <- fema %>%
  arrange(responder_id, request_received_time) %>%
  group_by(responder_id) %>%
  mutate(idle_time = difftime(deployment_start_time,
                              lag(deployment_end_time), units='days'))

fema$request_month = factor(month.abb[month(as.Date(fema$request_received_time))], levels=month.abb)
