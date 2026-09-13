library(dplyr)
library(ggplot2)
library(tidyr)
fema = read.csv('mock_data.csv')

#EDA
#Distinct reponders = 30
cnt = count(distinct(fema, responder_id))
#request for each responder
fema %>% group_by(responder_id) %>% tally()
#average number of tasks per responder = 7
nrow(fema) / cnt
#min time
min(fema$request_received_time)
min(fema$deployment_start_time)
min(fema$deployment_end_time)

# Feature engineering
fema$deployment_time_in_days = with(fema, difftime(deployment_end_time,
                                                    deployment_start_time,
                                                    units="days") )

fema$deployment_delay_in_days = with(fema, difftime(deployment_start_time,
                                                     request_received_time,
                                                    units="days") )


fema$request_dayofweek = weekdays(as.Date(fema$request_received_time))
fema$deployment_start_dayofweek = weekdays(as.Date(fema$deployment_start_time))
fema$deployment_end_dayofweek = weekdays(as.Date(fema$deployment_end_time))
fema$request_month = months(as.Date(fema$request_received_time))

fema = fema %>% group_by( responder_id) %>%
  mutate(deployment_lag=lag(deployment_end_time, n=1, order_by = responder_id))
fema$new_assignment_lag = with(test, difftime(deployment_start_time,
                                              deployment_lag,
                                                    units="days") )
    

fema %>% group_by(request_dayofweek) %>% tally()
fema %>% group_by(deployment_start_dayofweek) %>% tally()
fema %>% group_by(deployment_end_dayofweek) %>% tally()
sum_hours = fema %>% group_by(responder_id) %>% summarize(Total_hours_worked = sum(deployment_time_in_days))
  
fema %>% group_by(request_month) %>% tally() %>% sort(request_month)
