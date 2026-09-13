library(highcharter)
source('server/custom_valuebox_function.R')
source('server/sparkline_function.R')

# VALUE BOX FOR REQUEST COUNT ------------------------------
request_count_vb_fn = function(){

  req_per_month =  fema %>%
    group_by(request_month)  %>%
    summarize(count = n()) %>% 
    arrange(request_month)
  
  hc = hchart(req_per_month, "area", hcaes(x=request_month, y = count), 
              name = "Number of Requests")  %>% 
    hc_size(height = '50') %>% 
    hc_credits(enabled = FALSE) %>% 
    hc_add_theme(hc_theme_sparkline_vb()) 
  
  request_count_vb <- valueBoxSpark(
    value = length(unique(fema$request_id)),
    title = toupper("Number of Requests"),
    sparkobj = hc,
    subtitle = NULL,
    width = 2,
    color = "light-blue",
    href = NULL
  )
  
}

# VALUE BOX FOR UTILIZATION RATE ------------------------------
util_vb_fn = function(){
  
  fema$execution_time = as.numeric(fema$execution_time)
  fema$idle_time = as.numeric(fema$idle_time)
  fema$days_in_month = days_in_month(fema$request_received_time)
  fema_year = fema%>% filter(deployment_end_time < "2022-01-01")
   
  utilization = sum(fema_year$execution_time)*100/ 365/length(unique(fema_year$responder_id))
  util_per_month =  fema_year %>%
    group_by(request_month)  %>%
    summarise(total_exec = as.numeric(sum(execution_time)),
              total_delay = as.numeric(sum(fulfill_time)),
              total_idle = as.numeric(sum(idle_time, na.rm = T))) %>%
    mutate(util_rate = round(total_exec * 100/(total_exec+total_idle), 0)) %>%
    arrange(request_month)

  util_vb <- valueBoxSpark(
    value = paste0(round(utilization,1), '%'),
    title = toupper("Utilization Rate for 2021"),
    sparkobj = NULL,
    subtitle = NULL,
    width = 2,
    color = "light-blue",
    href = NULL
  )
  
}


# VALUE BOX FOR AVG FULFILLMENT TIME ------------------------------
fulfill_vb_fn= function(){
  
  avg_fulfil=  round(mean(fema$fulfill_time), 0)
  avg_fulfil_hours =  round(mean(fema$fulfill_time) * 24, 0)
  text_avg_exe = paste0(avg_fulfil, ' Days (', avg_fulfil_hours, ' Hours)')
  mean_exe_vb <- valueBoxSpark(
    value = text_avg_exe,
    title = toupper("AVERAGE FULFILL TIME"),
    sparkobj = NULL,
    subtitle = NULL,
    # info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
    # icon = 'fa-box-archive',
    width = 2,
    color = "light-blue",
    href = NULL
  )
}

# VALUE BOX FOR MEAN EXECTION TIME ------------------------------
mean_exe_vb_fn = function(){
  
  avg_execution_days =  round(mean(fema$execution_time), 0)
  avg_execution_hours =  round(mean(fema$execution_time) * 24, 0)
  text_avg_exe = paste0(avg_execution_days, ' Days (', avg_execution_hours, ' Hours)')
  mean_exe_vb <- valueBoxSpark(
    value = text_avg_exe,
    title = toupper("AVERAGE EXECUTION TIME"),
    sparkobj = NULL,
    subtitle = NULL,
    # info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
    # icon = 'fa-box-archive',
    width = 2,
    color = "light-blue",
    href = NULL
  )
}


# VALUE BOX FOR UTILIZATION RATE SIMULATED ------------------------------
util_vb_fn_sim = function(sim_data, team_size){
  
  metric = sim_data$metrics
  util_for_responder = metric %>% 
    filter(responder_num==team_size) %>% 
    select(util_rate)
  request_count_vb <- valueBoxSpark(
    value = paste0(round(util_for_responder * 100, 1), '%'),
    title = toupper(paste0("Simulated Utilization Rate Based on ", team_size, " Responders")),
    sparkobj = NULL,
    subtitle = NULL,
    width = 4,
    color = "light-blue",
    href = NULL
  )
  
}

# VALUE BOX FOR MEAN FULFILLMENT TIME SIMULATION ------------------------------
mean_fulfill_vb_fn_sim = function(sim_data, team_size){
  
  metric = sim_data$metrics
  fulfil_sim = metric %>% 
    filter(responder_num==team_size) %>% 
    select(avg_fulfillment_time)

  request_count_vb <- valueBoxSpark(
    value = paste0(round(fulfil_sim/24, 0), " Days (", round(fulfil_sim,0), "Hours)" ),
    title = toupper(paste0("Simulated Average Fulfillment Time Based on ", team_size, " Responders")),
    sparkobj = NULL,
    subtitle = NULL,
    width = 3,
    color = "light-blue",
    href = NULL
  )
  
}

