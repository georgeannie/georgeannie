# STANDARD PLOTS USED IN ALL GGPLOT ------------------------------
std_plot = theme_minimal() +
  theme(axis.line = element_line(color='grey'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_blank(), 
        axis.line.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(color="#066DA6", size=14,  face = 'bold'),
        axis.title.y = element_text(color="#066DA6", size=14 ,face = 'bold'),
        axis.text = element_text( size=14),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5, color = "black", size = 16, face = 'bold'))

# STANDARD PLOTS USED IN GANTT CHART ------------------------------
std_plotly = theme_minimal() +
  theme(axis.line = element_line(color='grey'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_blank(), 
        axis.line.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1),
        axis.title.x = element_text(color="#066DA6", size=16, face='bold'),
        axis.title.y = element_text(color="#066DA6", size=16, face='bold'),
        axis.text = element_text( size=14),
        plot.title = element_text(hjust=0.5, color = "black", size = 18, face='bold'))

##FULFILLMENT BY MONTH ------------------------------
plots_fulfill_by_month = function(){
  fema_group_by_month <- fema %>% 
    group_by(request_month) %>%
     summarise(avg_fulfillment_time = round(mean(fulfill_time), 0)) %>%
    arrange(request_month)
  
  idle_months_missing = left_join(tibble("request_month" = month.abb),
                                  fema_group_by_month, by = "request_month") 
  
  fema_group_by_month <- idle_months_missing %>%
    mutate(avg_fulfillment_time = ifelse(is.na(avg_fulfillment_time),0,avg_fulfillment_time)) %>%
    mutate(request_month = factor(request_month, levels = month.abb))
  
  p <- ggplot(fema_group_by_month, aes(request_month, avg_fulfillment_time)) +
    geom_col(fill='#066DA6') + 
    ggtitle("Average Time To Fulfill Request") +
    ylab(paste("Days")) +
    xlab("Request Received Month") +
    geom_text(aes(label=round(avg_fulfillment_time, 0)), 
              vjust=-0.1, 
                size = 4)+
    std_plot
  p
  
}
# BENCH TIME BY MONTH -----------------------------------------------
plots_idle_by_month = function(){
    fema_idle_time_month = fema %>% 
      filter(idle_time > 0) %>%
      group_by(request_month) %>%
      summarise(avg_idle_time = round(mean(idle_time), 0)) %>%
      arrange(request_month)
    
    #set missing months as zero
    idle_months_missing = left_join(tibble("request_month" = month.abb),
                                    fema_idle_time_month, by = "request_month") 
    
    fema_idle_time_month <- idle_months_missing %>%
      mutate(avg_idle_time = ifelse(is.na(avg_idle_time),0,avg_idle_time)) %>%
      mutate(request_month = factor(request_month, levels = month.abb))
    
    p <- ggplot(fema_idle_time_month, aes(request_month, avg_idle_time)) +
      geom_col(fill='#066DA6') + 
      ggtitle("Average Bench Time") +
      labs(y="Days", x="Request Received Month") +
      geom_text(aes(label=round(avg_idle_time, 0)),
                vjust=-0.1,
                size = 4)+
      std_plot
    p
}

# EXECUTION TIME BY MONTH --------- ------------------------------
plots_execution_time = function(){
  fema_exe_time = fema %>% 
    group_by(request_month) %>%
     summarise(execution_time = round(mean(execution_time),0)) %>%
    arrange(request_month)
  
  p <- ggplot(fema_exe_time, aes(request_month, execution_time)) +
    geom_col(fill='#066DA6') + 
    ggtitle("Average Request Execution Time") +
    labs(y=paste("Days"), x="Request Received Month") +
    geom_text(aes(label=execution_time),
             vjust=-0.1,
              size = 4)+
    std_plot
  
  p
}

# GANTT CHART OF EXECUTION AND BENCH TIME ----=------------------------------
plots_gantt_chart = function(){
  fema$responder_id = as.factor(fema$responder_id)
  p <- ggplot(fema, aes(x=deployment_start_time, xend=deployment_end_time,
                        y=responder_id, yend=responder_id)) +
    geom_segment(size=5, color='#066DA6') + 
    labs(title='2021 Execution Time (blue segments) and Bench Time (white gaps)', 
         x='Date',  y='Responder ID') +
    std_plotly
  
  
  p  
  
}

