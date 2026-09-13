# EXECUTION TIME FOR SIMULATION ------------------------------------
sim_execution_output = function(simulator_data, numofyear) {
      
  title_plot = sprintf('Total Time to Fulfill Request for %d Years', numofyear)
  sim_data = simulator_data$metrics
  sim_data$total_fulfillment_time = sim_data$total_fulfillment_time/24
  p = ggplot(sim_data, aes(x=responder_num, y=total_fulfillment_time)) +
    geom_point(color="#066DA6", size=5) +
    ggtitle(title_plot ) +
    labs(x="Number of Responders", y="Days") +
    scale_y_continuous(labels = label_number(accuracy=1, suffix = "K", scale = 1e-5)) +
    theme_minimal() +
    theme(axis.line = element_line(color='grey'),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.line.y=element_blank(),
          axis.line.x.bottom = element_line(colour = 'black', size=0.6),
          axis.title.x = element_text(color="#066DA6", size=18, face='bold'),
          axis.title.y = element_text(color="#066DA6", size=18, face='bold'),
          axis.text = element_text( size=14, color="#066DA6", face='bold'),
          plot.title = element_text(hjust=0.5, color = "#066DA6", size = 20, face='bold'))
  p
      
}  

# SIMULATED UTILIZATION RATE ------ ------------------------------
sim_utilization_output = function(simulator_data, numofyear, min_resp, max_resp) {
  sim_data  = simulator_data$metrics
  sim_data$util_rate = sim_data$util_rate * 100
  title_plot = sprintf('Utilization Rate over %d Years for %s - %s responders', 
                       numofyear, min_resp, max_resp)
  
  p = ggplot(sim_data, aes(x=responder_num, y=util_rate)) +
    geom_point(color="#066DA6", size=5) +
    ggtitle(title_plot ) +
    labs(x="Number of Responders", y="Rate") +
    scale_y_continuous(labels = label_number( suffix = "%")) +
    theme_minimal() +
    theme(axis.line = element_line(color='grey'),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.line.y=element_blank(),
          axis.line.x.bottom = element_line(colour = 'black', size=0.6),
          axis.title.x = element_text(color="#066DA6", size=18, face='bold'),
          axis.title.y = element_text(color="#066DA6", size=18, face='bold'),
          axis.text = element_text( size=14, color="#066DA6", face='bold'),
          plot.title = element_text(hjust=0.5, color = "#066DA6", size = 20, face='bold'))
  p
}  

