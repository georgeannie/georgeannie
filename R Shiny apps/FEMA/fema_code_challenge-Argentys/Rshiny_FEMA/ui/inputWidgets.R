# INPUT WIDGET FOR SIMULATOR --------------------------------------------
slider_numofresponder =sliderInput("numresp", "Number of Responders:",
                                   min = 1, max = 70, step = 1,
                                   value = c(1,40))

numinput_numofyears = numericInput("numofyear",
                                   label = "Number of years",
                                   value = 1,
                                   min = 1, 
                                   max = 15,
                                   step = 1)

numinput_mean_depl_time = numericInput("mean_dep_time", 
                                       "Average Execution Time (hours)", 1108.0, min = 1, 
                                       step=1)

numinput_depl_rate = numericInput("mean_dep_rate", 
                                  "Number of Requests", 230, min = 1, 
                                  step=1)


simulate_button = actionButton("simulate", "Run Resource utilization")

numinput_cost_ratio = formatNumericInput("est_cost_ratio",
                                         label = "Cost Ratio",
                                         value = 0.5)

optimize_button = actionButton("optimize", paste("Optimal \n Team Size"),
                               align='left')

