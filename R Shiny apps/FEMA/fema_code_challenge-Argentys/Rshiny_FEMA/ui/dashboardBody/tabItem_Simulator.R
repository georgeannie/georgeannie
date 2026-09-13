source('ui/inputWidgets.R')
sim_delay = box(width='100%',
                shinycssloaders::withSpinner(type= 4, color = "#066DA6", size = 1,
                                      plotOutput("sim_execution",
                                                 height = 450)
                                  )
                  )

utilization_sim = box(width='100%', 
                      shinycssloaders::withSpinner(type= 4, color = "#066DA6", size = 1,
                                                   plotOutput("sim_utilization",
                                                              height = 450)
                    )
)

optimal_sim = box(width='100%', height = '100px',
                      shinycssloaders::withSpinner(type= 4, color = "#066DA6", size = 1,
                                                   uiOutput("sim_optimal")
                      )
)


sim_plots = fluidRow(
                    column(6,sim_delay ),
                    column(6,utilization_sim)
                  )

tabItem_Simulator=tabItem(tabName = 'simulator',
                          fluidRow(
#PLOTS      
                            column(10,sim_plots),
                             column(2, 
                                  slider_numofresponder,
                                  numinput_numofyears,
                                  numinput_mean_depl_time,
                                  numinput_depl_rate,
                                  simulate_button
                              )
                          ),
                tags$hr(style="border-color: dimgrey;height: 10px;  "),      fluidRow(
                            column(10,
                              fluidRow(
                                column(4, valueBoxOutput("util_valuebox_sim")),
                                column(4, valueBoxOutput("mean_fulfill_time_valuebox_sim")),
                                column(4, optimal_sim)
                              )
                            ),
                            column(2,
 #INPUT IDGETS   
                               fluidRow(column(12, numinput_cost_ratio)),
                               fluidRow(column(12, optimize_button))
                            ),  
                        )
          )
