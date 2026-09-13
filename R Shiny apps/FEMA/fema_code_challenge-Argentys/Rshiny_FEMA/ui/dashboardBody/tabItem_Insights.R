

plots_start_time_gap=plotOutput("plots_start_time_gap_output",
                                      height = 195)

plots_idle_by_month=plotOutput("plots_idle_by_month_output",
                                height = 195)

plots_execution_time=plotOutput("plots_execution_time_output",
                                  height = 195)

plots_gantt_chart= shinycssloaders::withSpinner(type= 4, color = "#066DA6", size = 1,
                                                plotOutput("plots_gantt_chart_output",
                                  height = 620))


tabItem_Insights=tabItem(tabName = 'insights',
#PLOTS                           
                        fluidRow(
                            valueBoxOutput("request_valuebox"),
                            valueBoxOutput("mean_exe_time_valuebox"),
                            valueBoxOutput("util_valuebox"),
                            valueBoxOutput("fulfill_valuebox"),
                                   
                          ),
                        fluidRow(
                          column(6,

                                fluidRow(
                                  box(width='90%',
                                      plots_execution_time
                                  ),
                                   box(width='90%',
                                       plots_start_time_gap
                                   ),
                                   
                                   box(width='90%',
                                       plots_idle_by_month
                                   ),
                                   
                                 
                                 )
                                ),
                          column(6,

                                 box(width='90%',
                                     plots_gantt_chart
                                 )
                          ),

                        )

)
