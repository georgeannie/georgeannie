source('global.R')
source("server/valuebox_functions.R")


ui <-dashboardPage(
     
      dashboardHeader,

      dashboardSidebar,

      dashboardBody
  )

# # Define server logic required to draw a histogram
server <- function(input, output) {
#OUTPUTS FOR DECISION TOOL TAB --------------------------------------------
  
  numofyear = reactiveVal()
  min_numofresponder = reactiveVal()
  max_numofresponder = reactiveVal()
  mean_dep_time= reactiveVal()
  depl_rate = reactiveVal()
  
  sim_action <- eventReactive(input$simulate,{
    numofyear(req(input$numofyear))
    min_numofresponder(req(min(input$numresp)))
    max_numofresponder(req(max(input$numresp)))
    mean_dep_time(req(input$mean_dep_time))
    depl_rate(req(input$mean_dep_rate))

    sim <- simulate_deployments(numofyear(),
                                min_numofresponder(),
                                max_numofresponder(),
                                mean_dep_time(),
                                depl_rate() )
   
    } ,ignoreNULL = FALSE

  )
 
    observeEvent(sim_action(), {
      simulate_parms(sim_action())
    })
    
   output$sim_execution <- renderPlot({
      sim_execution_output(sim_action(), numofyear())
    })
    
  output$sim_utilization <- renderPlot({
      sim_utilization_output(simulate_parms(), numofyear(), min_numofresponder(),
                             max_numofresponder())
    })
    
    cost_ratio_txt = reactiveVal()
    simulate_parms = reactiveVal(NULL)
    optimal_parms = reactiveVal(NULL)
    
    optimal_action <- eventReactive(input$optimize,{
       cost_ratio_txt(req(input$est_cost_ratio))
       optimal <- suggest_responder_num(simulate_parms(), 
                                        cost_ratio_txt())
      } ,ignoreNULL = FALSE)
    
    
    observeEvent(optimal_action(), {
      optimal_parms(optimal_action())
    })

    output$sim_optimal <- renderUI({
        
            infoBox(optimal_action(),
                  "Recommended Optimal Team Size",
                  fill = TRUE,
                  color = "blue",
                  width = 500,
                  icon = icon("resource"))
       # )))
        })
  
     output$util_valuebox_sim = renderValueBox(util_vb_fn_sim(simulate_parms(), optimal_parms()))
     output$mean_fulfill_time_valuebox_sim = renderValueBox(mean_fulfill_vb_fn_sim(simulate_parms(), optimal_parms()))

     
#OUTPUTS FOR INSIGHT TAB ------------------------------------------------
  output$plots_start_time_gap_output <- renderPlot({
    plots_fulfill_by_month()
  })
  
  output$plots_idle_by_month_output <- renderPlot({
    plots_idle_by_month()
  })
  
  output$plots_execution_time_output <- renderPlot({

    plots_execution_time()
  })
  
  output$request_valuebox = renderValueBox(request_count_vb_fn())
  output$util_valuebox = renderValueBox(util_vb_fn())
  output$mean_exe_time_valuebox = renderValueBox(mean_exe_vb_fn())
  output$fulfill_valuebox = renderValueBox(fulfill_vb_fn())
  
  output$plots_gantt_chart_output = renderPlot({
      plots_gantt_chart()
  })

#OUTPUTS FOR GLOSSARY  --------------------------------------------
  output$glossary <-renderUI({
    includeHTML('./data/glossary.html')
  })
}

# Run the application
shinyApp(ui = ui, server = server)



