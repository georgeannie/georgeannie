#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
 #     fileInput("file1", "Choose CSV File",multiple = FALSE,accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"))),
  textInput("URL","URL"),
  submitButton("Update View", icon("refresh"))),
    # Show a plot of the generated distribution
    mainPanel(
      DT::DTOutput("dt_prediction_event_name")
      ,DT::DTOutput("dt_prediction_event_description")
      
    )
  )
))
