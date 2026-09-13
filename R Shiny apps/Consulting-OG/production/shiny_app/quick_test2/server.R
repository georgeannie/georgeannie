#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(jsonlite)
library(shiny)
library(dplyr)
library(rpart)
library(curl)
print(getwd())
#load('model_and_list.rda')
load("model_round2.rda")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$dt_prediction_event_name <- DT::renderDataTable({
    
    #validation_data = read.csv(input$file1$datapath)
    
     URL = paste0('https://5v6s2yl6c1.execute-api.us-east-1.amazonaws.com/default/scrape_to_url?url=',input$URL)
    #URL = "https://i6l78q2qkk.execute-api.us-east-1.amazonaws.com/default/Eratosthenes?url=https://www.eventbrite.com/e/baltimore-the-underground-railroad-project-collaborators-tickets-53504566599"
    
    #URL = 'https://i6l78q2qkk.execute-api.us-east-1.amazonaws.com/default/Eratosthenes?url=https://festivalnet.com/12623/Matthews-North-Carolina/Craft-Shows/Artwalk-Musicfest-of-Matthews'
    # returned_query = fromJSON(URL)
    # print(URL)
    # 
    # 
    # if(!is.matrix(returned_query$data))
    # {
    #   
    #  
    #   
    #   validation_data <<-  do.call(rbind, returned_query$data)
    #   validation_data <<-  data.frame(validation_data)
    #   
    #   validation_data$X1 <- unlist(validation_data$X1)
    #   validation_data$X2 <- unlist(validation_data$X2)
    #   validation_data$X3 <- unlist(validation_data$X3)
    #   validation_data$X4 <- unlist(validation_data$X4)
    #   validation_data$X5 <- unlist(validation_data$X5)
    #   validation_data$X6 <- unlist(validation_data$X6)
    #   validation_data$X7 <- unlist(validation_data$X7)
    #   validation_data$X8 <- unlist(validation_data$X8)
    #   validation_data$X9 <- unlist(validation_data$X9)
    #   
    #   
    #   validation_data <<- 
    #   str(head(validation_data))
    #   
    #   
    # }else{
    #   validation_data <<- data.frame(returned_query$data)
    # }
    # names(validation_data) <<- returned_query$columns
     validation_data<<- read.csv(readLines(URL))

    validation_data$body_length <<- as.numeric(as.character(validation_data$body_length))
    validation_data$site_length <<- as.numeric(as.character(validation_data$site_length))
    validation_data$position <<- as.numeric(as.character(validation_data$position))
    validation_data$run_num <<- as.numeric(as.character(validation_data$run_num))
    validation_data$index <<- as.numeric(as.character(validation_data$index))
                     
    if(length(sapply(validation_data,is.character)) > 0)
    {validation_data[,sapply(validation_data,is.character)] <<- lapply(validation_data[,sapply(validation_data,is.character)],as.factor)}
    
    validation_data <<- build_feature_variables(validation_data)
    event_name_table <- validation_data
    event_name_table <-  event_name_table[event_name_table$name %in% precizion_equals_event_name,] 
    event_name_table <- event_name_table[!is.na(event_name_table$value) & event_name_table$value!="",] 
    
    
   event_name_table$predicted_value <- predict(model_object_equals_event_model,event_name_table)[,2]
   event_name_table[,c("url","value","name","position","type" ,"body_length","site_length","predicted_value")] %>% 
     arrange(desc(predicted_value)) 
   
               #https://www.stlmag.com/events/show-reptile-exotics-show-7/?occ_dtstart=2019-02-17T10:00
  })
  
  
  
  
   output$dt_prediction_event_description <- DT::renderDataTable({
     URL = input$URL
     
     event_description_table <- validation_data
     event_description_table <-  event_description_table[event_description_table$name %in% precizion_equals_description,] 
     event_description_table <- event_description_table[!is.na(event_description_table$value) & event_description_table$value!="",] 
     
     

     event_description_table$predicted_value <- predict(model_object_equals_desc_model ,event_description_table)[,2]
     event_description_table[,c("url","value","name","position","type" ,"body_length","site_length","predicted_value")] %>% 
       arrange(desc(predicted_value)) 
     
     })
  
})
