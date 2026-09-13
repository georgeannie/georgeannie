#rm(list=ls())
library(dplyr)
library(rpart)
library(randomForest)
source("../r_functions/functions.r")
load("model_objects.rda")
load("og_provided.rda")
#modeling framework

# read train data

#og_provided = read.csv("../../data_og/og_provided_test_data.csv")
#names(og_provided) <- c("id","event_name","event_description", "url")


library(data.table)

list_of_csvs_all = list.files("../../data_scraped_raw",full.names = TRUE)
list_of_csvs_test = lapply(list_of_csvs_all[31:40],function(x){read.csv(x,colClasses =  c("factor", "factor", "factor", "integer", "integer", "factor", "factor", "factor", "integer", "integer"))})
test_data = dplyr::bind_rows(list_of_csvs_test)



test_data = data.table(test_data)
og_provided = data.table(og_provided %>% filter(event_description!="" & event_name!=""))



#merge train with actual
validate_data = merge(test_data,og_provided ,by= 'url')
validate_data <- data.frame(validate_data)

validate_data = validate_data[get_source_url(validate_data$url)!="https:www.surlatable.com",]
#filter values
#clean values/event names/event descriptions
#validate_data$event_name <- clean_text_vector(validate_data$event_name)
#validate_data$event_description <- clean_text_vector(validate_data$event_description)

#build target features, table function
validate_data <- build_feature_variables(validate_data)
validate_data$value <- clean_text_vector(validate_data$value)
validate_data$event_name <- clean_text_vector(validate_data$event_name)
validate_data$event_description <- clean_text_vector(validate_data$event_description)


#build target variables
validate_data$og_contains_event_name = mapply(grepl, MoreArgs = list(fixed = TRUE), validate_data$event_name, validate_data$value)
validate_data$og_equals_event_name = validate_data$event_name == validate_data$value
validate_data$og_equals_description = validate_data$event_description== validate_data$value


#remove sites w/o tgts being true, save them somewhere

no_data_urls <- find_no_data(validate_data)
#no_data_urls$no_equals_description

description_validate_data = filter_possibilities(validate_data,'equals_description',type="validate")
description_validate_data$og_equals_description <- as.factor(description_validate_data$og_equals_description)
#description_validate_data$predicted_contains_event <- predict(model_object_contains_event_model,description_validate_data)
description_validate_data = description_validate_data[description_validate_data$name %in% important_factors$precizion_equals_description,]
#description_validate_data <- droplevels (description_validate_data)
description_validate_data$predicted_equals_desc <- predict(model_object_equals_desc_model ,description_validate_data, type = "prob")[,'TRUE']
#View(description_validate_data[description_validate_data$url=="https://www.localwineevents.com/events/detail/737709/Affordable-Bordeaux",])
list_of_success = by(description_validate_data,description_validate_data$url,function(DF_to_validate)
{
  important_record = which.max(DF_to_validate$predicted_equals_desc )
  GuessedRight = DF_to_validate$og_equals_description [important_record] ==  TRUE#1 
  if(GuessedRight == FALSE)
  {  print(as.character(DF_to_validate$url[important_record]))}
  GuessedRight
})
#description
sum(list_of_success,na.rm = TRUE)/length(list_of_success)






event_name_validate_data = filter_possibilities(validate_data,'equals_event_name',important_factors$precizion_equals_event_name,type="validate")
event_name_validate_data$og_equals_event <- as.factor(event_name_validate_data$og_equals_event)
event_name_validate_data = event_name_validate_data[event_name_validate_data$name %in% important_factors$precizion_equals_event_name,]

event_name_validate_data$predicted_equals_event <- predict(model_object_equals_event_model ,event_name_validate_data, type = "prob")[,'TRUE']
#View(event_name_validate_data[event_name_validate_data$url=="https://festivalnet.com/26405/Jacksonville-Florida/Festivals/October-Fall-Festival",])


list_of_success_event_name = by(event_name_validate_data,event_name_validate_data$url,function(DF_to_validate)
{
  important_record = which.max(DF_to_validate$predicted_equals_event )
  GuessedRight = DF_to_validate$og_equals_event [important_record]  ==  TRUE#1 
  if(GuessedRight == FALSE)
  {  print(
    as.character(DF_to_validate$url[important_record]))
    }
  GuessedRight
})
#description
sum(list_of_success_event_name,na.rm = TRUE)/length(list_of_success_event_name)






# contains_event_name_validate_data = filter_possibilities(validate_data,'contains_event',important_factors$precizion_contains_event_name)
# contains_event_name_validate_data$og_contains_event <- as.factor(contains_event_name_validate_data$og_contains_event)
# contains_event_name_validate_data$predicted_contains_event <- predict(model_object_contains_event_model ,contains_event_name_validate_data, type = "prob")[,'TRUE']
# #View(contains_event_name_validate_data[contains_event_name_validate_data$url=="https://www.visitvirginiabeach.com/event/2018-sgk-gun-show/3386/",])
# 
# 
# list_of_success_contains_event_name = by(contains_event_name_validate_data,contains_event_name_validate_data$url,function(DF_to_validate)
# {
#   important_record = which.max(DF_to_validate$predicted_contains_event )
#   GuessedRight = DF_to_validate$og_contains_event [important_record]  ==  TRUE#1 
#   if(GuessedRight == FALSE)
#   {  print(as.character(DF_to_validate$url[important_record]))
#                   }
#   GuessedRight
# })
# #description
# sum(list_of_success_contains_event_name,na.rm = TRUE)/length(list_of_success_contains_event_name)




save(model_object_equals_desc_model,important_factors,model_object_equals_event_model,build_feature_variables,file= "../shiny_app/quick_test2/model_round2.rda")
