rm(list=ls())
library(dplyr)
library(rpart)
library(data.table)

source("../r_functions/functions.r")

#modeling framework

# read train data

if(1==2)
{
  og_provided = read.csv("../../data_og/og_provided_test_data.csv")
  names(og_provided) <- c("id","event_name","event_description", "url")
  og_provided$event_description <- as.character(og_provided$event_description)
  og_provided$event_description <- clean_text_vector(og_provided$event_description)
  
  og_provided$event_name <- as.character(og_provided$event_name)
  og_provided$event_name <- clean_text_vector(og_provided$event_name)
  
  save(og_provided,file="og_provided.rda")
}else{load("og_provided.rda")}

list_of_csvs_all = list.files("../../data_scraped_raw",full.names = TRUE)
list_of_csvs_train = lapply(list_of_csvs_all[1:30],function(x){read.csv(x,colClasses =  c("factor", "factor", "factor", "integer", "integer", "factor", "factor", "factor", "integer", "integer"))})
train_data = dplyr::bind_rows(list_of_csvs_train)

#rm(list=ls()[!grepl("og_provided|train_data",ls())])
source("../r_functions/functions.r")

train_data = data.table(train_data)
og_provided = data.table(og_provided)

#merge train with actual
#semi-remove all
pre_model_data = merge(train_data,og_provided ,by= 'url')
pre_model_data = data.frame(pre_model_data)

pre_model_data <- build_feature_variables(pre_model_data)
pre_model_data$value <- as.character(pre_model_data$value)



#filter values
#clean values/event names/event descriptions
pre_model_data$value <- clean_text_vector(pre_model_data$value)




##### data prep part 2 ####
model_data = pre_model_data[get_source_url(pre_model_data$url)!="https:www.surlatable.com",]
#build target variables
model_data$og_contains_event_name = mapply(grepl, MoreArgs = list(fixed = TRUE), model_data$event_name, model_data$value)
model_data$og_equals_event_name = model_data$event_name == model_data$value
model_data$og_equals_description = model_data$event_description== model_data$value

#build target features, table function
#finds sites w/o tgts being true, save them somewhere

no_data_urls <- find_no_data(model_data)


##### prep step 2 #####
### split by target ####

#removes sites where event descr doesnt exist
# 
# contains_event_name_model_data = filter_possibilities(model_data,'contains_event_name',type="train")
# contains_event_name_model_data$og_contains_event_name <- as.factor(contains_event_name_model_data$og_contains_event_name)
# contains_event_name_model_data[,c('value','event_name','event_description')] <- list(NULL)
# model_object_contains_event_model = rpart(og_contains_event_name~.,contains_event_name_model_data[,!grepl("og_equals_event_name|og_equals_description|value|url",names(contains_event_name_model_data))],cp=.0001)
# 



event_name_model_data = filter_possibilities(model_data,'equals_event_name',type="train",Threshhold = .8)
event_name_model_data$og_equals_event_name <- as.factor(event_name_model_data$og_equals_event_name)
event_name_model_data[,c('value','event_name','event_description')] <- list(NULL)
precizion_equals_event_name = unique(event_name_model_data$name)
model_object_equals_event_model = rpart(og_equals_event_name~.,event_name_model_data[,!grepl("og_equals_description|og_contains_event_name|url|body_length|site_length",names(event_name_model_data))],cp=.0001)
#model_object_equals_event_model = randomForest(og_equals_event_name~.,event_name_model_data[,!grepl("og_equals_description|og_contains_event_name|url|body_length|site_length",names(event_name_model_data))])
#build model contains event_name
#predict contains event_name using contains for equals event/description
#event_name_model_data$predicted_contains_event <- predict(model_object_contains_event_model,event_name_model_data)
#description_model_data$predicted_contains_event <- predict(model_object_contains_event_model,description_model_data)
#build model equals event_name
#
#model_object_equals_event_model = randomForest::randomForest(og_equals_event_name~.,event_name_model_data[,!grepl("og_equals_description|og_contains_event_name|url|body_length|site_length|position",names(event_name_model_data))])
#model_object_equals_desc_model = randomForest::randomForest(og_equals_description~.,description_model_data[,!grepl("og_equals_event_name|og_contains_event_name|url",names(description_model_data))])
# set.seed(10)
# model_object_equals_desc_model = rpart(og_equals_description~.,model_data_descr_model[,!grepl("og_equals_event_name|og_contains_event_name|site_length|body_length|position|predicted_contains_event|event_description",names(model_data_descr_model))],cp=.001)


description_model_data = filter_possibilities(model_data,'equals_description',Threshhold = .8,type="train")
description_model_data$og_equals_description <- as.factor(description_model_data$og_equals_description)
description_model_data[,c('value','event_name','event_description')] <- list(NULL)
precizion_equals_description = unique(description_model_data$name)
model_object_equals_desc_model = rpart(og_equals_description~.,description_model_data[,!grepl("og_equals_event_name|og_contains_event_name|url|valuebody_length|site_length",names(description_model_data))],cp=.00001)



important_factors = sapply(ls(pattern="precizion_"),get)
save(model_object_equals_event_model,model_object_equals_desc_model,important_factors,file="model_objects.rda")

