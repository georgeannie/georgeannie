rm(list=ls());
#read in training csvs
clean_text_vector <- function(text_var)
{
  text_var = gsub("[[:punct:]]", " ", text_var);
  text_var = iconv(text_var, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  tolower(trimws(text_var))
}

og_provided = read.csv("../data_og/og_provided_test_data.csv")
names(og_provided) <- c("id","event_name","event_description", "url")
foldered_data_files_list = list.files("../structuring_data_for_model/data",full.names = TRUE)
list_of_csvs_train = lapply(foldered_data_files_list[1:8],function(x){read.csv(x,colClasses =  c("factor", "factor", "factor", "integer", "integer", "factor", "factor", "factor", "integer", "integer"))})

list_of_csvs_test = lapply(foldered_data_files_list[9:12],function(x){read.csv(x,colClasses =  c("factor", "factor", "factor", "integer", "integer", "factor", "factor", "factor", "integer", "integer"))})



train_data = bind_rows(list_of_csvs_train)
test_data = bind_rows(list_of_csvs_test)





#connect them to target variable
model_data = merge(train_data,og_provided ,by= 'url')
model_data$description = NULL



model_data$value <- clean_text_vector(model_data$value)
model_data$event_name <- clean_text_vector(model_data$event_name)

model_data$contains_event = mapply(grepl, MoreArgs = list(fixed = TRUE), model_data$event_name, model_data$value)

#remove variables 
model_data$id <- model_data$event_description <- model_data$event_name <- model_data$url <- model_data$value <- NULL

model_data$body_length <- as.numeric(model_data$body_length)
model_data[,sapply(model_data,is.character)] <- lapply(model_data[,sapply(model_data,is.character)],as.factor)


# data_train <-
#   downSample(data_train,
#            data_train[[modelingvar_target]],
#            list = FALSE,
#            yname = 'deleteme')
# data_train$deleteme <- NULL


#model
model = rpart(contains_event~.,model_data,cp=.001)












#apply model




#connect them to target variable
validation_data = merge(test_data,og_provided ,by= 'url')
validation_data$description = NULL



validation_data$value <- clean_text_vector(validation_data$value)
validation_data$event_name <- clean_text_vector(validation_data$event_name)

validation_data$contains_event = mapply(grepl, MoreArgs = list(fixed = TRUE), validation_data$event_name, validation_data$value)

#remove variables 
validation_data$body_length <- as.numeric(validation_data$body_length)
validation_data[,sapply(validation_data,is.character)] <- lapply(validation_data[,sapply(validation_data,is.character)],as.factor)
validation_data <-  validation_data[validation_data$name %in% levels(model_data$name),] 



validation_data <- validation_data[!is.na(validation_data$value),] 


validation_data$predicted_value = predict(model,validation_data)

unique_urls = sample(unique(validation_data$url))

DF_to_view = validation_data[validation_data$url ==unique_urls[23], ]



list_of_success = by(validation_data,validation_data$url,function(DF_to_validate)
{
  important_record = which.max(DF_to_validate$predicted_value)
  GuessedRight = DF_to_validate$contains_event[important_record] == 1 
  if(GuessedRight == FALSE | is.na(GuessedRight))
  {  print(DF_to_validate$url[important_record])}
    GuessedRight
    
  
  
  
  
#as.character(DF_to_view[DF_to_view$predicted_value > .5,"value"])
  
}
  )





sum(list_of_success,na.rm = TRUE)/length(list_of_success)



DF_to_view = validation_data[validation_data$url ==
"http://dola.com/diadelosmuertoscoatlicue",]
















