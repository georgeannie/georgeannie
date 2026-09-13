#functions
#Message to print when functions are successfully sourced in another file
print("read functions")

clean_text_vector <- function(text_var)
{
  #text_var <<- text_var
  #Converts text encoding to ASCII from UTF-8
  text_var = stringi::stri_conv(text_var, from = 'UTF-8', to = 'ASCII')
  
  #Replaces any puncuation symbol with single space
  text_var = gsub("[[:punct:]]", " ", text_var,perl=TRUE);
  #Removes all spaces
  text_var = gsub("\\s","",text_var,perl=TRUE)
  text_var
}


build_feature_variables = function(DF)
{
  #Forces value column to character - DF must have a column named value
  DF$value <- as.character(DF$value)
  #Returns TRUE/FALSE if value contains string resembling a date (e.g. month name)
  DF$contains_date_looking_thing <-  grepl("201(8|9)|[[:digit:]]{1,4}|jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec",DF$value)
  
  
  #model_data$description = NULL
  #check to see if 
  #Returns TRUE/FALSE if value string contains day of week
  DF$day_of_week_abbr <- grepl("mon|tues|wed|thur|fri|sat|sun",DF$value)
  
  
  #space-delimtied statement
  DF$space_delimited <- grepl("[[:alnum:]]+\\s{1}[[:alnum:]]+", DF$value, ignore.case = TRUE)
  
  DF$body_length <- as.numeric(DF$body_length)
  DF$type <- as.factor(DF$type)
  #
  
  DF$nchar_value = nchar(DF$value)
  
  DF
  
}




find_no_data<-function(DF)
{
  #Identifies URLs with no records with event name indicator == 1
  no_contains_event_name =aggregate(og_contains_event_name~url,DF,max) %>% filter(og_contains_event_name==0)
  no_equals_event_name = aggregate(og_equals_event_name~url,DF,max) %>% filter(og_equals_event_name==0)
  #Identifies URLs with no records with event description indicator == 1
  no_equals_description = aggregate(og_equals_description~url,DF,max) %>% filter(og_equals_description==0)
  list_of_no_data = list(no_contains_event_name,no_equals_event_name,no_equals_description)
  
  names(list_of_no_data) <- c("no_contains_event_name","no_equals_event_name","no_equals_description")
  list_of_no_data
}

# remove_no_data <- function(DF)
# {
#   #DF = validate_data
#   
#   no_contains_event_name =aggregate(og_contains_event~url,DF,max) %>% filter(og_contains_event==1)
#   no_equals_event_name = aggregate(og_equals_event~url,DF,max) %>% filter(og_equals_event==1)
#   no_event_description = aggregate(og_equals_description~url,DF,max) %>% filter(og_equals_description==1)
#   
#   
#   merge_filter = bind_rows(list(no_contains_event_name,no_equals_event_name,no_event_description))
#   merge_filter <- unique(data.frame(merge_filter[,"url"]))
#   names(merge_filter) <- 'url'
#   merge(DF,merge_filter,by="url")
#   
# }


detect_important_factors <- function(FactorVector,TargetVector,Threshhold = .75){
  #FactorVector = description_model_data$name
  #TargetVector = description_model_data$og_equals_description
  
  #Counts TRUE/FALSE values for each level of Factor Vector
  factor_result_to_target = as.data.frame.matrix(table(FactorVector,TargetVector))
  #Returns levels of FactorVector with at least the Threshhold percentile of TRUE values
  row.names(factor_result_to_target) [quantile(factor_result_to_target[,2],Threshhold) < factor_result_to_target[,2]]
}



filter_possibilities <- function(DF,target,Threshhold = .95,type=c("train","validate"))
{
  # DF = model_data
  # target = "equals_description"
  # Threshhold = .95
  #rm(DF,target,Threshhold)
    #Filters out URLs with no records where target == 1
    DF = DF[!DF$url %in% no_data_urls[[paste0("no_",target)]]$url,]
    #relevel factors
  DF$name <- as.factor(DF$name)
  #good factors for description
  if(type=="train")
  {
  important_factors = detect_important_factors(DF$name,DF[[paste0('og_',target)]], Threshhold =Threshhold)
  #Reatins only "important" factors as defined by function
  DF = DF[DF$name %in% important_factors,]
  }
  #remove "empty" values
  DF = DF[nchar(as.character(DF$value)) > 3,]
  #Removes coulumns in DF containing certain strings
  DF = DF[ ,!grepl('index|run_num|id|nested',names(DF))]
  #Removes levels from factors with no observations in DF (i.e. the "not important" levels)
  DF = droplevels(DF)
  na.omit(DF)
}
#important_description_names



get_source_url <- function(URL)
{
  sapply(strsplit(gsub("//","",as.character(URL)),"/"),`[[`,1)  
}

