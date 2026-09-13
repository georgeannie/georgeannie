library(dplyr)
library(stringr)
library(lubridate)
library(htmltools)
library(Metrics)
set.seed(123)
read_rename_sinter = function(){
  folder <- "./data/"  
  file_list = list.files(path=folder, pattern="*.csv", full.names = T)
    
  plant =list()
  for (i in 1:length(file_list)){
    plant[[i]]=assign(file_list[i], 
           read.csv(file_list[i], sep=',', stringsAsFactors = FALSE))
    names(plant[[i]])=str_to_title(gsub("_", " ", names(plant[[i]])))
    plant[[i]] = plant[[i]] %>%
      #mutate(Timestamp=(as.character(as.POSIXct(dmy_hm(Timestamp))))) %>%
        plyr::rename(c("X.internal Return Fine" ="%Internal Return Fine",
                       "X.external Return Fine" ="%External Return Fine")) %>%
        select(-"Order")
      
  }
    return(plant)
}

read_rename_final_data = function(){
  sinter_full = read.csv("./data/syntheticData.csv",
                    stringsAsFactors = FALSE, header = TRUE)

  #Remove underscores and change column names to title format for display
      names(sinter_full)=str_to_title(gsub("_", " ", names(sinter_full)))

      return(sinter_full)
}


summary_table = function(input_table) {
  section=data.frame("Section" = input_table[,"Section"])
  
  input_table = input_table[,!names(input_table) %in% c("Section")]
  
  dep_no = apply(input_table, 2, function(x) (sum(!is.na(x))))
  dep_sd = apply(input_table, 2, sd, na.rm=TRUE)
  dep_mean = apply(input_table, 2, mean, na.rm=TRUE)
  dep_na = apply(input_table, 2, function(x) sum(is.na(x)))
  dep_median = apply(input_table, 2,  median, na.rm=T)
  dep_min = apply(input_table, 2, min, na.rm=TRUE)
  dep_max = apply(input_table, 2, max, na.rm=TRUE)
  
  dep_summary=data.table("Section" = section[1,],
                         "Variables" = names(input_table),
                         "# of Observations" = dep_no,
                         'Mean' = round(dep_mean, 2),
                         "Standard Deviation" = round(dep_sd,2),
                         '# of Missing data' = dep_na,
                         'Median' = round(dep_median,2),
                         "Minimum Value" = dep_min,
                         "Maximum Value" = dep_max
  )
  print(dep_summary)
}

sinter=read_rename_final_data()

feed_data=function(){
  sinter %>%
      select("Total Bmix Flow" , "Total Clime Flow", "Total Limestone Flow", 
             "Total Solid Fuel Flow",
             "Feo"  ,    "Cao" , "Al2o3",  "Tio2" ,                
             "K2o",  "P", "Fe"  ) %>%
       mutate("Section" = "Feed")
}

ignition_data = function(){
   sinter %>%
    select("Ih Combustion Air Temp" ,  "Ih Mixed Gas Temp" ,
           "Combustion Air Flow", "Ih Mixed Gas Flow", "Ih Temp M1" 
    )
}

sinter_bed = function(){
  sinter %>%
    select("Windbox 1 Pressure",       "Windbox 12 Pressure" ,
           "Machine Speed" , "Sinter Temp Sm Disch" ,"Sinter Bed Height" 
    )
  
}

stack= function(){
  sinter %>%
    select("Co", "Co2", "O2"                      
    )
}

esp=function(){
  sinter %>%
    select("Waste Water Flow" , "Temp Before Esp" , "Waste Gas Fan Rpm"                           
    )
  
}

cooler=function(){
  sinter %>%
    select("Cooler Pressure",  "Cooler Speed",  "Cooler Dischrg Temp"                             
    )
  
}

test_train=function(data){
  index = sample(1:nrow(data),size = 0.7*nrow(data))
  train = data[index,] 
  test = data [-index,]
  list(train, test)
 
}

xgtest_train=function(data){
  index = sample(1:nrow(data),size = 0.7*nrow(data))
  xgtrain = data[index,] 
  xgtest = data [-index,]
  
  train = as.matrix(model.matrix(~.-1,data=xgtrain)) 
  train_label <- xgtrain[,'IRF_PCT'] 
  
  test <- as.matrix(model.matrix(~.-1,data=xgtest))
  test_label <- xgtest[,'IRF_PCT'] 
  
  return(list(train, test))
  
}

r2_test = function(test_data, test_pred) {
  residuals = test_data$IRF_PCT - test_pred
  test_IRF_pct_mean = mean(test_data$IRF_PCT)
  tss =  sum((test_data$IRF_PCT - test_IRF_pct_mean)^2 )
  rss =  sum(residuals^2)
  
  # Calculate R-squared
  rsq  =  1 - (rss/tss)
  rsq
}

rsq=function(dataset, pred){
  
  residuals = dataset[,"IRF_PCT"]- pred
  test_IRF_pct_mean = mean(dataset[, "IRF_PCT"])
  tss =  sum((dataset[,"IRF_PCT"] - test_IRF_pct_mean)^2 )
  rss =  sum(residuals^2)
  
  # Calculate R-squared
  rsq  =  1 - (rss/tss)
  return(rsq)
  
}

results_df_rf = function(test, train, model){
  
  test_pred = predict(model,test)$prediction
  
  RMSE_TEST=rmse(test_pred, test[,"IRF_PCT"])
  
  RMSE_TRAIN = model$prediction.error
     
  rsq_test=rsq(test, test_pred)
  rsq_train = model$r.squared
 
  a=data.frame("Dataset" = c("train", 
                             "test"),
               "No of observation"= c(dim(train)[1], 
                                      dim(test)[1]),
               "RMSE" = c(RMSE_TRAIN, 
                          RMSE_TEST),
               "R-square" = c(rsq_train, rsq_test ))
  return(a)
}

results_df_xg = function(test, train, model){
  
  dtrain=xgb.DMatrix(as.matrix(train[,-1]),label=train[,1] )
  dtest=xgb.DMatrix(as.matrix(test[,-1]),label=test[,1])
  
  test_pred = predict(model,newdata = dtest, class='response')
  train_pred = predict(model,newdata = dtrain, class='response')
  
  RMSE_TEST=rmse(predicted=test_pred, actual=test[,1])
  
  RMSE_TRAIN = rmse(predicted=train_pred, actual=train[,1])
  
  rsq_test=rsq(test, test_pred)
  rsq_train = rsq(train, train_pred)
  # residuals = test[,1]- test_pred
  # test_IRF_pct_mean = mean(test[,1])
  # tss =  sum((test[,1] - test_IRF_pct_mean)^2 )
  # rss =  sum(residuals^2)
  # 
  # Calculate R-squared
  # rsq  =  1 - (rss/tss)
  # 
  a=data.frame("Dataset" = c("train", 
                             "test"),
               "No of observation"= c(dim(train)[1], 
                                      dim(test)[1]),
               "RMSE" = c(RMSE_TRAIN, 
                          RMSE_TEST),
               "R-square" = c(rsq_train, rsq_test ))
  return(a)
}
