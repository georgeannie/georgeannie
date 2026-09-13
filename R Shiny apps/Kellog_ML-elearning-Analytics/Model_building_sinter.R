library(randomForest)
library(ranger)
library(ggplot2)
#library(rsample)
library(caret)
library(h20)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rattle) 
library(PerformanceAnalytics)
library(corrplot)
library(ggcorrplot)
library(RColorBrewer)
library(Hmisc)
library(tree)
library(Metrics)
library(tidyr)
library(date)
library(reshape2)
library(tidyverse)
library(party)
library(dplyr)
library(xgboost)
library(stringr)
library(Ckmeans.1d.dp)
source("sinter_function.R")
####to keep the random picking same (reproducibility)###
set.seed(123)

MyData=read_rename_final_data()
names(MyData)=toupper(gsub(" ", "_", names(MyData)))

names(MyData)

#FUNCTION
## predict Internal Return Fines on train data
print_results = function(RF_model, test, train){
  train$pred <- predict(RF_model, train)$predictions
  
  print(head(var))
  var=as.data.frame(var)
  var$varnames = rownames(var)
  rownames(var) = NULL
# Plot Variable importance

  p=ggplot(var , 
       aes(x=reorder(varnames, var), y=var, fill=var)) + 
  geom_bar(stat="identity", position="dodge") + 
  coord_flip() +
  ggtitle("Information Value Summary") +
  xlab("") +
  guides(fill=F) + 
  scale_fill_gradient(low="red", high="blue")
  p

## predict Internal Return Fines on test data
  test$pred <- predict(RF_model, test)$predictions
  print(summary(test$pred))


## random forest (Using random forest package; syatem give up on this, using ranger package is better)
#RF_model2 <- randomForest(IRF_pct ~ MACHINE_SPEED_dial + COOLER_PRESSURE + COMBUSTION_AIR_FLOW + COMBUSTION_AIR_FLOW + IH_TEMP_M1 + Al2O3 + Fe + CaO + CO2 + Humidity, data = train, importance = TRUE)


## Calculate the RMSE of the predictions ON TEST DATA
  rmse_test <- rmse(actual = test$IRF_PCT, # the actual values
                  predicted = test$pred) # the predicted values
  print(rmse_test) # RMSE = 0.09519924

## Calculate the RMSE of the predictions ON Train DATA
  rmse_train <- rmse(actual = train$IRF_PCT, # the actual values
                   predicted = train$pred) # the predicted values
  print(rmse_train) # RMSE = 0.04406748

##ANother way of calculating RMSE on TEST DATA
  residuals = test$IRF_PCT - test$pred
  RMSE = sqrt(mean(residuals^2))
  print(RMSE) # RMSE = 0.09519924

## Calculate total sum of squares
  test_IRF_pct_mean = mean(test$IRF_PCT)
  test_IRF_pct_mean #result= 9.431212

  tss =  sum((test$IRF_PCT - test_IRF_pct_mean)^2 )
  tss #result = 70390.63

# Calculate residual sum of squares
  rss =  sum(residuals^2)
  rss #result = 393.6106

# Calculate R-squared
  rsq  =  1 - (rss/tss)
  print(rsq) #Result = 0.9944082

## Plot actual outcome vs predictions (predictions on x-axis)
  ggplot(test, aes(x = pred, y = IRF_PCT)) + 
    geom_point() + 
    geom_abline()

#Another way of Plot actual outcome vs predictions (predictions on x-axis)
  options(repr.plot.width=8, repr.plot.height=4)
  my_data = as.data.frame(cbind(predicted = test$pred,
                              observed = test$IRF_PCT))
# Plot predictions vs test data
  ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm) + ggtitle('Linear Regression ') + ggtitle("Random Forest: Prediction vs Test Data") +
  xlab("Predecited Internal Fine % ") + ylab("Observed Internal Fine % ") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
}


#1. MODEL 1 - USE ONLY COMPLETE OBSERVATIONS
#******************************************************************#
##Partioning Data into test and traindata setsfor Modeling##
#Training - 70% of data; Test - 30%  of data
#******************************************************************#

outcome <- "IRF_PCT"

## Complet obs - Defining Independent/Input Variable
vars <- c("IH_COMBUSTION_AIR_TEMP" ,  "IH_MIXED_GAS_TEMP"     ,   "WASTE_WATER_FLOW"       , 
          "TEMP_BEFORE_ESP"       ,   "WINDBOX_1_PRESSURE"    ,   "WINDBOX_12_PRESSURE"   ,  
          "COOLER_PRESSURE"       ,   "TOTAL_BMIX_FLOW"       ,   "TOTAL_CLIME_FLOW"      ,  
          "TOTAL_LIMESTONE_FLOW"  ,   "TOTAL_SOLID_FUEL_FLOW" ,   "CO"                    ,  
          "CO2"                   ,   "O2"                    ,   "MACHINE_SPEED"         ,  
          "COMBUSTION_AIR_FLOW"   ,   "COOLER_SPEED"          ,   "WASTE_GAS_FAN_RPM"     ,  
          "IH_MIXED_GAS_FLOW"     ,   "COOLER_DISCHRG_TEMP"   ,   "SINTER_TEMP_SM_DISCH"  ,  
          "IH_TEMP_M1"            ,   "SINTER_BED_HEIGHT"     ,   "FEO"                   ,  
          "CAO"                   ,   "AL2O3"                 ,   "TIO2"                  ,  
          "K2O"                   ,   "P"                     ,   "FE"  ) 

## randomly pick 70% of the number of observations 
MyData_comp = MyData[complete.cases(MyData),]
index <- sample(1:nrow(MyData_comp),size = 0.7*nrow(MyData_comp))

train <- MyData_comp [index,] 

test <- MyData_comp [-index,]

nrow(train)
nrow(test)
nrow(MyData_comp)

## Create the formula string for Internal Return Fines % as a function of the inputs

fmla <- paste(outcome, "~", paste(vars, collapse = " + "))

## Fit and print the random forest model on training data
(RF_model <- ranger(fmla,train, 
                    num.trees = 500, 
                    respect.unordered.factors = "order", importance = "impurity")) #OOB prediction error (MSE):       0.009584719 
#R squared (OOB):                  0.9942244 
(RF_model)

print_results(RF_model, test, train)

saveRDS(RF_model, "complete_case_rf.RDS")

#2. MODEL 2 - USE ONLY NOT NULL COLUMNS
vars <- c("IH_COMBUSTION_AIR_TEMP",   "IH_MIXED_GAS_TEMP"   ,     "WASTE_WATER_FLOW"   ,    
          "TEMP_BEFORE_ESP"       ,   "WINDBOX_1_PRESSURE"   ,    "WINDBOX_12_PRESSURE"  ,   
          "TOTAL_BMIX_FLOW"     ,     "TOTAL_CLIME_FLOW"    ,    
          "TOTAL_LIMESTONE_FLOW"  ,   "TOTAL_SOLID_FUEL_FLOW",    "CO"                  ,    
          "O2"                  ,     "MACHINE_SPEED"       ,    
          "COOLER_SPEED"        ,     "WASTE_GAS_FAN_RPM"   ,    
          "IH_MIXED_GAS_FLOW"     ,   "SINTER_TEMP_SM_DISCH",    
          "IH_TEMP_M1"            ,   "SINTER_BED_HEIGHT"   ,     "FEO"                 ,    
          "CAO"                  ,    "AL2O3"              ,      "TIO2"               ,     
          "K2O"                  ,    "FE"  ) 

index <- sample(1:nrow(MyData),size = 0.7*nrow(MyData))

train <- MyData[index,] 

test <- MyData [-index,]

row(train)
nrow(test)
nrow(MyData)

## Create the formula string for Internal Return Fines % as a function of the inputs

fmla <- paste(outcome, "~", paste(vars, collapse = " + "))

## Fit and print the random forest model on training data
(RF_model1 <- ranger(fmla,train, 
                    num.trees = 500, 
                    respect.unordered.factors = "order", importance = "impurity")) #OOB prediction error (MSE):       0.009584719 
#R squared (OOB):                  0.9942244 
(RF_model1)
print(RF_model1, test, train)
saveRDS(RF_model1, "no_na_rf.rds")

#3. Remove insignificant and correlated vars
vars <- c("IH_COMBUSTION_AIR_TEMP",   "IH_MIXED_GAS_TEMP"   ,         
          "TEMP_BEFORE_ESP"       ,   "WINDBOX_1_PRESSURE"   ,    "WINDBOX_12_PRESSURE"  ,   
          "TOTAL_BMIX_FLOW"     ,     "TOTAL_CLIME_FLOW"    ,    
          "TOTAL_LIMESTONE_FLOW"  ,   "TOTAL_SOLID_FUEL_FLOW",        
          "O2"                  ,     "MACHINE_SPEED"       ,    
          "COOLER_SPEED"        ,     "WASTE_GAS_FAN_RPM"   ,    
          "IH_MIXED_GAS_FLOW"     ,   "SINTER_TEMP_SM_DISCH",    
          "SINTER_BED_HEIGHT"   ,     "FEO"                 ,    
          "CAO"                  ,    "AL2O3"              ,        
          "FE"  ) 

## randomly pick 70% of the number of observations 
index <- sample(1:nrow(MyData),size = 0.7*nrow(MyData))

## subset MyData to include only the elements in the index
train <- MyData[index,] 

## subset MyData to include all but the elements in the index
test <- MyData [-index,]

## Checkin train data is 70% and test data is 30% of MyData
nrow(train)
nrow(test)
nrow(MyData)
## Create the formula string for Internal Return Fines % as a function of the inputs

fmla <- paste(outcome, "~", paste(vars, collapse = " + "))

## Fit and print the random forest model on training data
(RF_model2 <- ranger(fmla,train, 
                    num.trees = 500, 
                    respect.unordered.factors = "order", importance = "impurity")) #OOB prediction error (MSE):       0.009584719 
#R squared (OOB):                  0.9942244 
(RF_model2)

print(RF_model2, test, train)

saveRDS(RF_model2, "final_model_rf.RDS")

#******************************************************************#
##Xgboost
#******************************************************************#
set.seed(123)
#Delete variable from MyData that will not be used in model
MyData=MyData[,c(4, 19:48)]

index <- sample(1:nrow(MyData),size = 0.7*nrow(MyData))

xgtrain <- MyData [index,] 
xgtest <- MyData [-index,]

## Checkin train data is 70% and test data is 30% of MyData
nrow(xgtrain)
nrow(xgtest)
nrow(MyData)



## One hot encoding for training set
data_ohe <-as.data.frame(model.matrix(~.-1,data=xgtrain)) #it is a good prcatice if there are categorical variables in the model. For this data it will not do anything since we do not have categorical value
ohe_label <- data_ohe[,'IRF_PCT'] # Target variable - IRF_pct

head(data_ohe) #view top values 
str(data_ohe) #view structure of data

## One hot encoding for testing set
test_ohe <- as.data.frame(model.matrix(~.-1,data=xgtest))
test_label <- test_ohe[,'IRF_PCT'] # Target variable - IRF_pct

#Xgboost Model

##For xgboost, we'll use xgb.DMatrix to convert data table into a matrix (most recommended):
## Preparing Matrix
dtrain <- xgb.DMatrix(as.matrix(data_ohe%>%select(-IRF_PCT)),label=ohe_label)
dtest <- xgb.DMatrix(as.matrix(test_ohe%>%select(-IRF_PCT)),label=test_label)

#Training the xgboost model
set.seed(1234)

w <- list(xgtrain=dtrain,xgtest= dtest)

# xgb_model1 <- xgb.train(data=dtrain,booster='gbtree',nrounds=800,max_depth=6,eval_metric='rmse',eta=0.135,watchlist=w,early_stopping_rounds = 30) #result: xgtrain-rmse:0.059652	xgtest-rmse:0.078430
# summary(xgb_model1)
# xgb_model2 <- xgb.train(data=dtrain,booster='gbtree',nrounds=800,max_depth=6,eval_metric='rmse',eta=0.1,watchlist=w,early_stopping_rounds = 30) #result: xgtrain-rmse:0.057736	xgtest-rmse:0.079240
# 
# xgb_model3 <- xgb.train(data=dtrain,booster='gbtree',nrounds=1000,max_depth=8,eval_metric='rmse',eta=0.3,watchlist=w,early_stopping_rounds = 30) #result: xgtrain-rmse:0.050486	xgtest-rmse:0.080643

#Best Model is 2 (change nrounds to 114, based on the results from model 2)
xg_model1 <- xgb.train(data=dtrain,booster='gbtree',nrounds=800,max_depth=6,eval_metric='rmse',
                       eta=0.1,watchlist=w) 
xg_model1 = r

#Feature Importance
imp <- xgb.importance(colnames(dtrain),model=xg_model1)
xgb.plot.importance(imp) #similar to random forest CO2 and humidity are most important

#Prediction for test set
pred_IRF_pct <- predict(xg_model1,newdata = dtest, class='response')
pred_IRF_pct <- round(pred_IRF_pct)
head(pred_IRF_pct)

## Calculate the RMSE of the predictions ON TEST DATA
rmse_Xgtest <- rmse(actual = test_ohe$IRF_PCT, # the actual values
                  predicted = pred_IRF_pct) # the predicted values

rmse_Xgtest # RMSE= 0.2967

##ANother way of calculating RMSE
residuals = test_ohe$IRF_PCT - pred_IRF_pct
RMSE = sqrt(mean(residuals^2))
RMSE # RMSE = 0.2967445

## Calculate total sum of squares
test_ohe_IRF_pct_mean = mean(test_ohe$IRF_PCT)
test_ohe_IRF_pct_mean #result= 9.431212

tss =  sum((test_ohe$IRF_PCT - test_ohe_IRF_pct_mean)^2 )
tss #result = 70390.63

# Calculate residual sum of squares
rss =  sum(residuals^2)
rss #result = 3824.416

# Calculate R-squared
rsq  =  1 - (rss/tss)
rsq #Result = 0.9456687

xgb.ggplt=xgb.ggplot.importance(importance_matrix = imp,  n_clusters = 1)
# increase the font size for x and y axis
xgb.ggplt+theme( text = element_text(size = 10),
                 axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  geom_bar(fill="blue",width=0.5,stat="identity")  + 
  theme(legend.position = "none")

saveRDS(xg_model1, "xgb_all.rds")

r=readRDS("xgb_all.rds")
