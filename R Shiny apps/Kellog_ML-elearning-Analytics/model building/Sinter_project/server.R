options(shiny.reactlog=TRUE)
#access reactive only with X()
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggcorrplot)
library(corrplot)
library(reshape2)
library(lubridate)
library(data.table)
library(DT)
library(GGally)
#library(ranger)
library(xgboost)

source("sinter_function.R")
set.seed(123)
sinter= read_rename_sinter()

shinyServer(function(input, output, session) {
    var=reactive({
        input$radio_dep_choice
    })
    
    var2=reactive({
      input$radio_dep_choice2
    })
    
    var3=reactive({
       input$plant
    })
    
    var4=reactive({
       input$plant_tab2
    })
    
    var5=reactive({
       input$plant_tab3
    })
    
    var7=reactive({
       input$plant_tab4
    })
    
   output$correlation_matrix = renderPlot({
      var4 = var4()
      
      sinter_list = switch(var4, 
             "1" = sinter[[1]],
             "2" = sinter[[2]],
             "3" = sinter[[3]],
             "4" = sinter[[4]]
      )
      
       dep_sinter=sinter_list[, names(sinter_list) %in% c("Shatter Index",
                                                       "%External Return Fine", 
                                                      "%Internal Return Fine", 
                                                      "Overall Efficiency")]
       dep_sinter[is.na(dep_sinter)]=0  
       corr_dep = cor(rev(dep_sinter))
       
       p=ggcorrplot(corr_dep,  col=c("red", "white", "blue"),
                      type = "lower", legend.title = "", outline.color = "grey",
                      show.diag = TRUE, lab=TRUE) +
            ggtitle("Correlation of Potential Dependent Variables") +
            theme(plot.title = element_text(hjust = 0.5, colour = "black", size=18, face='bold', 
                                            vjust=0.5))
       p
   })

   output$scatter_matrix = renderPlot({
      var4 = var4()
      
      sinter_list = switch(var4, 
                           "1" = sinter[[1]],
                           "2" = sinter[[2]],
                           "3" = sinter[[3]],
                           "4" = sinter[[4]]
      )
      
       dep_sinter=sinter_list[, names(sinter_list) %in% c("Overall Efficiency", 
                                                          "%Internal Return Fine", 
                                                "%External Return Fine", 
                                                "Shatter Index")]
       dep_sinter=dep_sinter%>% gather(key, value, -"Overall Efficiency") 
       dep_sinter=dep_sinter[complete.cases(dep_sinter),]
       p1=ggplot(dep_sinter, aes(x=value, y=`Overall Efficiency`))+
           geom_point(col="navy")+
           geom_smooth(method = "lm", col="red", se=FALSE) +
           facet_wrap( ~ key, scales = "free") + 
           theme_minimal() +
           ylab("Overall Efficiency") +
           xlab("") +
           ggtitle("Scatter plot of Potential Dependent Variables") +
           theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, vjust = 0.3),
                 strip.text.x =  element_text(size=12),
                 axis.ticks = element_blank(),
                 panel.background = element_rect(fill = "white", colour = "grey50"),
                 panel.grid = element_blank(),
                 axis.title.y=element_text(size=12, face="bold")
           )
       p1
   })
   
   output$histogram = renderPlot({
      var3 = var3()
      
      sinter_list = switch(var3, 
                           "1" = sinter[[1]],
                           "2" = sinter[[2]],
                           "3" = sinter[[3]],
                           "4" = sinter[[4]]
      )
      
       dep_sinter=sinter_list[, names(sinter_list) %in% c("Overall Efficiency", 
                                                          "%Internal Return Fine", 
                                                "%External Return Fine", 
                                                "Shatter Index")]
        
       dep_sinter=dep_sinter%>% gather(key, value)
       dep_sinter$key = factor(dep_sinter$key, levels=c("Overall Efficiency", 
                                                        "%Internal Return Fine", 
                                                        "%External Return Fine", 
                                                        "Shatter Index")) 
       p=ggplot(dep_sinter, aes(x=value))+
           geom_histogram(color="white", fill="navyblue", position="identity")+
           facet_wrap( ~ key, scales = "free") + 
           theme_minimal() +
           xlab("") +
           ylab("Frequency") +
           ggtitle("Histogram of Potential Dependent Variables") +
           theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, vjust = 0.3),
                 strip.text.x =  element_text(size=12),
                 axis.ticks = element_blank(),
                 panel.background = element_rect(fill = "white", colour = "grey50"),
                 panel.grid = element_blank(),
                 axis.title.y=element_text(size=12, face="bold")
                )
       p    
   })
   
   output$summary = renderDataTable({
      var5 = var5()
      
      sinter_list = switch(var5, 
                           "1" = sinter[[1]],
                           "2" = sinter[[2]],
                           "3" = sinter[[3]],
                           "4" = sinter[[4]]
      )
   
      dep_sinter=sinter_list[, names(sinter_list) %in% c("Overall Efficiency", 
                                                         "%Internal Return Fine", 
                                                         "%External Return Fine", 
                                                         "Shatter Index")]
      dep_summary=summary_table(dep_sinter)
      DT::datatable(dep_summary, options = list(dom = 't'))   
   })
   
   var6=function(){ 
      
   sinter_new = list()
   for (i in 1:length(sinter)){
      sinter_new[[i]] = sinter[[i]][, names(sinter[[i]]) %in% c("Overall Efficiency", 
                                                                   "%Internal Return Fine", 
                                                                   "%External Return Fine", 
                                                                   "Shatter Index")]
         }
   return(sinter_new)
   }
   
   output$data_no = renderDataTable({
      sinter_new=var6()
      dep_no = rbind(t(apply(sinter_new[[1]], 2, function(x) (sum(!is.na(x))))),
                     t(apply(sinter_new[[2]], 2, function(x) (sum(!is.na(x))))),
                     t(apply(sinter_new[[3]], 2, function(x) (sum(!is.na(x))))),
                     t(apply(sinter_new[[4]], 2, function(x) (sum(!is.na(x))))))
      
      rownames(dep_no) = make.names(c("Plant 1", "Plant 2", "Plant 3", "Plant 4"))
      DT::datatable(dep_no,  options = list(dom = 't'))
   })
   
   output$data_mean = renderDataTable({
      sinter_new=var6()
      dep_mean = rbind(t(round(apply(sinter_new[[1]], 2, mean, na.rm=TRUE), 2)),
                      t(round(apply(sinter_new[[2]], 2, mean, na.rm=TRUE), 2)),
                      t(round(apply(sinter_new[[3]], 2, mean, na.rm=TRUE), 2)),
                      t(round(apply(sinter_new[[4]], 2, mean, na.rm=TRUE), 2)))
      rownames(dep_mean) = make.names(c("Plant 1", "Plant 2", "Plant 3", "Plant 4"))
      
      DT::datatable(dep_mean,  options = list(dom = 't'))
   })
   
   output$data_sd = renderDataTable({
      sinter_new=var6()
      dep_sd = rbind(t(round(apply(sinter_new[[1]], 2, sd, na.rm=TRUE), 2)),
                     t(round(apply(sinter_new[[2]], 2, sd, na.rm=TRUE), 2)),
                     t(round(apply(sinter_new[[3]], 2, sd, na.rm=TRUE), 2)),
                     t(round(apply(sinter_new[[4]], 2, sd, na.rm=TRUE), 2)))
      rownames(dep_sd) = make.names(c("Plant 1", "Plant 2", "Plant 3", "Plant 4"))
      DT::datatable(dep_sd,  options = list(dom = 't'))
   })
      
   output$data_na = renderDataTable({
      sinter_new=var6()
      dep_na = rbind(t(apply(sinter_new[[1]], 2, function(x) sum(is.na(x)))),
                     t(apply(sinter_new[[2]], 2, function(x) sum(is.na(x)))),
                     t(apply(sinter_new[[3]], 2, function(x) sum(is.na(x)))),
                     t(apply(sinter_new[[4]], 2, function(x) sum(is.na(x)))))
      rownames(dep_na) = make.names(c("Plant 1", "Plant 2", "Plant 3", "Plant 4"))
      
      DT::datatable(dep_na,  options = list(dom = 't'))
   })
  
   output$data_plant = renderDataTable({
      var7 = var7()
      
      sinter_list = switch(var7, 
                           "1" = sinter[[1]],
                           "2" = sinter[[2]],
                           "3" = sinter[[3]],
                           "4" = sinter[[4]]
      )
      
      DT::datatable(sinter_list)   
   })
   
   
   output$all_input = renderDataTable({
      feed=feed_data()
      feed[, "Section"]="Feed"
      feed_summary = summary_table(feed)
      
      ignition=ignition_data()
      ignition[, "Section"]="Ignition"
      ignition_summary = summary_table(ignition)
      
      sinter_bed=sinter_bed()
      sinter_bed[, "Section"]="Sinter Bed"
      sinterbed_summary = summary_table(sinter_bed)
      
      stack=stack()
      stack[, "Section"]="Stack"
      stack_summary = summary_table(stack)
      
      esp=esp()
      esp[, "Section"]="ESP"
      esp_summary = summary_table(esp)
      
      cooler=cooler()
      cooler[, "Section"]="Cooler Bed"
      cooler_summary = summary_table(cooler)
      
      dep_summary= rbind(feed_summary, ignition_summary, sinterbed_summary, stack_summary, 
                    esp_summary, cooler_summary)
      tail(dep_summary)
      DT::datatable(dep_summary)   
   })
   
   output$data_feed = renderDataTable({
      sinter=feed_data()
      sinter[, "Section"]="Feed"
      
      dep_summary=summary_table(sinter)
      DT::datatable(dep_summary, options = list(dom = 't'))   
   })
   
   output$ignition_hood = renderDataTable({
      sinter=ignition_data()   
      sinter[, "Section"]="Ignition"
      dep_summary=summary_table(sinter)
      DT::datatable(dep_summary, options = list(dom = 't'))   
   })
   
   output$sinter_bed = renderDataTable({
      sinter=sinter_bed()
      sinter[, "Section"]="Sinter Bed"
      dep_summary=summary_table(sinter)
      DT::datatable(dep_summary, options = list(dom = 't'))   
   })
   
   output$stack = renderDataTable({
      sinter=stack()
      sinter[, "Section"]="Stack"
      dep_summary=summary_table(sinter)
      DT::datatable(dep_summary, options = list(dom = 't'))   
   })
   
   output$esp = renderDataTable({
      sinter=esp()
      sinter[, "Section"]="ESP"
      dep_summary=summary_table(sinter)
      DT::datatable(dep_summary, options = list(dom = 't'))   
   })
   
   output$cooler = renderDataTable({
      sinter[, "Section"]="Cooler Bed"
      sinter=cooler()
      dep_summary=summary_table(sinter)
      DT::datatable(dep_summary, options = list(dom = 't'))   
   })
   
   feed_choices=c("Select All", names(feed_data())[-length(feed_data())])
   ignition_choices=c("Select All", names(ignition_data())[-length(ignition_data())])
   stack_choices =c("Select All", names(stack())[-length(stack())])
   sinter_choices = c("Select All", names(sinter_bed())[-length(sinter_bed())])
   esp_choices = c("Select All", names(esp())[-length(esp())])
   cooler_choices = c("Select All", names(cooler())[-length(cooler())])

   # 
   observe({
      if("Select All" %in% input$in_feed){
         selected_choices1=feed_choices[-1] # choose all the choices _except_ "Select All"
         updateSelectInput(session, "in_feed", selected = selected_choices1)
      } else
         selected_choices1=input$in_feed # update the select input with choice selected by user
         
   })
    
   observe({
      if("Select All" %in% input$in_ignition){
         selected_choices2=ignition_choices[-1] # choose all the choices _except_ "Select All"
         updateSelectInput(session, "in_ignition", selected = selected_choices2)
       } else
         selected_choices2=input$in_ignition # update the select input with choice selected by user
     
   })

   observe({
      if("Select All" %in% input$in_sinter_bed){
         selected_choices3=sinter_choices[-1] # choose all the choices _except_ "Select All"
         updateSelectInput(session, "in_sinter_bed", selected = selected_choices3)
      } else
         selected_choices3=input$in_sinter_bed # update the select input with choice selected by user
      
   })

   observe({
      if("Select All" %in% input$in_stack){
         selected_choices4=names(stack()) # choose all the choices _except_ "Select All"
         updateSelectInput(session, "in_stack", selected = selected_choices4)
      }else
         selected_choices4=input$in_stack # update the select input with choice selected by user
   })
   # 
   observe({

      if("Select All" %in% input$in_esp){
         selected_choices5=names(esp()) # choose all the choices _except_ "Select All"
         updateSelectInput(session, "in_esp", selected = selected_choices5)
      } else
         selected_choices5=input$in_esp # update the select input with choice selected by user
   })
   # 
   observe({
      if("Select All" %in% input$in_cooler){
         selected_choices6=names(cooler()) # choose all the choices _except_ "Select All"
         updateSelectInput(session, "in_cooler", selected = selected_choices6)
      }
      else
         selected_choices6=input$in_cooler # update the select input with choice selected by user
   })

   var_feed_sel=reactive({
      input$in_feed

   })
   
   var_ignition_sel=reactive({
      input$in_ignition
      
   })
   
   var_sinter_sel=reactive({
      input$in_sinter_bed
      
   })
   
   var_stack_sel=reactive({
      input$in_stack
      
   })

   var_esp_sel=reactive({
      input$in_esp
      
   })
   
   var_cooler_sel=reactive({
      input$in_cooler
      
   })
   correlation =reactive({
      sinter=read_rename_final_data()
      var_feed_sel = var_feed_sel()
      if("Select All" %in% var_feed_sel)
         var_feed=feed_choices[-1] # choose all the choices _except_ "Select All"
      else
          var_feed = var_feed_sel
      
      var_ignition_sel = var_ignition_sel()
      if("Select All" %in% var_ignition_sel)
         var_ignition=ignition_choices[-1] # choose all the choices _except_ "Select All"
      else
         var_ignition = var_ignition_sel
      

      var_sinter_sel = var_sinter_sel()
      if("Select All" %in% var_sinter_sel)
         var_sinter=sinter_choices[-1] # choose all the choices _except_ "Select All"
      else
         var_sinter = var_sinter_sel
      
      var_stack_sel = var_stack_sel()
      if("Select All" %in% var_stack_sel)
         var_stack=stack_choices[-1] # choose all the choices _except_ "Select All"
      else
         var_stack = var_stack_sel
      
      var_esp_sel = var_esp_sel()
      if("Select All" %in% var_esp_sel)
         var_esp=esp_choices[-1] # choose all the choices _except_ "Select All"
      else
         var_esp = var_esp_sel
      
      var_cooler_sel = var_cooler_sel()
      if("Select All" %in% var_cooler_sel)
         var_cooler=cooler_choices[-1] # choose all the choices _except_ "Select All"
      else
         var_cooler = var_cooler_sel
      
      new_dataset = cbind(sinter[,var_feed], 
                          sinter[,var_ignition],
                          sinter[,var_stack],
                          sinter[,var_sinter],
                          sinter[,var_esp], 
                          sinter[,var_cooler])
         new_dataset = new_dataset[complete.cases(new_dataset),]
         cor(new_dataset, 
             use = "pairwise.complete.obs", method = "pearson")
   })
   output$corrPlot = renderPlot({
      val=correlation()
      #if(is.null(val)) return(NULL)
      
      val[is.na(val)] = 0
      args <- list(val)
      do.call(corrplot, c(args, method="color",  
                          type="upper",
                           tl.srt =45, #Text label color and rotation,
                          addCoef.col = "black", # Add coefficient of correlation
                          diag=FALSE,
                          title="Correlation of Independent Variables",
                          mar=c(0,0,1,0)))
      
   })
   
   output$warning <- renderUI({
      val <- correlation()
      if(is.null(val)) {
         tags$i("Waiting for data input...")
      } else {
         isNA <- is.na(val)
         if(sum(isNA)) {
            tags$div(
               tags$h4("Warning: The following pairs in calculated correlation have been converted to zero because they produced NAs!"),
               helpText("Consider using an approriate NA Action to exclude missing data"),
               renderTable(expand.grid(attr(val, "dimnames"))[isNA,]))
         }
      }
   })
   
   var_imp = reactive({
      input$model_rf
   })
   
   var_imp_xg = reactive({
      input$model_xg
   })
   
   model = function(rds_model){
      readRDS(rds_model)
   }
   
   model_def= function(var_imp){
      model=switch(var_imp,  
                     "1" = model("./models/complete_case_rf.RDS"),
                     "2" = model("./models/no_na_rf.rds"),
                     "3" = model("./models/final_model_rf.RDS"),
                     "4" = model("./models/xgb_all.RDS"),
                     "5" = model("./models/lm_all.rds"))
      return(model)
   }
   
   set_imp=function(model){
      
      var=sort(model$variable.importance, decreasing=TRUE) #Sort the Variable importance; Co2 and humidity most important
      
      var=as.data.frame(var)
      var$varnames = rownames(var)
      rownames(var) = NULL
      return(var)
   }

   model_plot_rf=function(model, var_imp){
      model_typ=model_def(var_imp)
      
      df=set_imp(model_typ)
      p=ggplot(df , 
             aes(x=reorder(varnames, var), y=var, fill=var)) + 
         geom_bar(stat="identity", position="dodge") + 
         coord_flip() +
         ggtitle("Information Value Summary") +
         xlab("") +
         ylab("") +
         guides(fill=F)+
         scale_fill_gradient(low="red", high="blue") +
            theme(legend.title = element_blank(), legend.position = "none") 
            
      p
   }
  
   model_plot_xg=function(model, var_imp, train){
      model_typ=model_def(var_imp)
      
      dtrain=xgb.DMatrix(as.matrix(train[, !names(train) %in% c("IRF_PCT")]),
                         label=train[, 1])
      
      imp = xgb.importance(colnames(dtrain),model=model_typ)
      xgb.ggplot.importance(importance_matrix = imp, n_cluster=1) + 
         geom_bar(fill="blue",width=0.5,stat="identity")  + 
         theme(legend.position = "none")
      
   }
   
   output$feature_imp = renderPlot({
        var_imp = var_imp()
         
        switch(var_imp,
            "1" = model_plot_rf("1", var_imp),
            "2" = model_plot_rf("2", var_imp),
            "3" = model_plot_rf("3", var_imp)
         )
   })
   
   output$feature_imp_xg= renderPlot({
      var_imp = var_imp_xg()
      data=data(var_imp)
      test_train =  xgtest_train(data)
      data_train = test_train[[1]]
      
      switch(var_imp,
             "4" = model_plot_xg("4", var_imp, data_train)
      )
   })
   
no_nulls= c("IH_COMBUSTION_AIR_TEMP",   "IH_MIXED_GAS_TEMP"   ,     "WASTE_WATER_FLOW"   ,    
               "TEMP_BEFORE_ESP"       ,   "WINDBOX_1_PRESSURE"   ,    "WINDBOX_12_PRESSURE"  ,   
               "TOTAL_BMIX_FLOW"     ,     "TOTAL_CLIME_FLOW"    ,    
               "TOTAL_LIMESTONE_FLOW"  ,   "TOTAL_SOLID_FUEL_FLOW",    "CO"                  ,    
               "O2"                  ,     "MACHINE_SPEED"       ,    
               "COOLER_SPEED"        ,     "WASTE_GAS_FAN_RPM"   ,    
               "IH_MIXED_GAS_FLOW"     ,   "SINTER_TEMP_SM_DISCH",    
               "IH_TEMP_M1"            ,   "SINTER_BED_HEIGHT"   ,     "FEO"                 ,    
               "CAO"                  ,    "AL2O3"              ,      "TIO2"               ,     
               "K2O"                  ,    "FE", "IRF_PCT"  ) 
   
final_result =  c("IH_COMBUSTION_AIR_TEMP",   "IH_MIXED_GAS_TEMP"   ,         
                     "TEMP_BEFORE_ESP"       ,   "WINDBOX_1_PRESSURE"   ,    "WINDBOX_12_PRESSURE"  ,   
                     "TOTAL_BMIX_FLOW"     ,     "TOTAL_CLIME_FLOW"    ,    
                     "TOTAL_LIMESTONE_FLOW"  ,   "TOTAL_SOLID_FUEL_FLOW",        
                     "O2"                  ,     "MACHINE_SPEED"       ,    
                     "COOLER_SPEED"        ,     "WASTE_GAS_FAN_RPM"   ,    
                     "IH_MIXED_GAS_FLOW"     ,   "SINTER_TEMP_SM_DISCH",    
                     "SINTER_BED_HEIGHT"   ,     "FEO"                 ,    
                     "CAO"                  ,    "AL2O3"              ,        
                     "FE" , "IRF_PCT" )
   
   data = function(var_imp){
      sinter=read_rename_final_data()
      names(sinter)=toupper(gsub(" ", "_", names(sinter)))
      
      #Get train and test data
      data = switch(var_imp,
                    "1"= (sinter[complete.cases(sinter), ]),
                    "2"= sinter[, no_nulls],
                    "3" = (sinter[, final_result]),
                    "4" = sinter[,c(4, 19:48)])
      return(data)
   }
   
   output$results = renderTable({
         var_imp = var_imp()
         data=data(var_imp)
         
         test_train =  test_train(data)
         
         #Get model
         model = model_def(var_imp)
         test=test_train[[2]]
         train=test_train[[1]]
         
         results_df_rf(test, train, model)
   })
   
   output$results_xg  = renderTable({
      var_imp=var_imp_xg()
      data=data(var_imp)
      test_train =  xgtest_train(data)
      
      #Get model
      model = model_def(var_imp)
      
      test=test_train[[2]]
      train=test_train[[1]]
      
      results_df_xg(test, train, model)
   })
   
   
   output$coeff_lm =renderDataTable({
      lm=model_def("5")
     
      DT::datatable(data.frame("Coefficients" = sort(round(lm$coefficients, 2), decreasing =TRUE)), 
                     options = list(sDom  = '<"top">flrt<"bottom">ip'))
   })
   
   output$feature_imp_lm = renderPlot({
      lm=model_def("5")
      sinter=read_rename_final_data()
      names(sinter)=toupper(gsub(" ", "_", names(sinter)))
      sinter=sinter %>%
         plyr::rename(c("FEO" = "FeO",
                      "FE" = "Fe"))
      sinter$pred=predict(lm, sinter )
      ggplot(sinter, aes(y=IRF_PCT, x=pred)) +
         geom_point(col="navyblue") +
         geom_smooth(method = "lm", se = FALSE, color = "red") + 
         xlab("Predicted (IRF PCT)") + 
         ylab("Actual (IRF PCT)") +
         ggtitle("Predicted vs Actuals") + 
         theme_light() +
         theme(plot.title = element_text(hjust = 0.5), 
               text=element_text(size=12, 
                            family="Serif"),
               axis.ticks = element_blank(),
               panel.grid = element_blank()) 
      
      
   })   
   
   output$results_lm =renderTable({
      lm=model_def("5")
      sinter=read_rename_final_data()
      names(sinter)=toupper(gsub(" ", "_", names(sinter)))
      sinter=sinter %>%
         plyr::rename(c("FEO" = "FeO",
                        "FE" = "Fe"))
      
      data=test_train(sinter)
      test=data[[2]]
      train=data[[1]]
      
      train_pred= predict(lm, train)
      test_pred=predict(lm, test)
      
      train_rmse = rmse(actual = train$IRF_PCT, train_pred)
      test_rmse =  rmse(actual = test$IRF_PCT, test_pred)
      
      rsq_train = rsq(train, train_pred)
      rsq_test = rsq(test, test_pred)
      
      a=data.frame("Dataset" = c("train", 
                                 "test"),
                   "No of observation"= c(dim(train)[1], 
                                          dim(test)[1]),
                   "RMSE" = c(train_rmse, 
                              test_rmse),
                   "R-square" = c(rsq_train, rsq_test ))
      a
      
   })

}) 