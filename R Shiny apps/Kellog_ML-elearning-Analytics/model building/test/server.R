options(shiny.reactlog=TRUE)
#access reactive only with X()
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggcorrplot)
library(plotly)
library(reshape2)
library(lubridate)
source("sinter_function.R")

sinter= read_rename_sinter()

shinyServer(function(input, output) {
    var=reactive({
        input$radio_dep_choice
    })
    
    pct_var = reactive({
        ts_sinter=sinter[, names(sinter) %in% c('Timestamp', "% Internal Return Fines", 
                                                "% External Return Fines", "Irf", "Erf")]
        ts_sinter$Irf = coalesce(ts_sinter$Irf, 0)
        ts_sinter$Erf = coalesce(ts_sinter$Erf, 0)
        ts_sinter$`% Internal Return Fines` = coalesce(ts_sinter$`% Internal Return Fines`, 0)
        ts_sinter$`% External Return Fines` = coalesce(ts_sinter$`% External Return Fines`, 0)
        
        ts_sinter = ts_sinter[complete.cases(ts_sinter),]
        ts_sinter = ts_sinter %>%
            mutate(Timestamp=as.POSIXct(Timestamp, format="%m/%d/%Y %H:%M:%S"),
                    day=as.Date(mdy(format(as.POSIXct(Timestamp), format="%m/%d/%Y")))) %>%
            select(-Timestamp)
        
        ts_sinter_ref=aggregate(ts_sinter["% Internal Return Fines"], by=ts_sinter["day"],
                                                                              mean)
        ts_sinter_ref=cbind(ts_sinter_ref,
                        aggregate(ts_sinter["% External Return Fines"], by=ts_sinter["day"],
                                mean))
        
        ts_sinter_ref=ts_sinter_ref[,-c(3)]
        
        ggplot(ts_sinter_ref, aes(y=  "% Internal Return Fines", x=as.numeric(day), col="blue")) +
            geom_line() +
            scale_y_continuous(limits=c(9.2, 11))
        scale_color_manual(values=c("#009E73", "#0072B2"),name="% Fines") +
        ts_sinter_daily =  data.frame(ts(ts_sinter_ref, start=c(2017, 04), end=c(2019, 03), 
                                      frequency=7))
        plot(ts_sinter_daily)  
        
        
        ts_sinter_monthly = ts(ts_sinter_ref, start=c(2017, 04), end=c(2019, 03), frequency=12)
        
        
        plot(ts_sinter_monthly[, '% Internal Return Fines'])
        plot(ts_sinter_ts[, '% External Return Fines'])
        
#convert to weekly
        
        ggplot(ts_sinter$`% Internal Return Fines` ~ ts_sinter$Timestamp, type="l", col="red")
        ts_sinter_week = aggregate(`% Internal Return Fines`~ week(Timestamp), 
                                   data=ts_sinter, 
                                   mean)
        ts_sinter_week2 = cbind(ts_sinter_week, 
                                aggregate(`% External Return Fines`~ week(Timestamp), 
                                          data=ts_sinter, 
                                          mean))
        ts_sinter_week = ts_sinter_week[-c(3)]
        ts_sinter_week = ts_sinter_week %>%
            plyr::rename(c("ts_sinter$`% Internal Return Fines`" = "Avg % Internal Return Fines",
                           "ts_sinter$`% External Return Fines`" = "Avg % External Return Fines", 
                           "week(ts_sinter$Timestamp)" = "Week starting"))
        
    })
    
    output$correlation_dependent = renderPlot({
      dep_sinter=sinter[, names(sinter) %in% c('% Sinter', "% Internal Return Fines", 
                                         "% External Return Fines", "Tumbling Index",
                                         "Shatter Index")]
      corr_dep = round(cor(dep_sinter, use="pairwise.complete.obs"), 1)
      ggcorrplot(corr_dep, col=c("navy", "lightblue", "white")) +
             ggtitle("Correlation of Potential Dependent Variables vs % Sinter") + 
             theme(plot.title = element_text(hjust = 0.5, colour = "black", size=15, face='bold',
                                              vjust=-2))
     })
    
    
    output$hist_dependent = renderPlot({
       dep_sinter=sinter[, names(sinter) %in% c("% Internal Return Fines", 
                                                 "% External Return Fines", 
                                                 "Shatter Index")]
            
       dep_sinter=dep_sinter%>% gather(key, value)
       print(head(dep_sinter))
       
       p=ggplot(dep_sinter, aes(x=value))+
                geom_histogram(color="black", fill="lightblue", position="identity")+
                facet_wrap( ~ key, scales = "free") + 
                theme_minimal() +
                xlab("") +
                ylab("Frequency") +
                ggtitle("Histogram of Potential Dependent Variables") +
                theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, vjust = 0.3),
                      strip.text.x =  element_text(size=12))
        p    
    })
    
    output$scatter_dependent = renderPlot({
        if (input$radio_dep_choice == "All"){
            dep_sinter=sinter[, names(sinter) %in% c("% Sinter", "% Internal Return Fines", 
                                                     "% External Return Fines", "Tumbling Index",
                                                     "Shatter Index")]
            
            dep_sinter=dep_sinter%>% gather(key, value, -'% Sinter')
            p=ggplot(dep_sinter, aes(x='% Sinter', y=value))+
                geom_point(color="lightblue")+
                facet_wrap(key ~ ., scales = "free") + 
                theme_minimal() +
                ylab("") +
                ggtitle("Histogram of Potential Dependent Variables") +
                theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 0.3),
                      strip.text.x =  element_text(size=12))
            p    
         } #else {
        #     var = var()
        #     dep_sinter=data.frame(var=sinter[,var]) %>%
        #         na.omit()
        #     
        #     p=ggplot(dep_sinter, aes(x=var))+
        #         geom_histogram(color="black", fill="lightblue", stat = "bin", na.rm = TRUE, binwidth = 1)+
        #         theme_minimal() +
        #         xlab("") +
        #         ylab("Frequency") +
        #         ggtitle(paste("Histogram of ", var)) +
        #         theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, vjust = 0.3))
        #     p
        # }
        
    })
    
    output$ts_dependent_week_pct= renderPlot({
        ts_sinter_week=pct_var()
        # ts_sinter_ref = ts_sinter %>%
        #     mutate(Timestamp=as.POSIXct(Timestamp, format="%m/%d/%Y %H:%M:%S")) %>%
        #     na.omit(Timestamp) %>%
        #     gather(key, value, -Timestamp)
        # ggplot(ts_sinter_ref, aes(x=Timestamp, y=value, col=key, group=1)) +
        #     geom_line() + 
        #     ggtitle("Variation in % Return Fines by Date and Time") +
        #     xlab("Date/Time") +
        #     ylab("% Return Fines")
          ggplot(ts_sinter_week, aes(x=Timestamp, y=value, col=key, group=1)) +
            geom_line() + 
            ggtitle("Variation in % Return Fines by Date and Time") +
            xlab("Date/Time") +
            ylab("% Return Fines")
        
    })
    output$ts_dependent_pct= renderPlot({
        ts_sinter_ref = ts_sinter %>%
            mutate(Timestamp=as.POSIXct(Timestamp, format="%m/%d/%Y %H:%M:%S")) %>%
            na.omit(Timestamp) %>%
            gather(key, value, -Timestamp)
        ggplot(ts_sinter_ref, aes(x=Timestamp, y=value, col=key, group=1)) +
            geom_line() + 
            ggtitle("Variation in % Return Fines by Date and Time") +
            xlab("Date/Time") +
            ylab("% Return Fines")
        
    })
        
    output$ts_dependent_pct_time = renderPlot({
        ts_sinter=sinter[, names(sinter) %in% c('Timestamp', "% Internal Return Fines", 
                                                 "% External Return Fines")]
        ts_sinter$`% Internal Return Fines`[is.na(ts_sinter$`% Internal Return Fines`)] = 0
        ts_sinter$`% External Return Fines`[is.na(ts_sinter$`% External Return Fines`)] = 0
        
        ts_sinter_ref = ts_sinter %>%
            mutate(Timestamp=as.POSIXct(Timestamp, format="%m/%d/%Y %H:%M:%S"),
                   time_dep=(format(as.POSIXct(Timestamp), format="%T")))  %>%
            filter(!is.na(Timestamp),
                   !is.na(time_dep)) %>%
               select(-Timestamp) %>%
            gather(key, value, -time_dep) %>%
            group_by(time_dep, key) %>%
            summarise(mean=round(mean(value),0))
            
        ggplot(ts_sinter_ref, aes(x=time_dep, y=mean, col=key, group=1)) +
        geom_line()
    })
    

    output$ts_dependent_pct_day = renderPlot({
        ts_sinter=sinter[, names(sinter) %in% c('Timestamp', "% Internal Return Fines", 
                                                "% External Return Fines")]
        ts_sinter$`% Internal Return Fines`[is.na(ts_sinter$`% Internal Return Fines`)] = 0
        ts_sinter$`% External Return Fines`[is.na(ts_sinter$`% External Return Fines`)] = 0
        
        ts_sinter_ref = ts_sinter %>%
            mutate(Timestamp=as.POSIXct(Timestamp, format="%m/%d/%Y %H:%M:%S"),
                   time_dep=(format(as.POSIXct(Timestamp), format="%D")))  %>%
            filter(!is.na(Timestamp),
                   !is.na(time_dep)) %>%
            select(-Timestamp) %>%
            gather(key, value, -time_dep) %>%
            group_by(time_dep, key) %>%
            summarise(mean=round(mean(value),0))
        
        ggplot(ts_sinter_ref, aes(x=time_dep, y=mean, col=key, group=1)) +
            geom_line()
    })
    
    
        input_var = reactive({
        input$radio_choice
    })
    
    output$hist = renderPlot({
        input_var=input_var()
        if (is.numeric(sinter[, input_var])){
            hist(sinter[,input_var],
                 xlab=as.character(input_var),
                 main=paste("Histogram of ", as.character(input_var)),
                 col="yellow")
        }
    })
    
    output$corr_ind = renderPlot({
        indep_sinter=sinter[,-c(1:2, 4:10)]
        corr_indep = round(cor(indep_sinter, use="pairwise.complete.obs"), 1)
        ggcorrplot(corr_indep, col=c("orange", "yellow", "black")) #+
        # ggtitle("Correlation plot") + 
        #theme(plot.title = element_text(hjust = 0.5, colour = "purple", size=18, face='bold'))
    })
    
    
    # Age_out = reactive({
    #     AgeBin(as.numeric(input$age), as.character(input$units))
    # })
    # 
    # AgeBin = function(age_in, units)(
    #     dplyr::case_when(((units == 'Days' & age_in < 31) |
    #                           (units == 'Months' & age_in < 12)) ~ ("<1"), 
    #                      
    #                      (units == 'Years' & (age_in >=1 & age_in < 6)) ~  ("1-5"), 
    #                      
    #                      (units == 'Years' & (age_in >=6 & age_in < 11)) ~ ("6-10") ,
    #                      
    #                      (units == 'Years' & (age_in >=11 & age_in < 18)) ~ ("11-17"), 
    #                      
    #                      (units == 'Years' & (age_in >=18 & age_in < 35))  ~ ("18-34"), 
    #                      
    #                      (units == 'Years' & (age_in >=35 & age_in < 50)) ~ ("35-49"), 
    #                      
    #                      (units == 'Years' & (age_in >=50 & age_in < 65)) ~ ("50-64"), 
    #                      TRUE  ~ ("65+"))
    #     
    # )
    # 
    # pre_sbp = reactive({
    #     sbp = as.numeric(input$sbp)
    #     dplyr::case_when(
    #         sbp < 100 ~ "<100",
    #         (sbp >=100 & sbp < 145) ~ "100 - 145",
    #         (sbp >=145 & sbp < 170) ~ "145 - 170",
    #         TRUE ~ ">170")
    # })
    # 
    # pre_pulse = reactive({
    #     pulse =as.numeric(input$pulse_rate)
    #     dplyr::case_when(
    #         pulse <= 65 ~ "<65",
    #         (pulse >65 & pulse <= 75) ~  "65 - 75",
    #         (pulse >75 & pulse <= 100) ~"75 - 100",
    #         TRUE ~ ">100")
    #     
    # })
    # 
    # pre_oxi = reactive({
    #     oxi=as.numeric(input$oximetry)
    #     dplyr::case_when(
    #         (oxi < 92) ~ ("<92"),
    #         (oxi >=92 & oxi < 95) ~ ("92 - 95"),
    #         (oxi >=95 & oxi < 98) ~ ("95 - 98"),
    #         TRUE ~ ">98")
    #     
    # })
    # 
    # var1=reactive({
    #     scorecard$Points[scorecard$Variable == 'Age' & scorecard$Bin == Age_out()]
    # })
    # 
    # var2=reactive({
    #     scorecard$Points[scorecard$Variable == 'gender' & scorecard$Bin == input$Gender]
    #     
    # })
    # 
    # var3=reactive({
    #     scorecard$Points[scorecard$Variable == 'vehicle accident' & scorecard$Bin == input$Vehicle_Accident]
    # })
    # 
    # var4=reactive({
    #     scorecard$Points[scorecard$Variable == 'GCS motor' & scorecard$Bin == input$gcs_motor]
    # })
    # 
    # var5= reactive({
    #     scorecard$Points[scorecard$Variable == 'GCS verbal' & scorecard$Bin == input$gcs_verbal]
    # })
    # 
    # var6=reactive({
    #     scorecard$Points[scorecard$Variable == 'Pulse oximetry' & scorecard$Bin == pre_oxi()]
    # })
    # 
    # var7=reactive({
    #     scorecard$Points[scorecard$Variable == 'sbp' & scorecard$Bin == pre_sbp()]
    # })
    # 
    # var8=reactive({
    #     scorecard$Points[scorecard$Variable == "pulse rate" & scorecard$Bin == pre_pulse()]
    # })
    # 
    # total=reactive({
    #     base = 334
    #     total=var1() + var2() + var3() + var4() + var5() + var6() + var7() + var8() + base
    # })
    # 
    # prob=reactive({
    #     new_data=data.frame('score' = total())
    #     probability = predict(model_rapid, new_data, type="response") * 100
    # })
    # 
    # prob_result=reactive({
    #     new_data=data.frame('score' = total())
    #     probability = predict(model_rapid, new_data, type="response") * 100
    #     ifelse(probability > 80, "Need Rapid Transport", "Not needed Rapid Transport")
    # })
    # 
    # 
    # outcome=reactive({
    #     ifelse (prob_result() == "Need Rapid Transport" & 
    #                 round(distance() * 60 / 30, 0) > 60,  "Recommend HEMS Transport", 
    #             "Recommend Ground Ambulance")
    # })
    # 
    # injury_zip =reactive({
    #     injury_zip=zipcode_info$rural_ind[zipcode_info$zip_code == input$injury_zip][1]
    #     
    # })
    # 
    # facility_zip = reactive({
    #     facility_zip=facility$zip_code[facility$facility_name == input$facility]
    # })
    # 
    # distance=reactive({
    #     injury_zip = input$injury_zip
    #     facility_zip = facility_zip()
    #     from_lat = zipcode$latitude[zipcode$zip == injury_zip]
    #     from_long = zipcode$longitude[zipcode$zip == injury_zip]
    #     to_lat = zipcode$latitude[zipcode$zip == facility_zip]
    #     to_long = zipcode$longitude[zipcode$zip == facility_zip]
    #     dist=get_geo_distance(from_long, from_lat, to_long, to_lat)
    #     distance=round((dist + 1)*1.3, 2)
    #     
    # })
    # 
    # # #---------------------------------------------------------------------------#
    # # # Find difference from normal in sbp for all ages                           #
    # # #---------------------------------------------------------------------------#
    # 
    # diff_sbp=reactive({
    #     prehospital_sbp_tr18_67 = as.numeric(input$sbp)
    #     years=as.numeric(input$age)
    #     units =input$units
    #     
    #     dplyr::case_when( 
    #         prehospital_sbp_tr18_67 == 0 ~ 0,
    #         (units == 'Days' & years < 31) |
    #             (units == 'Months' & years < 12) & prehospital_sbp_tr18_67 < 72 ~ (prehospital_sbp_tr18_67 - 72),
    #         (units == 'Days' & years < 31) |
    #             (units == 'Months' & years < 12)  & prehospital_sbp_tr18_67 > 104 ~  (prehospital_sbp_tr18_67 - 104),
    #         (years >=1 & years < 3) & (units == 'Years') &
    #             (prehospital_sbp_tr18_67 < 86) ~  (prehospital_sbp_tr18_67 - 86) ,
    #         (years >=1 & years < 3) & (units == 'Years') &
    #             (prehospital_sbp_tr18_67 > 106) ~  (prehospital_sbp_tr18_67 - 106) ,
    #         (years >=3 & years < 6)  & (units == 'Years') &
    #             (prehospital_sbp_tr18_67 < 89) ~  (prehospital_sbp_tr18_67 - 89),
    #         (years >=3 & years < 6) & (units == 'Years') &
    #             (prehospital_sbp_tr18_67 > 112) ~  (prehospital_sbp_tr18_67 - 112),
    #         (years >=6 & years < 10) & (units == 'Years') &
    #             (prehospital_sbp_tr18_67 < 97) ~  (prehospital_sbp_tr18_67 - 97),
    #         (years >=6 & years < 10) & (units == 'Years') &
    #             (prehospital_sbp_tr18_67 > 115) ~  (prehospital_sbp_tr18_67 - 115),
    #         (years >=10 & years < 12) & (units == 'Years') &
    #             (prehospital_sbp_tr18_67 < 101) ~  (prehospital_sbp_tr18_67 - 101),
    #         (years >=10 & years < 12) & (units == 'Years') &
    #             (prehospital_sbp_tr18_67 > 120) ~  (prehospital_sbp_tr18_67 - 120),
    #         (years >=12& years < 16) & (units == 'Years') &
    #             (prehospital_sbp_tr18_67 < 110) ~  (prehospital_sbp_tr18_67 - 110),
    #         (years >=12 & years < 16) & (units == 'Years') &
    #             (prehospital_sbp_tr18_67 > 131) ~  (prehospital_sbp_tr18_67 - 131),
    #         (years >=16) & (units == 'Years') &
    #             (prehospital_sbp_tr18_67 < 90) ~  (prehospital_sbp_tr18_67 - 90),
    #         (years >=16) & (units == 'Years') &
    #             (prehospital_sbp_tr18_67 > 120) ~  (prehospital_sbp_tr18_67 - 120),
    #         TRUE ~ 0
    #     )
    # })
    # 
    # # #---------------------------------------------------------------------------#
    # # # Find difference from normal in respiratory for all ages                   #
    # # #---------------------------------------------------------------------------#
    # # 
    # diff_resp=reactive({
    #     prehospital_respiratory_rate_tr18_70 = as.numeric(input$resp)
    #     years=as.numeric(input$age)
    #     units =input$units
    #     
    #     dplyr::case_when( 
    #         prehospital_respiratory_rate_tr18_70 == 0 ~ 0,
    #         ((units == 'Days' & years < 31) |
    #              (units == 'Months' & years < 12) & prehospital_respiratory_rate_tr18_70 < 30) ~ (prehospital_respiratory_rate_tr18_70 - 30),
    #         ((units == 'Days' & years < 31) |
    #              (units == 'Months' & years < 12) & prehospital_respiratory_rate_tr18_70 > 53) ~ (prehospital_respiratory_rate_tr18_70 - 53),
    #         (years >=1 & years < 3) &
    #             (prehospital_respiratory_rate_tr18_70 < 22) ~  (prehospital_respiratory_rate_tr18_70 - 22) ,
    #         (years >=1 & years < 3) &
    #             (prehospital_respiratory_rate_tr18_70 > 37) ~  (prehospital_respiratory_rate_tr18_70 - 37) ,
    #         (years >=3 & years < 6) &
    #             (prehospital_respiratory_rate_tr18_70 < 20) ~  (prehospital_respiratory_rate_tr18_70 - 20),
    #         (years >=3 & years < 6) &
    #             (prehospital_respiratory_rate_tr18_70 > 28) ~  (prehospital_respiratory_rate_tr18_70 - 28),
    #         (years >=6 & years < 12) &
    #             (prehospital_respiratory_rate_tr18_70 < 18) ~  (prehospital_respiratory_rate_tr18_70 - 18),
    #         (years >=6 & years < 12) &
    #             (prehospital_respiratory_rate_tr18_70 > 25) ~  (prehospital_respiratory_rate_tr18_70 - 25),
    #         (years >=12 & years < 16) &
    #             (prehospital_respiratory_rate_tr18_70 < 12) ~  (prehospital_respiratory_rate_tr18_70 - 12),
    #         (years >=12 & years < 16) &
    #             (prehospital_respiratory_rate_tr18_70 > 20) ~  (prehospital_respiratory_rate_tr18_70 - 20),
    #         (years >=16) &
    #             (prehospital_respiratory_rate_tr18_70 < 12) ~  (prehospital_respiratory_rate_tr18_70 - 12),
    #         (years >=16) &
    #             (prehospital_respiratory_rate_tr18_70 > 18) ~  (prehospital_respiratory_rate_tr18_70 - 18),
    #         TRUE ~ 0
    #     )
    # })
    # 
    # 
    # # #---------------------------------------------------------------------------#
    # # # Find the difference from normal of pulse                                  #
    # # #---------------------------------------------------------------------------#
    # 
    # diff_pulse=reactive({
    #     prehospital_pulse_rate_tr18_69 = as.numeric(input$pulse_rate)
    #     patient_age_tr1_12=as.numeric(input$age)
    #     patient_age_units_tr1_14 =input$units
    #     
    #     dplyr::case_when( 
    #         (patient_age_tr1_12 <28 & patient_age_units_tr1_14 == 'Days') &
    #             prehospital_pulse_rate_tr18_69 < 100        ~ (prehospital_pulse_rate_tr18_69 - 100),
    #         
    #         (patient_age_tr1_12 <28 & patient_age_units_tr1_14 == 'Days') &
    #             prehospital_pulse_rate_tr18_69 > 205         ~ (prehospital_pulse_rate_tr18_69 - 205),
    #         
    #         ((patient_age_tr1_12 >=28 & patient_age_units_tr1_14 == 'Days') |
    #              (patient_age_tr1_12 >=1 & patient_age_tr1_12 <=12 & patient_age_units_tr1_14 == 'Months')) &
    #             prehospital_pulse_rate_tr18_69 < 100        ~ (prehospital_pulse_rate_tr18_69 - 100),
    #         
    #         ((patient_age_tr1_12 >=28 & patient_age_units_tr1_14 == 'Days') |
    #              (patient_age_tr1_12 >=1 & patient_age_tr1_12 <=12 & patient_age_units_tr1_14 == 'Months')) &
    #             prehospital_pulse_rate_tr18_69 > 190      ~ (prehospital_pulse_rate_tr18_69 - 190),
    #         
    #         ((patient_age_tr1_12 >12 & patient_age_tr1_12 <= 24 & patient_age_units_tr1_14 == 'Months') |
    #              (patient_age_tr1_12 >=1 & patient_age_tr1_12 <= 2 & patient_age_units_tr1_14 == 'Years')) &
    #             prehospital_pulse_rate_tr18_69 < 98         ~ (prehospital_pulse_rate_tr18_69 - 98),
    #         
    #         ((patient_age_tr1_12 >12 & patient_age_tr1_12 <= 24 & patient_age_units_tr1_14 == 'Months') |
    #              (patient_age_tr1_12 >=1 & patient_age_tr1_12 <= 2 & patient_age_units_tr1_14 == 'Years')) &
    #             prehospital_pulse_rate_tr18_69 >140         ~ (prehospital_pulse_rate_tr18_69 - 140),
    #         
    #         (patient_age_tr1_12 >=3 & patient_age_tr1_12 <= 5 & patient_age_units_tr1_14 == 'Years') &
    #             prehospital_pulse_rate_tr18_69 < 80         ~ (prehospital_pulse_rate_tr18_69 - 80),
    #         
    #         (patient_age_tr1_12 >=3 & patient_age_tr1_12 <= 5 & patient_age_units_tr1_14 == 'Years') &
    #             prehospital_pulse_rate_tr18_69 > 120        ~ (prehospital_pulse_rate_tr18_69 - 120),
    #         
    #         (patient_age_tr1_12 >=6 & patient_age_tr1_12 <= 11 & patient_age_units_tr1_14 == 'Years') &
    #             prehospital_pulse_rate_tr18_69 < 75          ~ (prehospital_pulse_rate_tr18_69 - 75),
    #         
    #         (patient_age_tr1_12 >=6 & patient_age_tr1_12 <= 11 & patient_age_units_tr1_14 == 'Years') &
    #             prehospital_pulse_rate_tr18_69 > 118        ~ (prehospital_pulse_rate_tr18_69 - 118),
    #         
    #         (patient_age_tr1_12 >=12 & patient_age_units_tr1_14 == 'Years') &
    #             prehospital_pulse_rate_tr18_69 < 60           ~ (prehospital_pulse_rate_tr18_69 - 60),
    #         
    #         (patient_age_tr1_12 >=12 & patient_age_units_tr1_14 == 'Years') &
    #             prehospital_pulse_rate_tr18_69 > 100          ~ (prehospital_pulse_rate_tr18_69 - 100),
    #         
    #         (is.na(prehospital_pulse_rate_tr18_69)) ~ 0,
    #         TRUE ~ 0)
    # })
    # 
    output$Age_points = renderText({
        var1()
    })
    
    output$gender_points = renderText({
        var2()
    })
    
    output$accident_points = renderText({
        var3()
    })
    
    output$motor_points = renderText({
        var4()
    })
    
    output$verbal_points = renderText({
        var5()
    })
    
    output$oxi_points = renderText({
        var6()
    })
    
    output$sbp_points = renderText({
        var7()
    })
    
    output$pulse_points = renderText({
        var8()
    })
    
    
    output$Total_points = renderText({
        total()
        
    })
    
    output$rapid_points = renderText({
        prob_result()
    }) 
    
    output$rural_ind = renderText({
        injury_zip()
    }) 
    
    output$distance = renderText({
        round(distance(),0)
    }) 
    
    output$time = renderText({
        round(distance() * 60 / 30, 0)
    })
    
    output$diff_sbp = renderText({
        round(diff_sbp(), 0)
    })
    
    output$diff_resp = renderText({
        round(diff_resp(), 0)
    })
    
    output$diff_pulse = renderText({
        round(diff_pulse(), 0)
    })
    
    output$Outcome = renderText({
        outcome()
    })
    
    
}) #do not touch   
