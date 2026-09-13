library(readxl)
library(xlsx)
library(sqldf)
library(plyr)
library(tidyr)
library(caret)
#read monthly shipment/pos
sun=read_excel("sun_drivers.xlsx",  col_names=TRUE)
names(sun)

#Brand and #Conversion currency =USD
nzv <- nearZeroVar(sun, saveMetrics= TRUE)
nzv[nzv$nzv,]
nzv <- nearZeroVar(sun)
sun <- sun[ , !names(sun) %in% c("Brand", "Currency Code", "platform2")]
names(sun)

num_var=sun[,c(2:3,7, 11:16, 20:25, 29:31, 35:37, 41:43, 47:63, 65:67)]
summary(num_var)
#pairs(cor(num_var[,c(4:10)]), cutoff = 0.8)
hist(log(num_var$`Ordered Units`), breaks = 20)
shapiro.test(num_var$`Ordered Units`)
num_var[is.na(num_var)] = 0


library(corrplot)
cor_var=num_var[,c(4:44)]
findCorrelation(cor(cor_var), cutoff = 0.8)
corrplot(cor(num_var[,c(24:27)]), method="number", type="upper")
#Correlated -  4 Week Forecast
#            Shipped Cost of Goods Sold, Average Ordered Price, Orders Trend
#           Ordered amount, Orders Year-Over-Year Trend, 
#             shipped amount , Shipped Units Year-Over-Year Trend"
#          1 Week Forecast, shipped Amount Trend, Page Views Trend
#              shipped units, Shipped Cost of Goods Sold Year-Over-Year Trend
#           Ordered Units, Shipped Units Trend, Page Views Year-Over-Year Trend
#           Orders, 12 Week Forecast, Unique Visitors Index, Sellable On Hand Cost, Unsellable On Hand Cost
findCorrelation(cor(num_var[4:34]), cutoff = 0.8)

cat_var=sun[,c(4:6, 8:10, 17:19, 26:28, 32:34, 38:40, 44:46, 64, 68)]
cat_var = as.data.frame(unclass(cat_var))            ##convert dataframe to factors


sunny=cbind(num_var, cat_var)
training=sunny[sunny$`Week Start` < '2017-01-01', ]
testing=sunny[sunny$`Week Start` >= '2017-01-01', ]
training=rename(training, c("Ordered Units" = "Ordered_units"))


trainY=training$`Ordered Units`
testY=testing$`Ordered Units`
trainX=training[,-23]
testX=testing[,-23]
library(glmnet)
library(Matrix)
#training=rename(training, c("Ordered Units" = "Ordered_units"))
X=model.matrix(Ordered_units ~ ., training)

lassoFit =  glmnet(X, training, alpha = 1, lambda = lambda)
#read asin keys
asin=read_excel("Sun_products.xlsx", sheet="ASIN KEY", col_names=TRUE)
names(asin)

#rename fields to match
sun=rename(sun, c("UPC Code"= "upc_code", "Matl DP Parent Code" = "parent_code",
                  "Matl DP Parent Code__1" = "Product_desc"))
asin = rename(asin, c("UPC CODE" = "upc_code", "PARENT CODE" = "parent_code"))

#get unique upc for sun products
sun_upc=data.frame("upc_code"=unique(sun$upc_code))

#use the unique upc to find the unique ASIN keys for platsforms pn and .com
sun_asin=unique(sqldf("select a.upc_code, b.parent_code, b.ASIN, b.platforms
                      from sun_upc a 
                      left join asin b
                      on a.upc_code = b.upc_code
                      where b.platforms like '%.com%'
                      or b.platforms like '%pn%'
                      "))


#read the driver file
drivers=read_excel("drivers.xlsx", sheet="Sheet1", col_names=TRUE)
names(drivers)
drivers=rename(drivers, c("UPC"= "upc_code"))

#get the drivers for the asin and upc code for sun care sold on amazon
driver_sun=sqldf('select b.* from sun_asin a 
                 left join drivers b
                 on a.upc_code = b.upc_code
                 where a.asin = b.asin
                 ')
write.xlsx(driver_sun, "sun_drivers.xlsx")

#get all weekly data for the upc codes for sun products
sun_data=(sqldf("select b.*
                from sun_upc a 
                left join sun b
                on a.upc_code = b.upc_code
                "))

library(zoo)
#rearrange the weekly data as POS and shipment values and set null values as 0
monthly_sun=gather(sun_data, "year", "units", c(7:42))
monthly_sun$year=substring(monthly_sun$year, first=2)
monthly_sun$year=as.Date(as.yearmon(monthly_sun$year, "%m/%Y")) 
monthly_sun=monthly_sun[!(monthly_sun$Type == "POS" & monthly_sun$year <= "2016-07-01"), ]
monthly_sun=spread(monthly_sun, "Type", "units")
monthly_sun$POS[is.na(monthly_sun$POS)]=0
monthly_sun$Shipments[is.na(monthly_sun$Shipments)] = 0

write.xlsx(monthly_sun, "monthly_sun.xlsx")


#find the total of suncare products by POS and shipment
monthly_sun_total = sqldf('select year, sum(POS) as POS_TOTAL, sum(Shipments) as SHIPMENT_TOTAL
                          from monthly_sun
                          group by year')

#find difference in  POS and shipment over the weeks 
monthly_sun_ratio = sqldf('select year, (sum(POS) - sum(Shipments))/sum(POS) * 100 as Deficit_Percentage
                          from monthly_sun
                          group by year')
#all products monthly plot
library(ggplot2)
monthly_sun_total$year = as.Date(monthly_sun_total$year)
ggplot(data=monthly_sun_total, aes(x=year)) +
  geom_line(aes(y=POS_TOTAL, color="POS_TOTAL"), size=2)+
  geom_line(aes(y=SHIPMENT_TOTAL, color="SHIPMENT_TOTAL"), size=2) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  ylab(" ") +
  xlab("Weekly") + 
  scale_color_discrete(name = " ", labels = c("POS Total", "Shipment total")) +
  ggtitle("Shipment and POS totals for suncare products") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5, size=20),
        axis.ticks.y = element_blank())

#all products monthly difference in shipment and POS plot
monthly_sun_ratio$Deficit_Percentage = round(monthly_sun_ratio$Deficit_Percentage) 
monthly_sun_ratio$year = as.Date(monthly_sun_ratio$year)
ggplot(data=monthly_sun_ratio, aes(x=year, label=Deficit_Percentage)) +
  geom_line(aes(y=Deficit_Percentage), size=2, color="blue")+
  geom_point(aes(y=Deficit_Percentage)) +
  geom_text(aes(y=(Deficit_Percentage), hjust= -0.8, vjust=0.8)) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  ylab("% of differnce in sales and shipment ") +
  xlab("Weekly") +
  ggtitle("Difference in sales and shipment") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5, size=20),
        axis.ticks.y = element_blank()) 

#each product POS and shipment 
library(ggplot2)
monthly_sun$year = as.Date(monthly_sun$year)
ggplot(data=monthly_sun, aes(x=year)) +
  geom_line(aes(y=POS, color="POS_TOTAL"), size=1.5)+
  geom_line(aes(y=Shipments, color="SHIPMENT_TOTAL"), size=1.5) +
  facet_wrap(~Product_desc, ncol=4) +
  #theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  ylab(" ") +
  xlab("Weekly") + 
  scale_color_discrete(name = " ", labels = c("POS", "Shipment")) +
  ggtitle("Shipment and POS for each suncare products") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0.5, size=20),
        axis.ticks.y = element_blank())




#is there a difference in sales and shipments in the suncare products
#p-value = 0.1195 = There is difference
t.test(monthly_sun$POS[monthly_sun$year >='2016-08-01'], 
       monthly_sun$Shipments[monthly_sun$year >='2016-08-01'], 
       alternative = "two.sided")

#just more stats
library(psych)
describe(monthly_sun$POS)
hist(monthly_sun$POS)
describe(monthly_sun$Shipments)
hist(monthly_sun$Shipments)

#find difference in  POS and shipment over the weeks


#satisfaction checks?

