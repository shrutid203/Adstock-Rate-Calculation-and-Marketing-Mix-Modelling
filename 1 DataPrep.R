#############################################################################################
############################ 0. Installing and Loading Libraries ############################
#############################################################################################
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("minpack.lm")
#install.packages("plotrix")
#install.packages("Hmisc")
#install.packages("data.table")
#install.packages("car")
#install.packages("zoo")
#install.packages("glmnet")
#install.packages("DataCombine")
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotrix)
library(Hmisc)
library(data.table)
library(MASS)
library(car)
library(minpack.lm)
library(zoo)
library(glmnet)
library(DAAG)
library(DataCombine)
library(openxlsx)

###################################################################################################
############################ 1. Initial Data Loading, EDA and Cleaning ############################
###################################################################################################

# 1.1 Load Dataset in working directory 
eleck <- read.csv("ConsumerElectronics.csv")


# 1.2 Look at the structure of the data
str(eleck)

# 1.3 Summary of dataset 
summary(eleck)


# 1.4 Data should be between july 2015 and June 2016 as per problem statement
eleck_f <- filter(eleck, (Month >6 & Year==2015|Month <7 & Year==2016))

# 1.5 Check above operation
max(paste(eleck_f$Year,eleck_f$Month))
min(paste(eleck_f$Year,eleck_f$Month))


# 1.6 Convert the date timestamp into date
eleck_f$date<-date(eleck_f$order_date)
# Checking if the dates fall into the category
max(eleck_f$date)
min(eleck_f$date)

# Note: For the analysis we will be taking only the relevant columns, so that we don't remove rows with data if there is NA in irrelevant columns

eleck_f <- subset(eleck_f, select = -c(cust_id,pincode))

# 1.7 Counting the missing values. There are 4904 missing values in gmv,cust_id and pincode.
# These values can be removed since % of NA values is less than 1%
sapply(eleck_f, function(x) sum(is.na(x)))

# Removing NA values
eleck_f<- na.omit(eleck_f)



# 1.8 Since the model needs to be at weekly level, calculating week of month
eleck_f$week <-isoweek(eleck_f$order_date)

# 1.9 Replacing where GMV=0 with 1, since GMV can not be 0 and 1 Re will be numerically negligible while modelling
eleck_f$gmv[which(eleck_f$gmv==0)] <- 1 

# 1.10 Removing the products which have MRP=0 , since we are not considering free items.
eleck_f<-eleck_f[eleck_f$product_mrp>0,]


# 1.11 Removing the orders where gmv > mrp * units as that entails a data issue
eleck_f$discount = eleck_f$gmv- (eleck_f$product_mrp*eleck_f$units)
eleck_f<-filter(eleck_f,discount>=0)

# 1.12 Filtering dataset for three sub-categories camera accessory, home audio and gaming accessory as per requirements in problem statement
eleck_f2<-filter(eleck_f,product_analytic_sub_category %in% c("GamingAccessory","CameraAccessory","HomeAudio"))


# 1.13 Making weeks after 2015 sequential after 53 (last week of 2015), so that sequence of the time is preserved.
# Week data starts from 27 (2015) to 79 (2016) which is 53 weeks for the analysis
eleck_f2$week<- ifelse(eleck_f2$week<=26 & eleck_f2$Year==2016,eleck_f2$week  +53,eleck_f2$week)

########################################################################################
############################ 2. EDA and Feature Engineering ############################
########################################################################################

# 2.1 Deriving Advertising KPIs for Model at a Weekly Level
###########################################################

# 2.1.1 We will prorate the Monthly spends at weekly level by first getting it at a daily level (Monthly/days in month). Then, the data is  rolled up to Week level to calculate adstock data at a weekly level

week_day_data <- data.frame(eleck_f2$Month,eleck_f2$week,eleck_f2$date,eleck_f2$Year,eleck_f2$gmv,eleck_f2$product_analytic_sub_category)
colnames(week_day_data)=c("Month","Week","Date","Year","gmv","sub_cat")
week_day_data<-week_day_data %>% group_by(Month,Week,Date,Year,sub_cat) %>% summarise(sales=sum(gmv))
week_day_data=data.frame(week_day_data)

# 2.1.2  We will then Imputing missing dates with sales = 0. This is done so that we dont miss out on the adstock's lag effect

all_days_ga<- data.frame(Date=seq(as.Date("2015-07-01"),as.Date("2016-06-30"),by="day"),sub_cat="GamingAccessory")
all_days_ca<-data.frame(Date=seq(as.Date("2015-07-01"),as.Date("2016-06-30"),by="day"),sub_cat="CameraAccessory")
all_days_ha<-data.frame(Date=seq(as.Date("2015-07-01"),as.Date("2016-06-30"),by="day"),sub_cat="HomeAudio")
all_days<-rbind(all_days_ca,all_days_ga,all_days_ha)
all_days$Week <- isoweek(all_days$Date)
all_days$Month <- month(all_days$Date)
all_days$Year <- year(all_days$Date)
all_days$Week<-ifelse(all_days$Week<=26 & all_days$Year==2016,all_days$Week  +53,all_days$Week)

# 2.1.3 Left Join week_day_data with all_days data frame.
week_day_data<-merge(all_days,week_day_data,all.x = T)
week_day_data[is.na(week_day_data)]<- 0
rm(all_days)
rm(all_days_ca)

# 2.1.4 Calculating monthly days data
monthly_days <- week_day_data %>% group_by(sub_cat,Year,Month) %>% summarise(days=n())
monthly_days=data.frame(monthly_days)

# 2.1.5 We merge the data to get data at day level

colnames(monthly_days)=c("sub_cat","Year","Month","Days_in_month")
week_day_data<-merge(week_day_data,monthly_days)


#2.1.6 Get money spent on media information. Manually taking Media info from Media Investment Tab
media<- read.xlsx("Media data and other information.xlsx", sheet = 2, startRow = 3)
colnames(media)[1]<-"Year"
media$Total.Investment<- NULL

#2.1.7 We currently have media spends for all 14 subcategories. Assuming that the spends are done equally across all categories
media$TV<-media$TV/14
media$Digital<-media$Digital/14
media$Sponsorship <-media$Sponsorship/14
media$Content.Marketing<-media$Content.Marketing/14
media$Online.marketing<-media$Online.marketing/14
media$Affiliates<-media$Affiliates/14
media$SEM <- media$SEM/14
media$Radio <-media$Radio/14
media$Other <-media$Other/14


#2.1.8 Merge Media investment with week_day_data 

week_day_data<-merge(week_day_data,media)


#2.1.9 Assuming equal amount of money is spent for all sale days.
week_day_data$TV_new<-week_day_data$TV/week_day_data$Days_in_month
week_day_data$Digital_new<-week_day_data$Digital/week_day_data$Days_in_month
week_day_data$Sponsorship_new<-week_day_data$Sponsorship/week_day_data$Days_in_month
week_day_data$Content.Marketing_new<-week_day_data$Content.Marketing/week_day_data$Days_in_month
week_day_data$Online.marketing_new <-week_day_data$Online.marketing/week_day_data$Days_in_month
week_day_data$Affiliates_new <- week_day_data$Affiliates/week_day_data$Days_in_month
week_day_data$SEM_new <- week_day_data$SEM/week_day_data$Days_in_month
week_day_data$Radio_new<-week_day_data$Radio/week_day_data$Days_in_month
week_day_data$Other_new <-week_day_data$Other/week_day_data$Days_in_month

#2.1.10 Roll up Weekly media data

media_week_data=week_day_data %>% group_by(Week,sub_cat) %>% summarise(TV=sum(TV_new),Digital=sum(Digital_new),Sponsorship=sum(Sponsorship_new),Content_Marketing=sum(Content.Marketing_new),Online_Marketing=sum(Online.marketing_new),Affiliates=sum(Affiliates_new),SEM=sum(SEM_new),Radio=sum(Radio_new),Other=sum(Other_new),WK_Sales=sum(sales))

media_week_data=as.data.frame(media_week_data)

# 2.2 Calculating Optimized Adstock Rate (Customized for Each Sub Category)
###########################################################################

# 2.2.1 Creating function to calculate optimized Adstock Rate
# We need to calculate an optimum adstock rate for all models. We will be optimising the formula Predicted Sales = B0 + B1 * adstock(Advertising)

# Run Optimization

adstock <- function(x, rate=0){
  return(as.numeric(stats::filter(x=x, filter=rate, method="recursive")))
}

optimizeAdStock <- function(spends,sales,week,media_name,subcat){
  
  modFit <- nlsLM(sales~b0+b1*adstock(spends, rate),
                  start=c(b0=1, b1=1, rate=0))  
  rmax<-coef(modFit)[3]
  
  rmax<-ifelse(rmax<0,0,rmax)
  adstock_final<-stats::filter(spends, filter=rmax, method = "recursive")
  title=paste("Sales,Adstock vs time",rmax)
  df= data.frame(subcat,adstock_final,sales,week,spends,adstockrate=rmax)
  df$adstock_final=as.numeric(df$adstock_final)
  return (df)
}


# 2.2.2 Calculating the optimized Adstock for Gaming Accessories for various Media spends
#######################################################################################

gamingacc_media <- media_week_data[media_week_data$sub_cat %in% "GamingAccessory",]
gamingacc_media<-gamingacc_media[order(gamingacc_media$Week),]


# 2.2.2.1 For TV 

TV_1<-optimizeAdStock(gamingacc_media$TV*10000000,gamingacc_media$WK_Sales,gamingacc_media$Week,"TV",gamingacc_media$sub_cat)


e1<- twoord.plot(TV_1$week,TV_1$sales/10000000,TV_1$week,TV_1$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and TV Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e1

p1<- twoord.plot(TV_1$week,TV_1$sales/10000000,TV_1$week,TV_1$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and TV Adstock vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p1
p2<- ggplot(TV_1,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "TV AdStock", y = "GMV")
p2


# 2.2.2.2 For Digital

Digital_1<-optimizeAdStock(gamingacc_media$Digital*10000000,gamingacc_media$WK_Sales,gamingacc_media$Week,"Digital",gamingacc_media$sub_cat)


e2<- twoord.plot(Digital_1$week,Digital_1$sales/10000000,Digital_1$week,Digital_1$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Digital Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e2

p3<- twoord.plot(Digital_1$week,Digital_1$sales/10000000,Digital_1$week,Digital_1$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Digital Adstock vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p3
p4<- ggplot(Digital_1,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Digital AdStock", y = "GMV")
p4


# 2.2.2.3 For Sponsorship

Sponsorship_1<-optimizeAdStock(gamingacc_media$Sponsorship*10000000,gamingacc_media$WK_Sales,gamingacc_media$Week,"Sponsorship",gamingacc_media$sub_cat)

e3<- twoord.plot(Sponsorship_1$week,Sponsorship_1$sales/10000000,Sponsorship_1$week,Sponsorship_1$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Sponsorship Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e3

p5<- twoord.plot(Sponsorship_1$week,Sponsorship_1$sales/10000000,Sponsorship_1$week,Sponsorship_1$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Sponsorship Adstock vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p5
p6<- ggplot(Sponsorship_1,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Sponsorship AdStock", y = "GMV")
p6


# 2.2.2.4 For Content.Marketing

Content.Marketing_1<-optimizeAdStock(gamingacc_media$Content_Marketing*10000000,gamingacc_media$WK_Sales,gamingacc_media$Week,"Content_Marketing",gamingacc_media$sub_cat)

e4<- twoord.plot(Content.Marketing_1$week,Content.Marketing_1$sales/10000000,Content.Marketing_1$week,Content.Marketing_1$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Content Marketing Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e4

p7<- twoord.plot(Content.Marketing_1$week,Content.Marketing_1$sales/10000000,Content.Marketing_1$week,Content.Marketing_1$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Content.Marketing Adstock vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p7
p8<- ggplot(Content.Marketing_1,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Content.Marketing AdStock", y = "GMV")
p8


# 2.2.2.5 For Online.marketing

Online.marketing_1<-optimizeAdStock(gamingacc_media$Online_Marketing*10000000,gamingacc_media$WK_Sales,gamingacc_media$Week,"Online.marketing",gamingacc_media$sub_cat)

e5<- twoord.plot(Online.marketing_1$week,Online.marketing_1$sales/10000000,Online.marketing_1$week,Online.marketing_1$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Online Marketing Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e5

p9<- twoord.plot(Online.marketing_1$week,Online.marketing_1$sales/10000000,Online.marketing_1$week,Online.marketing_1$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Online.marketing Adstock vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p9
p10<- ggplot(Online.marketing_1,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Online.marketing AdStock", y = "GMV")
p10

# 2.2.2.6 For Affiliates

Affiliates_1<-optimizeAdStock(gamingacc_media$Affiliates*10000000,gamingacc_media$WK_Sales,gamingacc_media$Week,"Affiliates",gamingacc_media$sub_cat)

e6<- twoord.plot(Affiliates_1$week,Affiliates_1$sales/10000000,Affiliates_1$week,Affiliates_1$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Affiliate Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e6

p11<- twoord.plot(Affiliates_1$week,Affiliates_1$sales/10000000,Affiliates_1$week,Affiliates_1$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and Affiliates Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p11
p12<- ggplot(Affiliates_1,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Affiliates AdStock", y = "GMV")
p12

# 2.2.2.7 For SEM

SEM_1<-optimizeAdStock(gamingacc_media$SEM*10000000,gamingacc_media$WK_Sales,gamingacc_media$Week,"SEM",gamingacc_media$sub_cat)

e7<- twoord.plot(SEM_1$week,SEM_1$sales/10000000,SEM_1$week,SEM_1$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and SEM Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e7

p13<- twoord.plot(SEM_1$week,SEM_1$sales/10000000,SEM_1$week,SEM_1$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and SEM Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p13
p14<- ggplot(SEM_1,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "SEM AdStock", y = "GMV")
p14

# 2.2.3 Calculating the optimized Adstock for Camera Accessory for various Media spends
#######################################################################################

camacc_media <- media_week_data[media_week_data$sub_cat %in% "CameraAccessory",]
camacc_media<-camacc_media[order(camacc_media$Week),]

# 2.2.3.1 For TV 

TV_1_camacc<-optimizeAdStock(camacc_media$TV*10000000,camacc_media$WK_Sales,camacc_media$Week,"TV",camacc_media$sub_cat)

e8<- twoord.plot(TV_1_camacc$week,TV_1_camacc$sales/10000000,TV_1_camacc$week,TV_1_camacc$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and TV Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e8

p15<- twoord.plot(TV_1_camacc$week,TV_1_camacc$sales/10000000,TV_1_camacc$week,TV_1_camacc$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and TV Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p15
p16<- ggplot(TV_1_camacc,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "TV AdStock", y = "GMV")
p16


# 2.2.3.2 For Digital
Digital_1_camacc<-optimizeAdStock(camacc_media$Digital*10000000,camacc_media$WK_Sales,camacc_media$Week,"Digital",camacc_media$sub_cat)

e9<- twoord.plot(Digital_1_camacc$week,Digital_1_camacc$sales/10000000,Digital_1_camacc$week,Digital_1_camacc$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Digital Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e9

p17<- twoord.plot(Digital_1_camacc$week,Digital_1_camacc$sales/10000000,Digital_1_camacc$week,Digital_1_camacc$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and Digital Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p17
p18<- ggplot(Digital_1_camacc,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Digital AdStock", y = "GMV")
p18


# 2.2.3.3 For Sponsorship
Sponsorship_1_camacc<-optimizeAdStock(camacc_media$Sponsorship*10000000,camacc_media$WK_Sales,camacc_media$Week,"Sponsorship",camacc_media$sub_cat)

e10<- twoord.plot(Sponsorship_1_camacc$week,Sponsorship_1_camacc$sales/10000000,Sponsorship_1_camacc$week,Sponsorship_1_camacc$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Sponsorship Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e10

p19<- twoord.plot(Sponsorship_1_camacc$week,Sponsorship_1_camacc$sales/10000000,Sponsorship_1_camacc$week,Sponsorship_1_camacc$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and Sponsorship Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p19
p20<- ggplot(Sponsorship_1_camacc,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Sponsorship AdStock", y = "GMV")
p20


# 2.2.3.4 For Content.Marketing
Content.Marketing_1_camacc<-optimizeAdStock(camacc_media$Content_Marketing*10000000,camacc_media$WK_Sales,camacc_media$Week,"Content_Marketing",camacc_media$sub_cat)

e11<- twoord.plot(Content.Marketing_1_camacc$week,Content.Marketing_1_camacc$sales/10000000,Content.Marketing_1_camacc$week,Content.Marketing_1_camacc$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Content Marketing Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e11

p21<- twoord.plot(Content.Marketing_1_camacc$week,Content.Marketing_1_camacc$sales/10000000,Content.Marketing_1_camacc$week,Content.Marketing_1_camacc$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and Content.Marketing Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p21
p22<- ggplot(Content.Marketing_1_camacc,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Content.Marketing AdStock", y = "GMV")
p22


# 2.2.3.5 For Online.marketing
Online.marketing_1_camacc<-optimizeAdStock(camacc_media$Online_Marketing*10000000,camacc_media$WK_Sales,camacc_media$Week,"Online.marketing",camacc_media$sub_cat)

e12<- twoord.plot(Online.marketing_1_camacc$week,Online.marketing_1_camacc$sales/10000000,Online.marketing_1_camacc$week,Online.marketing_1_camacc$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Online Marketing Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e12

p23<- twoord.plot(Online.marketing_1_camacc$week,Online.marketing_1_camacc$sales/10000000,Online.marketing_1_camacc$week,Online.marketing_1_camacc$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and Online.marketing Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p23
p24<- ggplot(Online.marketing_1_camacc,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Online.marketing AdStock", y = "GMV")
p24

# 2.2.3.6 For Affiliates
Affiliates_1_camacc<-optimizeAdStock(camacc_media$Affiliates*10000000,camacc_media$WK_Sales,camacc_media$Week,"Affiliates",camacc_media$sub_cat)

e13<- twoord.plot(Affiliates_1_camacc$week,Affiliates_1_camacc$sales/10000000,Affiliates_1_camacc$week,Affiliates_1_camacc$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Affiliates Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e13

p25<- twoord.plot(Affiliates_1_camacc$week,Affiliates_1_camacc$sales/10000000,Affiliates_1_camacc$week,Affiliates_1_camacc$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and Affiliates Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p25
p26<- ggplot(Affiliates_1_camacc,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Affiliates AdStock", y = "GMV")
p26

# 2.2.3.7 For SEM
SEM_1_camacc<-optimizeAdStock(camacc_media$SEM*10000000,camacc_media$WK_Sales,camacc_media$Week,"SEM",camacc_media$sub_cat)

e14<- twoord.plot(SEM_1_camacc$week,SEM_1_camacc$sales/10000000,SEM_1_camacc$week,SEM_1_camacc$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and SEM Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e14

p27<- twoord.plot(SEM_1_camacc$week,SEM_1_camacc$sales/10000000,SEM_1_camacc$week,SEM_1_camacc$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and SEM Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p27
p28<- ggplot(SEM_1_camacc,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "SEM AdStock", y = "GMV")
p28

# 2.2.4 Calculating the optimized Adstock for Home Audio for various Media spends
#################################################################################

homeaud_media <- media_week_data[media_week_data$sub_cat %in% "HomeAudio",]
homeaud_media<-homeaud_media[order(homeaud_media$Week),]

# 2.2.4.1 For TV
TV_1_homeaud<-optimizeAdStock(homeaud_media$TV*10000000,homeaud_media$WK_Sales,homeaud_media$Week,"TV",homeaud_media$sub_cat)

e15<- twoord.plot(TV_1_homeaud$week,TV_1_homeaud$sales/10000000,TV_1_homeaud$week,TV_1_homeaud$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and TV Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e15

p29<- twoord.plot(TV_1_homeaud$week,TV_1_homeaud$sales/10000000,TV_1_homeaud$week,TV_1_homeaud$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and TV Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p29
p30<- ggplot(TV_1_homeaud,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "TV AdStock", y = "GMV")
p30


# 2.2.4.2 For Digital
Digital_1_homeaud<-optimizeAdStock(homeaud_media$Digital*10000000,homeaud_media$WK_Sales,homeaud_media$Week,"Digital",homeaud_media$sub_cat)

e16<- twoord.plot(Digital_1_homeaud$week,Digital_1_homeaud$sales/10000000,Digital_1_homeaud$week,Digital_1_homeaud$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Digital Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e16

p31<- twoord.plot(Digital_1_homeaud$week,Digital_1_homeaud$sales/10000000,Digital_1_homeaud$week,Digital_1_homeaud$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and Digital Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p31
p32<- ggplot(Digital_1_homeaud,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Digital AdStock", y = "GMV")
p32


# 2.2.4.3 For Sponsorship
Sponsorship_1_homeaud<-optimizeAdStock(homeaud_media$Sponsorship*10000000,homeaud_media$WK_Sales,homeaud_media$Week,"Sponsorship",homeaud_media$sub_cat)

e17<- twoord.plot(Sponsorship_1_homeaud$week,Sponsorship_1_homeaud$sales/10000000,Sponsorship_1_homeaud$week,Sponsorship_1_homeaud$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Sponsorship Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e17

p33<- twoord.plot(Sponsorship_1_homeaud$week,Sponsorship_1_homeaud$sales/10000000,Sponsorship_1_homeaud$week,Sponsorship_1_homeaud$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and Sponsorship Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p33
p34<- ggplot(Sponsorship_1_homeaud,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Sponsorship AdStock", y = "GMV")
p34


# 2.2.4.4 For Content.Marketing
Content.Marketing_1_homeaud<-optimizeAdStock(homeaud_media$Content_Marketing*10000000,homeaud_media$WK_Sales,homeaud_media$Week,"Content_Marketing",homeaud_media$sub_cat)

e18<- twoord.plot(Content.Marketing_1_homeaud$week,Content.Marketing_1_homeaud$sales/10000000,Content.Marketing_1_homeaud$week,Content.Marketing_1_homeaud$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Content Marketing Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e18

p35<- twoord.plot(Content.Marketing_1_homeaud$week,Content.Marketing_1_homeaud$sales/10000000,Content.Marketing_1_homeaud$week,Content.Marketing_1_homeaud$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and Content.Marketing Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p35
p36<- ggplot(Content.Marketing_1_homeaud,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Content.Marketing AdStock", y = "GMV")
p36


# 2.2.4.5 For Online.marketing
Online.marketing_1_homeaud<-optimizeAdStock(homeaud_media$Online_Marketing*10000000,homeaud_media$WK_Sales,homeaud_media$Week,"Online.marketing",homeaud_media$sub_cat)

e19<- twoord.plot(Online.marketing_1_homeaud$week,Online.marketing_1_homeaud$sales/10000000,Online.marketing_1_homeaud$week,Online.marketing_1_homeaud$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Online Marketing Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e19

p37<- twoord.plot(Online.marketing_1_homeaud$week,Online.marketing_1_homeaud$sales/10000000,Online.marketing_1_homeaud$week,Online.marketing_1_homeaud$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and Online.marketing Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p37
p38<- ggplot(Online.marketing_1_homeaud,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Online.marketing AdStock", y = "GMV")
p38

# 2.2.4.6 For Affiliates
Affiliates_1_homeaud<-optimizeAdStock(homeaud_media$Affiliates*10000000,homeaud_media$WK_Sales,homeaud_media$Week,"Affiliates",homeaud_media$sub_cat)

e20<- twoord.plot(Affiliates_1_homeaud$week,Affiliates_1_homeaud$sales/10000000,Affiliates_1_homeaud$week,Affiliates_1_homeaud$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and Affiliates Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e20

p39<- twoord.plot(Affiliates_1_homeaud$week,Affiliates_1_homeaud$sales/10000000,Affiliates_1_homeaud$week,Affiliates_1_homeaud$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and Affiliates Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p39
p40<- ggplot(Affiliates_1_homeaud,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "Affiliates AdStock", y = "GMV")
p40

# 2.2.4.7 For SEM
SEM_1_homeaud<-optimizeAdStock(homeaud_media$SEM*10000000,homeaud_media$WK_Sales,homeaud_media$Week,"SEM",homeaud_media$sub_cat)

e21<- twoord.plot(SEM_1_homeaud$week,SEM_1_homeaud$sales/10000000,SEM_1_homeaud$week,SEM_1_homeaud$spends/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                 rylab="Adstock (in Cr)",main="Sales and SEM Spends vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
e21

p41<- twoord.plot(SEM_1_homeaud$week,SEM_1_homeaud$sales/10000000,SEM_1_homeaud$week,SEM_1_homeaud$adstock_final/10000000,type=c("l","b"),lcol="black",rcol="blue",ylab="Sales (in Cr)",
                  rylab="Adstock (in Cr)",main="Sales and SEM Adstock vs Time",
                  halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

p41
p42<- ggplot(SEM_1_homeaud,aes(adstock_final,sales))+geom_point()+ geom_smooth(aes(method="lm"))+ labs(x = "SEM AdStock", y = "GMV")
p42

# 2.2.5 Collating all Adstock Data with their Adstock Rates
###########################################################``

TV_Adstock <- rbind(TV_1,TV_1_camacc,TV_1_homeaud)
colnames(TV_Adstock)[2] <- "TV_Adstock"
colnames(TV_Adstock)[5] <- "TV_Spends"
colnames(TV_Adstock)[6] <- "TV_adstockrate"
Digital_Adstock<-rbind(Digital_1,Digital_1_camacc,Digital_1_homeaud)
colnames(Digital_Adstock)[2]<-"Digital_Adstock"
colnames(Digital_Adstock)[5] <- "Dig_Spends"
colnames(Digital_Adstock)[6] <- "Dig_adstockrate"
Sponsorship_Adstock<-rbind(Sponsorship_1,Sponsorship_1_camacc,Sponsorship_1_homeaud)
colnames(Sponsorship_Adstock)[2] <-"Spons_Adstock"
colnames(Sponsorship_Adstock)[5] <- "Spons_Spends"
colnames(Sponsorship_Adstock)[6] <- "Spons_adstockrate"
Content_Adstock <-rbind(Content.Marketing_1,Content.Marketing_1_camacc,Content.Marketing_1_homeaud)
colnames(Content_Adstock)[2] <-"Cont_Adstock"
colnames(Content_Adstock)[5] <- "Cont_Spends"
colnames(Content_Adstock)[6] <- "Cont_adstockrate"
Online_Marketing <- rbind(Online.marketing_1,Online.marketing_1_camacc,Online.marketing_1_homeaud)
colnames(Online_Marketing)[2] <-"Online_Marketing_Adstock"
colnames(Online_Marketing)[5] <- "Online_Marketing_Spends"
colnames(Online_Marketing)[6] <- "Online_Marketing_adstockrate"
Affilates_Marketing <- rbind(Affiliates_1,Affiliates_1_camacc,Affiliates_1_homeaud)
colnames(Affilates_Marketing)[2] <-"Aff_Adstock"
colnames(Affilates_Marketing)[5] <- "Aff_Spends"
colnames(Affilates_Marketing)[6] <- "Aff_adstockrate"
SEM <-rbind(SEM_1,SEM_1_camacc,SEM_1_homeaud)
colnames(SEM)[2] <-"SEM_Adstock"
colnames(SEM)[5] <- "SEM_Spends"
colnames(SEM)[6] <- "SEM_adstockrate"

# 2.2.5.1 Merging all the Adstock features

Marketing_Features <-merge(TV_Adstock,Digital_Adstock)
Marketing_Features<-merge(Marketing_Features,Sponsorship_Adstock)
Marketing_Features<-merge(Marketing_Features,Content_Adstock)
Marketing_Features<-merge(Marketing_Features,Online_Marketing)
Marketing_Features<-merge(Marketing_Features,Affilates_Marketing)
Marketing_Features<-merge(Marketing_Features,SEM)


# 2.3 Calculating Pricing Metrics (Shelf Price Inflation and Discount Percentage)
#################################################################################

# 2.3.1 Calculating Shelf price inflation and Discount Percentage for three categories

# Rolling up the data to date level.

pricing_day_data <- eleck_f2[,c("Month","week","date","Year","gmv","product_analytic_sub_category","discount","units","product_mrp","product_analytic_vertical")]
pricing_day_data = pricing_day_data %>% group_by(product_analytic_sub_category,product_analytic_vertical,Year,Month,week) %>% summarise(gmv=sum(gmv),discount=sum(discount),units=sum(units))

# Calculating the listing price
pricing_day_data$listing_price= pricing_day_data$gmv/pricing_day_data$units

# Breaking down the pricing data into three subcategories

gaming_pricing = pricing_day_data[pricing_day_data$product_analytic_sub_category %in% "GamingAccessory",]
gaming_pricing=gaming_pricing[with(gaming_pricing,order(product_analytic_vertical,week)),]

cam_pricing = pricing_day_data[pricing_day_data$product_analytic_sub_category %in% "CameraAccessory",]
cam_pricing=cam_pricing[with(cam_pricing,order(product_analytic_vertical,week)),]

homeaud_pricing = pricing_day_data[pricing_day_data$product_analytic_sub_category %in% "HomeAudio",]
homeaud_pricing=homeaud_pricing[with(homeaud_pricing,order(product_analytic_vertical,week)),]


# 2.3.2 Calculating Shelf Price Inflation and Discount Percentage for Gaming Category
gaming_pricing_lag<-  gaming_pricing %>%
  group_by(product_analytic_vertical) %>%
  mutate(lag_list_price = dplyr::lag(listing_price, n = 1, default = NA),ma2_lag_list_price = rollapply(listing_price,2,mean,align='right',fill=NA),ma3_lag_list_price=rollapply(listing_price,3,mean,align='right',fill=NA))


gaming_pricing_lag$shelf_price_inflation=gaming_pricing_lag$listing_price/gaming_pricing_lag$lag_list_price
gaming_pricing_lag$ma2_shelf_price_inflation=gaming_pricing_lag$listing_price/gaming_pricing_lag$ma2_lag_list_price
gaming_pricing_lag$ma3_shelf_price_inflation =gaming_pricing_lag$listing_price/gaming_pricing_lag$ma3_lag_list_price

gaming_pricing_lag[is.na(gaming_pricing_lag$shelf_price_inflation),"shelf_price_inflation"]<-1
gaming_pricing_lag[is.na(gaming_pricing_lag$ma2_shelf_price_inflation),"ma2_shelf_price_inflation"]<-1
gaming_pricing_lag[is.na(gaming_pricing_lag$ma3_shelf_price_inflation),"ma3_shelf_price_inflation"]<-1


gaming_pricing_lag$discount_percentage=(gaming_pricing_lag$discount/gaming_pricing_lag$units)/gaming_pricing_lag$listing_price

gaming_pricing_final <- gaming_pricing_lag %>% group_by(week,product_analytic_sub_category) %>% summarise(shelfprice_mean=weighted.mean(shelf_price_inflation,units),discounted_perc=weighted.mean(discount_percentage,units),gmv=sum(gmv),listing_price=sum(gmv)/sum(units),ma2_shelfprice_mean=weighted.mean(ma2_shelf_price_inflation,units),ma3_shelfprice_mean=weighted.mean(ma3_shelf_price_inflation,units))

# Plotting shelf price inflation and discount percentage relation with Sales (GMV)


twoord.plot(gaming_pricing_final$week,gaming_pricing_final$gmv,gaming_pricing_final$week,gaming_pricing_final$listing_price,type=c("l","l"),lcol="black",rcol="blue",ylab="Sales (in Cr)",xlab="Week",
            rylab="Listing Price",main="Sales and List Price vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

twoord.plot(gaming_pricing_final$week,gaming_pricing_final$gmv,gaming_pricing_final$week,gaming_pricing_final$shelfprice_mean,type=c("l","bar"),lcol="black",rcol="maroon",ylab="Sales (in Cr)",xlab="Week",
            rylab="Shelf Price Inflation",main="Sales and Shelf Price Inflation vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")
twoord.plot(gaming_pricing_final$week,gaming_pricing_final$gmv,gaming_pricing_final$week,gaming_pricing_final$ma2_shelfprice_mean,type=c("l","bar"),lcol="black",rcol="maroon",ylab="Sales (in Cr)",xlab="Week",
            rylab="MA2 Shelf Price Inflation",main="Sales and Shelf Price Inflation vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

twoord.plot(gaming_pricing_final$week,gaming_pricing_final$gmv,gaming_pricing_final$week,gaming_pricing_final$ma3_shelfprice_mean,type=c("l","bar"),lcol="black",rcol="maroon",ylab="Sales (in Cr)",xlab="Week",
            rylab="MA3 Shelf Price Inflation",main="Sales and Shelf Price Inflation vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

twoord.plot(gaming_pricing_final$week,gaming_pricing_final$gmv,gaming_pricing_final$week,gaming_pricing_final$discounted_perc,type=c("l","b"),lcol="black",rcol="dark red",ylab="Sales (in Cr)",xlab="Week",
            rylab="Discount Percentage (Discount Offered /List Price)",main="Sales and Discount %age vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

# 2.3.3 Calculating Shelf Price Inflation and Discount Percentage  For Camera Accessory

cam_pricing_lag<-  cam_pricing %>%
  group_by(product_analytic_vertical) %>%
  mutate(lag_list_price = dplyr::lag(listing_price, n = 1, default = NA),ma2_lag_list_price = rollapply(listing_price,2,mean,align='right',fill=NA),ma3_lag_list_price=rollapply(listing_price,3,mean,align='right',fill=NA))


cam_pricing_lag$shelf_price_inflation=cam_pricing_lag$listing_price/cam_pricing_lag$lag_list_price
cam_pricing_lag$ma2_shelf_price_inflation=cam_pricing_lag$listing_price/cam_pricing_lag$ma2_lag_list_price
cam_pricing_lag$ma3_shelf_price_inflation=cam_pricing_lag$listing_price/cam_pricing_lag$ma3_lag_list_price


cam_pricing_lag[is.na(cam_pricing_lag$shelf_price_inflation),"shelf_price_inflation"]<-1
cam_pricing_lag[is.na(cam_pricing_lag$ma2_shelf_price_inflation),"ma2_shelf_price_inflation"]<-1
cam_pricing_lag[is.na(cam_pricing_lag$ma3_shelf_price_inflation),"ma3_shelf_price_inflation"]<-1


cam_pricing_lag$discount_percentage=(cam_pricing_lag$discount/cam_pricing_lag$units)/cam_pricing_lag$listing_price

cam_pricing_final <- cam_pricing_lag %>% group_by(week,product_analytic_sub_category) %>% summarise(shelfprice_mean=weighted.mean(shelf_price_inflation,units),discounted_perc=weighted.mean(discount_percentage,units),gmv=sum(gmv),listing_price=sum(gmv)/sum(units),ma2_shelfprice_mean=weighted.mean(ma2_shelf_price_inflation,units),ma3_shelfprice_mean=weighted.mean(ma3_shelf_price_inflation,units))

# Plotting shelf price inflation and discount percentage relation with Sales (GMV)
twoord.plot(cam_pricing_final$week,cam_pricing_final$gmv,cam_pricing_final$week,cam_pricing_final$listing_price,type=c("l","l"),lcol="black",rcol="blue",ylab="Sales (in Cr)",xlab="Week",
            rylab="Listing Price",main="Sales and List Price vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

twoord.plot(cam_pricing_final$week,cam_pricing_final$gmv,cam_pricing_final$week,cam_pricing_final$shelfprice_mean,type=c("l","bar"),lcol="black",rcol="maroon",ylab="Sales (in Cr)",xlab="Week",
            rylab="Shelf Price Inflation",main="Sales and Shelf Price Inflation vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

twoord.plot(cam_pricing_final$week,cam_pricing_final$gmv,cam_pricing_final$week,cam_pricing_final$ma2_shelfprice_mean,type=c("l","bar"),lcol="black",rcol="maroon",ylab="Sales (in Cr)",xlab="Week",
            rylab="MA2 Shelf Price Inflation",main="Sales and Shelf Price Inflation vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

twoord.plot(cam_pricing_final$week,cam_pricing_final$gmv,cam_pricing_final$week,cam_pricing_final$ma3_shelfprice_mean,type=c("l","bar"),lcol="black",rcol="maroon",ylab="Sales (in Cr)",xlab="Week",
            rylab="MA3 Shelf Price Inflation",main="Sales and Shelf Price Inflation vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

twoord.plot(cam_pricing_final$week,cam_pricing_final$gmv,cam_pricing_final$week,cam_pricing_final$discounted_perc,type=c("l","b"),lcol="black",rcol="dark red",ylab="Sales (in Cr)",xlab="Week",
            rylab="Discount Percentage (Discount Offered /List Price)",main="Sales and Discount %age vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

# 2.3.4 Calculating Shelf Price Inflation and Discount Percentage For Home Audio


homeaud_pricing_lag<-  homeaud_pricing %>%
  group_by(product_analytic_vertical)  %>%
  mutate(lag_list_price = dplyr::lag(listing_price, n = 1, default = NA),ma2_lag_list_price = rollapply(listing_price,2,mean,align='right',fill=NA),ma3_lag_list_price=rollapply(listing_price,3,mean,align='right',fill=NA))

homeaud_pricing_lag$shelf_price_inflation=homeaud_pricing_lag$listing_price/homeaud_pricing_lag$lag_list_price
homeaud_pricing_lag$ma2_shelf_price_inflation=homeaud_pricing_lag$listing_price/homeaud_pricing_lag$ma2_lag_list_price
homeaud_pricing_lag$ma3_shelf_price_inflation=homeaud_pricing_lag$listing_price/homeaud_pricing_lag$ma3_lag_list_price


homeaud_pricing_lag[is.na(homeaud_pricing_lag$shelf_price_inflation),"shelf_price_inflation"]<-1
homeaud_pricing_lag[is.na(homeaud_pricing_lag$ma2_lag_list_price),"ma2_lag_list_price"]<-1
homeaud_pricing_lag[is.na(homeaud_pricing_lag$ma3_lag_list_price),"ma3_lag_list_price"]<-1


homeaud_pricing_lag$discount_percentage=(homeaud_pricing_lag$discount/homeaud_pricing_lag$units)/homeaud_pricing_lag$listing_price

homeaud_pricing_final <- homeaud_pricing_lag %>% group_by(week,product_analytic_sub_category) %>% summarise(shelfprice_mean=weighted.mean(shelf_price_inflation,units),discounted_perc=weighted.mean(discount_percentage,units),gmv=sum(gmv),listing_price=sum(gmv)/sum(units),ma2_shelfprice_mean=weighted.mean(ma2_shelf_price_inflation,units),ma3_shelfprice_mean=weighted.mean(ma3_shelf_price_inflation,units))

# Plotting shelf price inflation and discount percentage relation with Sales (GMV)



twoord.plot(homeaud_pricing_final$week,homeaud_pricing_final$gmv,homeaud_pricing_final$week,homeaud_pricing_final$listing_price,type=c("l","l"),lcol="black",rcol="blue",ylab="Sales (in Cr)",xlab="Week",
            rylab="Listing Price",main="Sales and Listing Price vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")


twoord.plot(homeaud_pricing_final$week,homeaud_pricing_final$gmv,homeaud_pricing_final$week,homeaud_pricing_final$shelfprice_mean,type=c("l","bar"),lcol="black",rcol="maroon",ylab="Sales (in Cr)",xlab="Week",
            rylab="Shelf Price Inflation",main="Sales and Shelf Price Inflation vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

twoord.plot(homeaud_pricing_final$week,homeaud_pricing_final$gmv,homeaud_pricing_final$week,homeaud_pricing_final$ma2_shelfprice_mean,type=c("l","bar"),lcol="black",rcol="maroon",ylab="Sales (in Cr)",xlab="Week",
            rylab="Shelf Price Inflation",main="Sales and Shelf Price Inflation vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

twoord.plot(homeaud_pricing_final$week,homeaud_pricing_final$gmv,homeaud_pricing_final$week,homeaud_pricing_final$ma3_shelfprice_mean,type=c("l","bar"),lcol="black",rcol="maroon",ylab="Sales (in Cr)",xlab="Week",
            rylab="Shelf Price Inflation",main="Sales and Shelf Price Inflation vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")


twoord.plot(homeaud_pricing_final$week,homeaud_pricing_final$gmv,homeaud_pricing_final$week,homeaud_pricing_final$discounted_perc,type=c("l","b"),lcol="black",rcol="dark red",ylab="Sales (in Cr)",xlab="Week",
            rylab="Discount Percentage (Discount Offered /List Price)",main="Sales and Discount %age vs Time",
            halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

# 2.3.5 Collating all the pricing features

Pricing_Features<- rbind(gaming_pricing_final,homeaud_pricing_final,cam_pricing_final)


# 2.4 Adding Sale KPIs 
#########################################################

list_sale<-c("2015-07-18","2015-07-19","2015-08-15",
             "2015-08-16","2015-08-17","2015-08-28",
             "2015-08-29","2015-08-30","2015-10-15",
             "2015-10-16","2015-10-17","2015-11-07","2015-11-08","2015-11-09","2015-11-10",
             "2015-10-11","2015-10-12","2015-11-13","2015-11-14","2015-12-25","2015-12-26",
             "2015-12-27","2015-12-28","2015-12-29","2015-12-30","2016-01-01","2016-01-02",
             "2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01","2016-02-02",
             "2016-02-20","2016-02-21","2016-02-14","2016-02-15","2016-03-07","2016-03-08",
             "2016-03-09","2016-05-25","2016-05-26","2016-05-27")

sale_list <-as.Date(list_sale)
week_sale<- data.frame(sale_list,isoweek(sale_list))
colnames(week_sale)[2] <- "week"


# Creating  Weekly Sale Flag and %of days in  a week with sale

week_sale<- week_sale %>% group_by(week) %>% summarise(n_days_sale=n())
week_sale$flag=1
week_sale$perc_sale= week_sale$n_days_sale/7

#2.5 Creating Payment Type KPIs
#########################################################

prepaid_orders=eleck_f2[eleck_f2$s1_fact.order_payment_type %in% "Prepaid",]
total_orders<- eleck_f2 %>% group_by(product_analytic_sub_category,week) %>% summarise(total_orders=n())
prepaid_orders<-prepaid_orders%>% group_by(product_analytic_sub_category,week) %>% summarise(prepaid_orders=n())
prepaid_orders_features<-merge(total_orders,prepaid_orders,all.x = T)
prepaid_orders_features[is.na(prepaid_orders_features)]<-0
prepaid_orders_features$perc_prepaid= prepaid_orders_features$prepaid_orders/prepaid_orders_features$total_orders


#2.6 Creating SLA KPIs
#########################################################
sla_features =eleck_f2[,c("week","product_analytic_sub_category","sla","product_procurement_sla")]
sla_features$less_than_3_days=ifelse(sla_features$sla<=3,1,0)
sla_features$order_flag=1
sla_features<-sla_features %>% group_by(week,product_analytic_sub_category) %>% summarise(sum_sla=sum(less_than_3_days),total_orders=sum(order_flag),avg_sla=mean(sla),avg_procurement_sla=mean(product_procurement_sla))
sla_features=data.frame(sla_features)
sla_features$perc_less_than_3dayssla=sla_features$sum_sla/sla_features$total_orders
sla_features<-sla_features[,c(1,2,5,6,7)]

#2.7 Creating GRP KPI
#########################################################

ProductList = read.xlsx("Media data and other information.xlsx",sheet = 1)
colnames(ProductList)[1]<-"product_analytic_vertical"
GRP = merge(eleck_f2[,c("week","product_analytic_vertical","product_analytic_sub_category")],ProductList,all.x = T)
GRP$grp= as.numeric(GRP$Frequency) * GRP$Percent
GRP <- GRP %>% group_by(week,product_analytic_sub_category) %>% summarise(avg_grp=mean(grp))
colnames(GRP)[2] <- "subcat"


#2.8 Creating NPS KPI by reading NPS CSV
#########################################################

nps<-read.xlsx("Media data and other information.xlsx", sheet = 4,colNames = F,rowNames = F)
nps<-as.data.frame(t(nps))
nps<-nps[-1,]
nps<- data.frame(Month=c(7,8,9,10,11,12,1,2,3,4,5,6),Year=c(2015,2015,2015,2015,2015,2015,2016,2016,2016,2016,2016,2016),NPS=c(54.6,60,46.9,44.4,47,45.8,47.1,50.3,49,51.8,47.3,50.5))

nps_feature <- merge(unique(week_day_data[,c("Week","Month","Year","sub_cat")]),nps,all.x = T)
nps_feature <- nps_feature %>% group_by (Week,sub_cat) %>% summarise(mean_nps=mean(NPS))
colnames(nps_feature)[2] <-"subcat"
colnames(nps_feature)[1] <-"week"


#2.9 Getting all KPIs Together and making the columns uniform for merging
#########################################################################

colnames(Marketing_Features)
colnames(Pricing_Features)
colnames(Pricing_Features)[2]<-"subcat"
colnames(Pricing_Features)[5]<-"sales"
colnames(prepaid_orders_features)
colnames(prepaid_orders_features)[1]<-"subcat"
colnames(week_sale)
colnames(sla_features)
colnames(sla_features)[2] <-"subcat"

Final_table=merge(Marketing_Features,Pricing_Features,all.x=T)
Final_table=merge(Final_table,prepaid_orders_features,all.x=T)
Final_table=merge(Final_table,week_sale,all.x=T)
Final_table=merge(Final_table,sla_features,all.x=T)
Final_table=merge(Final_table,GRP,all.x = T)
Final_table=merge(Final_table,nps_feature,all.x = T)

# Replacing all NA values by 0 
Final_table[is.na(Final_table)]<-0
Final_table<-Final_table[with(Final_table, order(subcat, week)), ]

#2.10 Plotting graph using final dataframe (Key Input Variables )
##############################################################################
ga_plot<- Final_table[Final_table$subcat=="GamingAccessory",]
home_audio_plot<-Final_table[Final_table$subcat=="HomeAudio",]
model_camera_plot<-Final_table[Final_table$subcat=="CameraAccessory",]

# Plotting GA  Holiday vs Sales 
 ggplot(ga_plot,aes(ga_plot$week,ga_plot$sales, fill = as.factor(ga_plot$flag)))+geom_bar(stat="identity") + labs(fill = "Holiday_flag", x = "Week", y = "Sales") + ggtitle("Gaming Accessories Holiday vs Sales Plot ")

# Plotting HA  Holiday vs Sales 
 ggplot(home_audio_plot,aes(home_audio_plot$week,home_audio_plot$sales, fill = as.factor(home_audio_plot$flag)))+geom_bar(stat="identity") + labs(fill = "Holiday_flag", x = "Week", y = "Sales") + ggtitle("Gaming Accessories Holiday vs Sales Plot ")


# Plotting MC  Holiday vs Sales 
 ggplot(model_camera_plot,aes(model_camera_plot$week,model_camera_plot$sales, fill = as.factor(model_camera_plot$flag)))+geom_bar(stat="identity") + labs(fill = "Holiday_flag", x = "Week", y = "Sales") + ggtitle("Gaming Accessories Holiday vs Sales Plot ")


# Plotting GA number of Prepaid orders vs Sales and Time 
twoord.plot(ga_plot$week,ga_plot$sales/10000000,ga_plot$week,ga_plot$prepaid_orders,type=c("l","l"),lcol="black",rcol="blue",ylab="Sales (in Cr.)", rylab="Number of Prepaid Orders ",main="Sales and Number of Prepaid Orders vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

# Plotting HA number of Prepaid orders vs Sales and Time 
twoord.plot(home_audio_plot$week,home_audio_plot$sales/10000000,home_audio_plot$week,home_audio_plot$prepaid_orders,type=c("l","l"),lcol="black",rcol="blue",ylab="Sales (in Cr.)", rylab="Number of Prepaid Orders ",main="Sales and Number of Prepaid Orders vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

# Plotting Camera Accessories number of Prepaid orders vs Sales and Time 
twoord.plot(model_camera_plot$week,model_camera_plot$sales/10000000,model_camera_plot$week,model_camera_plot$prepaid_orders,type=c("l","l"),lcol="black",rcol="blue",ylab="Sales (in Cr.)", rylab="Number of Prepaid Orders ",main="Sales and Number of Prepaid Orders vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")


# Plotting GA Avg SLA vs Sales and Time 
twoord.plot(ga_plot$week,ga_plot$sales/10000000,ga_plot$week,ga_plot$avg_sla,type=c("l","l"),lcol="black",rcol="blue",ylab="Sales (in Cr.)", rylab="SLA (in days)",main="Sales and Average SLA vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")


# Plotting HA Avg SLA vs Sales and Time

twoord.plot(home_audio_plot$week,home_audio_plot$sales/10000000,home_audio_plot$week,home_audio_plot$avg_sla,type=c("l","l"),lcol="black",rcol="blue",ylab="Sales (in Cr.)", rylab="SLA (in days) ",main="Sales and Average SLA vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")


# Plotting Camera Accessories Avg SLA vs Sales and Time
twoord.plot(model_camera_plot$week,model_camera_plot$sales/10000000,model_camera_plot$week,model_camera_plot$avg_sla,type=c("l","l"),lcol="black",rcol="blue",ylab="Sales (in Cr.)", rylab="SLA (in days) ",main="Sales and Average SLA vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

# Plotting GA Procuremnt SLA vs Sales and Time 
twoord.plot(ga_plot$week,ga_plot$sales/10000000,ga_plot$week,ga_plot$avg_procurement_sla,type=c("l","l"),lcol="black",rcol="blue",ylab="Sales (in Cr.)", rylab=" Proc SLA (in days)",main="Sales and Average Procurement SLA vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")

# Plotting HA Procurement SLA vs Sales and Time

twoord.plot(home_audio_plot$week,home_audio_plot$sales/10000000,home_audio_plot$week,home_audio_plot$avg_procurement_sla,type=c("l","l"),lcol="black",rcol="blue",ylab="Sales (in Cr.)", rylab="Proc SLA (in days) ",main="Sales and Average Proc SLA vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")


# Plotting Camera Accessories Avg Proc SLA vs Sales and Time
twoord.plot(model_camera_plot$week,model_camera_plot$sales/10000000,model_camera_plot$week,model_camera_plot$avg_procurement_sla,type=c("l","l"),lcol="black",rcol="blue",ylab="Sales (in Cr.)", rylab="Proc SLA (in days) ",main="Sales and Average Procurement SLA vs Time",
                 halfwidth=0.2,lwd=2,do.first="plot_bg(\"beige\")")


#2.11 Writing in final data frame to use in next code
###################################################################################
write.csv(Final_table,"Final_table.csv",row.names=F)


