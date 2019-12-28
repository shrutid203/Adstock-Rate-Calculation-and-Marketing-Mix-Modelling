
#########################################################################################
######################## 3. Modeling for Gaming Accessories #############################
#########################################################################################
# Reading the output dataframe 
Final_table<-read.csv("Final_table.csv")

# We have run the following 5 types of models below - 
# 3.1 Linear Regression
# 3.2 Multiplicative Model
# 3.3 Distributed Lag Model
# 3.4 Koyck Model
# 3.5 Multiplicative and Distributed Lag Combined Model

############################################################
# 3.1 Running multiple iterations of Linear Regression Model
############################################################
# Also, performing Step AIC and VIF for feature selection
# Finally, performed a Cross-Validation to ensure stability of selected model
# Running the first round of model for gaming accessory

model_gaming<-Final_table[Final_table$subcat=="GamingAccessory",]
model_gaming<- subset(model_gaming,select = -c(week,subcat,TV_Spends,TV_adstockrate,Dig_Spends,Dig_adstockrate,Spons_Spends,Spons_adstockrate,Cont_Spends,Cont_adstockrate,Online_Marketing_Spends,Online_Marketing_adstockrate, Aff_Spends,Aff_adstockrate,SEM_Spends,SEM_adstockrate,total_orders,prepaid_orders,flag,n_days_sale))

ga_model_data<-as.data.frame(scale(model_gaming))

ga_model_1=lm(sales~.,data=ga_model_data)

#Multiple R-squared:  0.7911, Adjusted R-squared:  0.6708 
summary(ga_model_1)

vif(ga_model_1)

# Running step AIC for selection of features
step <- stepAIC(ga_model_1,direction = "both")


# Running second round of regression
ga_model_2=lm(sales ~ TV_Adstock + Spons_Adstock + Online_Marketing_Adstock + 
                Aff_Adstock + SEM_Adstock + shelfprice_mean + listing_price + 
                ma2_shelfprice_mean + ma3_shelfprice_mean + perc_prepaid + 
                perc_sale + avg_grp + avg_sla + perc_less_than_3dayssla + 
                mean_nps,data=ga_model_data)
#Multiple R-squared:  0.7838, Adjusted R-squared:  0.6961 
summary(ga_model_2)
vif(ga_model_2)

# Running third round of regression; Removing Aff_Adstock due to high VIF 
ga_model_3=lm(sales ~ TV_Adstock + Spons_Adstock + Online_Marketing_Adstock  
              + SEM_Adstock + shelfprice_mean + listing_price + 
                ma2_shelfprice_mean + ma3_shelfprice_mean + perc_prepaid + 
                perc_sale + avg_grp + avg_sla + perc_less_than_3dayssla + 
                mean_nps ,data=ga_model_data)
#Multiple R-squared:  0.6844, Adjusted R-squared:  0.5681 
summary(ga_model_3)
vif(ga_model_3)

# Running 4th  round of regression; Removing ma2_shelfprice_mean due to high VIF and p value

ga_model_4=lm(sales ~ TV_Adstock + Spons_Adstock + Online_Marketing_Adstock  
              + SEM_Adstock + shelfprice_mean + listing_price 
              + ma3_shelfprice_mean + perc_prepaid + 
                perc_sale + avg_grp + avg_sla + perc_less_than_3dayssla + 
                mean_nps ,data=ga_model_data)
#Multiple R-squared:  0.6594, Adjusted R-squared:  0.5458 
summary(ga_model_4)
vif(ga_model_4)

# Running 5th  round of regression; Removing mean_nps due to high VIF and p value

ga_model_5=lm(sales ~   TV_Adstock + Spons_Adstock + Online_Marketing_Adstock  
              + SEM_Adstock + shelfprice_mean + listing_price 
              + ma3_shelfprice_mean + perc_prepaid + 
                perc_sale + avg_grp + avg_sla + perc_less_than_3dayssla  
              ,data=ga_model_data)
summary(ga_model_5)
vif(ga_model_5)

# Running 6th round of regression; Removing ma3_shelfprice_mean  due to high VIF and p value


ga_model_6=lm(sales ~   TV_Adstock + Spons_Adstock + Online_Marketing_Adstock  
              + SEM_Adstock + shelfprice_mean + listing_price 
              + perc_prepaid + 
                perc_sale + avg_grp + avg_sla + perc_less_than_3dayssla  ,data=ga_model_data)
#Multiple R-squared:   0.64,  Adjusted R-squared:  0.5434 
summary(ga_model_6)
vif(ga_model_6)

# Running 7th round of regression; Removing avg_sla due to high VIF and p value


ga_model_7=lm(sales ~ TV_Adstock + Spons_Adstock + Online_Marketing_Adstock  
              + SEM_Adstock + shelfprice_mean + listing_price 
              + perc_prepaid + 
                perc_sale + avg_grp  + perc_less_than_3dayssla  ,data=ga_model_data)
#Multiple R-squared:  0.638,  Adjusted R-squared:  0.5518 
summary(ga_model_7)
vif(ga_model_7)

# Running 8th round of regression; Removing shelfprice_mean due to high VIF and p value. 

ga_model_8=lm(sales ~   TV_Adstock + Spons_Adstock + Online_Marketing_Adstock  
              + SEM_Adstock  + listing_price 
              + perc_prepaid + 
                perc_sale + avg_grp  + perc_less_than_3dayssla  ,data=ga_model_data)

# Multiple R-squared:  0.6278,  Adjusted R-squared:  0.5499 
summary(ga_model_8)
vif(ga_model_8)


# Running 9th round of regression; Removing perc_less_than_3dayssla due to high VIF and p value. 

ga_model_9=lm(sales ~   TV_Adstock + Spons_Adstock + Online_Marketing_Adstock  
              + SEM_Adstock  + listing_price 
              + perc_prepaid + 
                perc_sale + avg_grp    ,data=ga_model_data)

# Multiple R-squared:  0.6267,  Adjusted R-squared:  0.5589 
summary(ga_model_9)
vif(ga_model_9)

# Running 10th round of regression; Removing TV_Adstock due to high VIF and p value. 
ga_model_10=lm(sales ~   Spons_Adstock + Online_Marketing_Adstock  
               + SEM_Adstock  + listing_price 
               + perc_prepaid + 
                 perc_sale + avg_grp    ,data=ga_model_data)

# Multiple R-squared:  0.6203,  Adjusted R-squared:  0.5612 
summary(ga_model_10)
vif(ga_model_10)

# Running 11th round of regression; Removing perc_sale due to high VIF and p value. 
ga_model_11=lm(sales ~   Spons_Adstock + Online_Marketing_Adstock  
               + SEM_Adstock  + listing_price 
               + perc_prepaid + 
                 avg_grp    ,data=ga_model_data)

# Multiple R-squared:  0.6203,  Adjusted R-squared:  0.5612 
summary(ga_model_11)
vif(ga_model_11)

# Running 12th round of regression; Removing Online_Marketing_Adstock due to high VIF and p value. 
ga_model_12=lm(sales ~   Spons_Adstock  
               + SEM_Adstock  + listing_price 
               + perc_prepaid + 
                 avg_grp    ,data=ga_model_data)

# Multiple R-squared:  0.6203,  Adjusted R-squared:  0.5612 
summary(ga_model_12)
vif(ga_model_12)


# Running 13th round of regression; Removing SEM_Adstock due to high p value. 
ga_model_13=lm(sales ~   Spons_Adstock  
               + listing_price 
               + perc_prepaid + 
                 avg_grp    ,data=ga_model_data)

# Multiple R-squared:  0.6203,  Adjusted R-squared:  0.5612 
summary(ga_model_13)
vif(ga_model_13)


# Running 14th round of regression; Removing SEM_Adstock due to high p value. 
ga_model_14=lm(sales ~   Spons_Adstock  
               + listing_price 
               + perc_prepaid  
               ,data=ga_model_data)

#Multiple R-squared:  0.4989, Adjusted R-squared:  0.4682
summary(ga_model_14)
vif(ga_model_14)


#Spons_Adstock listing_price  perc_prepaid 
#1.105954      1.319439      1.247959 

#3.1.2 Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = ga_model_data, form.lm = formula(sales ~   Spons_Adstock  + listing_price + perc_prepaid), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.

#3.1.3 From linear model, we see that sponsorship adstock,listing price and percent of orders with prepaid option. Plotting the effect of these three variables 


# 3.2 Gaming Accessories: Multiplicative Model 
###################################################################

# 3.2.1 Creating a logarithmic training dataset
model_gaming_imputed <- model_gaming[model_gaming$sales>0,]
model_gaming_imputed[model_gaming_imputed==0]<-1
log_model_gaming <- log(model_gaming_imputed)

# 3.2.2 Standardizing data
log_model_gaming<- as.data.frame(scale(log_model_gaming))

# 3.2.3 Performing multiple iterations of the multiplicative model, Step AIC and VIF Treatment. Finally, we have also performed K-Fold cross validation to check if the model is robust

# Running first round of linear regression 
log_ga_model_1=lm(sales~.,data=log_model_gaming)
#Multiple R-squared:  0.8391, Adjusted R-squared:  0.7337 
summary(log_ga_model_1)

# Running stepAIC on the first model
step_log <- stepAIC(log_ga_model_1)

# Running second round using stepAIC results

log_ga_model_2=lm(sales ~ Cont_Adstock + SEM_Adstock + discounted_perc + listing_price + 
                    avg_grp + avg_sla + avg_procurement_sla,data=log_model_gaming)

#Multiple R-squared:  0.8202, Adjusted R-squared:  0.7895 
summary(log_ga_model_2)
vif(log_ga_model_2)

# Removing SEM_Adstock since it has high VIF and high p-value

log_ga_model_3=lm(sales ~ Cont_Adstock  + discounted_perc + listing_price + 
                    avg_grp + avg_sla + avg_procurement_sla,data=log_model_gaming)
#Multiple R-squared:  0.7958, Adjusted R-squared:  0.7666 
summary(log_ga_model_3)
vif(log_ga_model_3)

# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = log_model_gaming, form.lm = formula(sales ~ Cont_Adstock  + discounted_perc + listing_price + avg_grp + avg_sla + avg_procurement_sla), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.





# 3.3  Gaming Accessories: Distributed Lag Model
###############################################################

# Here, we prepare data for the distrbuted lag model and execute it. We also perform StepAIC and VIF techniques to select variables and finally perform K-Fold cross validation to ensure robustness of the model

# 3.3.1 Creating the lagged dataset
dist_model_gaming<-model_gaming
dist_model_gaming <- slide(dist_model_gaming, Var = "listing_price",slideBy = -1)
dist_model_gaming <- slide(dist_model_gaming, Var = "sales",slideBy = -1)

# Removing NAs and Scaling the variables
dist_model_gaming <- na.omit(dist_model_gaming)
dist_model_gaming<-as.data.frame(scale(dist_model_gaming))

# Running first iteration of the model
ga_dist_model_1 <- lm(sales~.,data=dist_model_gaming)
#Multiple R-squared:  0.7166, Adjusted R-squared:  0.4876 
summary(ga_dist_model_1)
vif(ga_dist_model_1)


# Running stepAIC on the model
step_dist <- stepAIC(ga_dist_model_1)

# Running second itiration 
ga_dist_model2<- lm(sales ~TV_Adstock + Spons_Adstock + Online_Marketing_Adstock + 
                      Aff_Adstock + SEM_Adstock + shelfprice_mean + listing_price + 
                      ma2_shelfprice_mean + ma3_shelfprice_mean + perc_prepaid + 
                      perc_sale + avg_sla + avg_grp + `sales-1`,data=dist_model_gaming)
#Multiple R-squared:  0.7072, Adjusted R-squared:  0.583 
summary(ga_dist_model2)
vif(ga_dist_model2)

# Removing Aff_Adstock
ga_dist_model3<- lm(sales ~TV_Adstock + Spons_Adstock + Online_Marketing_Adstock + 
                      SEM_Adstock + shelfprice_mean + listing_price + 
                      ma2_shelfprice_mean + ma3_shelfprice_mean + perc_prepaid + 
                      perc_sale + avg_sla + avg_grp + `sales-1`,data=dist_model_gaming)
#Multiple R-squared:  0.6445, Adjusted R-squared:  0.5085 
summary(ga_dist_model3)
vif(ga_dist_model3)

# Removing ma2_shelfprice_mean
ga_dist_model4<- lm(sales ~TV_Adstock + Spons_Adstock + Online_Marketing_Adstock + 
                      SEM_Adstock + shelfprice_mean + listing_price + 
                      ma3_shelfprice_mean + perc_prepaid + 
                      perc_sale + avg_sla + avg_grp + `sales-1`,data=dist_model_gaming)
#Multiple R-squared:  0.6303, Adjusted R-squared:  0.5035 
summary(ga_dist_model4)
vif(ga_dist_model4)

# Removing shelfprice_mean
ga_dist_model5<- lm(sales ~TV_Adstock + Spons_Adstock + Online_Marketing_Adstock + 
                      SEM_Adstock  + listing_price + 
                      ma3_shelfprice_mean + perc_prepaid + 
                      perc_sale + avg_sla + avg_grp + `sales-1`,data=dist_model_gaming)
#Multiple R-squared:  0.6303, Adjusted R-squared:  0.5035 
summary(ga_dist_model5)
vif(ga_dist_model5)

# Removing Spons_Adstock
ga_dist_model6<- lm(sales ~TV_Adstock  + Online_Marketing_Adstock + 
                      SEM_Adstock  + listing_price + 
                      ma3_shelfprice_mean + perc_prepaid + 
                      perc_sale + avg_sla + avg_grp + `sales-1`,data=dist_model_gaming)
#Multiple R-squared:  0.6141, Adjusted R-squared:  0.5098 
summary(ga_dist_model6)
vif(ga_dist_model6)

# Removing Online_Marketing_Adstock
ga_dist_model7<- lm(sales ~TV_Adstock   + 
                      SEM_Adstock  + listing_price + 
                      ma3_shelfprice_mean + perc_prepaid + 
                      perc_sale + avg_sla + avg_grp + `sales-1`,data=dist_model_gaming)
#Multiple R-squared:  0.6141, Adjusted R-squared:  0.5098 
summary(ga_dist_model7)
vif(ga_dist_model7)


# Removing avg_grp
ga_dist_model8<- lm(sales ~TV_Adstock   + 
                      SEM_Adstock  + listing_price + 
                      ma3_shelfprice_mean + perc_prepaid + 
                      perc_sale + avg_sla  + `sales-1`,data=dist_model_gaming)
#Multiple R-squared:  0.5689, Adjusted R-squared:  0.4805 
summary(ga_dist_model8)
vif(ga_dist_model8)

# Removing ma3_shelfprice_mean
ga_dist_model9<- lm(sales ~TV_Adstock   + 
                      SEM_Adstock  + listing_price + 
                      perc_prepaid + 
                      perc_sale + avg_sla  + `sales-1`,data=dist_model_gaming)
#Multiple R-squared:  0.5469, Adjusted R-squared:  0.4676 
summary(ga_dist_model9)
vif(ga_dist_model9)


# Removing avg_sla
ga_dist_model10<- lm(sales ~TV_Adstock   + 
                       SEM_Adstock  + listing_price + 
                       perc_prepaid + 
                       perc_sale   + `sales-1`,data=dist_model_gaming)
#Multiple R-squared:  0.524,  Adjusted R-squared:  0.454
summary(ga_dist_model10)
vif(ga_dist_model10)

# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = dist_model_gaming, form.lm = formula(sales ~TV_Adstock + SEM_Adstock  + listing_price + perc_prepaid + perc_sale + `sales-1`,data=dist_model_gaming), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.


# 3.4 Koyck Model for Gaming Accessories
########################################

koyck_model_gaming<-model_gaming
koyck_model_gaming <- slide(koyck_model_gaming, Var = "sales",slideBy = -1)
koyck_model_gaming <- slide(koyck_model_gaming, Var = "sales",slideBy = -2)
koyck_model_gaming <- slide(koyck_model_gaming, Var = "sales",slideBy = -3)

koyck_model_gaming<-na.omit(koyck_model_gaming)
koyck_model_gaming<-as.data.frame(scale(koyck_model_gaming))

# Running first iteration of the model

ga_koyck_model1<- lm(sales~., data=koyck_model_gaming)
#Multiple R-squared:  0.8132, Adjusted R-squared:  0.661
summary(ga_koyck_model1)
vif(ga_koyck_model1)

# Running stepAIC 
step_koyck <- stepAIC(ga_koyck_model1)

# Running second itiration of the model

ga_koyck_model2<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Online_Marketing_Adstock +                        Aff_Adstock + listing_price + ma3_shelfprice_mean + perc_prepaid + 
                       perc_sale + avg_sla + avg_grp + `sales-1` + `sales-3`, data=koyck_model_gaming)
# Multiple R-squared:  0.7939,  Adjusted R-squared:  0.7194 
summary(ga_koyck_model2)
vif(ga_koyck_model2)


# Removing Aff_Adstock

ga_koyck_model3<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Online_Marketing_Adstock +                          listing_price + ma3_shelfprice_mean + perc_prepaid + 
                       perc_sale + avg_sla + avg_grp + `sales-1` + `sales-3`, data=koyck_model_gaming)
#Multiple R-squared:  0.7493, Adjusted R-squared:  0.6679 
summary(ga_koyck_model3)
vif(ga_koyck_model3)


# Removing ma3_shelfprice_mean


ga_koyck_model4<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Online_Marketing_Adstock +                          listing_price  + perc_prepaid + 
                       perc_sale + avg_sla + avg_grp + `sales-1` + `sales-3`, data=koyck_model_gaming)
#Multiple R-squared:  0.701,  Adjusted R-squared:  0.6144 
summary(ga_koyck_model4)
vif(ga_koyck_model4)


# Removing avg_sla


ga_koyck_model5<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Online_Marketing_Adstock +                          listing_price  + perc_prepaid + 
                       perc_sale  + avg_grp + `sales-1` + `sales-3`, data=koyck_model_gaming)
#Multiple R-squared:  0.701,  Adjusted R-squared:  0.6144 
summary(ga_koyck_model5)
vif(ga_koyck_model5)


# Removing Online_Marketing_Adstock
ga_koyck_model6<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock  +                          listing_price  + perc_prepaid + 
                       perc_sale  + avg_grp + `sales-1` + `sales-3`, data=koyck_model_gaming)
#Multiple R-squared:  0.701,  Adjusted R-squared:  0.6144 
summary(ga_koyck_model6)
vif(ga_koyck_model6)


# Removing Spons_Adstock
ga_koyck_model7<- lm(sales ~ TV_Adstock + Digital_Adstock   + listing_price  + perc_prepaid + 
                       perc_sale  + avg_grp + `sales-1` + `sales-3`, data=koyck_model_gaming)
#Multiple R-squared:  0.6666, Adjusted R-squared:  0.6016 
summary(ga_koyck_model7)
vif(ga_koyck_model7)

# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = koyck_model_gaming, form.lm = formula(sales ~ TV_Adstock + Digital_Adstock   + listing_price  + perc_prepaid + perc_sale  + avg_grp + `sales-1` + `sales-3`), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.

# 3.5 Combination of Distributed Lag and Multiplicative Models
##############################################################

mult_koyck_model_gaming<-model_gaming
mult_koyck_model_gaming <- slide(mult_koyck_model_gaming, Var = "sales",slideBy = -1)
mult_koyck_model_gaming <- slide(mult_koyck_model_gaming, Var = "sales",slideBy = -2)
mult_koyck_model_gaming <- slide(mult_koyck_model_gaming, Var = "sales",slideBy = -3)
mult_koyck_model_gaming <- slide(mult_koyck_model_gaming, Var = "listing_price",slideBy = -1)
mult_koyck_model_gaming <- slide(mult_koyck_model_gaming, Var = "mean_nps",slideBy = -3)
mult_koyck_model_gaming <- slide(mult_koyck_model_gaming, Var = "avg_grp",slideBy = -1)


mult_koyck_model_gaming<-na.omit(mult_koyck_model_gaming)
mult_koyck_model_gaming[mult_koyck_model_gaming==0]<-1
mult_koyck_model_gaming<- log(mult_koyck_model_gaming)
mult_koyck_model_gaming<-as.data.frame(scale(mult_koyck_model_gaming))


# Starting first itiration 

ga_mu_koyck_model1<- lm(sales~., data=mult_koyck_model_gaming)
#Multiple R-squared:  0.9961, Adjusted R-squared:  0.9929 
summary(ga_mu_koyck_model1)

# Step AIC Function

step_mu_koyck <- stepAIC(ga_mu_koyck_model1)

# Second itiration 

ga_mu_koyck_model2<- lm(sales ~ TV_Adstock + Cont_Adstock + Online_Marketing_Adstock + 
                          Aff_Adstock + discounted_perc + listing_price + perc_prepaid + 
                          avg_sla + avg_procurement_sla + perc_less_than_3dayssla + 
                          avg_grp + mean_nps + `listing_price-1` + `mean_nps-3` + `avg_grp-1`, data=mult_koyck_model_gaming)
#Multiple R-squared:  0.9964, Adjusted R-squared:  0.9948 
summary(ga_mu_koyck_model2)
vif(ga_mu_koyck_model2)


# Removing Online_Marketing_Adstock 

ga_mu_koyck_model3<- lm(sales ~ TV_Adstock + Cont_Adstock  + 
                          Aff_Adstock + discounted_perc + listing_price + perc_prepaid + 
                          avg_sla + avg_procurement_sla + perc_less_than_3dayssla + 
                          avg_grp + mean_nps + `listing_price-1` + `mean_nps-3` + `avg_grp-1`, data=mult_koyck_model_gaming)
#Multiple R-squared:  0.9962, Adjusted R-squared:  0.9946
summary(ga_mu_koyck_model3)
vif(ga_mu_koyck_model3)



# Removing avg_grp due to high VIF 

ga_mu_koyck_model4<- lm(sales ~ TV_Adstock + Cont_Adstock  + 
                          Aff_Adstock + discounted_perc + listing_price + perc_prepaid + 
                          avg_sla + avg_procurement_sla + perc_less_than_3dayssla + 
                          mean_nps + `listing_price-1` + `mean_nps-3` + `avg_grp-1`, data=mult_koyck_model_gaming)
#Multiple R-squared:  0.9947, Adjusted R-squared:  0.9928 
summary(ga_mu_koyck_model4)
vif(ga_mu_koyck_model4)



# Removing TV_Adstock due to high VIF 

ga_mu_koyck_model5<- lm(sales ~   Cont_Adstock  + 
                          Aff_Adstock + discounted_perc + listing_price + perc_prepaid + 
                          avg_sla + avg_procurement_sla + perc_less_than_3dayssla + 
                          mean_nps + `listing_price-1` + `mean_nps-3` + `avg_grp-1`, data=mult_koyck_model_gaming)
#Multiple R-squared:  0.9947, Adjusted R-squared:  0.9928 
summary(ga_mu_koyck_model5)
vif(ga_mu_koyck_model5)


# Removing `avg_grp-1` due to high VIF 

ga_mu_koyck_model6<- lm(sales ~   Cont_Adstock  + 
                          Aff_Adstock + discounted_perc + listing_price + perc_prepaid + 
                          avg_sla + avg_procurement_sla + perc_less_than_3dayssla + 
                          mean_nps + `listing_price-1` + `mean_nps-3` , data=mult_koyck_model_gaming)
#Multiple R-squared:  0.9935, Adjusted R-squared:  0.9917 
summary(ga_mu_koyck_model6)
vif(ga_mu_koyck_model6)


# Removing avg_sla due to high VIF 

ga_mu_koyck_model7<- lm(sales ~   Cont_Adstock  + 
                          Aff_Adstock + discounted_perc + listing_price + perc_prepaid + 
                          avg_procurement_sla + perc_less_than_3dayssla + 
                          mean_nps + `listing_price-1` + `mean_nps-3` , data=mult_koyck_model_gaming)
#Multiple R-squared:  0.9935, Adjusted R-squared:  0.9917 
summary(ga_mu_koyck_model7)
vif(ga_mu_koyck_model7)


# Removing Aff_Adstock due to high VIF 

ga_mu_koyck_model8<- lm(sales ~   Cont_Adstock  + 
                          discounted_perc + listing_price + perc_prepaid + 
                          avg_procurement_sla + perc_less_than_3dayssla + 
                          mean_nps + `listing_price-1` + `mean_nps-3` , data=mult_koyck_model_gaming)
#Multiple R-squared:  0.9882, Adjusted R-squared:  0.9855 
summary(ga_mu_koyck_model8)
vif(ga_mu_koyck_model8)



# Removing listing_price due to high VIF 

ga_mu_koyck_model9<- lm(sales ~   Cont_Adstock  + 
                          discounted_perc  + perc_prepaid + 
                          avg_procurement_sla + perc_less_than_3dayssla + 
                          mean_nps + `listing_price-1` + `mean_nps-3` , data=mult_koyck_model_gaming)
#Multiple R-squared:  0.923,  Adjusted R-squared:  0.9079 
summary(ga_mu_koyck_model9)
vif(ga_mu_koyck_model9)


# Removing  Cont_Adstock due to high VIF 

ga_mu_koyck_model10<- lm(sales ~ 
                           discounted_perc  + perc_prepaid + 
                           avg_procurement_sla + perc_less_than_3dayssla + 
                           mean_nps + `listing_price-1` + `mean_nps-3` , data=mult_koyck_model_gaming)
#Multiple R-squared:  0.923,  Adjusted R-squared:  0.9101 
summary(ga_mu_koyck_model10)
vif(ga_mu_koyck_model10)



# Removing  discounted_perc due to high VIF 

ga_mu_koyck_model11<- lm(sales ~ 
                           perc_prepaid + 
                           avg_procurement_sla + perc_less_than_3dayssla + 
                           mean_nps + `listing_price-1` + `mean_nps-3` , data=mult_koyck_model_gaming)
#Multiple R-squared:  0.9113, Adjusted R-squared:  0.8989 
summary(ga_mu_koyck_model11)
vif(ga_mu_koyck_model11)

# Removing  `mean_nps-3`   due to high VIF 

ga_mu_koyck_model12<- lm(sales ~ 
                           perc_prepaid + 
                           avg_procurement_sla + perc_less_than_3dayssla + 
                           mean_nps + `listing_price-1`  , data=mult_koyck_model_gaming)
#Multiple R-squared:  0.9113, Adjusted R-squared:  0.8989 
summary(ga_mu_koyck_model12)
vif(ga_mu_koyck_model12)


# Removing  `listing_price-1`   due to high VIF 

ga_mu_koyck_model13<- lm(sales ~ 
                           perc_prepaid + 
                           avg_procurement_sla + perc_less_than_3dayssla + 
                           mean_nps  , data=mult_koyck_model_gaming)
#Multiple R-squared:  0.904,  Adjusted R-squared:  0.8954 
summary(ga_mu_koyck_model13)
vif(ga_mu_koyck_model13)

# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = mult_koyck_model_gaming, form.lm = formula(sales ~ perc_prepaid + avg_procurement_sla + perc_less_than_3dayssla + mean_nps), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.



# 3.6 Calculating and plotting elasticities 
##############################################################

# Calculating elasticity of log equation 

model<-log_ga_model_3
data_model<-log_model_gaming

elast_calc_log <- function(var,data_model){
  
  e1 <-as.numeric(model$coefficients[var])
  
  return(e1)
} 

  elastvar_list <- list()
  for(i in 2:length(model$coefficients)){
    
    elastvar_list[i-1] <-elast_calc_log(names(model$coefficients)[i],data_model)
    
  }  
  
  elast_output <- data.frame(names(model$coefficients[2:length(model$coefficients)]))
  elast_output <- cbind(elast_output,do.call(rbind.data.frame, elastvar_list))
  colnames(elast_output) <- c("Variable","Elasticity")
  
  elast_output$direction <- ifelse(elast_output$Elasticity > 0, "Positive", "Negative")
  
  
  plot<-ggplot(data=elast_output, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
    geom_bar(position="dodge",stat="identity") + 
    coord_flip() +
    ggtitle("Gaming Accessory- Multiplicative Model") +xlab("Variables")


# In Gaming accessories we saw that the Multiplicative is the best model for Gaming Accessories since it gives highest r square and lowest mse.
# Let's calculate and plot the elasticity found in the model
# We see that the avg_sla,discounted_perc,cont_adstock have top positive elasticities which implies investing in them will help increase GMV
