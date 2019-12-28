
#########################################################################################
######################## 4. Modeling for Camera Accessories #############################
#########################################################################################
# Reading the output dataframe 
Final_table<-read.csv("Final_table.csv")
# We have run the following 5 types of models below - 
# 4.1 Linear Regression
# 4.2 Multiplicative Model
# 4.3 Distributed Lag Model
# 4.4 Koyck Model
# 4.5 Multiplicative and Distributed Lag Combined Model

############################################################
# 4.1 Running multiple iterations of Linear Regression Model
############################################################
# Also, performing Step AIC and VIF for feature selection
# Finally, performed a Cross-Validation to ensure stability of selected model

# Running the first round of model for camera accessory

model_camera<-Final_table[Final_table$subcat=="CameraAccessory",]
model_camera<- subset(model_camera,select = -c(week,subcat,TV_Spends,TV_adstockrate,Dig_Spends,Dig_adstockrate,Spons_Spends,Spons_adstockrate,Cont_Spends,Cont_adstockrate,Online_Marketing_Spends,Online_Marketing_adstockrate, Aff_Spends,Aff_adstockrate,SEM_Spends,SEM_adstockrate,total_orders,prepaid_orders,flag,n_days_sale))

ca_model_data<-as.data.frame(scale(model_camera))

ca_model_1=lm(sales~.,data=ca_model_data)

#Multiple R-squared:  0.8739, Adjusted R-squared:  0.8012 
summary(ca_model_1)

vif(ca_model_1)

# Running step AIC for selection of features
step <- stepAIC(ca_model_1,direction = "both")


# Running second round of regression
ca_model_2=lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + shelfprice_mean + 
                discounted_perc + listing_price + ma2_shelfprice_mean + avg_procurement_sla + 
                perc_less_than_3dayssla + avg_grp + Cont_Adstock,data=ca_model_data)
#Multiple R-squared:  0.8601, Adjusted R-squared:  0.8226 
summary(ca_model_2)
vif(ca_model_2)

# Running third round of regression; Removing Cont_Adstock due to high VIF and high p-value
ca_model_3=lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + shelfprice_mean + 
                discounted_perc + listing_price + ma2_shelfprice_mean + avg_procurement_sla + 
                perc_less_than_3dayssla + avg_grp,data=ca_model_data)
#Multiple R-squared:  0.8528, Adjusted R-squared:  0.8177
summary(ca_model_3)
vif(ca_model_3)

# Running 4th  round of regression; Removing ma2_shelfprice_mean due to high VIF

ca_model_4=lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + shelfprice_mean + 
                discounted_perc + listing_price + avg_procurement_sla + 
                perc_less_than_3dayssla + avg_grp,data=ca_model_data)
#Multiple R-squared:  0.8145, Adjusted R-squared:  0.7756 
summary(ca_model_4)
vif(ca_model_4)

# Running 5th  round of regression; Removing perc_less_than_3dayssla due to high p value

ca_model_5=lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + shelfprice_mean + 
                discounted_perc + listing_price + avg_procurement_sla + 
                avg_grp  
              ,data=ca_model_data)
# Multiple R-squared:  0.8126,  Adjusted R-squared:  0.7785
summary(ca_model_5)
vif(ca_model_5)

# Running 6th round of regression; Removing TV_Adstock due to high p value


ca_model_6=lm(sales ~ Digital_Adstock + Spons_Adstock + shelfprice_mean + 
                discounted_perc + listing_price + avg_procurement_sla + 
                avg_grp,data=ca_model_data)
#Multiple R-squared:  0.809,  Adjusted R-squared:  0.7793 
summary(ca_model_6)
vif(ca_model_6)


# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = ca_model_data, form.lm = formula(sales ~ Digital_Adstock + Spons_Adstock + shelfprice_mean + discounted_perc + listing_price + avg_procurement_sla + avg_grp), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.


# 4.2 Camera Accessories: Multiplicative Model 
###################################################################

# 4.2.1 Creating a logarithmic training dataset
model_camera_imputed <- model_camera[model_camera$sales>0,]
model_camera_imputed[model_camera_imputed==0]<-1
log_model_camera <- log(model_camera_imputed)

# 4.2.2 Standardizing data
log_model_camera<- as.data.frame(scale(log_model_camera))

# 4.2.3 Performing multiple iterations of the multiplicative model, Step AIC and VIF Treatment. Finally, we have also performed K-Fold cross validation to check if the model is robust

# Running first round of linear regression 
log_ca_model_1=lm(sales~.,data=log_model_camera)
#Multiple R-squared:  0.96, Adjusted R-squared:  0.934 
summary(log_ca_model_1)

# Running stepAIC on the first model
step_log <- stepAIC(log_ca_model_1)

# Running second round using stepAIC results

log_ca_model_2=lm(sales ~ TV_Adstock + Spons_Adstock + Cont_Adstock + Online_Marketing_Adstock + 
                    Aff_Adstock + SEM_Adstock + shelfprice_mean + listing_price + 
                    perc_prepaid + avg_sla + avg_grp + mean_nps,data=log_model_camera)

#Multiple R-squared:  0.957,  Adjusted R-squared:  0.944 
summary(log_ca_model_2)
vif(log_ca_model_2)

# Removing Online_Marketing_Adstock since it has high VIF

log_ca_model_3=lm(sales ~ TV_Adstock + Spons_Adstock + Cont_Adstock + 
                    Aff_Adstock + SEM_Adstock + shelfprice_mean + listing_price + 
                    perc_prepaid + avg_sla + avg_grp + mean_nps,data=log_model_camera)
#Multiple R-squared:  0.919,  Adjusted R-squared:  0.896 
summary(log_ca_model_3)
vif(log_ca_model_3)

# Removing Aff_Adstock since it has high VIF

log_ca_model_4=lm(sales ~ TV_Adstock + Spons_Adstock + Cont_Adstock + 
                    SEM_Adstock + shelfprice_mean + listing_price + 
                    perc_prepaid + avg_sla + avg_grp + mean_nps,data=log_model_camera)
#Multiple R-squared:  0.915,  Adjusted R-squared:  0.893 
summary(log_ca_model_4)
vif(log_ca_model_4)

# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = log_model_camera, form.lm = formula(sales ~ TV_Adstock + Spons_Adstock + Cont_Adstock + SEM_Adstock + shelfprice_mean + listing_price + perc_prepaid + avg_sla + avg_grp + mean_nps), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.


# 4.3  Camera Accessories: Distributed Lag Model
###############################################################

# Here, we prepare data for the distrbuted lag model and execute it. We also perform StepAIC and VIF techniques to select variables and finally perform K-Fold cross validation to ensure robustness of the model

# 4.3.1 Creating the lagged dataset
dist_model_camera<-model_camera
dist_model_camera <- slide(dist_model_camera, Var = "listing_price",slideBy = -1)
dist_model_camera <- slide(dist_model_camera, Var = "sales",slideBy = -1)

# Removing NAs and Scaling the variables
dist_model_camera <- na.omit(dist_model_camera)
dist_model_camera<-as.data.frame(scale(dist_model_camera))

# Running first iteration of the model
ca_dist_model_1 <- lm(sales~.,data=dist_model_camera)
#Multiple R-squared:  0.896,  Adjusted R-squared:  0.824 
summary(ca_dist_model_1)
vif(ca_dist_model_1)


# Running stepAIC on the model
step_dist <- stepAIC(ca_dist_model_1, direction = "both")

# Running second itiration 
ca_dist_model2<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Cont_Adstock + 
                      Online_Marketing_Adstock + Aff_Adstock + shelfprice_mean + 
                      discounted_perc + listing_price + ma2_shelfprice_mean + avg_procurement_sla + 
                      perc_less_than_3dayssla + avg_grp + `listing_price-1` + `sales-1`,data=dist_model_camera)
#Multiple R-squared:  0.889,  Adjusted R-squared:  0.843
summary(ca_dist_model2)
vif(ca_dist_model2)

# Removing Online_Marketing_Adstock
ca_dist_model3<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Cont_Adstock + 
                      Aff_Adstock + shelfprice_mean + 
                      discounted_perc + listing_price + ma2_shelfprice_mean + avg_procurement_sla + 
                      perc_less_than_3dayssla + avg_grp + `listing_price-1` + `sales-1`,data=dist_model_camera)
#Multiple R-squared:  0.884,  Adjusted R-squared:  0.841 
summary(ca_dist_model3)
vif(ca_dist_model3)

# Removing Cont_Adstock
ca_dist_model4<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + 
                      Aff_Adstock + shelfprice_mean + 
                      discounted_perc + listing_price + ma2_shelfprice_mean + avg_procurement_sla + 
                      perc_less_than_3dayssla + avg_grp + `listing_price-1` + `sales-1`,data=dist_model_camera)
#Multiple R-squared:  0.878,  Adjusted R-squared:  0.837 
summary(ca_dist_model4)
vif(ca_dist_model4)

# Removing perc_less_than_3dayssla
ca_dist_model5<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + 
                      Aff_Adstock + shelfprice_mean + 
                      discounted_perc + listing_price + ma2_shelfprice_mean + avg_procurement_sla + 
                      avg_grp + `listing_price-1` + `sales-1`,data=dist_model_camera)
#Multiple R-squared:  0.871,  Adjusted R-squared:  0.831  
summary(ca_dist_model5)
vif(ca_dist_model5)

# Removing ma2_shelfprice_mean
ca_dist_model6<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + 
                      Aff_Adstock + shelfprice_mean + 
                      discounted_perc + listing_price + avg_procurement_sla + 
                      avg_grp + `listing_price-1` + `sales-1`,data=dist_model_camera)
#Multiple R-squared:  0.833,  Adjusted R-squared:  0.787
summary(ca_dist_model6)
vif(ca_dist_model6)

# Removing `listing_price-1`
ca_dist_model7<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + 
                      Aff_Adstock + shelfprice_mean + 
                      discounted_perc + listing_price + avg_procurement_sla + 
                      avg_grp + `sales-1`,data=dist_model_camera)
#Multiple R-squared:  0.826,  Adjusted R-squared:  0.783 
summary(ca_dist_model7)
vif(ca_dist_model7)


# Removing shelfprice_mean
ca_dist_model8<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + 
                      Aff_Adstock + 
                      discounted_perc + listing_price + avg_procurement_sla + 
                      avg_grp + `sales-1`,data=dist_model_camera)
#Multiple R-squared:  0.75, Adjusted R-squared:  0.697
summary(ca_dist_model8)
vif(ca_dist_model8)

# Removing `sales-1`
ca_dist_model9<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + 
                      Aff_Adstock + 
                      discounted_perc + listing_price + avg_procurement_sla + 
                      avg_grp,data=dist_model_camera)
#Multiple R-squared:  0.746,  Adjusted R-squared:  0.699  
summary(ca_dist_model9)
vif(ca_dist_model9)


# Removing discounted_perc
ca_dist_model10<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + 
                       Aff_Adstock + 
                       listing_price + avg_procurement_sla + 
                       avg_grp,data=dist_model_camera)
#Multiple R-squared:  0.739,  Adjusted R-squared:  0.698
summary(ca_dist_model10)
vif(ca_dist_model10)

# Removing TV_Adstock
ca_dist_model11<- lm(sales ~ Digital_Adstock + Spons_Adstock + 
                       Aff_Adstock + 
                       listing_price + avg_procurement_sla + 
                       avg_grp,data=dist_model_camera)
#Multiple R-squared:  0.729,  Adjusted R-squared:  0.693 
summary(ca_dist_model11)
vif(ca_dist_model11)

# Removing Aff_Adstock
ca_dist_model12<- lm(sales ~ Digital_Adstock + Spons_Adstock + 
                       listing_price + avg_procurement_sla + 
                       avg_grp,data=dist_model_camera)
#Multiple R-squared:  0.729,  Adjusted R-squared:  0.693 
summary(ca_dist_model12)
vif(ca_dist_model12)

# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = dist_model_camera, form.lm = formula(sales ~ Digital_Adstock + Spons_Adstock + listing_price + avg_procurement_sla + avg_grp,data=dist_model_camera), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.


# 4.4 Koyck Model for Camera Accessories
########################################

koyck_model_camera<-model_camera
koyck_model_camera <- slide(koyck_model_camera, Var = "sales",slideBy = -1)
koyck_model_camera <- slide(koyck_model_camera, Var = "sales",slideBy = -2)
koyck_model_camera <- slide(koyck_model_camera, Var = "sales",slideBy = -3)

koyck_model_camera<-na.omit(koyck_model_camera)
koyck_model_camera<-as.data.frame(scale(koyck_model_camera))

# Running first iteration of the model

ca_koyck_model1<- lm(sales~., data=koyck_model_camera)
#Multiple R-squared:  0.905,  Adjusted R-squared:  0.828
summary(ca_koyck_model1)
vif(ca_koyck_model1)

# Running stepAIC 
step_koyck <- stepAIC(ca_koyck_model1)

# Running second itiration of the model

ca_koyck_model2<- lm(sales ~ TV_Adstock + Spons_Adstock + SEM_Adstock + shelfprice_mean + 
                       discounted_perc + listing_price + ma2_shelfprice_mean + perc_prepaid + 
                       avg_procurement_sla + perc_less_than_3dayssla + avg_grp + 
                       mean_nps + `sales-1` + `sales-2`, data=koyck_model_camera)
# Multiple R-squared:  0.897, Adjusted R-squared:  0.856  
summary(ca_koyck_model2)
vif(ca_koyck_model2)


# Removing avg_procurement_sla

ca_koyck_model3<- lm(sales ~ TV_Adstock + Spons_Adstock + SEM_Adstock + shelfprice_mean + 
                       discounted_perc + listing_price + ma2_shelfprice_mean + perc_prepaid + 
                       perc_less_than_3dayssla + avg_grp + 
                       mean_nps + `sales-1` + `sales-2`, data=koyck_model_camera)
#Multiple R-squared:  0.863,  Adjusted R-squared:  0.814 
summary(ca_koyck_model3)
vif(ca_koyck_model3)


# Removing ma2_shelfprice_mean


ca_koyck_model4<- lm(sales ~ TV_Adstock + Spons_Adstock + SEM_Adstock + shelfprice_mean + 
                       discounted_perc + listing_price + perc_prepaid + 
                       perc_less_than_3dayssla + avg_grp + 
                       mean_nps + `sales-1` + `sales-2`, data=koyck_model_camera)
#Multiple R-squared:  0.832,  Adjusted R-squared:  0.777  
summary(ca_koyck_model4)
vif(ca_koyck_model4)


# Removing mean_nps
ca_koyck_model5<- lm(sales ~ TV_Adstock + Spons_Adstock + SEM_Adstock + shelfprice_mean + 
                       discounted_perc + listing_price + perc_prepaid + 
                       perc_less_than_3dayssla + avg_grp + 
                       `sales-1` + `sales-2`, data=koyck_model_camera)
#Multiple R-squared:  0.783,  Adjusted R-squared:  0.721 
summary(ca_koyck_model5)
vif(ca_koyck_model5)


# Removing shelfprice_mean
ca_koyck_model6<- lm(sales ~ TV_Adstock + Spons_Adstock + SEM_Adstock + 
                       discounted_perc + listing_price + perc_prepaid + 
                       perc_less_than_3dayssla + avg_grp + 
                       `sales-1` + `sales-2`, data=koyck_model_camera)
#Multiple R-squared:  0.717,  Adjusted R-squared:  0.644  
summary(ca_koyck_model6)
vif(ca_koyck_model6)


# Removing `sales-1`
ca_koyck_model7<- lm(sales ~ TV_Adstock + Spons_Adstock + SEM_Adstock + 
                       discounted_perc + listing_price + perc_prepaid + 
                       perc_less_than_3dayssla + avg_grp + 
                       `sales-2`, data=koyck_model_camera)
#Multiple R-squared:  0.716,  Adjusted R-squared:  0.653 
summary(ca_koyck_model7)
vif(ca_koyck_model7)

# Removing perc_prepaid
ca_koyck_model8<- lm(sales ~ TV_Adstock + Spons_Adstock + SEM_Adstock + 
                       discounted_perc + listing_price + 
                       perc_less_than_3dayssla + avg_grp + 
                       `sales-2`, data=koyck_model_camera)
#Multiple R-squared:  0.716,  Adjusted R-squared:  0.66 
summary(ca_koyck_model8)
vif(ca_koyck_model8)

# Removing TV_Adstock
ca_koyck_model9<- lm(sales ~ Spons_Adstock + SEM_Adstock + 
                       discounted_perc + listing_price + 
                       perc_less_than_3dayssla + avg_grp + 
                       `sales-2`, data=koyck_model_camera)
#Multiple R-squared:  0.712,  Adjusted R-squared:  0.664 
summary(ca_koyck_model9)
vif(ca_koyck_model9)

# Removing `sales-2`
ca_koyck_model10<- lm(sales ~ Spons_Adstock + SEM_Adstock + 
                        discounted_perc + listing_price + 
                        perc_less_than_3dayssla + avg_grp 
                      , data=koyck_model_camera)
#Multiple R-squared:  0.708,  Adjusted R-squared:  0.667 
summary(ca_koyck_model10)
vif(ca_koyck_model10)

# Removing perc_less_than_3dayssla
ca_koyck_model11<- lm(sales ~ Spons_Adstock + SEM_Adstock + 
                        discounted_perc + listing_price + 
                        avg_grp 
                      , data=koyck_model_camera)
#Multiple R-squared:  0.708,  Adjusted R-squared:  0.667 
summary(ca_koyck_model11)
vif(ca_koyck_model11)

# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = koyck_model_camera, form.lm = formula(sales ~ Spons_Adstock + SEM_Adstock +  discounted_perc + listing_price + avg_grp), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.

# 4.5 Combination of Distributed Lag and Multiplicative Models
##############################################################

mult_koyck_model_camera<-model_camera
mult_koyck_model_camera <- slide(mult_koyck_model_camera, Var = "sales",slideBy = -1)
mult_koyck_model_camera <- slide(mult_koyck_model_camera, Var = "sales",slideBy = -2)
mult_koyck_model_camera <- slide(mult_koyck_model_camera, Var = "sales",slideBy = -3)
mult_koyck_model_camera <- slide(mult_koyck_model_camera, Var = "listing_price",slideBy = -1)
mult_koyck_model_camera <- slide(mult_koyck_model_camera, Var = "mean_nps",slideBy = -3)
mult_koyck_model_camera <- slide(mult_koyck_model_camera, Var = "avg_grp",slideBy = -1)


mult_koyck_model_camera<-na.omit(mult_koyck_model_camera)
mult_koyck_model_camera[mult_koyck_model_camera==0]<-1
mult_koyck_model_camera<- log(mult_koyck_model_camera)
mult_koyck_model_camera<-as.data.frame(scale(mult_koyck_model_camera))


# Starting first itiration 

ca_mu_koyck_model1<- lm(sales~., data=mult_koyck_model_camera)
#Multiple R-squared:  0.997,  Adjusted R-squared:  0.994 
summary(ca_mu_koyck_model1)

# Step AIC Function

step_mu_koyck <- stepAIC(ca_mu_koyck_model1)

# Second itiration 

ca_mu_koyck_model2<- lm(sales ~ TV_Adstock + Online_Marketing_Adstock + Aff_Adstock + 
                          SEM_Adstock + shelfprice_mean + listing_price + ma2_shelfprice_mean + 
                          avg_procurement_sla + avg_grp + mean_nps + `sales-1` + `sales-3` + 
                          `listing_price-1`, data=mult_koyck_model_camera)
#Multiple R-squared:  0.997,  Adjusted R-squared:  0.996 
summary(ca_mu_koyck_model2)
vif(ca_mu_koyck_model2)


# Removing Online_Marketing_Adstock 

ca_mu_koyck_model3<- lm(sales ~ TV_Adstock + Aff_Adstock + 
                          SEM_Adstock + shelfprice_mean + listing_price + ma2_shelfprice_mean + 
                          avg_procurement_sla + avg_grp + mean_nps + `sales-1` + `sales-3` + 
                          `listing_price-1`, data=mult_koyck_model_camera)
#Multiple R-squared:  0.994,  Adjusted R-squared:  0.992
summary(ca_mu_koyck_model3)
vif(ca_mu_koyck_model3)



# Removing `sales-1` 

ca_mu_koyck_model4<- lm(sales ~ TV_Adstock + Aff_Adstock + 
                          SEM_Adstock + shelfprice_mean + listing_price + ma2_shelfprice_mean + 
                          avg_procurement_sla + avg_grp + mean_nps + `sales-3` + 
                          `listing_price-1`, data=mult_koyck_model_camera)
#Multiple R-squared:  0.994,  Adjusted R-squared:  0.992
summary(ca_mu_koyck_model4)
vif(ca_mu_koyck_model4)



# Removing avg_grp 

ca_mu_koyck_model5<- lm(sales ~ TV_Adstock + Aff_Adstock + 
                          SEM_Adstock + shelfprice_mean + listing_price + ma2_shelfprice_mean + 
                          avg_procurement_sla + mean_nps + `sales-3` + 
                          `listing_price-1`, data=mult_koyck_model_camera)
#Multiple R-squared:  0.994,  Adjusted R-squared:  0.992 
summary(ca_mu_koyck_model5)
vif(ca_mu_koyck_model5)


# Removing TV_Adstock 

ca_mu_koyck_model6<- lm(sales ~ Aff_Adstock + 
                          SEM_Adstock + shelfprice_mean + listing_price + ma2_shelfprice_mean + 
                          avg_procurement_sla + mean_nps + `sales-3` + 
                          `listing_price-1`, data=mult_koyck_model_camera)
#Multiple R-squared:  0.988,  Adjusted R-squared:  0.985 
summary(ca_mu_koyck_model6)
vif(ca_mu_koyck_model6)


# Removing Aff_Adstock 

ca_mu_koyck_model7<- lm(sales ~ SEM_Adstock + shelfprice_mean + listing_price + ma2_shelfprice_mean + 
                          avg_procurement_sla + mean_nps + `sales-3` + 
                          `listing_price-1`, data=mult_koyck_model_camera)
#Multiple R-squared:  0.986,  Adjusted R-squared:  0.983 
summary(ca_mu_koyck_model7)
vif(ca_mu_koyck_model7)


# Removing listing_price 

ca_mu_koyck_model8<- lm(sales ~ SEM_Adstock + shelfprice_mean + ma2_shelfprice_mean + 
                          avg_procurement_sla + mean_nps + `sales-3` + 
                          `listing_price-1`, data=mult_koyck_model_camera)
#Multiple R-squared:  0.778,  Adjusted R-squared:  0.741
summary(ca_mu_koyck_model8)
vif(ca_mu_koyck_model8)



# Removing SEM_Adstock 

ca_mu_koyck_model9<- lm(sales ~ shelfprice_mean + ma2_shelfprice_mean + 
                          avg_procurement_sla + mean_nps + `sales-3` + 
                          `listing_price-1`, data=mult_koyck_model_camera)
#Multiple R-squared:  0.778,  Adjusted R-squared:  0.747 
summary(ca_mu_koyck_model9)
vif(ca_mu_koyck_model9)


# Removing  ma2_shelfprice_mean 

ca_mu_koyck_model10<- lm(sales ~ shelfprice_mean + 
                           avg_procurement_sla + mean_nps + `sales-3` + 
                           `listing_price-1`, data=mult_koyck_model_camera)
#Multiple R-squared:  0.774,  Adjusted R-squared:  0.748  
summary(ca_mu_koyck_model10)
vif(ca_mu_koyck_model10)



# Removing  shelfprice_mean 

ca_mu_koyck_model11<- lm(sales ~ avg_procurement_sla + mean_nps + `sales-3` + 
                           `listing_price-1`, data=mult_koyck_model_camera)
#Multiple R-squared:  0.756,  Adjusted R-squared:  0.735 
summary(ca_mu_koyck_model11)
vif(ca_mu_koyck_model11)


# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = mult_koyck_model_camera, form.lm = formula(sales ~ avg_procurement_sla + mean_nps + `sales-3` + `listing_price-1`), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.

# 3.6 Calculating and plotting elasticities 
##############################################################
# Calculating elasticity of log equation 


model<-ca_dist_model12
data_model<-dist_model_camera

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
    ggtitle("Camera Accessory- Multiplicative Model") +xlab("Variables")


# In Camera Accessories we saw that the Multiplicative  is the best model for Camera Accessories since it gives highest r square and lowest mse.
# We see that the listing_price and Spons_adstock have top positive elasticities which implies investing in them will help increase GMV. 
# Also procurement SLA is negative,which shows that the faster the item is procured, the better the sales will be 
# Elasticity of digital adstock is negative, which shows that investment in digital spends isn't really helping CA.

