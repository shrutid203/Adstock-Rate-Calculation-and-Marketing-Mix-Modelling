
#################################################################################
######################## 5. Modeling for Home Audio #############################
#################################################################################
# Reading the output dataframe 
Final_table<-read.csv("Final_table.csv")
# We have run the following 5 types of models below - 
# 5.1 Linear Regression
# 5.2 Multiplicative Model
# 5.3 Distributed Lag Model
# 5.4 Koyck Model
# 5.5 Multiplicative and Distributed Lag Combined Model

############################################################
# 5.1 Running multiple iterations of Linear Regression Model
############################################################
# Also, performing Step AIC and VIF for feature selection
# Finally, performed a Cross-Validation to ensure stability of selected model

# Running the first round of model for camera accessory

model_home_audio<-Final_table[Final_table$subcat=="HomeAudio",]
model_home_audio<- subset(model_home_audio,select = -c(week,subcat,TV_Spends,TV_adstockrate,Dig_Spends,Dig_adstockrate,Spons_Spends,Spons_adstockrate,Cont_Spends,Cont_adstockrate,Online_Marketing_Spends,Online_Marketing_adstockrate, Aff_Spends,Aff_adstockrate,SEM_Spends,SEM_adstockrate,total_orders,prepaid_orders,flag,n_days_sale))

ha_model_data<-as.data.frame(scale(model_home_audio))

ha_model_1=lm(sales~.,data=ha_model_data)

#Multiple R-squared:  0.815,  Adjusted R-squared:  0.709 
summary(ha_model_1)

vif(ha_model_1)

# Running step AIC for selection of features
step <- stepAIC(ha_model_1,direction = "both")


# Running second round of regression
ha_model_2=lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Cont_Adstock + 
                Aff_Adstock + shelfprice_mean + listing_price + ma2_shelfprice_mean + 
                ma3_shelfprice_mean,data=ha_model_data)
#Multiple R-squared:  0.803,  Adjusted R-squared:  0.762 
summary(ha_model_2)
vif(ha_model_2)

# Running third round of regression; Removing shelfprice_mean due to high VIF and high p-value
ha_model_3=lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Cont_Adstock + 
                Aff_Adstock + listing_price + ma2_shelfprice_mean + 
                ma3_shelfprice_mean,data=ha_model_data)
#Multiple R-squared:  0.783,  Adjusted R-squared:  0.744
summary(ha_model_3)
vif(ha_model_3)

# Running 4th  round of regression; Removing ma2_shelfprice_mean due to high VIF

ha_model_4=lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Cont_Adstock + 
                Aff_Adstock + listing_price + 
                ma3_shelfprice_mean,data=ha_model_data)
#Multiple R-squared:  0.779,  Adjusted R-squared:  0.745 
summary(ha_model_4)
vif(ha_model_4)


# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = ha_model_data, form.lm = formula(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Cont_Adstock + Aff_Adstock + listing_price + ma3_shelfprice_mean), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.


# 5.2 Home Audio: Multiplicative Model 
###################################################################

# 5.2.1 Creating a logarithmic training dataset
model_home_audio_imputed <- model_home_audio[model_home_audio$sales>0,]
model_home_audio_imputed[model_home_audio_imputed==0]<-1
log_model_home_audio <- log(model_home_audio_imputed)

# 5.2.2 Standardizing data
log_model_home_audio<- as.data.frame(scale(log_model_home_audio))

# 5.2.3 Performing multiple iterations of the multiplicative model, Step AIC and VIF Treatment. Finally, we have also performed K-Fold cross validation to check if the model is robust

# Running first round of linear regression 
log_ha_model_1=lm(sales~.,data=log_model_home_audio)
#Multiple R-squared:  0.714,  Adjusted R-squared:  0.52 
summary(log_ha_model_1)

# Running stepAIC on the first model
step_log <- stepAIC(log_ha_model_1, direction = "both")

# Running second round using stepAIC results

log_ha_model_2=lm(sales ~ Digital_Adstock + Spons_Adstock + Cont_Adstock + listing_price + 
                    ma3_shelfprice_mean + perc_less_than_3dayssla,data=log_model_home_audio)

#Multiple R-squared:  0.661,  Adjusted R-squared:  0.612 
summary(log_ha_model_2)
vif(log_ha_model_2)

# VIF is less than 2 for all variables
# Removing ma3_shelfprice_mean since it has high p-value

log_ha_model_3=lm(sales ~ Digital_Adstock + Spons_Adstock + Cont_Adstock + listing_price + 
                    perc_less_than_3dayssla,data=log_model_home_audio)
#Multiple R-squared:  0.643,  Adjusted R-squared:   0.6 
summary(log_ha_model_3)
vif(log_ha_model_3)

# Removing perc_less_than_3dayssla since it has high p-value

log_ha_model_4=lm(sales ~ Digital_Adstock + Spons_Adstock + Cont_Adstock + listing_price,data=log_model_home_audio)
#Multiple R-squared:  0.624,  Adjusted R-squared:  0.589 
summary(log_ha_model_4)
vif(log_ha_model_4)

# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = log_model_home_audio, form.lm = formula(sales ~ Digital_Adstock + Spons_Adstock + Cont_Adstock + listing_price), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.


# 5.3  Home Audio: Distributed Lag Model
###############################################################

# Here, we prepare data for the distrbuted lag model and execute it. We also perform StepAIC and VIF techniques to select variables and finally perform K-Fold cross validation to ensure robustness of the model

# 5.3.1 Creating the lagged dataset
dist_model_home_audio<-model_home_audio
dist_model_home_audio <- slide(dist_model_home_audio, Var = "listing_price",slideBy = -1)
dist_model_home_audio <- slide(dist_model_home_audio, Var = "sales",slideBy = -1)

# Removing NAs and Scaling the variables
dist_model_home_audio <- na.omit(dist_model_home_audio)
dist_model_home_audio<-as.data.frame(scale(dist_model_home_audio))

# Running first iteration of the model
ha_dist_model_1 <- lm(sales~.,data=dist_model_home_audio)
#Multiple R-squared:  0.822,  Adjusted R-squared:  0.697 
summary(ha_dist_model_1)
vif(ha_dist_model_1)


# Running stepAIC on the model
step_dist <- stepAIC(ha_dist_model_1, direction = "both")

# Running second itiration 
ha_dist_model2<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Aff_Adstock + 
                      SEM_Adstock + shelfprice_mean + listing_price + ma2_shelfprice_mean + 
                      ma3_shelfprice_mean + `listing_price-1` + `sales-1`,data=dist_model_home_audio)
#Multiple R-squared:  0.808,  Adjusted R-squared:  0.755
summary(ha_dist_model2)
vif(ha_dist_model2)

# Removing shelfprice_mean
ha_dist_model3<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Aff_Adstock + 
                      SEM_Adstock + listing_price + ma2_shelfprice_mean + 
                      ma3_shelfprice_mean + `listing_price-1` + `sales-1`,data=dist_model_home_audio)
#Multiple R-squared:  0.778,  Adjusted R-squared:  0.724 
summary(ha_dist_model3)
vif(ha_dist_model3)

# Removing SEM_Adstock
ha_dist_model4<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Aff_Adstock + 
                      listing_price + ma2_shelfprice_mean + 
                      ma3_shelfprice_mean + `listing_price-1` + `sales-1`,data=dist_model_home_audio)
#Multiple R-squared:  0.736,  Adjusted R-squared:  0.679  
summary(ha_dist_model4)
vif(ha_dist_model4)

# Removing `sales-1`
ha_dist_model5<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Aff_Adstock + 
                      listing_price + ma2_shelfprice_mean + 
                      ma3_shelfprice_mean + `listing_price-1`,data=dist_model_home_audio)
#Multiple R-squared:  0.735,  Adjusted R-squared:  0.686  
summary(ha_dist_model5)
vif(ha_dist_model5)

# Removing `listing_price-1`
ha_dist_model6<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Aff_Adstock + 
                      listing_price + ma2_shelfprice_mean + 
                      ma3_shelfprice_mean,data=dist_model_home_audio)
#Multiple R-squared:  0.733,  Adjusted R-squared:  0.69 
summary(ha_dist_model6)
vif(ha_dist_model6)

# Removing TV_Adstock
ha_dist_model7<- lm(sales ~ Digital_Adstock + Spons_Adstock + Aff_Adstock + 
                      listing_price + ma2_shelfprice_mean + 
                      ma3_shelfprice_mean,data=dist_model_home_audio)
#Multiple R-squared:  0.726,  Adjusted R-squared:  0.689 
summary(ha_dist_model7)
vif(ha_dist_model7)


# Removing ma2_shelfprice_mean
ha_dist_model8<- lm(sales ~ Digital_Adstock + Spons_Adstock + Aff_Adstock + 
                      listing_price + 
                      ma3_shelfprice_mean,data=dist_model_home_audio)
#Multiple R-squared:  0.708,  Adjusted R-squared:  0.676
summary(ha_dist_model8)
vif(ha_dist_model8)

# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = dist_model_home_audio, form.lm = formula(sales ~ Digital_Adstock + Spons_Adstock + Aff_Adstock + listing_price + ma3_shelfprice_mean), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.


# 3.4 Koyck Model for Home Audio
########################################

koyck_model_home_audio<-model_home_audio
koyck_model_home_audio <- slide(koyck_model_home_audio, Var = "sales",slideBy = -1)
koyck_model_home_audio <- slide(koyck_model_home_audio, Var = "sales",slideBy = -2)
koyck_model_home_audio <- slide(koyck_model_home_audio, Var = "sales",slideBy = -3)

koyck_model_home_audio<-na.omit(koyck_model_home_audio)
koyck_model_home_audio<-as.data.frame(scale(koyck_model_home_audio))

# Running first iteration of the model

ha_koyck_model1<- lm(sales~., data=koyck_model_home_audio)
#Multiple R-squared:  0.84, Adjusted R-squared:  0.709
summary(ha_koyck_model1)
vif(ha_koyck_model1)

# Running stepAIC 
step_koyck <- stepAIC(ha_koyck_model1, direction = "both")

# Running second itiration of the model

ha_koyck_model2<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Online_Marketing_Adstock +
                       SEM_Adstock + shelfprice_mean + listing_price + ma2_shelfprice_mean + 
                       ma3_shelfprice_mean + `sales-1`, data=koyck_model_home_audio)
#Multiple R-squared:  0.82, Adjusted R-squared:  0.774  
summary(ha_koyck_model2)
vif(ha_koyck_model2)


# Removing shelfprice_mean

ha_koyck_model3<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Online_Marketing_Adstock +
                       SEM_Adstock + listing_price + ma2_shelfprice_mean + 
                       ma3_shelfprice_mean + `sales-1`, data=koyck_model_home_audio)
#Multiple R-squared:  0.804,  Adjusted R-squared:  0.759 
summary(ha_koyck_model3)
vif(ha_koyck_model3)


# Removing SEM_Adstock
ha_koyck_model4<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Online_Marketing_Adstock +
                       listing_price + ma2_shelfprice_mean + 
                       ma3_shelfprice_mean + `sales-1`, data=koyck_model_home_audio)
#Multiple R-squared:  0.785,  Adjusted R-squared:  0.743  
summary(ha_koyck_model4)
vif(ha_koyck_model4)


# Removing ma2_shelfprice_mean
ha_koyck_model5<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Online_Marketing_Adstock +
                       listing_price + 
                       ma3_shelfprice_mean + `sales-1`, data=koyck_model_home_audio)
#Multiple R-squared:  0.742,  Adjusted R-squared:   0.7 
summary(ha_koyck_model5)
vif(ha_koyck_model5)


# Removing `sales-1`
ha_koyck_model6<- lm(sales ~ TV_Adstock + Digital_Adstock + Spons_Adstock + Online_Marketing_Adstock +
                       listing_price + 
                       ma3_shelfprice_mean, data=koyck_model_home_audio)
#Multiple R-squared:  0.742,  Adjusted R-squared:  0.706  
summary(ha_koyck_model6)
vif(ha_koyck_model6)


# Removing TV_Adstock
ha_koyck_model7<- lm(sales ~ Digital_Adstock + Spons_Adstock + Online_Marketing_Adstock +
                       listing_price + 
                       ma3_shelfprice_mean, data=koyck_model_home_audio)
#Multiple R-squared:  0.74, Adjusted R-squared:  0.711 
summary(ha_koyck_model7)
vif(ha_koyck_model7)

# Removing Online_Marketing_Adstock
ha_koyck_model8<- lm(sales ~ Digital_Adstock + Spons_Adstock +
                       listing_price + 
                       ma3_shelfprice_mean, data=koyck_model_home_audio)
#Multiple R-squared:  0.728,  Adjusted R-squared:  0.704 
summary(ha_koyck_model8)
vif(ha_koyck_model8)

# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = koyck_model_home_audio, form.lm = formula(sales ~ Digital_Adstock + Spons_Adstock + listing_price + ma3_shelfprice_mean), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.

# 3.5 Combination of Distributed Lag and Multiplicative Models
##############################################################

mult_koyck_model_home_audio<-model_home_audio
mult_koyck_model_home_audio <- slide(mult_koyck_model_home_audio, Var = "sales",slideBy = -1)
mult_koyck_model_home_audio <- slide(mult_koyck_model_home_audio, Var = "sales",slideBy = -2)
mult_koyck_model_home_audio <- slide(mult_koyck_model_home_audio, Var = "sales",slideBy = -3)
mult_koyck_model_home_audio <- slide(mult_koyck_model_home_audio, Var = "listing_price",slideBy = -1)
mult_koyck_model_home_audio <- slide(mult_koyck_model_home_audio, Var = "mean_nps",slideBy = -3)
mult_koyck_model_home_audio <- slide(mult_koyck_model_home_audio, Var = "avg_grp",slideBy = -1)


mult_koyck_model_home_audio<-na.omit(mult_koyck_model_home_audio)
mult_koyck_model_home_audio[mult_koyck_model_home_audio==0]<-1
mult_koyck_model_home_audio<- log(mult_koyck_model_home_audio)
mult_koyck_model_home_audio<-as.data.frame(scale(mult_koyck_model_home_audio))


# Starting first itiration 

ha_mu_koyck_model1<- lm(sales~., data=mult_koyck_model_home_audio)
#Multiple R-squared:  0.997,  Adjusted R-squared:  0.994 
summary(ha_mu_koyck_model1)

# Step AIC Function

step_mu_koyck <- stepAIC(ha_mu_koyck_model1, direction = "both")

# Second itiration 

ha_mu_koyck_model2<- lm(sales ~ Digital_Adstock + Spons_Adstock + Cont_Adstock + SEM_Adstock + 
                          shelfprice_mean + listing_price + perc_sale + avg_sla + `sales-3` + 
                          `mean_nps-3` + avg_grp, data=mult_koyck_model_home_audio)
#Multiple R-squared:  0.996,  Adjusted R-squared:  0.995 
summary(ha_mu_koyck_model2)
vif(ha_mu_koyck_model2)


# Removing avg_grp 

ha_mu_koyck_model3<- lm(sales ~ Digital_Adstock + Spons_Adstock + Cont_Adstock + SEM_Adstock + 
                          shelfprice_mean + listing_price + perc_sale + avg_sla + `sales-3` + 
                          `mean_nps-3`, data=mult_koyck_model_home_audio)
#Multiple R-squared:  0.996,  Adjusted R-squared:  0.995
summary(ha_mu_koyck_model3)
vif(ha_mu_koyck_model3)



# Removing listing_price 

ha_mu_koyck_model4<- lm(sales ~ Digital_Adstock + Spons_Adstock + Cont_Adstock + SEM_Adstock + 
                          shelfprice_mean + perc_sale + avg_sla + `sales-3` + 
                          `mean_nps-3`, data=mult_koyck_model_home_audio)
#Multiple R-squared:  0.98, Adjusted R-squared:  0.976
summary(ha_mu_koyck_model4)
vif(ha_mu_koyck_model4)



# Removing `mean_nps-3` 

ha_mu_koyck_model5<- lm(sales ~ Digital_Adstock + Spons_Adstock + Cont_Adstock + SEM_Adstock + 
                          shelfprice_mean + perc_sale + avg_sla + `sales-3`, data=mult_koyck_model_home_audio)
#Multiple R-squared:  0.974,  Adjusted R-squared:  0.969 
summary(ha_mu_koyck_model5)
vif(ha_mu_koyck_model5)


# Removing Cont_Adstock 

ha_mu_koyck_model6<- lm(sales ~ Digital_Adstock + Spons_Adstock + SEM_Adstock + 
                          shelfprice_mean + perc_sale + avg_sla + `sales-3`, data=mult_koyck_model_home_audio)
#Multiple R-squared:  0.974,  Adjusted R-squared:  0.969 
summary(ha_mu_koyck_model6)
vif(ha_mu_koyck_model6)


# Removing shelfprice_mean 

ha_mu_koyck_model7<- lm(sales ~ Digital_Adstock + Spons_Adstock + SEM_Adstock + 
                          perc_sale + avg_sla + `sales-3`, data=mult_koyck_model_home_audio)
#Multiple R-squared:  0.974,  Adjusted R-squared:  0.97 
summary(ha_mu_koyck_model7)
vif(ha_mu_koyck_model7)


# Removing SEM_Adstock 

ha_mu_koyck_model8<- lm(sales ~ Digital_Adstock + Spons_Adstock + 
                          perc_sale + avg_sla + `sales-3`, data=mult_koyck_model_home_audio)
#Multiple R-squared:  0.973,  Adjusted R-squared:  0.97
summary(ha_mu_koyck_model8)
vif(ha_mu_koyck_model8)



# Removing perc_sale 

ha_mu_koyck_model9<- lm(sales ~ Digital_Adstock + Spons_Adstock + 
                          avg_sla + `sales-3`, data=mult_koyck_model_home_audio)
#Multiple R-squared:  0.972,  Adjusted R-squared:  0.969 
summary(ha_mu_koyck_model9)
vif(ha_mu_koyck_model9)


# Removing  `sales-3` 

ha_mu_koyck_model10<- lm(sales ~ Digital_Adstock + Spons_Adstock + 
                           avg_sla, data=mult_koyck_model_home_audio)
#Multiple R-squared:  0.97, Adjusted R-squared:  0.968  
summary(ha_mu_koyck_model10)
vif(ha_mu_koyck_model10)


# Performing 10-fold cross validation to ensure that the model is robust and has not overfit
cv.lm(data = mult_koyck_model_home_audio, form.lm = formula(sales ~ Digital_Adstock + Spons_Adstock + avg_sla), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
# Since the fitted lines of all the folds are close to each other, we can see that the model is robust. We will also compare the mean square error across models at the end to select the best model.


################################################################################################
# Calculating elasticities

# Calculating elasticity of linear equation 


model<-ha_model_4
data_model<-ha_model_data

elast_calc_linear <- function(var,data_model){
  
  e1 <-as.numeric(model$coefficients[var])
  
  return(e1)
} 

  
  elastvar_list <- list()
  for(i in 2:length(model$coefficients)){
    
    elastvar_list[i-1] <-elast_calc_linear(names(model$coefficients)[i],data_model)
    }
  
  
  elast_output <- data.frame(names(model$coefficients[2:length(model$coefficients)]))
  elast_output <- cbind(elast_output,do.call(rbind.data.frame, elastvar_list))
  colnames(elast_output) <- c("Variable","Elasticity")
  
  elast_output$direction <- ifelse(elast_output$Elasticity > 0, "Positive", "Negative")
  
  
    ggplot(data=elast_output, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
    geom_bar(position="dodge",stat="identity") + 
    coord_flip() +
    ggtitle("Home Audio Linear Model") +xlab("Variables")


# In Home Audio we see that the linear model is the best model of all since it gives maximum action items, has good adj. r square and low mse.
# We see that listing price, sponsorship and affiliate adstock give the most GMV, and hence should be invested upon
# However Digital and Content adstock should not be invested upon as it is not  helping the sales.
# The Moving Average Shelf Price inflation  suggests that the lower the price inflation the bettter for the category. This again re-emphasizes our first point that this category is price sensitive.