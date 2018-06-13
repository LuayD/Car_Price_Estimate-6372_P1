##########
# Car Price Estimate
# Observtion 1 Regression Model
##########

## @knitr reg_setup

# Initialize Libraries
require("leaps")
library(leaps)

# functions
fun_observation_plots <- function (dfm_data, lm_fit, str_fitname){
  par(mfrow=c(2,2))
  # residual plot
  plot(lm_fit$residuals, main = c("Resdiual Plot: ", str_fitname) )
  abline(0,0)
  # histogram
  hist(lm_fit$residuals, main = c("Hist: ", str_fitname) )
  # qqplot
  qqnorm(lm_fit$residuals, main = c("QQ-Plot: ", str_fitname) )
  qqline(lm_fit$residuals)
  # Cook's D
  cutoff <- 4/((nrow(dfm_data)-length(lm_fit$coefficients)-2))
  plot(lm_fit, which=4, cook.levels=cutoff, main = c("Cook's: ", str_fitname))
  par(mfrow=c(1,1))
}

fun_do_predictions <- function (name, lm_fitmodel){
  
  sum_lm_fitmodel <- summary(lm_fitmodel)
  
  # do the prediction against the training dfm
  price_predictions1 <- (predict(lm_fitmodel,dfm_train))
  dfm_train2 <- dfm_train
  dfm_train2$log_price <- log(dfm_train2$price)
  dfm_train2$log_price_exp <- exp(dfm_train2$log_price)
  dfm_train2$log_pred_price <- price_predictions1
  dfm_train2$pred_price <- exp(price_predictions1)
  
  # do the prediction against the testing dfm
  dfm_test2 <- dfm_test;
  price_predictions2 <- (predict(lm_fitmodel,dfm_test2))
  dfm_test2$log_price <- log(dfm_test2$price)
  dfm_test2$log_price_exp <- exp(dfm_test2$log_price)
  dfm_test2$log_pred_price <- price_predictions2
  dfm_test2$pred_price <- exp(price_predictions2)
  
  testMSE_lm_fitmodel_train<-mean((log(dfm_train$price)-(price_predictions1))^2)
  testMSE_lm_fitmodel_test<-mean((log(dfm_test$price)-(price_predictions2))^2)
  
  # Add to test description file
  dfm_model_comparisons_local <-data.frame(name, sum_lm_fitmodel$r.squared, sum_lm_fitmodel$adj.r.squared,
                                           AIC(lm_fitmodel), testMSE_lm_fitmodel_train, 
                                           testMSE_lm_fitmodel_test, stringsAsFactors = FALSE)
  names(dfm_model_comparisons_local) <- c("name","R^2","adj-R^2","AIC", "TestMSE_train", "TestMSE_test")
  return(dfm_model_comparisons_local)
}

predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

# Create comparisons table to print out later
dfm_model_comparisons <- data.frame(0, 0, 0, 0, 0, 0)
names(dfm_model_comparisons) <- c("name","R^2","adj-R^2","AIC", "TestMSE_train", "TestMSE_test")
dfm_model_comparisons <- dfm_model_comparisons[-1,]

# split the data frame into test and train objects
set.seed(1)
vec_index<-sample(1:nrow(dfm_AutoData),nrow(dfm_AutoData)/2)
dfm_train<-dfm_AutoData[vec_index,]
dfm_test<-dfm_AutoData[-vec_index,]

## @knitr reg_simplefits

lm_simplemodel <- lm(log(price)~kilometer, data = dfm_train)
dfm_model_comparisons <- rbind(dfm_model_comparisons, fun_do_predictions("l_price~kilometer", lm_simplemodel) )

lm_simplemodel <- lm(log(price)~powerPS, data = dfm_train)
dfm_model_comparisons <- rbind(dfm_model_comparisons, fun_do_predictions("l_price~powerPS", lm_simplemodel) )

lm_simplemodel <- lm(log(price)~yearOfRegistration, data = dfm_train)
dfm_model_comparisons <- rbind(dfm_model_comparisons, fun_do_predictions("l_price~year", lm_simplemodel) )

lm_categories_model <- lm(log(price)~kilometer+yearOfRegistration+powerPS, data = dfm_train)
dfm_model_comparisons <- rbind(dfm_model_comparisons, fun_do_predictions("combined above:", lm_categories_model) )

## @knitr reg_simplefits_plots
par(mfrow=c(1,3))
plot(log(dfm_AutoData$price), dfm_AutoData$kilometer, main="log(price) vs. KM", xlab = "log(price)", ylab = "KM")
plot(log(dfm_AutoData$price), dfm_AutoData$powerPS, main="log(price) vs. Power", xlab = "log(price)", ylab = "PS")
plot(log(dfm_AutoData$price), dfm_AutoData$yearOfRegistration, main="log(price) vs. Year Reg", xlab = "log(price)", ylab = "Year")
par(mfrow=c(1,3))

## @knitr reg_richfit

lm_fullmodel <- lm(log(price)~kilometer+brand+model+vehicleType+yearOfRegistration+powerPS+fuelType, data = dfm_train)

dfm_model_comparisons <- rbind(dfm_model_comparisons, fun_do_predictions("Full Model", lm_fullmodel) )
#fun_observation_plots(dfm_AutoData, lm_fullmodel, "Full Model")

## @knitr reg_stepwise

# Stepwise regression on rich model
lm_fullmodel_step <- step(lm_fullmodel, direction = "both")
dfm_model_comparisons <- rbind(dfm_model_comparisons, fun_do_predictions("Step on Full Model", lm_fullmodel_step) )

## @knitr reg_richfit_lasso

# Lasso regression on rich model

#LASSO call requires us to format the data set a little 
x1 <- model.matrix(log(price)~kilometer+brand+model+vehicleType+yearOfRegistration+powerPS+fuelType,dfm_train)[,-1]
y1 <- dfm_train$price
#Remind them about categorical predictor logistics

xtest1=model.matrix(log(price)~kilometer+brand+model+vehicleType+yearOfRegistration+powerPS+fuelType,dfm_test)[,-1]
ytest1=dfm_test$price

cv.out=cv.glmnet(x1,y1,alpha=1,type.measure = "class") #alpha=1 performs LASSO
plot(cv.out)
bestlambda<-cv.out$lambda.1se
#Refit full data set
lasso.mod<-glmnet(x1,y1,alpha=1,lambda=bestlambda, type.measure = "class")

lasso.pred=predict (lasso.mod ,s=bestlambda ,newx=xtest1)

testMSE_LASSO<-mean((ytest-lasso.pred)^2)
testMSE_LASSO

## @knitr reg_richfit_mlrfor

lm_fullmodel_mlr_for <- regsubsets(log(price)~kilometer+brand+model+vehicleType+yearOfRegistration+powerPS+fuelType,
                                             data=dfm_AutoData, method="forward")

sum_model <- summary(lm_fullmodel_mlr_for)
#coef(lm_fullmodel_mlr_for,8)

#Calculate test MSE
testMSE<-c()
#note my index is to 8 since that what I set it in regsubsets
for (i in 1:8){
  predictions<-predict.regsubsets(object=lm_fullmodel_mlr_for,newdata=dfm_test,id=i) 
  testMSE[i]<-mean((log(dfm_test$price)-predictions)^2)
}
#par(mfrow=c(1,1))
#plot(1:8,testMSE,type="l",xlab="# of predictors",ylab="test MSE")
index<-which(testMSE==min(testMSE))
#points(index,testMSE[index],col="red",pch=10)

dfm_model_comparisons <- rbind(dfm_model_comparisons, c("Full Model MLRFOR", sum_model$rsq[length(sum_model$rsq)], sum_model$adjr2[length(sum_model$adjr2)],
                               "N/A", "N/A", testMSE[index])
                               , stringsAsFactors = FALSE)
  
## @knitr reg_complexfit

lm_complexmodel <- lm(log(price)~kilometer + brand + model + vehicleType + yearOfRegistration + powerPS + fuelType +
                   kilometer:brand + kilometer:model + kilometer:vehicleType + kilometer:yearOfRegistration +
                   yearOfRegistration:vehicleType ,
                 data = dfm_train)

dfm_model_comparisons <- rbind(dfm_model_comparisons, fun_do_predictions("Complex Model", lm_complexmodel) )
#fun_observation_plots(dfm_AutoData, lm_complexmodel, "Complex Model")

## @knitr reg_complex_stepwise
# Stepwise regression on full model
lm_complexmodel_step <- step(lm_complexmodel, direction = "both")
dfm_model_comparisons <- rbind(dfm_model_comparisons, fun_do_predictions("Step on Complex Model", lm_complexmodel_step) )




#########################################################

## @knitr OUT

# Current "Rich" model 2: log(price) = B0(kilometer) +  B1(brand) + B2 (model) + B3(vehicleType) + B4(yearOfRegistration) + B5(PowerPS) +B6(fuelType) + 
#   kilometer*model
# lm to look at current model
lm_autos_2 <- lm(log(price)~kilometer + brand + model + vehicleType + yearOfRegistration + powerPS + fuelType +
                   kilometer:brand + kilometer:model + kilometer:vehicleType + kilometer:yearOfRegistration +
                   yearOfRegistration:vehicleType ,
                 data = dfm_AutoData)
sum_lm_autos_2 <- summary(lm_autos_2)
#plots
observation_plots(dfm_AutoData, lm_autos_2, "Rich Fit 2")
#Add to model comparison
dfm_model_comparisons <- rbind(dfm_model_comparisons, c("rich fit 2", sum_lm_autos_2$r.squared, sum_lm_autos_2$adj.r.squared, AIC(lm_autos_2)))

################

## @knitr reg_stepwise2
# Forward regression
lm_autos_forward_2 <- step(lm_autos_2, direction = "forward")
sum_lm_autos_forward_2 <- summary(lm_autos_forward_2)
# plots
observation_plots(dfm_AutoData, lm_autos_forward_2, "Forward Fit 2")
# Add to model comparison
dfm_model_comparisons <- rbind(dfm_model_comparisons, c("Forward Fit 2", sum_lm_autos_forward_2$r.squared, sum_lm_autos_forward_2$adj.r.squared, AIC(lm_autos_forward_2)))


################

## @knitr reg_MLR_FWD
require("leaps")
library(leaps)

# Forward Regression alt
#reg.fwd=regsubsets(log(mpg)~cylinders+displacement + horsepower + weight + acceleration+origin,data=Auto,method="forward",nvmax=6)
lm_autos_for_alt <- regsubsets(log(price)~kilometer+brand+model+vehicleType+yearOfRegistration+powerPS+fuelType, 
                               data = dfm_AutoData, method="forward")
sum_lm_autos_for_alt <- summary(lm_autos_for_alt)
# plots
observation_plots(dfm_AutoData, lm_autos_for_alt, "Forward Fit")
#Add to model comparison
dfm_model_comparisons <- rbind(dfm_model_comparisons, c("forward_alt", 
                                                        sum_lm_autos_for_alt$rsq[length(sum_lm_autos_for_alt$rsq)], 
                                                        sum_lm_autos_for_alt$adjr2[length(sum_lm_autos_for_alt$adjr2)], 
                                                        NULL))







####################################################
