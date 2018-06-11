##########
# Car Price Estimate
# Observtion 1 Regression Model
##########

## @knitr reg_setup
observation_plots <- function (dfm_data, lm_fit, str_fitname){
  # residual plot
  plot(lm_fit$residuals, main = c("Resdiual Plot: ", str_fitname) )
  abline(0,0)
  # hitogram
  hist(lm_fit$residuals, main = c("Hist: ", str_fitname) )
  # qqplot
  qqnorm(lm_fit$residuals, main = c("QQ-Plot: ", str_fitname) )
  qqline(lm_fit$residuals)
  # Cook's D
  cutoff <- 4/((nrow(dfm_data)-length(lm_fit$coefficients)-2))
  plot(lm_fit, which=4, cook.levels=cutoff, main = c("Cook's: ", str_fitname) )
}

## @knitr reg_richfit
# Current "Rich" model: log(price) = B0(kilometer) +  B1(brand) + B2 (model) + B3(vehicleType) + B4(yearOfRegistration) + B5(PowerPS) +B6(fuelType)
# lm to look at current model
lm_autos <- lm(log(price)~kilometer+brand+model+vehicleType+yearOfRegistration+powerPS+fuelType, data = dfm_AutoData)
sum_lm_autos <- summary(lm_autos)
#plots
observation_plots(dfm_AutoData, lm_autos, "Rich Fit")

## @knitr reg_stepwise
# Stepwise regression
lm_autos_step <- step(lm_autos, direction = "both")
sum_lm_autos_step <- summary(lm_autos_step)
# plots
# residual plot
plot(lm_autos_step$residuals, main = "Resdiuals of stepwise fit")
abline(0,0)
# hitogram
hist(lm_autos$residuals, main = "Histogram of stepwise fit")
# qqplot
qqnorm(lm_autos$residuals, main = "QQ of stepwise fit")
qqline(lm_autos$residuals)

lm_autos_for <- step(lm_autos, direction = "forward")
sum_lm_autos_for <- summary(lm_autos_for)

lm_autos_back <- step(lm_autos, direction = "backward")
sum_lm_autos_back <- summary(lm_autos_back)

## @knitr model_comparisons

dfm_model_comparisons <- data.frame()
dfm_model_comparisons <- data.frame("Original", sum_lm_autos$r.squared, sum_lm_autos$adj.r.squared, 0, stringsAsFactors=FALSE)
names(dfm_model_comparisons) <- c("name","R^2","adj-R^2","AIC")

dfm_model_comparisons <- rbind(dfm_model_comparisons, c("step", sum_lm_autos_step$r.squared, sum_lm_autos_step$adj.r.squared, lm_autos_step$anova$AIC))

