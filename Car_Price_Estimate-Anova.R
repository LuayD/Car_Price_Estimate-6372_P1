##########
# Car Price Estimate
# Observtion 2 Time series or 2-way ANOVA Model
##########

# Import Data File into Data Frame
# dfm_AutoData <- read.csv("data/autos_clean.csv")
## @knitr anova_comparisons
autos_cleaned <- dfm_AutoData[dfm_AutoData$fuelType != "",]
autos_cleaned <- autos_cleaned[autos_cleaned$gearbox != "",]
fit <- aov(price ~ gearbox*fuelType, data = autos_cleaned)
summary(fit)
  
## @knitr anova_boxplots
par(las=2) # make y-axis & axis orientation change
boxplot(price ~ gearbox, data = autos_cleaned, main = "Price vs. Gearbox type")

## @knitr anova_log
autos_cleaned$logPrice <- log(autos_cleaned$price)
fit2 <- aov(logPrice ~ gearbox*fuelType, data = autos_cleaned)
summary(fit2)

## @knitr anova_log_gearbox
boxplot(logPrice ~ gearbox, data = autos_cleaned, main = "log Price vs. Gearbox type",
        col=rainbow(3))

## @knitr anova_log_fueltype
# Now for the fuel type:
boxplot(logPrice ~ fuelType, data = autos_cleaned, main = "log Price vs. Fuel Type",
        col=rainbow(7))
fitBrand <- aov(logPrice ~ brand*fuelType, data = autos_cleaned)
summary(fitBrand)
par(las=2)
boxplot(logPrice ~ brand, data = autos_cleaned, main = "log Price vs. Brand",
        col=rainbow(40))

boxplot(logPrice ~ vehicleType, data = autos_cleaned, main = "log Price vs. Vehicle Type", col=rainbow(9))

library(tables)
