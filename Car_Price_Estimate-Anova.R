##########
# Car Price Estimate
# Observtion 2 Time series or 2-way ANOVA Model
##########

# Import Data File into Data Frame
dfm_AutoData <- read.csv("data/autos_clean.csv")

## @knitr anova_comparisons
autos_cleaned <- dfm_AutoData[dfm_AutoData$fuelType != "",]
autos_cleaned <- autos_cleaned[autos_cleaned$gearbox != "",]

# We are interested in knowing if there is a difference between groups
# where there is a different gearbox (manual or automatic) and whether
# the the fuel type makes a difference when it comes to the .
# There are seeral
fit <- aov(price ~ gearbox*fuelType, data = autos_cleaned)
summary(fit)
# The results indicates that there is indeed a significant difference (p < 0.05)
# between each group, especially based on type of gear box. Surprisingly, price 
# for fuel type sees a significant difference (p < 0.05) but has a less pronounced 
# F value. The question is what this might mean. 

## @knitr anova_boxplots
par(las=2) # make y-axis & axis orientation change
boxplot(price ~ gearbox, data = autos_cleaned, main = "Price vs. Gearbox type")
# After creating a boxplot of type of gearbox, it appears that there is not an
# even distribution of the data. Just a few outliers may be affecting the results.
# So it seems important to check to see if the statistically significant result holds
# following a log transformation

## @knitr anova_log
autos_cleaned$logPrice <- log(autos_cleaned$price)
fit2 <- aov(logPrice ~ gearbox*fuelType, data = autos_cleaned)
summary(fit2)
# The result appears to be more pronounced, rather than less pronounced.

## @knitr anova_log_gearbox
boxplot(logPrice ~ gearbox, data = autos_cleaned, main = "log Price vs. Gearbox type",
        col=rainbow(2))

## @knitr anova_log_fueltype
# Now for the fuel type:
boxplot(logPrice ~ fuelType, data = autos_cleaned, main = "log Price vs. Fuel Type",
        col=rainbow(7))
## @knitr anova_log_brand
fitBrand <- aov(logPrice ~ brand*vehicleType, data = autos_cleaned)
summary(fitBrand)

## @knitr anova_box_brand
par(las=2)
boxplot(logPrice ~ brand, data = autos_cleaned, main = "log Price vs. Brand",
        col=rainbow(40))

boxplot(logPrice ~ vehicleType, data = autos_cleaned, main = "log Price vs. Vehicle Type", col=rainbow(9))

