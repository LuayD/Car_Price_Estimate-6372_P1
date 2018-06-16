##########
# Car Price Estimate
# Observtion 2 Time series or 2-way ANOVA Model
##########

# Import Data File into Data Frame
autos <- read.csv("data/autos_clean.csv")

## @knitr regression
autos_cleaned <- autos[autos$fuelType != "",]
autos_cleaned <- autos_cleaned[autos$gearbox != "",]

## @knitr model_comparisons
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

# TODO: clean the graph
boxplot(price ~ gearbox, data = autos_cleaned)
# After creating a boxplot of type of gearbox, it appears that there is not an
# even distribution of the data. Just a few outliers may be affecting the results.
# So it seems important to check to see if the statistically significant result holds
# following a log transformation
autos_cleaned$logPrice <- log(autos_cleaned$price)
fit2 <- aov(logPrice ~ gearbox*fuelType, data = autos_cleaned)
summary(fit2)
# The result appears to be more pronounced, rather than less pronounced.

boxplot(logPrice ~ gearbox, data = autos_cleaned)
# Now for the fuel type:
boxplot(logPrice ~ fuelType, data = autos_cleaned)
# These distributions make it appear that there may not be a practically significant
# difference between different groups. While the median result appears to be different
# between the groups, the distributions seem to be wide, and in some cases to be 
# significantly skewed.

