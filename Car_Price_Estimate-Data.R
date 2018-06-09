##########
# Car Price Estimate
# Data Import, clean, and manipulate File
##########

# Import Data File into Data Frame

## @knitr dataimport

# Read in file
str_fileName <- "data/autos_clean.csv"
dfm_AutoData <- read.csv(str_fileName)

dfm_AutoData$vehicleType <- as.character(dfm_AutoData$vehicleType)

# Exploratory Data

## @knitr explore_data_plots

# Basic Scatterplot Matrix
pairs(~log(price)+yearOfRegistration+kilometer, 
      data = dfm_AutoData,
      main="Simple Scatterplot Matrix"
)

# Basic Scatterplot Matrix
plot(dfm_AutoData$price, dfm_AutoData$vehicleType, 
      data = dfm_AutoData,
      main="log(price) vs. vehicleType"
)

scatter(~log(price)+yearOfRegistration+kilometer, 
      data = dfm_AutoData,
      main="Simple Scatterplot Matrix"
)

# Histogram of Price
hist(dfm_AutoData$price, 
     main = "Histogram of Price",
     xlab = "Price"
)

# Histogram of log(Price)
hist(log(dfm_AutoData$price), 
     main = "Histogram of Price (log natural)",
     xlab = "log_Price"
     )

# Histogram of Price
hist(dfm_AutoData$yearOfRegistration, 
     main = "Histogram of year of registration",
     xlab = "Year"
)

# Histogram of log(Price)
hist(log(dfm_AutoData$yearOfRegistration), 
     main = "Histogram of year of registration (log natural)",
     xlab = "log_Year"
)

# Histogram of KM
hist(dfm_AutoData$kilometer, 
     main = "Histogram of KM",
     xlab = "KM"
)






