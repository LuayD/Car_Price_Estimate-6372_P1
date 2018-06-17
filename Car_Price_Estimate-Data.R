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

par(mfrow=c(2,3))

plot(log(dfm_AutoData$price), dfm_AutoData$kilometer, 
      main="log(price) vs. km",
      xlab="log(price)",
      ylab="km"
)

# Break down of price for Volkswagon golf

# Scenario 1, price vs. kilometer for volks 
dfm_volks <- dfm_AutoData[dfm_AutoData[,"brand"] == "volkswagen",]

plot(log(dfm_volks[,"price"]), dfm_volks[,"kilometer"],
     main="log(price) vs km: volks",
     xlab="log(price)",
     ylab="km"
)

# Scenario 2, price vs. kilometer for volks golf 
dfm_volks_golf <- dfm_AutoData[dfm_AutoData[,"brand"] == "volkswagen" & dfm_AutoData[,"model"] == "golf",]

plot(log(dfm_volks_golf[,"price"]), dfm_volks_golf[,"kilometer"],
     main=c("log(price) vs km:", "volks golf"),
     xlab="log(price)",
     ylab="km"
)


# Scenario 3, price vs. kilometer for volks golf 1999
dfm_volks_golf_1999 <- dfm_AutoData[dfm_AutoData[,"brand"] == "volkswagen" & dfm_AutoData[,"model"] == "golf" 
                                    & dfm_AutoData[,"yearOfRegistration"] == 1999,]

plot(dfm_volks_golf_1999[,"price"], dfm_volks_golf_1999[,"kilometer"],
     main=c("price vs km:", "volks golf 1999"),
     xlab="price",
     ylab="km"
)

# Scenario 4, volks golf 1999 small car (kleinwagen)
dfm_volks_golf_1999_smcar <- dfm_AutoData[dfm_AutoData[,"brand"] == "volkswagen" & dfm_AutoData[,"model"] == "golf" 
                                    & dfm_AutoData[,"yearOfRegistration"] == 1999 & dfm_AutoData[,"vehicleType"] == "kleinwagen",]

plot(dfm_volks_golf_1999_smcar[,"price"], dfm_volks_golf_1999_smcar[,"kilometer"],
     main= c("price/km:", "volks golf 99 smcar"),
     xlab="price",
     ylab="km"
)
par(mfrow=c(1,1))

## @knitr histogram_plots
par(mfrow=c(2,3))
# Histogram of Price
hist(dfm_AutoData$price, 
     main = "Hist: Price",
     xlab = "Price"
)

# Histogram of log(Price)
hist(log(dfm_AutoData$price), 
     main = "Hist: log(Price)",
     xlab = "log_Price"
     )

# Histogram of Year
hist(dfm_AutoData$yearOfRegistration, 
     main = "Hist: YearOfReg",
     xlab = "Year"
)

# Histogram of log(Year)
hist(log(dfm_AutoData$yearOfRegistration), 
     main = "Hist: log(YearOfReg)",
     xlab = "log_Year"
)

# Histogram of KM
hist(dfm_AutoData$kilometer, 
     main = "Hist: KM",
     xlab = "KM"
)
par(mfrow=c(1,1))