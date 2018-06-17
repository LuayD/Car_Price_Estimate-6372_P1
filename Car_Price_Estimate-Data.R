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

<<<<<<< HEAD
par(mfrow=c(2,3))

=======
>>>>>>> 4d82c1b6e57e75622bfa7ab670f1cf9a2ed7388b
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
<<<<<<< HEAD
par(mfrow=c(1,1))
=======
par(mfrow=c(1,1))
## @knitr correlation_data
# correlation

length(dfm_AutoData)

names(dfm_AutoData)

dfm_correlation = data.frame();

int_i = 7
for (temp in dfm_AutoData) {
  int_i <- int_i + 1
  print(names(dfm_AutoData[int_i]))
  tryCatch(
  dfm_correlation[int_1] <- cor.test(dfm_AutoData[,"price"], dfm_AutoData[,int_t]),
  error = function(e) e
#  finally = print("error")
  )

}
print(int_i)

dfm_correlation = data.frame();
mtx_correlation = matrix();

xyz <- lapply(dfm_AutoData, cor.test)

cor.mat(dfm_AutoData)

x <- cor.test(dfm_AutoData$price,dfm_AutoData$kilometer)
>>>>>>> 4d82c1b6e57e75622bfa7ab670f1cf9a2ed7388b
