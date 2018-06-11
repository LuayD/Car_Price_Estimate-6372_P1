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
#pairs(~price+brand+model+yearOfRegistration+kilometer, 
#      data = dfm_AutoData,
#      main="Scatterplot Matrix w/log price",
#      lower.panel = NULL,
#      col=rgb(0,100,0,50,maxColorValue=255)
#)

# Basic Scatterplot Matrix
#pairs(~log(price)+brand+model+yearOfRegistration+kilometer, 
#      data = dfm_AutoData,
#      main="Scatterplot Matrix w/log price",
#      lower.panel = NULL,
#      col=rgb(0,100,0,50,maxColorValue=255)
#)

# Basic Scatterplot Matrix
#pairs(~log(price)+gearbox+powerPS+fuelType+notRepairedDamage, 
#      data = dfm_AutoData,
#      main="Scatterplot Matrix w/log price and others",
#      lower.panel = NULL,
#      col=rgb(0,100,0,50,maxColorValue=255)
#)

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
     main="log(price) vs km: volks golf",
     xlab="log(price)",
     ylab="km"
)


# Scenario 3, price vs. kilometer for volks golf 1999
dfm_volks_golf_1999 <- dfm_AutoData[dfm_AutoData[,"brand"] == "volkswagen" & dfm_AutoData[,"model"] == "golf" 
                                    & dfm_AutoData[,"yearOfRegistration"] == 1999,]

plot(dfm_volks_golf_1999[,"price"], dfm_volks_golf_1999[,"kilometer"],
     main="price vs km: volks golf 1999",
     xlab="price",
     ylab="km"
)

# Scenario 4, volks golf 1999 small car (kleinwagen)
dfm_volks_golf_1999_smcar <- dfm_AutoData[dfm_AutoData[,"brand"] == "volkswagen" & dfm_AutoData[,"model"] == "golf" 
                                    & dfm_AutoData[,"yearOfRegistration"] == 1999 & dfm_AutoData[,"vehicleType"] == "kleinwagen",]

plot(dfm_volks_golf_1999_smcar[,"price"], dfm_volks_golf_1999_smcar[,"kilometer"],
     main="price vs km: volks golf 99 smcar",
     xlab="price",
     ylab="km"
)

# moving 3d plot of volks golf small car vs. year vs. price
#dfm_volks_golf_smcar <- dfm_AutoData[dfm_AutoData[,"brand"] == "volkswagen" & dfm_AutoData[,"model"] == "golf" 
#                                          & dfm_AutoData[,"vehicleType"] == "kleinwagen",]

#install.packages("rgl")
#library(rgl)
#plot3d(log(dfm_volks_golf_smcar[,"price"]), dfm_volks_golf_smcar[,"kilometer"], dfm_volks_golf_smcar[,"yearOfRegistration"], col="red", size=3)

#install.packages("scatterplot3d")
#library(scatterplot3d)
#attach(mtcars)
#scatterplot3d(wt,disp,mpg, main="3D Scatterplot")
#scatterplot3d(log(dfm_volks_golf_smcar[,"price"]), dfm_volks_golf_smcar[,"kilometer"], dfm_volks_golf_smcar[,"yearOfRegistration"], 
#              main = "Log Price by Kilometer by Year")

## @knitr histogram_plots

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

#cor.test(dfm_AutoData$price,c$)
#cor.test(dfm_AutoData$price,dfm_AutoData$)
#cor.test(dfm_AutoData$price,dfm_AutoData$)
#cor.test(dfm_AutoData$price,dfm_AutoData$)
#cor.test(dfm_AutoData$price,dfm_AutoData$)
#cor.test(dfm_AutoData$price,dfm_AutoData$)
#cor.test(dfm_AutoData$price,dfm_AutoData$)






