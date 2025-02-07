library("ggplot2")
library("readr")

NY_House_Dataset <- read_csv("C:\\Users\\kiernm\\Downloads\\NY-House-Dataset.csv")

#Fit 3 linear models with Price as the response variable and combinations of
#PropertySqFt, Beds, and Bath as predictors

dataset <- NY_House_Dataset

#filter data

dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset$PROPERTYSQFT[dataset$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

names(dataset)

#linear models

#sqft filter
dataset <- dataset[dataset$PROPERTYSQFT<40000]

#price vs. sqft
lmod <- lm(PRICE~PROPERTYSQFT, data = dataset)
lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
summary(lmod)

#print price vs. sqft plot
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod)

#adjusted log model 
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod)

#price vs. beds
lmod <- lm(PRICE~BEDs, data = dataset)
lmod <- lm(log10(PRICE)~log10(BEDS), data = dataset)
summary(lmod)

#print price vs. beds plot
plot(PRICE~BEDS, data = dataset)
abline(lmod)

#adjusted log model
plot(log10(PRICE)~log10(BEDS), data = dataset)
abline(lmod)

#price vs. sqft and baths
lmod <- lm(PRICE~PROPERTYSQFT+BATH, data = dataset)
lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT+BATH), data = dataset)
summary(lmod)

#print price vs. sqft and baths plot
plot(PRICE~PROPERTYSQFT+BATH, data = dataset)
abline(lmod)

#adjusted log model
plot(log10(PRICE)~log10(PROPERTYSQFT+BATH), data = dataset)
abline(lmod)

