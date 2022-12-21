#Asking Questions from the data

# Importing all the files and libraries
library(ggplot2) # Visualization (Matplotlib in case of python)
library(ggthemes) # Add-n with ggplot
library(dplyr) # Data manipulation (Pandas in case of python)
library(lubridate) # Date and Time
library(scales) # Graphical scaling
library(tidyr) # Tidy the data
library(tidyverse) # Tidy the data
library(readxl) # To read the data from excel file
library(DT) # Table formatted results
library(reshape2) # To change the orientation of a dataframe
library(plotrix) # To plot 3D pie charts
library(data.table) # To transpose the dataframe

# Reading the data using CSV
data <- read_excel("R Programs/Project/COE28282_Emissions2C_Emissions_Intensities2C_and_Emissions_Multipliers.xlsx")

# Visualize the data
head(data)

# Creating a structure for the data
str(data)

# Clean up data for easier data readability 

## Indicator column 
## CO2 emissions               <- Type 1
## CO2 emissions intensities   <- Type 2
## CO2 emissions multipliers   <- Type 3

data$Indicator[data$Indicator == "CO2 emissions"] <- "Type 1"
data$Indicator[data$Indicator == "CO2 emissions intensities"] <- "Type 2"
data$Indicator[data$Indicator == "CO2 emissions multipliers"] <- "Type 3"

## CTS_Code column 

data$CTS_Code[data$CTS_Code == "ECNC"] <- "Type 1"
data$CTS_Code[data$CTS_Code == "ECNI"] <- "Type 2"
data$CTS_Code[data$CTS_Code == "ECNM"] <- "Type 3"

## CTS_Name column 

data$CTS_Name[data$CTS_Name == "CO2 Emissions"] <- "Type 1"
data$CTS_Name[data$CTS_Name == "CO2 Emissions Intensities"] <- "Type 2"
data$CTS_Name[data$CTS_Name == "CO2 Emissions Multipliers"] <- "Type 3"

## Unit column
data$Unit[data$Unit == "Millions of Metric tons of CO2"] <- "1 Million"
data$Unit[data$Unit == "Metric Tons of CO2 Emissions per $1million USD of output"] <- "1 Million"

# Deleting useless columns using subset() function

pollutionData <- subset(data , select = -c(Country , ISO2 , Indicator , Unit , Industry , ObjectId , Scale , CTS_Code , CTS_Name , CTS_Full_Descriptor , ISO3))

data <- subset(data, select = -c(Scale , CTS_Code , CTS_Name , CTS_Full_Descriptor , ISO3))

# Summary of the over all data set
summary(data)
# summary(pollutionData)

# Data types
glimpse(data)

# Splitting the data frame into smaller ones based on type
splitData <- split(data , data$Indicator)

typeOneData <- splitData[["Type 1"]]
typeTwoData <- splitData[["Type 2"]]
typeThreeData <- splitData[["Type 3"]]

# Getting unique value of Countries and Industry

countries <- as.list(unique(data$Country))
industry <- as.list(unique(data$Industry))

# Getting the number of years from table
years <- data %>% select(starts_with('F'))
years <- colnames(years)
print(years)

# Get number of industries
numberOfIndustries <- length(industry)
numberOfCountries <- length(countries)

# Total stores the total pollution of each country over the years
total  <-  rowSums(pollutionData)

# Adding this value to the data dataframe
data  <-  cbind(data, total)

# Plotting the total pollution of each country over the years from different industries

plot(total , type = 'h' , main="Total pollution" , xlab = "Index" , ylab = "Million metric Ton")

# Initializing empty lists to store data
first <- list()
second <- list()
third <- list()

# Dividing the data into three list based on type

for(country in countries){
    first <- append(first , (data[which(data$Country==country & data$Indicator=='Type 1'), 31]))
    second <- append(second , (data[which(data$Country==country & data$Indicator=='Type 2'), 31]))
    third <- append(third , (data[which(data$Country==country & data$Indicator=='Type 3'), 31]))
}

# Plotting the pollution based on three types

plot(c(1:2970) , first , main = "Type 1 pollution over the years" , type='h' , xlab = "Type 1 pollution from all industries", ylab = "Million metric Tons")

plot(c(1:2970) , second , main = "Type 2 pollution over the years" , type='h' , xlab = "Type 2 pollution from all industries", ylab = "Million metric Tons")

plot(c(1:2970) , third , main = "Type 3 pollution over the years" , type='h' , xlab = "Type 3 pollution from all industries", ylab = "Million metric Tons")


newData <- data.frame(S.No = c(1:66))
newData$Countries <- countries

firstTemp <- list()
secondTemp <- list()
thirdTemp <- list()

totalSum1 <- 0
totalSum2 <- 0
totalSum3 <- 0

for(i in 1:2970){
    if(i %% 45 == 0){
        totalSum1 <- totalSum1 + first[[i]]
        firstTemp <- append(firstTemp , totalSum1)
        totalSum1 <- 0
        
        totalSum2 <- totalSum2 + second[[i]]
        secondTemp <- append(secondTemp , totalSum2)
        totalSum2 <- 0
        
        totalSum3 <- totalSum3 + third[[i]]
        thirdTemp <- append(thirdTemp , totalSum3)
        totalSum3 <- 0
    }else{
        totalSum1 <- totalSum1 + first[[i]]
        totalSum2 <- totalSum2 + second[[i]]
        totalSum3 <- totalSum3 + third[[i]]
    }
}

newData$Type_1 <- firstTemp
newData$Type_2 <- secondTemp
newData$Type_3 <- thirdTemp

print(newData)

pie(unlist(firstTemp) , radius = 1)
pie(unlist(secondTemp) , radius = 1)
pie(unlist(thirdTemp) , radius = 1)


# Plotting the types of pollution cumulative 

plot(newData$S.No , newData$Type_1 , xlab = "Country number" , ylab = "Million Metric Tons" , main = "Type 1 Pollution cummalative" , type = "o")

plot(newData$S.No , newData$Type_2 , xlab = "Country number" , ylab = "Million Metric Tons" , main = "Type 2 Pollution cummalative" , type = "o")

plot(newData$S.No , newData$Type_3 , xlab = "Country number" , ylab = "Million Metric Tons" , main = "Type 3 Pollution cummalative" , type = "o")

# Taking a linear regression of each type with another

relation12 <- lm(unlist(newData$Type_1) ~ unlist(newData$Type_2))
relation23 <- lm(unlist(newData$Type_2) ~ unlist(newData$Type_3))
relation13 <- lm(unlist(newData$Type_1) ~ unlist(newData$Type_3))

# plot(unlist(newData$Type_1) , unlist(newData$Type_2))
# plot(unlist(newData$Type_2) , unlist(newData$Type_3))
# plot(unlist(newData$Type_1) , unlist(newData$Type_3))

print(summary(relation12))
print(summary(relation23))
print(summary(relation13))

print(summary(typeOneData))
print(summary(typeTwoData))
print(summary(typeThreeData))


# To predict the total future pollution levels of India from all three types and a combination of industries

predictionData <- data.frame(S.No = c(1:66))
predictionData$Countries <- countries

new <- list()

for(item in paste0('F' , 1995:2018)){
    new <- append(new , item)
}

#---------------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F1995[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F1995[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[3] <- "F1995"

#---------------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F1996[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F1996[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[4] <- "F1996"

#---------------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F1997[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F1997[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[5] <- "F1997"

#--------------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F1998[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F1998[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[6] <- "F1998"

#-------------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F1999[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F1999[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[7] <- "F1999"

#------------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2000[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2000[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[8] <- "F2000"

#-----------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2001[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2001[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[9] <- "F2001"

#---------------------------------------------------------

temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2002[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2002[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[10] <- "F2002"

#---------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2003[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2003[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[11] <- "F2003"

#---------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2004[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2004[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[12] <- "F2004"

#----------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2005[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2005[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[13] <- "F2005"

#---------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2006[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2006[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[14] <- "F2006"

#---------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2007[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2007[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[15] <- "F2007"

#---------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2008[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2008[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[16] <- "F2008"

#---------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2009[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2009[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[17] <- "F2009"

#----------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2010[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2010[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[18] <- "F2010"

#----------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2011[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2011[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[19] <- "F2011"

#----------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2012[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2012[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[20] <- "F2012"

#----------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2013[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2013[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[21] <- "F2013"

#---------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2014[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2014[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[22] <- "F2014"

#----------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2015[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2015[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[23] <- "F2015"

#----------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2016[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2016[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[24] <- "F2016"

#----------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2017[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2017[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[25] <- "F2017"

#----------------------------------------------------------
temp <- list()
total <- 0

for(i in 1:8910){
    if(i %% 135 == 0){
        total <- total + pollutionData$F2018[[i]]
        temp <- append(temp , total)
        total <- 0
    }else{
        total <- total + pollutionData$F2018[[i]]
    }
}

predictionData <- cbind(predictionData , unlist(temp))
colnames(predictionData)[26] <- "F2018"

# The data frame used to predict the future pollution of each country
print(predictionData)

summary(predictionData)

print(summary(predictionData$F1995))
print(summary(predictionData$F2018))

# Plotting all the plots for each country

for(i in 1:66){

    temp <- list()
    
    temp <- append(temp , predictionData[i,])
    
    temp[c(1,2)] <- NULL
    
    years <- as.list(years)
    
    # par(mfrow=c(11,6))
    
    plot(c(1995:2018) , temp , main = countries[[i]] , xlab = "Years" , ylab = "Metric ton pollution of Co2" , type="l")
    # axis(1, at = seq(1995, 2018, by = 1), las=2)

}

pieTemp <- list()
    
pieTemp <- append(pieTemp , predictionData[26,])
    
pieTemp[c(1,2)] <- NULL
    
years <- as.list(years)

      
# pie(unlist(pieTemp) , label = years , main = countries[[26]] , col= rainbow(length((years))) , radius = 0.75 )

pie(unlist(pieTemp) , label = years , main = countries[[26]] , col=gray(seq(0.4, 1.0, length = length(pieTemp))) , radius = 0.75 )

legend("bottom", unlist(years), cex = 0.4,
       fill = gray(seq(0.4, 1.0, length = length(pieTemp))) , ncol = 11)

# pie(unlist(pieTemp) , label = years , main = countries[[22]] , col= rainbow(length((years))) , radius = 0.75 )
# 
# 
# legend("bottom", unlist(years), cex = 0.4,
#        fill = rainbow(length(years)) , ncol = 11)
# 
# pie(unlist(pieTemp) , label = years , main = countries[[65]] , col= rainbow(length((years))) , radius = 0.75 )
# 
# 
# legend("bottom", unlist(years), cex = 0.4,
#        fill = rainbow(length(years)) , ncol = 11)



# Perform linear regression to India to predict future pollution

regress <- lm(unlist(pieTemp) ~ c(1:24))

print(summary(regress))

plot(c(1:24) , unlist(pieTemp) , xlab = "Years" , ylab = "Pollution in million metric ton" , main = "Regression line for Pollution v/s Years" , pch = 16, cex = 1, col = "blue")

abline(regress)

# To use ANOVA to find relation between three countries from three different continents

predictionDataTranspose <- t(predictionData)

predictionDataTranspose <- predictionDataTranspose[-c(1,2),]

colnames(predictionDataTranspose) <- unlist(countries)

predictionDataTranspose <- data.frame(predictionDataTranspose)


# Clean up useless variables

# rm(numberOfCountries , first , firstTemp , new , pieTemp , second , secondTemp , temp , third , thirdTemp , total , totalSum1 , totalSum2 , totalSum3 , i , country , item , numberOfIndustries)

hist(unlist(predictionDataTranspose$India) , main = "India" , xlab = "Pollution in Million metric Ton")




