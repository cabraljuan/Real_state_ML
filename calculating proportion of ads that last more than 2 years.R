###########################################################################
###########################################################################
###                                                                     ###
###      CALCULATING PROPORTION OF ADS THAT LAST MORE THAN 2 YEARS      ###
###                                                                     ###
###########################################################################
###########################################################################


# Remove variables
rm(list=ls())

# Paths:
# fuente is where input and output folders should be
fuente<-"G:/MELI/"
setwd(fuente)



#Open:
library(beepr) # To make sounds
library(bannerCommenter) # To write banners: banner("Loading all datasets", centre = TRUE, bandChar = "-")
# banner("Section 3:", "LOADING AND MERGING DATA FROM BS AS", emph = TRUE)
library(ggplot2) # Graphs
library(gridExtra)
library(grid)
library(readxl) # read excels
library(expss)
library("writexl") # write excels
library(scales) 
library(plyr)
library(dplyr) # pipes like %>%
library(sf)
library(raster)
library(rgdal)
library(shape)
library(data.table) # More efficient than dataframes
library(lubridate) # Dates manipulating
library(DescTools)
library(zoo) # for rollsum
library(testthat) # Testing code
library(stringr) # extract from strings


# We are only interested in some inmuebles not all
INMUEBLES <- c("Departamento","Casa","Local","Oficina")


# All data is provided for AMBA, now we need to restrict the sample because the data below is from 
# argentina not only AMBA
data1 <- read.csv(paste0(fuente,"input/REALESTATE_MLA_202007.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
table(data1$ITE_ADD_STATE_NAME)
# For july and August we have data from Argentina, we need to restrict to AMBA
data1 <- data1[data1$TIPOPROPIEDADNORM %in% INMUEBLES,]
data1 <- data1[data1$ITE_ADD_STATE_NAME %in% c("Bs.As. G.B.A. Norte","Bs.As. G.B.A. Oeste","Bs.As. G.B.A. Sur","Capital Federal"),]
write.csv(data1,"input/REALESTATE_MLA_AMBA_202007.csv",row.names=FALSE)


data1 <- read.csv("input/REALESTATE_MLA_202008.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
table(data1$ITE_ADD_STATE_NAME)
data1 <- data1[data1$TIPOPROPIEDADNORM %in% INMUEBLES,]
data1 <- data1[data1$ITE_ADD_STATE_NAME %in% c("Bs.As. G.B.A. Norte","Bs.As. G.B.A. Oeste","Bs.As. G.B.A. Sur","Capital Federal"),]
write.csv(data1,"input/REALESTATE_MLA_AMBA_202008.csv",row.names=FALSE)


# Calculating proportions

percentagetable<-data.frame()
months<-c("01","02","03","04","05","06","07","08","09","10","11","12")
years<-c("2017","2018","2019")
for (year in years){ 
  for (month in months) {
    # Load data
    data <- read.csv(paste0("input/REALESTATE_MLA_AMBA_",year,month,".csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
    data <- data[data$TIPOPROPIEDADNORM %in% INMUEBLES,]
    # Two years after
    year2<-as.numeric(year)+2
    twoyearsafter<-paste0(year2,"-",month,"-01")
    # Percentage finishing two years after
    percentage<-nrow(data[as.Date(data$ITE_AUCTION_STOP)>as.Date(twoyearsafter),])/nrow(data)
    # Creating dataframe
    percentage<-c(percentage)
    percentagetable<-rbind(percentagetable,percentage,make.row.names=FALSE)
    names(percentagetable)<-c("Percentage")
  }
}
percentagetable


# For 2020

percentagetable2<-data.frame()
months<-c("01","02","03","04","05","06","07","08","09","10")
years<-c("2020")
for (year in years){ 
  for (month in months) {
    # Load data
    data <- read.csv(paste0("input/REALESTATE_MLA_AMBA_",year,month,".csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
    data <- data[data$TIPOPROPIEDADNORM %in% INMUEBLES,]
     # Two years after
    year2<-as.numeric(year)+2
    twoyearsafter<-paste0(year2,"-",month,"-01")
    # Percentage finishing two years after
    percentage<-nrow(data[as.Date(data$ITE_AUCTION_STOP)>as.Date(twoyearsafter),])/nrow(data)
    # Creating dataframe
    percentage<-c(percentage)
    percentagetable2<-rbind(percentagetable2,percentage,make.row.names=FALSE)
    names(percentagetable2)<-c("Percentage")
  }
}
percentagetable2


# Combining
percentagetable<-rbind(percentagetable,percentagetable2)


months<-c("01","02","03","04","05","06","07","08","09","10","11","12")
years<-c("2017","2018","2019","2020")
date<-c()
for (year in years){ 
  for (month in months) {
    date1<-paste0(year,"-",month)
    date<-c(date,date1)
  }
}

date<-date[1:nrow(percentagetable)]

percentagetable<-cbind(percentagetable,date)

# Percentage
percentagetable$Percentage<-percentagetable$Percentage*100


# Checking
percentagetable
