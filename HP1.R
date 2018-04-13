#=========================Set Working Directory=======================================
setwd("C:\\Users\\ROS\\Documents\\R Tutorial Data\\R Tutorial Data Sets\\ROSDA\\M3")
#=================== Loading libraries====================================
library(tidyverse) 
library(chron)
library(magrittr)
library(openair)
library(hydroTSM)
library(lubridate)
library(cowplot)
library(reshape2)
library(openair)
library(forecast)
#====================Data import==========================================
mydata <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, fill = T,
                     na.strings = c("?"))
mydata1 <- mydata %>% 
  rename(kitchen = Sub_metering_1,laundry=Sub_metering_2,heating=Sub_metering_3) %>%
  mutate(Global_active_power = Global_active_power *100/6 ,
         Global_reactive_power = Global_reactive_power *100/6) %>% drop_na()
mydata1$DateTime = with(mydata1, dmy(Date) + hms(Time)) 
mydata1$DateTime <- as.POSIXct(mydata1$DateTime, tz ="GMT")
mydata1 <- mydata1 %>% mutate(Res= Global_active_power - (kitchen + laundry + heating))
mydata1  <- mydata1 %>% select(c(10,3:11))

#====================functions==========================================
time2season <- function(x, out.fmt="months", type="default") {
  
  # Checking that 'class(x)==Date'
  if ( ( !( class(x) %in% c("Date", "POSIXct", "POSIXt") ) ) && TRUE )
    stop("Invalid argument: 'x' must be of class 'Date'")
  
  # Checking the class of out.fmt
  if (is.na(match(out.fmt, c("seasons", "months") ) ) )
    stop("Invalid argument: 'out.fmt' must be in c('seasons', 'months')")
  
  # Checking that the user provied a valid value for 'type'   
  valid.types <- c("default", "FrenchPolynesia")    
  if (length(which(!is.na(match(type, valid.types )))) <= 0)  
    stop("Invalid argument: 'type' must be in c('default', 'FrenchPolynesia')")
  
  ####################
  months <- format(x, "%m")
  
  if (type=="default") {
    winter <- which( months %in% c("12", "01", "02") )
    spring <- which( months %in% c("03", "04", "05") )
    summer <- which( months %in% c("06", "07", "08") )
    autumm <- which( months %in% c("09", "10", "11") ) 
  } else if (type=="FrenchPolynesia") {
    winter <- which( months %in% c("12", "01", "02", "03") )
    spring <- which( months %in% c("04", "05") )
    summer <- which( months %in% c("06", "07", "08", "09") )
    autumm <- which( months %in% c("10", "11") ) 
  } # ELSE end
  
  # Creation of the output, with the same length of the 'x' input
  seasons <- rep(NA, length(x))
  
  if (out.fmt == "seasons") {
    
    seasons[winter] <- "winter"
    seasons[spring] <- "spring"
    seasons[summer] <- "summer"
    seasons[autumm] <- "autumm"
    
  } else { # out.fmt == "months"
    
    if (type=="default") {
      seasons[winter] <- "DJF"
      seasons[spring] <- "MAM"
      seasons[summer] <- "JJA"
      seasons[autumm] <- "SON"
    } else  if (type=="FrenchPolynesia") {
      seasons[winter] <- "DJFM"
      seasons[spring] <- "AM"
      seasons[summer] <- "JJAS"
      seasons[autumm] <- "ON"
    } # IF end
    
  } # IF end
  
  return(seasons)
  
}
Islice <- function(DF,date1,date2){
  int <- interval(date1, date2)
  Dummy = DF[DF$DateTime %within% int,]
  return(Dummy) 
}
#====================Data clustering==========================================
mydata1hour <- mydata1 %>%aggregate(by =list(hour(mydata1$DateTime),date(mydata1$DateTime)), FUN = mean)
mydata1date <- mydata1 %>% aggregate(by =list(date(mydata1$DateTime)), FUN = mean) 
mydata1date$day <- weekdays(as.Date(mydata1date$DateTime))
mydata1date$season <- time2season(mydata1date$DateTime, out.fmt = "seasons", type="default")
mydata1Season <- mydata1date %>% aggregate(by = list( mydata1date$season, year( mydata1date$DateTime)), FUN = mean)
mydata1week <- mydata1 %>% aggregate(by = list(week(mydata1$DateTime), year(mydata1$DateTime)), FUN = mean)
mydata1year <- mydata1 %>% aggregate(by =list(year(mydata1$DateTime)), FUN = mean)
mydata1month<- mydata1 %>% aggregate(by = list(month(mydata1$DateTime), year(mydata1$DateTime)), FUN = mean)
mydata15<- mydata1 %>%   rename(date = DateTime )
mydata15 <- timeAverage(mydata15 , avg.time = "15 min", fill = TRUE,statistic = "mean")