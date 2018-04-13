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