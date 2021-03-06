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

#====================Data filter==========================================
mydata1wend <- mydata1date  %>% filter(mydata1date$day=="Saturday" | mydata1date$day=="Sunday" & year(mydata1date$DateTime)==2006)
mydata1wday <- mydata1date  %>% filter(mydata1date$day!="Saturday" & mydata1date$day!="Sunday")
Aug2007 <- mydata1date %>% filter(year(mydata1date$DateTime)==2007,month(mydata1date$DateTime)==08)
Aug2008 <- mydata1date %>% filter(year(mydata1date$DateTime)==2008,month(mydata1date$DateTime)==08)
Aug2009 <- mydata1date %>% filter(year(mydata1date$DateTime)==2009,month(mydata1date$DateTime)==08)
Aug2010 <- mydata1date %>% filter(year(mydata1date$DateTime)==2010,month(mydata1date$DateTime)==08)
julyaug2010 <- Islice(mydata1hour,as.POSIXct("2010-07-20"),as.POSIXct("2010-08-20"))
Gap30 <- mydata1date %>% filter(Global_active_power>40)
#====================vizualization complete linegraph==========================================
Allplot<- function(DF) {
  ggplot(data=DF , aes(x = DateTime , y= Global_active_power)) + 
    geom_line(aes(color="global"))+
    geom_line(aes(y=heating, color = "heating"))+
    geom_line(aes(y=laundry,color="laundry"))+
    geom_line(aes(y=kitchen,color="kitchen"))+
    geom_line(aes(y=Res,color="Residual"))+
    scale_color_manual(values = c("red", "black","green", "blue", "darkseagreen3"))+ labs(y = "Power Watt Hour")
}
plot_grid(Allplot(mydata1date), Allplot(mydata1week), Allplot(mydata1month),Allplot(mydata1year), labels = c('DayAVG',"weekavg", 'MonthAVG', 'yearAVG')) 
plot_grid(Allplot(Aug20), Allplot(Aug2008),Allplot(Aug2009),Allplot(Aug2010), labels = c('2007', '2008', '2009','2010')) 
Allplot(julyaug2010)
plot_grid(Allplot(mydata1wend), Allplot(mydata1wday), labels = c('weekdays', 'weekends')) 
#====================vizualization Histogram==========================================
AllPlotHist <-function(DF){
  ggplot(DF, aes(x=Global_active_power),) + geom_histogram( bandwidth=4 ,colour= "cornflowerblue",fill="red")
}

plot_grid(AllPlotHist(mydata1date), AllPlotHist(mydata1month),AllPlotHist(mydata1year), labels = c('date', 'month', 'year')) 
#====================vizualization pie==========================================

# Pie Chart with Percentages
piepieday <- function(d,m,y){
  mydf=mydata1date %>%  filter(year(mydata1date$DateTime)==y,month(mydata1date$DateTime)==m,day(mydata1date$DateTime)==m)
  
  slices <- c(mydf$kitchen, mydf$laundry, mydf$heating, mydf$Res) 
  lbls <- c("Kitchen", "Laundry", "Heating", "Residual")
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie3D(slices,labels=lbls,explode=0.1,
        main=" 3d Pie Chart of energy usage ")
  # pie(slices,labels = lbls, col=rainbow(length(lbls)),
  #  main="Pie Chart of energy usage") 
}

piepieday(10,11,2007)


seasonpie <- function(SS,y){
  
  mydf  <- mydata2Season  %>%  filter(year==y, Season == SS)
  slices <- c(mydf$kitchen, mydf$laundry, mydf$heating, mydf$Res) 
  lbls <- c("Kitchen", "Laundry", "Heating", "Residual")
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie(slices,labels = lbls, col=rainbow(length(lbls)),
      main="Pie Chart of energy of 2007") 
}

grid.arrange(seasonpie("winter",2007), seasonpie("autumm",2007),seasonpie("spring",2007),seasonpie("summer",2007), nrow=2) 

seasonpie("summer",2007)

#====================Time series==========================================
myts <- ts(mydata1week$Global_active_power, start=c(2007, 1), end=c(2010,11), frequency=52) 
myts <- ts(mydata1Season$Global_active_power, frequency=04) 
tail(mydata1month)
plot(myts)
plot(log(myts))  # make it stationary.. makes the varience equall
plot(diff(log(myts)))# makes the mean constant with time
#---------tslm function----------
my_df_ts <- data.frame(Globalactive = myts, as.numeric(time(myts)))
names(my_df_ts) <- c("Global_active_power", "time")
mymodel <- tslm(Global_active_power~season+trend,my_df_ts)
my_fc <- forecast(mymodel,h=120)
autoplot(my_fc)
#---------TS forecasting decomp------
myds_month <- decompose(myts)
plot(myds_month)
summary(myds_month)
#---------holt winters------

HWplot <- function(DF,  n.ahead = 120,  CI = .95,  error.ribbon = 'green', line.size = 1){
  
  hw <- HoltWinters(DF)
  
  forecast <- predict(hw, n.ahead = n.ahead,  prediction.interval = T,  level = CI)
  
  
  for_values <- data.frame(time = round(time(forecast),  3),  value_forecast = as.data.frame(forecast)$fit,  dev = as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
  
  fitted_values <- data.frame(time = round(time(hw$fitted),  3),  value_fitted = as.data.frame(hw$fitted)$xhat)
  
  actual_values <- data.frame(time = round(time(hw$x),  3),  Actual = c(hw$x))
  
  
  graphset <- merge(actual_values,  fitted_values,  by = 'time',  all = TRUE)
  graphset <- merge(graphset,  for_values,  all = TRUE,  by = 'time')
  graphset[is.na(graphset$dev),  ]$dev<-0
  
  graphset$Fitted <- c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
  
  
  graphset.melt <- melt(graphset[, c('time', 'Actual', 'Fitted')], id = 'time')
  
  p <- ggplot(graphset.melt,  aes(x = time,  y = value)) + 
    geom_ribbon(data = graphset, aes(x = time, y = Fitted, ymin = Fitted-dev,  ymax = Fitted + dev),  alpha = .2,  fill = error.ribbon) + 
    geom_line(aes(colour = variable), size = line.size) + geom_vline(x = max(actual_values$time),  xintercept = 2) + 
    xlab('Time') + ylab('Value')
  return(p)
  
}

HWplot(myts) + xlim(2007,2010)

