install.packages("data.table")
library(data.table)
library(ggplot2)
setwd("C:/COMMUNISM/coursera/Data Science/5/week2/work/RepData_PeerAssessment1")
data <-  read.csv("activity/activity.csv")
datadf <- data.table(data)
datadfDays <- datadf[ ,sum(steps), by=date]
datadfDays$date <- as.Date(datadfDays$date)

##ex1
hist(datadfDays$V1, xlab="Number of steps", ylab="Days", main="Number of steps by days", col="blue", breaks=12)
mn <- mean(datadfDays$V1, na.rm=TRUE)
mdn <- median(datadfDays$V1, na.rm=TRUE)
abline(v = mn, col = "red")
abline(v = mdn, col = "green")



##ex2
datadf[,"trueTime"] <- NA

times <- datadf[,"date", with=FALSE]
times <- as.POSIXlt(as.character(times$date))
intervals <-rep(seq(from =0, by=5, to=(60*24)-5), times=numberOfDays)
numberOfDays <- length(datadfDays$date)
times <- times + intervals
datadf[,"trueTime"] <- times
avgIntervalsByDays <- datadf[,mean(steps, na.rm=TRUE), by=interval]
avgIntervalsByDays$times <- times[1:288]
plot(avgIntervalsByDays$times, avgIntervalsByDays$V1, type = "l")
avgIntervalsByDays [V1 == max(avgIntervalsByDays$V1),]$interval

##ex3
length((datadf [is.na(datadf$steps), "steps", with=FALSE])$steps)
datadf[is.na(datadf$steps)]$steps <- NA
replace <- function (i){
  avgIntervalsByDays[interval==i]$V1
}

##imputing
datadfImp <- datadf
for (i in 1:length (data$steps)){
  if(is.na(datadfImp[i]$steps))
    datadfImp[i]$steps <- replace(datadfImp[i]$interval)
}
datadfDaysImp <- datadfImp[ ,sum(steps), by=date]
datadfDaysImp$date <- as.Date(datadfDaysImp$date)
hist(datadfDaysImp$V1, xlab="Number of steps", ylab="Days", main="Number of steps by days", col="red", breaks=12)
mnImp <- mean(datadfDaysImp$V1, na.rm=TRUE)
mdnImp <- median(datadfDaysImp$V1, na.rm=TRUE)
abline(v = mnImp, col = "blue")
abline(v = mdnImp, col = "green")

##ex4

isWeekend <- function(day){
  if(day=="суббота"||day=="воскресенье")
    TRUE
  else FALSE
}
dayType <- sapply(weekdays(datadfImp$trueTime), isWeekend)
datadfImp <- cbind(datadfImp, dayType)

dataWEImp <- datadfImp[dayType==TRUE,]
dataNWEImp <- datadfImp[dayType==FALSE,]

getAvgs <- function (df){
  avgs <- df[,mean(steps, na.rm=TRUE), by=interval]
  avgs$times <- times[1:288]  
  avgs
}
WEIavg <- getAvgs(dataWEImp)
NWEIavg <- getAvgs(dataNWEImp)

plot(WEIavg$times, WEIavg$V1, type = "l")
plot(NWEIavg$times, NWEIavg$V1, type = "l")

