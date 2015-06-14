library(ggplot2)
library(dplyr)
Sys.setlocale("LC_TIME", "C")

data <- read.csv(unz("activity.zip", "activity.csv"))
data.DF <- data.frame(steps=data$steps,date=as.Date(data$date),interval=data$interval)


days <- unique(data.DF$date)
totSteps <- sapply(days, function(day) sum(data.DF[which(data.DF$date==day),]$steps))
qplot(totSteps, geom="histogram",binwidth=sd(totSteps,na.rm=T)/4,xlab="N of steps",ylab="count",main="Histogram of the total number of steps in a whole day")
print(sprintf("The mean number of steps per day: %.3f, while the median is: %.3f",mean(totSteps,na.rm=T),median(totSteps,na.rm=T)))


intervals <- unique(data.DF$interval)
meanSteps <- sapply(intervals, function(interval) mean(data.DF[which(data.DF$interval==interval),]$steps,na.rm=T))
qplot(x=intervals,y=meanSteps,geom="line",xlab="interval",ylab="mean number of steps",main="The mean number of steps for given interval\nacross all days")
print(sprintf("The largest mean number of steps %.3f was made in the interval number %d",max(meanSteps),intervals[which(meanSteps==max(meanSteps))]))


which.NA <- which(!complete.cases(data.DF))
print(sprintf("The number of rows containing NAs is: %d (out of %d rows)",length(which.NA),dim(data.DF)[1]))
meanSteps.list <- as.list(meanSteps)
names(meanSteps.list) <- intervals
data.without.NAs.DF <- data.DF
for(na.row in which.NA) {
  data.without.NAs.DF[na.row,]$steps <- meanSteps.list[[as.character(data.DF[na.row,]$interval)]]
}
totSteps.no.NA <- sapply(days, function(day) sum(data.without.NAs.DF[which(data.DF$date==day),]$steps))
qplot(totSteps.no.NA, geom="histogram",binwidth=sd(totSteps,na.rm=T)/4,xlab="N of steps",ylab="count",main="Histogram of the total number of steps in a whole day\nfor data with imputed NAs")
print(sprintf("The mean number of steps for data with imputed NAs per day: %.3f, while the median is: %.3f",mean(totSteps.no.NA,na.rm=T),median(totSteps.no.NA,na.rm=T)))


assign.weekday <- function(date) {
  output <- as.factor("weekday")
  if(weekdays(date)%in%c("Sunday","Saturday")) 
    output <- as.factor("weekend")
  output
}
data.without.NAs.DF$weekday <- sapply(data.without.NAs.DF$date, function(date) assign.weekday(date))
data.subset.weekend.DF <- subset(data.without.NAs.DF,weekday=="weekend")
meanSteps.weekend <- sapply(intervals, function(interval) mean(data.subset.weekend.DF[which(data.subset.weekend.DF$interval==interval),]$steps,na.rm=T))
data.subset.weekday.DF <- subset(data.without.NAs.DF,weekday=="weekday")
meanSteps.weekday <- sapply(intervals, function(interval) mean(data.subset.weekday.DF[which(data.subset.weekday.DF$interval==interval),]$steps,na.rm=T))
tmp.DF <- data.frame(x=intervals,meanSteps=c(meanSteps.weekend,meanSteps.weekday),weekday=as.factor(c(rep("weekend",length(meanSteps.weekend)),rep("weekday",length(meanSteps.weekday)))))
#ggplot(tmp.DF, aes(intervals, y = value, color = variable)) + geom_line(aes(y = meanSteps.weekend, col = "mean N of steps (weekend)")) + geom_line(aes(y = meanSteps.weekday, col = "mean N of steps (weekday)"))
p <- ggplot(tmp.DF, aes(x, meanSteps)) + geom_line() + facet_grid(.~weekday) + xlab("interval") + ylab("mean N of steps")
p