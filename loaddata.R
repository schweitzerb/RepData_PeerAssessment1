### Libraries
library(plyr)
library(ggplot2)

### Read Data
stepsData <- read.csv("activity.csv")

### format variables
stepsData$date <- as.POSIXct(as.character(stepsData$date),format = "%Y-%m-%d")
stepsData$interval <- sprintf("%04d",stepsData$interval)
##stepsData$interval <- format(strptime(stepsData$interval, format="%H%M"), format = "%H:%M")
stepsData$interval <- as.ordered(stepsData$interval)


###Basic stats
stepsByDay <- ddply(stepsData,"date",summarise,total=sum(steps,na.rm=T),
                    mean=mean(steps,na.rm=T),median=median(steps,na.rm=T))
meansteps <- mean(stepsByDay$total)
medianSteps <-median(stepsByDay$total)


stepsByInterval <- ddply(stepsData,"interval",summarise,total=sum(steps,na.rm=T),
                         mean=mean(steps,na.rm=T),median=median(steps,na.rm=T))
maxSteps <- stepsByInterval$interval[which.max(stepsByInterval$mean)]


###Basic Plots
hist(stepsByDay$total,breaks=10,main="Histogram of the Total Number of Steps per Day",
     ylab="Number of Days",xlab="Total Steps")

plot(as.numeric(stepsByInterval$interval)/288*100/4.16667,stepsByInterval$mean,type="l",
     main="Mean Steps Per Interval", ylab="Number of Steps",xlab="Interval (by Hour of Day)")



#Missing values
totalMissing <- sum(is.na(stepsData$steps))
missingIndex <- which(is.na(stepsData$steps))
stepsImputed <- stepsData

for (i in 1:length(missingIndex)){
      matchInterval <- stepsImputed[missingIndex[i],3]
      stepsImputed[missingIndex[i],1] <- stepsByInterval[stepsByInterval$interval
                                                         ==matchInterval,3]
}

### stats with imputed data
stepsByDay_imputed <- ddply(stepsImputed,"date",summarise,total=sum(steps,na.rm=T),
                            mean=mean(steps,na.rm=T),median=median(steps,na.rm=T))
meanstepsI <- mean(stepsByDay_imputed$total)
medianStepsI <-median(stepsByDay_imputed$total)

### weekday/weekend
stepsImputed$day <- weekdays(stepsImputed$date)
stepsImputed$day <- with(stepsImputed, replace(day,day%in%c("Saturday","Sunday"),"Weekend"))
stepsImputed$day <- with(stepsImputed, replace(day,!(day%in%"Weekend"),"Weekday"))
stepsImputed$day <- as.factor(stepsImputed$day)

ByWeekday <- ddply(stepsImputed,c("interval","day"),summarise,total=sum(steps,na.rm=T),
                   mean=mean(steps,na.rm=T),median=median(steps,na.rm=T))

panels <- qplot(as.numeric(interval)/288*100/4.16667,mean,data=ByWeekday,facets=day~.,color=day)+
      geom_line(aes(group=day))
print(panels) 