a <- read.csv("activity.csv")
dsum = aggregate(steps ~ date, a, sum)
hist(dsum$steps, 10, main="Total Steps Per Day (10/2 - 11/29)", xlab="Number of Steps")
mean(dsum$steps)
median(dsum$steps)

isum = aggregate(steps ~ interval, a, mean)
itimes = substring(strptime(isum$interval, format="%H%M"), 12,16)

library(strinr)
itimes = substring(strptime(str_pad(isum$interval, pad="0",4), format="%H%M"), 12,16)
df <- data.frame(itimes, isum$steps)

library(ggplot2)
ggplot(df, aes(x=isum$interval, y = isum$steps, group = 1)) + geom_line()

highest = max(isum$steps)
max = subset(isum, steps == highest)
max$interval


nrow (subset(a,is.na(steps)))

# first make a copy of the original dataset
a2 <- a

#loop through every row and if the number of steps is missing
#look up the average value for that interval and replace the 
#missing value with the average.
for (row in 1:nrow(a2)) {
  if (is.na(a2[row,]$steps) == TRUE) {
    # get the interval that is missing the value
    i <- a2[row,"interval"]
    
    #determine the average value by looking up in isum
    iavg = isum[isum$interval == i, "steps"]
    
    #replace the value
    a2[row,"steps"] <- iavg    
  }
}

dsum = aggregate(steps ~ date, a2, sum)
hist(dsum$steps, 10, main="Total Steps Per Day (with na replacement)", xlab="Number of Steps")
mean(dsum$steps)
median(dsum$steps)

# Store the day of the week of each date
a2$day.of.week= weekdays(as.Date(a$date))
#initialize everything to weekdays
a2$day.type = "weekday"
#change Saturdays and Sundays to weekends
a2[a2$day.of.week %in% c("Saturday", "Sunday"),]$day.type = "weekend"

#reaggregate by interval with the day type included
isum = aggregate(steps ~ day.type+interval, a2, mean)
day.f<-factor(a2$day.of.week,levels=c("Saturday", "Sunday", "Monday","Tuesday","Wednedsay","Thursday","Friday"),
              labels=c("Weekend", "Weekend","Weekday","Weekday","Weekday","Weekday","Weekday")) 

library(lattice)

attach(isum)
xyplot(steps~interval|day.type, type="l",
       xlab = "Interval", ylab="Steps",
       main="Steps by Interval and Day Type", 
       layout=c(1,2))

