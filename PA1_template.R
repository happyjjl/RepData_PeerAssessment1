## PA1_template.R
## version:	1.0
## author:	jin.jiangli@mayo.edu

## This script load data from file "activity.csv",process data,make 4 plot

library(plyr)
library(lattice)

## Loading and preprocessing the data:  
walk_data <- read.csv("activity.csv")

## Grouped by date, make total steps taken per day 
day_total_steps <- ddply(walk_data, .(date), summarise, totalSteps = sum(steps, na.rm=TRUE)) 

png(filename = "figure/plot1.png")
## Make a histogram of the total number of steps taken each day
hist(day_total_steps$totalSteps, 
     breaks = 10, 
     xlab = "Total Steps", 
     main = "Histogram of Daily Steps, Ignoring NA",
     col = "red"
)
dev.off()

## Get mean and median of the total steps per day
day_step_mean <- mean(day_total_steps$totalSteps, na.rm = TRUE)
day_step_median <- median(day_total_steps$totalSteps, na.rm = TRUE)

## Grouped by interval, make average steps
mean_interval <- ddply(walk_data, .(interval), summarise,  AverageSteps = mean(steps, na.rm=TRUE))

png(filename = "figure/plot2.png")
## Make a time series plot of the 5-minute interval (x-axis) and 
## the average number of steps taken, averaged across all days (y-axis)
plot(mean_interval$interval, 
     mean_interval$AverageSteps, 
     type = "l", 
     xlab = "5 min interval", 
     ylab = "Average Steps", 
     main = "Average Steps per interval",
     col="red"
)
dev.off()

## get interval index of max average steps
max_steps_mean_index <- which.max(mean_interval$AverageSteps)

## number of NAs
sum_na <- sum(is.na(walk_data))

## Replace NAs with the mean for that 5-minute interval
walk_data_new <- merge(walk_data, mean_interval, by="interval", sort=TRUE)
index <- is.na(walk_data_new$steps)
walk_data_new$steps[index] <- walk_data_new$AverageSteps[index] 

day_total_steps2 <- ddply(walk_data_new, .(date), summarise, totalSteps=sum(steps, na.rm = TRUE)) #Group on date, total steps

## Histogram of the total number of steps taken each day with fixed NAs
png(filename = "figure/plot3.png")
hist(day_total_steps2$totalSteps, 
     breaks=10, 
     main = "Histogram of Daily Steps, Including NA", 
     xlab="Total Steps",
     col = "red"
) #Histogram with imputed missing values
dev.off()

## Recalculate the mean and median total number of steps taken per day.
day_step_mean2 <- mean(day_total_steps2$totalSteps)
day_step_median2 <- median(day_total_steps2$totalSteps)

## New dataset with a new factor variable indicating whether a given date is a weekday or weekend day
walk_data_new$date <- as.Date(walk_data_new$date)
day_new <- factor(weekdays(walk_data_new$date) %in% c("Saturday", "Sunday"), level=c(TRUE, FALSE), labels=c("weekend", "weekday"))
tmp1 <- mutate(walk_data_new, day = day_new)
tmp2 <- ddply(tmp1, .(interval, day), summarise, averageSteps = mean(steps))

## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
## and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
png("figure/plot4.png")
p <- xyplot(averageSteps ~ interval | day, 
       data = tmp2, 
       type = "l",
       layout = c(1, 2),
       ylab = "Average Steps", 
       main = "Average steps , Weekday vs Weekend"
)
print(p)
dev.off()