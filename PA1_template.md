
Reproducible Research - Course Project 1


The assignment is about making analyses on data collected at 5 minute intervals through out the day during the months of October and November, 2012. It include the number of steps taken in 5 minute intervals each day.

## Loading and processing the data
1. Load the data: Read data from the downloaded csv file in your working directory


```r
##setwd("c:/courseProject1/xxx")
data_activity <- read.csv("activity.csv")
head(data_activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
dim(data_activity)
```

```
## [1] 17568     3
```
2. Convert date to a variable of Date

```r
dataDate<-as.Date(data_activity$date)
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
totalStepsByDay <- tapply(data_activity$steps, data_activity$date, sum, na.rm=TRUE)
```
2. Make a histogram of the total number of steps taken each day

```r
hist(x=totalStepsByDay, col="blue", breaks=20, xlab="Total steps per day",
     ylab="Count",
     main="Total number of steps taken per day")
```

![plot of chunk Histogram](figure/Histogram-1.png)
3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps <- mean(totalStepsByDay)
median_steps <- median(totalStepsByDay)
```
Mean of total steps taken is 9354.2295082 
Median of total steps taken is 10395 

## What is the average daily activity pattern?
1. Make a time series plot of the average steps taken for each interval for all days

```r
avg_steps_by_interval <- aggregate(steps ~ interval, data_activity, mean)
##Plot the Average steps by interval:
   plot(avg_steps_by_interval$interval,avg_steps_by_interval$steps, type="l", 
   xlab="5-minute Interval", ylab="Number of Steps",main="Average Number of Daily Steps by Interval")
```

![plot of chunk SeriesPlot](figure/SeriesPlot-1.png)
2. Find the 5-min interval that contains the maximum average steps:

```r
interval_max_steps <- avg_steps_by_interval[which.max(avg_steps_by_interval$steps),]
```
The interval that contains the maximum average steps is 835, 206.1698113

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset

```r
NumMissingValues <- sum(is.na(data_activity$steps))
```
Number of missing values is 2304

2. Devise a strategy for filling in all of the missing values in the dataset.

```r
library(magrittr)
library(dplyr)
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
new_dataset_activity <- data_activity
nas <- is.na(new_dataset_activity$steps)
mean_interval<- tapply(new_dataset_activity$steps, new_dataset_activity$interval, mean, na.rm=TRUE, simplify = TRUE)

new_dataset_activity$steps[nas] <- mean_interval[as.character(new_dataset_activity$interval[nas])]
sum(is.na(new_dataset_activity))
```

```
## [1] 0
```

```r
head(new_dataset_activity)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
4. Make a histogram of the total number of steps taken each day

```r
new_total_dailysteps <- tapply(new_dataset_activity$steps, new_dataset_activity$date, sum, na.rm=TRUE, simplify=T)
hist(x=new_total_dailysteps,
        col="red",
        breaks=20,
        xlab="Total daily steps",
        ylab="Count",
        main="Total number of steps taken per day (with missing data filled)")
```

![plot of chunk Histogram2](figure/Histogram2-1.png)


```r
mean_steps2 <- mean(new_total_dailysteps)
median_steps2 <- median(new_total_dailysteps)
```
Mean of total steps (with missing data filled) taken is 1.0766189 &times; 10<sup>4</sup>
Median of total steps (with missing data filled) taken is 1.0766189 &times; 10<sup>4</sup>

Comparing the newly imputed data set with the previous, the new mean is 10766.19 while the original was 9354.23. And the new median is 10766.19, while the original was 10395. Both the mean and median values changed, and become the same. This could be the impact of imputing missing data on the estimates.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend"

```r
new_dataset_activity$WeekendOrWeekday <-  ifelse(as.POSIXlt(new_dataset_activity$date)$wday %in% c(0,6), 'weekend', 'weekday')
median(totalStepsByDay)
```

```
## [1] 10395
```
2.

```r
avg_new_dataset_activity <- aggregate(steps ~ interval + WeekendOrWeekday, data=new_dataset_activity, mean)

library(ggplot2)
   ggplot(avg_new_dataset_activity, aes(interval, steps, color=WeekendOrWeekday)) + 
       geom_line() + 
       facet_grid(WeekendOrWeekday ~ .) +
       xlab("5-minute interval") + 
       ylab("avarage number of steps") + 
       ggtitle("Average number of steps per day by interval")
```

![plot of chunk AveNewDatasetPlot](figure/AveNewDatasetPlot-1.png)
