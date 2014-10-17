## Reproducible Research Peer Assessment 1
### Loading and preprocessing the data

1. Load the data

```r
activity = read.csv("activity.csv")
```
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
day_steps = aggregate(steps~date,data=activity,sum,na.rm=TRUE)
interval_steps = aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
```
### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in
the dataset.

1. Make a histogram of the total number of steps taken each day

```r
hist(day_steps$steps)
```

![plot of chunk step_hist](figure/step_hist.png) 
2. Calculate and report the **mean** and **median** total number of steps taken per day

```r
day_step_mean <- mean(day_steps$steps)
day_step_median <- median(day_steps$steps)
```
the **mean** and **median** total number of steps are 1.0766 &times; 10<sup>4</sup> and 10765  

### What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(interval_steps$interval,interval_steps$steps,type='l')
```

![plot of chunk interval_plot](figure/interval_plot.png) 
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval = interval_steps[which.max(interval_steps$steps),]$interval
```
  The anwser is 835 interval
### Imputing missing values
1 Report the number of missing values and fill missing values with the mean for that 5-minute interval

```r
totalMissing <- sum(is.na(activity$steps))
interval2steps<-function(interval){
  interval_steps[interval_steps$interval==interval,]$steps
}

activityFilled<-activity
for(i in 1:nrow(activityFilled)){
  if(is.na(activityFilled[i,]$steps)){
    activityFilled[i,]$steps<-interval2steps(activityFilled[i,]$interval)
  }
}
```
Total number of missing values is 2304

2 Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totalSteps2<-aggregate(steps~date,data=activityFilled,sum)
new_mean <- mean(totalSteps2$steps)
new_median <- median(totalSteps2$steps)
```
the **mean** and **median** total number of the new dataset are 1.0766 &times; 10<sup>4</sup> and 1.0766 &times; 10<sup>4</sup>

```r
hist(totalSteps2$steps)
```

![plot of chunk new_hist](figure/new_hist.png) 
### Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.


```r
activityFilled$day=ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6==0,"weekend","weekday")
activityFilled$day=factor(activityFilled$day,levels=c("weekday","weekend"))

stepsInterval2=aggregate(steps~interval+day,activityFilled,mean)

library(lattice)
xyplot(steps~interval|factor(day),data=stepsInterval2,aspect=1/2,type="l")
```

![plot of chunk weekdays_pattern](figure/weekdays_pattern.png) 
