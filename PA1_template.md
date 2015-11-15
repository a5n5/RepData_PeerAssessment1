
This document shows the codes & results for first assignment of Reproducible research section of Coursera Data Science specialization

**Loading and preprocessing the data**


```r
echo = TRUE
setwd("~/Documents/Data Science/RCodes/mo5/assign1")
activity<-read.csv("activity.csv",header = TRUE)
```

**What is mean total number of steps taken per day?**

-Aggregate the data


```r
totalSteps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```

-Make a histogram of the total number of steps taken each day 


```r
hist(totalSteps$steps)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-15-1.png" title="" alt="" width="672" />

-Calculate and report the mean and median total number of steps taken per day


```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```

**What is the average daily activity pattern?**

-Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsInt <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
plot(steps ~ interval, data = stepsInt, type = "l")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-17-1.png" title="" alt="" width="672" />

-Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsInt[which.max(stepsInt$steps), ]$interval
```

```
## [1] 835
```

**Imputing missing values**

-Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

-Devise a strategy for filling in all of the missing values in the dataset.

*Will use the means for the 5-minute intervals as replacement for missing values.*


```r
int<- function(interval) {
    stepsInt[stepsInt$interval == interval, ]$steps
}

activityNew <- activity  

for (i in 1:nrow(activityNew)) {
    if (is.na(activityNew[i, ]$steps)) {
        activityNew[i, ]$steps <- int(activityNew[i, ]$interval)
        
    }
}
summary(activityNew)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```

-Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
totalSteps2 <- aggregate(steps ~ date, data = activityNew, sum)
hist(totalSteps2$steps)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-21-1.png" title="" alt="" width="672" />

```r
mean(totalSteps2$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps2$steps)
```

```
## [1] 10766.19
```

-Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
*Mean is the same as before. Slight difference in median, depending on where the missing values were located.*

**Are there differences in activity patterns between weekdays and weekends?**

-Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activityNew$day = ifelse(as.POSIXlt(as.Date(activityNew$date))$wday%%6 == 
    0, "weekend", "weekday")
activityNew$day = factor(activityNew$day, levels = c("weekday", "weekend"))
```

-Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
stepsInt2 = aggregate(steps ~ interval + day, activityNew, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = stepsInt2, aspect = 1/2, 
    type = "l")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-23-1.png" title="" alt="" width="672" />

**Create md and html file**

