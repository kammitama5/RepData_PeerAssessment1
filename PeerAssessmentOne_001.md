Reproducible Research
=======================================

##Loading and Preprocessing the Data  

[1] Ensure that the data is in your set working directory  
[2] Load the data  


```r
options(scipen = 1)
activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "character"))
activity$day <- factor(paste(substring(months(as.Date(activity$date)),0,3), 
                      substring(as.Date  (activity$date), 9), sep=" ")) 
tidyData <- na.omit(activity)
head(tidyData)
```

```
##     steps       date interval    day
## 289     0 2012-10-02        0 Oct 02
## 290     0 2012-10-02        5 Oct 02
## 291     0 2012-10-02       10 Oct 02
## 292     0 2012-10-02       15 Oct 02
## 293     0 2012-10-02       20 Oct 02
## 294     0 2012-10-02       25 Oct 02
```


```r
names(activity)
```

```
## [1] "steps"    "date"     "interval" "day"
```


```r
library(lattice)
```

##What is the Mean Total Number of Steps Taken Per Day?

[1] Make a Histogram of the Total Number of Steps Taken Each Day.


```r
tot_steps <- aggregate(tidyData$steps, list(tidyData$date), FUN="sum")
names(tot_steps) <- c("date","total")
tot_steps$date <- factor(paste(substring(months(as.Date(tot_steps$date)),0,3), 
                      substring(as.Date  (tot_steps$date), 9), sep=" ")) 
barplot(tot_steps$total, names.arg=tot_steps$date, xlab="Days", ylab="Total Steps", main="Total Steps Count Per Day", col="steelblue")
```

![](PeerAssessmentOne_001_files/figure-html/unnamed-chunk-4-1.png) 

[1] Calculate mean total Number of steps taken per day.  


```r
print(round(mean(tot_steps$total)))  
```

```
## [1] 10766
```

[2] Calculate median total Number of Steps taken per Day.  


```r
print(median(tot_steps$total))
```

```
## [1] 10765
```
## What is the average daily activity pattern?


```r
activity_steps <- aggregate(tidyData$steps, list(as.numeric(tidyData$interval)), FUN="mean")
names(activity_steps) <- c("interval","mean")
plot(activity_steps, type="l", xlab="Interval", ylab="Number of Steps", main="Daily Activity Pattern", col=1)
```

![](PeerAssessmentOne_001_files/figure-html/unnamed-chunk-7-1.png) 


## Maximum number of steps for 5-minute interval

```r
maxI <- activity_steps[which.max(activity_steps$mean),]
print(maxI)
```

```
##     interval     mean
## 104      835 206.1698
```

#Imputing Missing Values


```r
missingSteps <- which(is.na(activity))
print(length(missingSteps))
```

```
## [1] 2304
```



```r
steps.date <- aggregate(steps ~ date,activity,FUN = sum)
nas <- is.na(activity$steps)
activity <- activity [, c(1:3)]
```

[1] Make a histogram of the number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
newData <- activity
for(i in 1:length(missingSteps)){
    newData[missingSteps[i], 1] <- activity_steps[activity_steps$interval ==
                                   newData[missingSteps[i],]$interval,]$mean
}

## Histogram of total Number of Steps per day
new_steps <- aggregate(newData$steps, list(newData$date), FUN="sum")
names(new_steps) <- c("date","total")
new_steps$date <- factor(paste(substring(months(as.Date(new_steps$date)),0,3), 
                      substring(as.Date  (new_steps$date), 9), sep=" ")) 
barplot(new_steps$total, names.arg=new_steps$date, xlab="Days", ylab="Total Steps",
        main="Total Steps Count Per Day (Without Missing Data)", col="steelblue")
```

![](PeerAssessmentOne_001_files/figure-html/unnamed-chunk-11-1.png) 

[2] mean

```r
print(round(mean(new_steps$total)))
```

```
## [1] 10766
```

[3] median


```r
print(median(new_steps$total))
```

```
## [1] 10766.19
```

```r
## Difference From TidyData Values
print(mean(new_steps$total) - mean(tot_steps$total))
```

```
## [1] 0
```

```r
print(median(new_steps$total) - median(tot_steps$total))
```

```
## [1] 1.188679
```


```r
head(newData)
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

```r
newData$daysOfWeek <- factor(weekdays(newData$date))
levels(newData$daysOfWeek)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

[1] Make a panel plot (ie. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
levels(newData$daysOfWeek) <- list(weekday = c("Monday", "Tuesday", "Wednesday", 
                                               "Thursday", "Friday"),
                                   weekend = c("Saturday", "Sunday"))
table(newData$daysOfWeek)
```

```
## 
## weekday weekend 
##   12960    4608
```


```r
library(lattice)
meanSteps <- aggregate(newData$steps, list(as.numeric(newData$interval),newData$daysOfWeek),        FUN = "mean")
names(meanSteps) <- c("interval","weekDays", "avgSteps")

xyplot(meanSteps$avgSteps ~ meanSteps$interval | meanSteps$weekDays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![](PeerAssessmentOne_001_files/figure-html/unnamed-chunk-19-1.png) 
