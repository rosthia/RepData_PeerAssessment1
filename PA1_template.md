# Reproducible Research: Peer Assessment 1
## Settings

```r
#Make code visible and turn off scientific notations for numbers
echo = TRUE
options(scipen = 1) 
```


## Loading and preprocessing the data

```r
setwd("~/RepData_PeerAssessment1")
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
data$month <- as.numeric(format(data$date, "%m"))
noNA <- na.omit(data)
rownames(noNA) <- 1:nrow(noNA)
head(noNA)
```

```
##   steps       date interval month
## 1     0 2012-10-02        0    10
## 2     0 2012-10-02        5    10
## 3     0 2012-10-02       10    10
## 4     0 2012-10-02       15    10
## 5     0 2012-10-02       20    10
## 6     0 2012-10-02       25    10
```

```r
dim(noNA)
```

```
## [1] 15264     4
```

```r
library(ggplot2)
library(lattice)
```


## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day


```r
ggplot(noNA, aes(date, steps)) + geom_bar(stat = "identity", colour = "blue", fill = "blue", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs (title = "Total no. of steps taken per day", x = "Date", y = "Total no of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
totSteps <- aggregate(noNA$steps, list(Date = noNA$date), FUN = "sum")$x
mean(totSteps)
```

```
## [1] 10766.19
```

```r
median(totSteps)
```

```
## [1] 10765
```




## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgSteps <- aggregate(noNA$steps, list(interval = as.numeric(as.character(noNA$interval))), FUN = "mean")
names(avgSteps)[2] <- "meanOfSteps"

ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "blue", size = 0.8)+ labs(title = "Time series plot of 5 min interval", x = "5 min interval", y = "Avg no of steps taken")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]
```

```
##     interval meanOfSteps
## 104      835    206.1698
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Fill each NA value with mean from 5 min interval.


```r
Data2 <- data 
for (i in 1:nrow(Data2)) {
    if (is.na(Data2$steps[i])) {
        Data2$steps[i] <- avgSteps[which(Data2$interval[i] == avgSteps$interval), ]$meanOfSteps
    }
}

head(Data2)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
sum(is.na(Data2))
```

```
## [1] 0
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
ggplot(Data2, aes(date, steps))+ geom_bar(stat = "identity",colour = "blue", fill = "blue", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Total no of steps taken per day (with missing data) ", x = "Date", y = "Total no of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-9-1.png) 


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean and Median total no of steps per day

```r
TotSteps2 <- aggregate(Data2$steps, list(Date = Data2$date), FUN = "sum")$x
Mean2 <- mean(TotSteps2)
Mean2
```

```
## [1] 10766.19
```

```r
Median2 <- median(TotSteps2)
Median2
```

```
## [1] 10766.19
```

Comparing both means

```r
Mean1 <- mean(totSteps)
Median1 <- median(totSteps)
Mean2 - Mean1
```

```
## [1] 0
```

```r
Median2 - Median1
```

```
## [1] 1.188679
```
Revised mean remains the same. However revised median is greater than the original median.



## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
head(Data2)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
Data2$weekdays <- factor(format(Data2$date, "%A"))
levels(Data2$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(Data2$weekdays) <- list(weekday = c("Monday", "Tuesday","Wednesday", "Thursday", "Friday"),weekend = c("Saturday", "Sunday"))
levels(Data2$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(Data2$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
avgSteps <- aggregate(Data2$steps, list(interval = as.numeric(as.character(Data2$interval)), weekdays = Data2$weekdays), FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "No of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-13-1.png) 


