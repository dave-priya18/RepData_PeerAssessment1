---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity <- read.csv("activity.csv")
```



## What is mean total number of steps taken per day?


```r
mean_steps <- aggregate(data =activity, steps ~ date, FUN = mean, na.rm=TRUE)
hist(mean_steps$steps, main = "Average Steps per Day", col = "blue", xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
median_steps <-aggregate(data =activity, steps ~ date, FUN = median, na.rm=TRUE)
```

### Mean Steps per Day

```
##          date      steps
## 1  2012-10-02  0.4375000
## 2  2012-10-03 39.4166667
## 3  2012-10-04 42.0694444
## 4  2012-10-05 46.1597222
## 5  2012-10-06 53.5416667
## 6  2012-10-07 38.2465278
## 7  2012-10-09 44.4826389
## 8  2012-10-10 34.3750000
## 9  2012-10-11 35.7777778
## 10 2012-10-12 60.3541667
## 11 2012-10-13 43.1458333
## 12 2012-10-14 52.4236111
## 13 2012-10-15 35.2048611
## 14 2012-10-16 52.3750000
## 15 2012-10-17 46.7083333
## 16 2012-10-18 34.9166667
## 17 2012-10-19 41.0729167
## 18 2012-10-20 36.0937500
## 19 2012-10-21 30.6284722
## 20 2012-10-22 46.7361111
## 21 2012-10-23 30.9652778
## 22 2012-10-24 29.0104167
## 23 2012-10-25  8.6527778
## 24 2012-10-26 23.5347222
## 25 2012-10-27 35.1354167
## 26 2012-10-28 39.7847222
## 27 2012-10-29 17.4236111
## 28 2012-10-30 34.0937500
## 29 2012-10-31 53.5208333
## 30 2012-11-02 36.8055556
## 31 2012-11-03 36.7048611
## 32 2012-11-05 36.2465278
## 33 2012-11-06 28.9375000
## 34 2012-11-07 44.7326389
## 35 2012-11-08 11.1770833
## 36 2012-11-11 43.7777778
## 37 2012-11-12 37.3784722
## 38 2012-11-13 25.4722222
## 39 2012-11-15  0.1423611
## 40 2012-11-16 18.8923611
## 41 2012-11-17 49.7881944
## 42 2012-11-18 52.4652778
## 43 2012-11-19 30.6979167
## 44 2012-11-20 15.5277778
## 45 2012-11-21 44.3993056
## 46 2012-11-22 70.9270833
## 47 2012-11-23 73.5902778
## 48 2012-11-24 50.2708333
## 49 2012-11-25 41.0902778
## 50 2012-11-26 38.7569444
## 51 2012-11-27 47.3819444
## 52 2012-11-28 35.3576389
## 53 2012-11-29 24.4687500
```
### Median Steps per Day

```
##          date steps
## 1  2012-10-02     0
## 2  2012-10-03     0
## 3  2012-10-04     0
## 4  2012-10-05     0
## 5  2012-10-06     0
## 6  2012-10-07     0
## 7  2012-10-09     0
## 8  2012-10-10     0
## 9  2012-10-11     0
## 10 2012-10-12     0
## 11 2012-10-13     0
## 12 2012-10-14     0
## 13 2012-10-15     0
## 14 2012-10-16     0
## 15 2012-10-17     0
## 16 2012-10-18     0
## 17 2012-10-19     0
## 18 2012-10-20     0
## 19 2012-10-21     0
## 20 2012-10-22     0
## 21 2012-10-23     0
## 22 2012-10-24     0
## 23 2012-10-25     0
## 24 2012-10-26     0
## 25 2012-10-27     0
## 26 2012-10-28     0
## 27 2012-10-29     0
## 28 2012-10-30     0
## 29 2012-10-31     0
## 30 2012-11-02     0
## 31 2012-11-03     0
## 32 2012-11-05     0
## 33 2012-11-06     0
## 34 2012-11-07     0
## 35 2012-11-08     0
## 36 2012-11-11     0
## 37 2012-11-12     0
## 38 2012-11-13     0
## 39 2012-11-15     0
## 40 2012-11-16     0
## 41 2012-11-17     0
## 42 2012-11-18     0
## 43 2012-11-19     0
## 44 2012-11-20     0
## 45 2012-11-21     0
## 46 2012-11-22     0
## 47 2012-11-23     0
## 48 2012-11-24     0
## 49 2012-11-25     0
## 50 2012-11-26     0
## 51 2012-11-27     0
## 52 2012-11-28     0
## 53 2012-11-29     0
```
## What is the average daily activity pattern?


```r
library(stringr)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
for (arow in 1:nrow(activity)) {
  time_field <- str_pad(as.character(activity[arow,"interval"]), 4, pad = "0")
  tmp1 <- substr(time_field,1,2)
  tmp2 <- substr(time_field, 3,4)
  tmp_time <- paste(tmp1,tmp2,sep = ":")
  tmp_date <- as.character(activity[arow,"date"])
  dtime <- paste(tmp_date, tmp_time, sep = " ")
  activity[arow,"date_time"] <- dtime
}

with(activity, {
        plot(factor(date_time), steps, type="l",xaxt="n")
})
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

###Average daily activity, second possibility (not sure which the instructor was looking for)


```r
with(mean_steps, {
plot.ts(factor(date), steps)
})
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max(activity$steps, na.rm=TRUE)
```

```
## [1] 806
```

## Inputing missing values

### Number of missing values


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


```r
full_activity <- activity[1:3]
full_activity[] <- lapply(full_activity, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
```

### Total steps each day


```r
total_full_steps <- aggregate(data =full_activity, steps ~ date, FUN = sum, na.rm=TRUE)
hist(total_full_steps$steps, xlab = "Steps Each Day", main="Total Steps Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
### Mean steps in revised data set


```r
aggregate(data =full_activity, steps ~ date, FUN = mean, na.rm=TRUE)
```

```
##    date      steps
## 1     1 37.3825996
## 2     2  0.4375000
## 3     3 39.4166667
## 4     4 42.0694444
## 5     5 46.1597222
## 6     6 53.5416667
## 7     7 38.2465278
## 8     8 37.3825996
## 9     9 44.4826389
## 10   10 34.3750000
## 11   11 35.7777778
## 12   12 60.3541667
## 13   13 43.1458333
## 14   14 52.4236111
## 15   15 35.2048611
## 16   16 52.3750000
## 17   17 46.7083333
## 18   18 34.9166667
## 19   19 41.0729167
## 20   20 36.0937500
## 21   21 30.6284722
## 22   22 46.7361111
## 23   23 30.9652778
## 24   24 29.0104167
## 25   25  8.6527778
## 26   26 23.5347222
## 27   27 35.1354167
## 28   28 39.7847222
## 29   29 17.4236111
## 30   30 34.0937500
## 31   31 53.5208333
## 32   32 37.3825996
## 33   33 36.8055556
## 34   34 36.7048611
## 35   35 37.3825996
## 36   36 36.2465278
## 37   37 28.9375000
## 38   38 44.7326389
## 39   39 11.1770833
## 40   40 37.3825996
## 41   41 37.3825996
## 42   42 43.7777778
## 43   43 37.3784722
## 44   44 25.4722222
## 45   45 37.3825996
## 46   46  0.1423611
## 47   47 18.8923611
## 48   48 49.7881944
## 49   49 52.4652778
## 50   50 30.6979167
## 51   51 15.5277778
## 52   52 44.3993056
## 53   53 70.9270833
## 54   54 73.5902778
## 55   55 50.2708333
## 56   56 41.0902778
## 57   57 38.7569444
## 58   58 47.3819444
## 59   59 35.3576389
## 60   60 24.4687500
## 61   61 37.3825996
```

### Median steps in revised data set


```r
aggregate(data =full_activity, steps ~ date, FUN = median, na.rm=TRUE)
```

```
##    date   steps
## 1     1 37.3826
## 2     2  0.0000
## 3     3  0.0000
## 4     4  0.0000
## 5     5  0.0000
## 6     6  0.0000
## 7     7  0.0000
## 8     8 37.3826
## 9     9  0.0000
## 10   10  0.0000
## 11   11  0.0000
## 12   12  0.0000
## 13   13  0.0000
## 14   14  0.0000
## 15   15  0.0000
## 16   16  0.0000
## 17   17  0.0000
## 18   18  0.0000
## 19   19  0.0000
## 20   20  0.0000
## 21   21  0.0000
## 22   22  0.0000
## 23   23  0.0000
## 24   24  0.0000
## 25   25  0.0000
## 26   26  0.0000
## 27   27  0.0000
## 28   28  0.0000
## 29   29  0.0000
## 30   30  0.0000
## 31   31  0.0000
## 32   32 37.3826
## 33   33  0.0000
## 34   34  0.0000
## 35   35 37.3826
## 36   36  0.0000
## 37   37  0.0000
## 38   38  0.0000
## 39   39  0.0000
## 40   40 37.3826
## 41   41 37.3826
## 42   42  0.0000
## 43   43  0.0000
## 44   44  0.0000
## 45   45 37.3826
## 46   46  0.0000
## 47   47  0.0000
## 48   48  0.0000
## 49   49  0.0000
## 50   50  0.0000
## 51   51  0.0000
## 52   52  0.0000
## 53   53  0.0000
## 54   54  0.0000
## 55   55  0.0000
## 56   56  0.0000
## 57   57  0.0000
## 58   58  0.0000
## 59   59  0.0000
## 60   60  0.0000
## 61   61 37.3826
```
###What is the impact of imputing missing data on the estimates of the total daily number of steps?
The mean and median values both increase.

## Are there differences in activity patterns between weekdays and weekends?


```r
library(lattice)

mean_steps$weekend <- ifelse(weekdays(as.Date(mean_steps$date)) %in% c("Saturday","Sunday"), "weekend", "weekday")

panel.smoother <- function(x, y) {
  panel.xyplot(x, y) # show points 
  panel.loess(x, y)  # show smoothed line 
}

with(mean_steps, {
  xyplot(steps ~factor(date) |factor(weekend), 
         type = "l",
         xlab = "Date", ylab = "Number of Steps", 
         main = "Average Steps per Day on Weekdays/Weekends")    
})
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->