# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
```


## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day.

```r
total_steps_per_day <- aggregate(x = data$steps, by = list(data$date), FUN = sum, 
                                 na.rm = TRUE)
names(total_steps_per_day) <- c("date", "total_steps")
```

2. Make a histogram of the total number of steps taken each day.

```r
library(ggplot2)
q <- qplot(y = total_steps, x = date, data = total_steps_per_day, geom = "histogram", 
      stat = "identity", xlab = "", ylab = "Total number of steps",
      main = "Total Number of Steps taken each day")
q + theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](PA1_template_files/figure-html/histogram-1.png) 

3. Calculate the mean and median of the total number of steps taken each day.

```r
mean(total_steps_per_day$total_steps, na.rm = TRUE)
```

```
## [1] 9354.23
```


```r
median(total_steps_per_day$total_steps, na.rm = TRUE)
```

```
## [1] 10395
```


## What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval (x-axis) and the average 
number of steps taken, averaged across all days (y-axis)

```r
interval_average_steps <- aggregate(x = data$steps, by = list(data$interval), data = data, 
                                    FUN = mean, na.rm = TRUE)
names(interval_average_steps) <- c("interval", "avg_steps")
plot(x = interval_average_steps$interval, y = interval_average_steps$avg_steps, 
     type = "l", xlab = "Interval", ylab = "Average steps")
```

![](PA1_template_files/figure-html/timeseriesplot-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?

```r
interval_average_steps[which.max(interval_average_steps$avg_steps), c("interval")]
```

```
## [1] 835
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset.

```r
sum(is.na(data))
```

```
## [1] 2304
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in. (In this, I will use the average 5-minute interval)

```r
merged_data <- merge(x = data, y = interval_average_steps, by = "interval", all.x = TRUE)
merged_data[is.na(merged_data$steps), c("steps")] <- 
    merged_data[is.na(merged_data$steps), c("avg_steps")]
```

4. Make a histogram of the total number of steps taken each day and Calculate and 
report the mean and median total number of steps taken per day.

```r
total_steps_with_filled_NAs_each_day <- aggregate(x = merged_data$steps, 
                                                  by = list(merged_data$date),
                                                  FUN = sum, na.rm = TRUE)
names(total_steps_with_filled_NAs_each_day) <- c("date", "avg_steps")
q <- qplot(y = avg_steps, x = date, data = total_steps_with_filled_NAs_each_day, 
           geom = "histogram", stat = "identity", xlab = "", 
           ylab = "Total number of steps", main = "Total Number of Steps taken each day")
q + theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 


```r
mean(total_steps_with_filled_NAs_each_day$avg_steps, na.rm = TRUE)
```

```
## [1] 10766.19
```


```r
median(total_steps_with_filled_NAs_each_day$avg_steps, na.rm = TRUE)
```

```
## [1] 10766.19
```



## Are there differences in activity patterns between weekdays and weekends?
