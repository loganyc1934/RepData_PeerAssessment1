# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

Subset the complete cases,


```r
data <- read.csv("activity.csv")
comp_data <- data[complete.cases(data),]
```

Get the total steps for each day,


```r
step_sum <- aggregate(comp_data[,-2],comp_data["date"],sum)
step_sum <- step_sum[-3]
step_day <- as.numeric(unlist(step_sum["steps"]))
```

Get the average steps for each interval over all the days,


```r
step_int <- aggregate(comp_data[,-c(2,3)],comp_data["interval"],sum)
step_int$x <- step_int$x/nrow(step_sum)
```

## What is mean total number of steps taken per day?

Plot the histogram for the no. steps (missing data ignored), and get the mean, median no. steps per day by *summary()*,


```r
hist(step_sum$steps, breaks = 15, main = "Steps per Day", col = "red", xlab = "")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
summary(step_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8840   10800   10800   13300   21200
```

## What is the average daily activity pattern?

Plot the time series, get the mean of the no. steps for each interval, 


```r
plot(step_int$interval, step_int$x, type = "l", xlab = "Intervals", ylab = "Activities")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Print the interval where we have the maximum no. steps,


```r
step_int$interval[which.max(step_int$x)]
```

```
## [1] 835
```

## Imputing missing values

Print the total number of missing data, create a new dataset with missing data replaced by the mean no. steps for each interval over all days, and plot the new histogram to compare with the previous one,


```r
# number of missing data
print(nrow(data[!complete.cases(data),]))
```

```
## [1] 2304
```

```r
N <- nrow(data)
# use mean of the days to impute missing data
new_data <- data
for(i in 1:N){
        if(is.na(new_data$steps[i])){
                inte <- new_data$interval[i]
                temp <- subset(step_int, interval == inte)
                new_data$steps[i] <- temp$x
        }
}
# histogram for new_data
new_step_sum <- aggregate(new_data[,-2],new_data["date"],sum)
new_step_sum <- new_step_sum[-3]
new_step_day <- as.numeric(unlist(new_step_sum["steps"]))
hist(new_step_sum$steps, breaks = 15, main = "Steps per Day", col = "red", xlab = "")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
summary(new_step_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9820   10800   10800   12800   21200
```

The two histograms are similar, the latter has all the missing data filled out so that the distribution is slightly different, but it still looks like a normal bell curve.

## Are there differences in activity patterns between weekdays and weekends?

Plot the two time series. The plot shows that the activities are noticeably different for weekdays and weekends. The activities at the weekends tend to be more uniform than their weekday counterparts.


```r
new_data$date <- strptime(new_data$date, format="%Y-%m-%e")
new_data["wd"] <- NA
for(i in 1:N){
        tempdate <- new_data$date[i]
        if(tempdate$wday <= 5 & tempdate$wday >= 1){
                new_data$wd[i] <- "weekday"
        } else {
                new_data$wd[i] <- "weekend"
        }
}
wd_data <- subset(new_data, wd == "weekday")
we_data <- subset(new_data, wd == "weekend")

wd_step_int <- aggregate(wd_data[,-c(2,3,4)],wd_data["interval"],sum)
wd_step_int$x <- wd_step_int$x/length(unique(wd_data$date))

we_step_int <- aggregate(we_data[,-c(2,3,4)],we_data["interval"],sum)
we_step_int$x <- we_step_int$x/length(unique(we_data$date))

par(mfrow = c(2, 1))
par(mar=c(4, 4, 2, 0.5))
plot(wd_step_int$interval, wd_step_int$x, type = "l", xlab = "Weekday Intervals", ylab = "Activities")
plot(we_step_int$interval, we_step_int$x, type = "l", xlab = "Weekend Intervals", ylab = "Activities")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 
