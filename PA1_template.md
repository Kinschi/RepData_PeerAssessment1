---
title: 'Peer-graded Assignment: Course Project 1'
author: "Kinschi"
date: "11 Juni 2017"
output: html_document
---

```{r, echo=TRUE}
library(ggplot2)
library(scales)
library(Hmisc)
```

## Loading and preparing the data
### 1. Load the data (i.e. read.csv())
```{r, echo=TRUE}
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
data <- read.csv('activity.csv', header=TRUE, sep=",")
```

## What is mean total number of steps taken per day?
### 1. Calculate the total number of steps taken per day
```{r, echo=TRUE}
steps.total <- tapply(data$steps, data$date, sum, na.rm=TRUE)
```

### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
```{r, echo=TRUE}
qplot(steps.total, xlab='Total number of steps per day', ylab='Frequency', binwidth=800)
```

### 3. Calculate and report the mean and median of the total number of steps taken per day
```{r, echo=TRUE}
steps.mean <- mean(steps.total, na.rm=TRUE)
steps.median <- median(steps.total, na.rm=TRUE)
```

The mean of the total number of steps taken per day is `r steps.mean`.

The median of the total number of steps taken per day is `r steps.median`. 

## What is the average daily activity pattern?
### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r, echo=TRUE}
steps.averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)

ggplot(data=steps.averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r, echo=TRUE}
steps.averages[which.max(steps.averages$steps),]
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r, echo=TRUE}
nr.values.missing <- length(which(is.na(data$steps)))
```

The total number of missing values is `r nr.values.missing`.

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r, echo=TRUE}
value.na <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (steps.averages[steps.averages$interval==interval, "steps"])
    return(filled)
}
```


### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r, echo=TRUE}
data.filled <- data
data.filled$steps <- mapply(value.na, data.filled$steps, data.filled$interval)
```
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r, echo=TRUE}
steps.total <- tapply(data.filled$steps, data.filled$date, FUN=sum)
qplot(steps.total, binwidth=800, xlab="total number of steps taken each day")

steps.total.mean <- mean(steps.total)
steps.total.median <- median(steps.total)
```

The mean of the total number of steps taken per day is `r steps.total.mean`.

The median of the total number of steps taken per day is `r steps.total.median`.

Mean and median get higher as NA-values beeing set to 0 by default are now set to the mean steps of the appropriate interval.

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r, echo=TRUE}
data.filled$date.type <- ifelse(as.POSIXlt(data.filled$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r, echo=TRUE}
steps.averages <- aggregate(steps ~ interval + date.type, data=data.filled, mean)
ggplot(steps.averages, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(date.type ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```


