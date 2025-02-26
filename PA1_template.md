---
output: 
  html_document: 
    keep_md: yes
---
---
title: "CourseProject1"
author: "Shey"
date: "7/17/2021"
output: html_document

**Course Project 1**
====================
### **part 1 (Loading and preprocessing the data)** 

Show any code that is needed to
 
1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
setwd("C:/PhD/Statistics/Coursera/Reproducible Research/w2/Project1")
activity<- read.csv("activity.csv" , na.strings = "NA")
```


to see the structure of datase:


```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


### **part 2 (What is mean total number of steps taken per day?)**

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.3     v purrr   0.3.4
## v tibble  3.0.6     v dplyr   1.0.4
## v tidyr   1.1.3     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(datasets)
activity_day <- activity %>% group_by(date) %>% summarise(sum= sum(steps, na.rm=TRUE)) %>%
        filter(sum!= 0)
hist(activity_day$sum , breaks = length(activity_day$date) , col = "purple",
     main="Steps taken per day" , xlab = "Daily Steps",  ylim = range(0:12))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
summary(activity_day$sum)[3:4]
```

```
##   Median     Mean 
## 10765.00 10766.19
```

### **part 3 (What is the average daily activity pattern?)**
        
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?        
        

```r
daily<- activity %>% group_by(interval) %>% summarise(average= mean(steps, na.rm=TRUE))%>%
                filter( average!= "NaN")
with(daily, plot(interval, average, type="l" , main="Average Daily Activity Pattern" , ylab=("Average Number of Steps")))        
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
daily[which.max(daily$average),]
```

```
## # A tibble: 1 x 2
##   interval average
##      <int>   <dbl>
## 1      835    206.
```

The interval with the most average steps is 835.


### **part 4 (Imputing missing values)**

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  


```r
summary(activity$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```
The number of rows with NA’s is 2304.  

I will be using the average steps of that 5-min interval to impute the missing values.


```r
imputNA<- activity %>% full_join( daily, by= "interval") 
imputNA$fillsteps<- ifelse( is.na(imputNA$steps), imputNA$average, imputNA$steps)
imputNA$average <- NULL
imputNA$steps <- NULL
colnames(imputNA)<- c( "date","interval", "steps")
imputNA<- imputNA[,c(3,1,2)]
head(imputNA)
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
imputNA_day<- imputNA %>% group_by(date) %>% summarise(sum= sum(steps, na.rm=TRUE)) %>%
        filter(sum!= 0)
hist(imputNA_day$sum , breaks = length(imputNA_day$date) , col = "darkolivegreen1" ,main="Steps taken per day" , xlab = "Daily Steps" ,ylim = range(0:12))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
summary(imputNA_day$sum)[3:4]
```

```
##   Median     Mean 
## 10766.19 10766.19
```

Here both mean and median of steps taken per day became exactly same which was not the case prior to imputing missing values.
Imputing with mean values make the histogram more normally distributed.



### **part 5 (Are there differences in activity patterns between weekdays and weekends?)**

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.  

The date is in factor class, so we need to change it to date format. 



```r
imputNA$date<- as.Date(imputNA$date)
imputNA$weekday <- weekdays(as.Date(imputNA$date)) 
imputNA$weekdayType <- ifelse(weekdays(imputNA$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
```

creating a new data frame with the average number of steps per interval:


```r
wday <- imputNA %>% group_by(interval, weekdayType) %>% summarise(meanSteps = mean(steps, 
                                                                                           na.rm = TRUE))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
ggplot(data = wday, mapping = aes(x = interval, y = meanSteps, col="red")) + 
        geom_line(size=1) + facet_grid(weekdayType ~ .) + theme_bw()+
        scale_x_continuous("Day Interval", breaks = seq(min(wday$interval), max(wday$interval), 100)) + scale_y_continuous("Average Number of Steps") + ggtitle("Average Number of Steps Taken by Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

There are some differences in average steps between Weekdays and Weekends. During weekdays, the person is more active at the start of the day and less active during the day. Meanwhile, during weekends, the person is less active at start of the day and more active throughout the day.
