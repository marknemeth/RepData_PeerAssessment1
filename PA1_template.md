---
title: "Reproducible Research: Peer Assessment 1"
author: "Mark Nemeth"
date: "7/9/2021"
output: 
  html_document:
    keep_md:  true
---



## Introduction / Purpose

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


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
library(ggplot2)

classes<-c("numeric","character","numeric")
activity_df<-read.csv("./data/activity.csv", colClasses=classes, header=TRUE)

activity_df$date<-as.Date(activity_df$date, "%Y-%m-%d")
str(activity_df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activity_df)
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
summary(activity_df)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

We see the data is tidy: It contains single observation records for a particular anonymous individual, where each record records the number of steps measured in an indexed time interval of the day, for each date.  The records are number of steps (numeric), date (calendar date - character read), time interval of day (numeric).

- steps: Number of steps taking in a 5-minute interval (missing values are coded as **NA**  
- date: The date on which the measurement was taken in YYYY-MM-DD format  
- interval: Identifier for the 5-minute interval in which measurement was taken  


## What is mean total number of steps taken per day?
We note that there are 2,304 NA's in the data file.  Our instructions are that we can ignore missings in this part of our work, so we remove them from our daily build.


```r
dailySteps<-filter(activity_df,!is.na(steps)) %>% group_by(date) %>% summarize(steps=sum(steps))
```

The instructions for this part of the analysis are these:

1.  Calculate the total number of steps taken per day.  
2. (If you do not understand the difference between a histogram and a barplot, research the difference between them.) Make a histogram of the total number of steps taken each day. (We draw both to show difference: a barplot first, then a histogram.)  
3. Calculate and report the mean and median of the total number of steps taken per day.  
  

```r
ggplot(data=dailySteps, aes(x=date, y=steps)) + geom_bar(stat="identity", colour="blue", fill="blue", na.rm=TRUE) +
        scale_x_date(breaks="1 day",
                     limits=c(as.Date("2012-10-01"),as.Date("2012-11-30"))) +
        ylab("Steps") + xlab("Date") + 
        theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        ggtitle("Barplot of Total Steps per Day")
```

![](PA1_template_files/figure-html/Part2_dailysteps_plottingAndStats-1.png)<!-- -->

```r
# The above is really a barplot.  
# A histogram would be more like this.

hist(dailySteps$steps, col="green", breaks=length(unique(dailySteps$steps)), main="Histogram of Daily Steps",ylab="Count of Dates", xlab="Daily Steps")
rug(dailySteps$steps)
abline(v=median(dailySteps$steps),col="blue",lwd=4)
abline(v=mean(dailySteps$steps),col="red",lwd=2)
```

![](PA1_template_files/figure-html/Part2_dailysteps_plottingAndStats-2.png)<!-- -->

```r
summary(dailySteps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

And, so we find also that the daily **mean** number of steps is **10,766** and the **median** is **10,765**.

## What is the average daily activity pattern?

Here, we are asked to do the following:  

1. Make a time series plot (i.e. type = "l", for line plot) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).   
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  


```r
intervalSteps<-filter(activity_df,!is.na(steps)) %>% group_by(interval) %>% summarize(steps=mean(steps), na.rm=TRUE)

ggplot(data=intervalSteps, aes(x=interval, y=steps)) + geom_point(color="blue") +
        geom_line(color="black") +
        ylab("Steps") + xlab("Interval") + 
        theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

![](PA1_template_files/figure-html/Part3_intervalsteps-1.png)<!-- -->

For answering part 3, we can approach this in two ways:  
  a. Find the maximum mean value over the intervals and isolate the interval with that value.  
  b. Sort the intervals on mean number of steps and check the tail of the sort.  
We perform both approaches here.  
  

```r
#Answer a.:        
summary(intervalSteps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.486  34.113  37.383  52.835 206.170
```

```r
#Max steps on average in an interval are 206.170
maxIntervalSteps<-intervalSteps[intervalSteps$steps>205,]
maxIntervalSteps
```

```
## # A tibble: 1 x 3
##   interval steps na.rm
##      <dbl> <dbl> <lgl>
## 1      835  206. TRUE
```

```r
#Answer b,:
head(arrange(intervalSteps,by_group=steps))
```

```
## # A tibble: 6 x 3
##   interval steps na.rm
##      <dbl> <dbl> <lgl>
## 1       40     0 TRUE 
## 2      120     0 TRUE 
## 3      155     0 TRUE 
## 4      200     0 TRUE 
## 5      205     0 TRUE 
## 6      215     0 TRUE
```

```r
tail(arrange(intervalSteps,by_group=steps))
```

```
## # A tibble: 6 x 3
##   interval steps na.rm
##      <dbl> <dbl> <lgl>
## 1      820  171. TRUE 
## 2      830  177. TRUE 
## 3      845  180. TRUE 
## 4      850  183. TRUE 
## 5      840  196. TRUE 
## 6      835  206. TRUE
```

So, interval 835 has the maximum mean number of steps over all days measured.  That maximum is 206 steps (206.170).  This is the single interval attaining that maximum, as is verified in the graph above, as well.
  

## Imputing missing values

We are instructed as follows in answering this particular portion of the analysis.

Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  
  

```r
summary(activity_df)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```
  
>  
> So, there are 2,304 missings in the dataset (i.e. rows with NAs)  
>  

2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. 
  
    >  
    > Means by day and means by 5-minute interval make sense, but people have different habits on weekends and weekdays, even Saturdays and Sundays differ.  Let's try to impute with means of the 5-minute intervals over the distinct days of the week.
    >  
  
3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.  
  
    >  
    > Let's calculate the means for each 5-minute interval by weekday.  Then we will perform the imputation.
    >  

```r
activity_df<-mutate(activity_df,dayOfWeek=weekdays(date))

dayIntMeanSteps<-filter(activity_df,!is.na(steps)) %>% group_by(dayOfWeek,interval) %>% summarize(meanSteps=mean(steps), na.rm=TRUE)
```

```
## `summarise()` has grouped output by 'dayOfWeek'. You can override using the `.groups` argument.
```

```r
# Viewing dayIntMeanSteps:  Results confirmed with PivotTable analysis in MS Excel.  Means are by dayOfWeekXIntedrval.

#So, we merge these means into the main dataset, activity_df.
mrgActivity_df<-left_join(activity_df,dayIntMeanSteps,by=c("dayOfWeek","interval"))

#Then, where there are NAs, we populate with the proper mean value.
mrgActivity_df<-left_join(activity_df,dayIntMeanSteps,by=c("dayOfWeek","interval"))
mrgActivity_df$imputeRow<-FALSE
mrgActivity_df$imputeRow[is.na(mrgActivity_df$steps)]<-TRUE
mrgActivity_df$steps[is.na(mrgActivity_df$steps)]<-mrgActivity_df$meanSteps[is.na(mrgActivity_df$steps)]


# Check imputed row count matches the NA count and that the stats for the unimputed rows remains as in Step 1.
#Non-imputed
summary(mrgActivity_df[!mrgActivity_df$imputeRow,])
```

```
##      steps             date               interval       dayOfWeek        
##  Min.   :  0.00   Min.   :2012-10-02   Min.   :   0.0   Length:15264      
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Class :character  
##  Median :  0.00   Median :2012-10-29   Median :1177.5   Mode  :character  
##  Mean   : 37.38   Mean   :2012-10-30   Mean   :1177.5                     
##  3rd Qu.: 12.00   3rd Qu.:2012-11-16   3rd Qu.:1766.2                     
##  Max.   :806.00   Max.   :2012-11-29   Max.   :2355.0                     
##    meanSteps       na.rm         imputeRow      
##  Min.   :  0.00   Mode:logical   Mode :logical  
##  1st Qu.:  0.00   TRUE:15264     FALSE:15264    
##  Median : 12.62                                 
##  Mean   : 37.38                                 
##  3rd Qu.: 55.00                                 
##  Max.   :328.57
```

```r
#Imputed
summary(mrgActivity_df[mrgActivity_df$imputeRow,])
```

```
##      steps             date               interval       dayOfWeek        
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Length:2304       
##  1st Qu.:  0.00   1st Qu.:2012-10-26   1st Qu.: 588.8   Class :character  
##  Median : 12.14   Median :2012-11-06   Median :1177.5   Mode  :character  
##  Mean   : 38.84   Mean   :2012-11-01   Mean   :1177.5                     
##  3rd Qu.: 59.32   3rd Qu.:2012-11-11   3rd Qu.:1766.2                     
##  Max.   :328.57   Max.   :2012-11-30   Max.   :2355.0                     
##    meanSteps       na.rm         imputeRow     
##  Min.   :  0.00   Mode:logical   Mode:logical  
##  1st Qu.:  0.00   TRUE:2304      TRUE:2304     
##  Median : 12.14                                
##  Mean   : 38.84                                
##  3rd Qu.: 59.32                                
##  Max.   :328.57
```

```r
mrgActivity_df<-select(mrgActivity_df, -c("na.rm","meanSteps", "imputeRow"))
```
    
>  
>> The means found for each (dayOfWeek-interval) pair were confirmed in a pivot-table anlysis of the activity file in MS Excel.  
>>  
>> We peform the left-join with the main dataset on the left and means table on the right.
>> The imputation is performed by a filtering on the rows with steps = NA and writing the meanSteps value into the "steps" column. 
>>  
>> The resulting datset has a column clean up after the imputation is confirmed.  
>  
  
4.  Similar to part 1 of this assignment.  
    a.Make a histogram of the total number of steps taken each day and   
    b.Calculate and report the mean and median total number of steps taken per day. 
  
Dataset rebuild

```r
dailySteps<-filter(mrgActivity_df,!is.na(steps)) %>% group_by(date) %>% summarize(steps=sum(steps))
```

Plotting and Statistics with Imputations Included

```r
hist(dailySteps$steps, col="green", breaks=length(unique(dailySteps$steps)), main="Histogram of Daily Steps (Imputations)",ylab="Count of Dates", xlab="Daily Steps")
rug(dailySteps$steps)
abline(v=median(dailySteps$steps),col="blue",lwd=4)
abline(v=mean(dailySteps$steps),col="red",lwd=2)
```

![](PA1_template_files/figure-html/Part4_dailysteps_plottingAndStats-1.png)<!-- -->

```r
summary(dailySteps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8918   11015   10821   12811   21194
```
    
> Do these values differ from the estimates from the first part of the assignment?  
>  
>> And so, with imputations, we find also that the daily **mean** and daily **median** number of steps has changed. 
>>  
>><table><tr><th>Number of steps Statistics&nbsp;</th><th>&nbsp;**Mean**&nbsp;</th><th>&nbsp;**Median**&nbsp;</th></tr>  
<tr><td>First part (without imputations):&nbsp;</td><td>&nbsp;*10,766*&nbsp;</td><td>&nbsp;*10,765*&nbsp;</td></tr>   
<tr><td>First part (with    imputations):&nbsp;</td><td>&nbsp;*10,821*&nbsp;</td><td>&nbsp;*11,015*&nbsp;</td></tr></table>  
>  
> What is the impact of imputing missing data on the estimates of the total daily number of steps?  
>>  The mean daily steps increases by   10,821 - 10,766 = 55    steps.  
>>  And, the median increases by        11,015 - 10,765 = 250   steps.
  
    

## Are there differences in activity patterns between weekdays and weekends?

*(Instructions for this part of the analysis follows here.)*

For this part, the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the simulated plot provided in the instructions for this assignment.
  
**Simulated Plot**    
![](instructions_fig/sample_panelplot.png)<!-- -->.
  
Dataset rebuild
---

```r
intervalSteps<-mutate(mrgActivity_df, weekPart=as.factor(ifelse(dayOfWeek=="Saturday" | dayOfWeek=="Sunday","Weekend","Weekday"))) %>% group_by(weekPart,interval) %>% summarize(steps=mean(steps), na.rm=TRUE)
```

```
## `summarise()` has grouped output by 'weekPart'. You can override using the `.groups` argument.
```
Actual Weekday / Weekend Plot
---

```r
#Preferred look:  
#ggplot(data=intervalSteps, aes(x=interval, y=steps)) + geom_point(color="blue") +
#        geom_line(color="black") + facet_grid(rows = vars(weekPart)) +
#        ylab("Average Number of Steps") + xlab("Interval") +
#        theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Requested look
ggplot(data=intervalSteps, aes(x=interval, y=steps)) +
        geom_line(color="blue") + facet_grid(rows = vars(weekPart)) +
        ylab("Average Number of Steps") + xlab("5-Minute Interval") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

![](PA1_template_files/figure-html/Part5_weekdays_weekends-1.png)<!-- -->


