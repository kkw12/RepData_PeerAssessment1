# Reproducible Research: Peer Assessment 1




```r
knitr::opts_chunk$set(echo = TRUE,fig.path='Figs/')
```

## Loading and preprocessing the data

The first step is the retrieve the right data set from the web. The script is added below. 


```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              temp)
con <- unz(temp, "activity.csv")
dat <- read.table(con, header=T, sep=",")
unlink(temp)
```

Ensure the data fields are in the date formate


```r
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
library(markdown)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
dat$date<-as_date(dat$date)
head(dat)
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

## What is mean total number of steps taken per day?


```r
mean1<-dat%>%
  group_by(date)%>%
  summarize(steps=sum(steps))
  
mean2<-mean(mean1$steps,na.rm=TRUE)

median2<-median(mean1$steps,na.rm=TRUE)
    


cat("the mean of the total number or steps per day is ",mean2 ,"the median is ", median2)
```

```
## the mean of the total number or steps per day is  10766.19 the median is  10765
```
make histogram from data


```r
stepspd<-dat %>%
  #per date herewith drop the interval
  group_by(date) %>%
  #summarize total steps by taking the sum of steps per    #date
  summarize(totalstep=sum(steps))

#make plot
library(ggplot2)
#define ggplot
plot<-ggplot(stepspd,aes(x=date,y=totalstep))
#characteristics plot choose identity since already summarize date (otherwise a hist is plotted)
plot+geom_bar(stat="identity",color="black")+geom_hline(yintercept=mean(stepspd$totalstep,na.rm=TRUE))
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](Figs/histogram graphs-1.png)<!-- -->

```r
## What is the average daily activity pattern?
```
Plot timeseries steps per 5 minutes

```r
ts<-dat%>%
  group_by(interval)%>%
  summarize(average=mean(steps,na.rm=TRUE))
tsplot<-ggplot(ts,aes(x=interval,y=average))
tsplot+geom_line()
```

![](Figs/timeseries-1.png)<!-- -->

In which interval are the highest number of average steps and how much are these

```r
maximuminterval<-which.max(ts$average)
absolutemax<-max(ts$average)
cat("interval with maximum value is",maximuminterval)
```

```
## interval with maximum value is 104
```

```r
cat(", maximum value is", absolutemax)
```

```
## , maximum value is 206.1698
```

## Imputing missing values
first calculate the number of mission values and the percentage of missing value
replacing NA with mean for the interval
via merging and than mutate functions


```r
numberofmissingvalues<-count(dat[!complete.cases(dat),])
percentageofmissingvalue<-numberofmissingvalues/count(dat)
cat("number of missing values are", as.character(numberofmissingvalues))
```

```
## number of missing values are 2304
```

```r
cat(" ,percentage of missing values is", as.character(percentageofmissingvalue*100),"%")
```

```
##  ,percentage of missing values is 13.1147540983607 %
```

```r
#create identical dataframe with NA replace
new7<-dat%>%
  group_by(interval)%>%
  summarize(mean=mean(steps,na.rm=TRUE))%>%
  merge(dat)%>%
  mutate(steps=ifelse(is.na(steps),mean,steps))%>%
  group_by(date)%>%
  summarize(totalstep=sum(steps))

#make plot
library(ggplot2)
#define ggplot
plot5<-ggplot(new7,aes(x=date,y=totalstep))
#characteristics plot choose identity since already summarize date (otherwise a hist is plotted)
plot5+geom_bar(stat="identity",color="black")+geom_hline(yintercept=mean(new7$totalstep))
```

![](Figs/imputing mission values-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?
create identifier for weekends via wday() function and ifelse


```r
new6<-dat%>%
  group_by(interval)%>%
  summarize(mean=mean(steps,na.rm=TRUE))%>%
  merge(dat)%>%
  mutate(steps=ifelse(is.na(steps),mean,steps))%>%
  mutate(wd=wday(date))%>%
  mutate(ident=ifelse(wd==1|wd==7,"weekend","weekday"))%>%
  group_by(interval,ident)%>%
  summarize(mean=mean(steps))

#plot
plot2<-ggplot(new6,aes(x=interval,y=mean))
plot2+geom_line()+facet_grid(ident~.)
```

![](Figs/workday weekenddays-1.png)<!-- -->


