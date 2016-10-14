#Assignment 1

##Getting data

We are going to unzip it and read the document it has using R.
```r
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip','data.zip')
c=unzip('data.zip')
b=read.csv(c)
library(dplyr)
library(ggplot2)

```

##Splitting the steps by dates and plotting the Histogram


```r
g=split(b$steps,b$date)
steps=sapply(g,sum)
hist(steps)
```
![Histogram](/plot1.png)

##Getting the mean

With the data that we have now,we will calculate the mean number of steps taken each day
```r
mean(steps,na.rm=TRUE)
```
The mean is 10766.19


##Getting the median

With the data that we have now,we will find the median of the total number of steps taken each day
```r
median(steps,na.rm=TRUE)
```
The median is 10765

##Time series plot
###Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days

We will first group the number of steps in the different interval and plot the time series thereafter.
```r
q = tapply(b$steps,b$interval,mean,na.rm= TRUE,simplify = TRUE)
plot(y = q, x = names(q), type = "l", xlab = "5-Minute-Interval", 
         main = "Daily Activity Pattern", ylab = "Average number of steps")
```
![TS plot](/plot2.png)


This is the time interval that contains the maximum no. of steps
```r
sort(q,decreasing = TRUE)[1]
```
835

206.1698

##Inputting missing values

This is the no. of rows with NA in the dataset
```r
w=complete.cases(b)
r=b[!w,]
nrow(r)
```
2304


We will now fill the N.A with the mean for that 5-min interval
```r
f=b
for( i in 1:nrow(f)) {
  if (is.na(f$steps[i])){
              e=f$interval[i]  
              t=q[as.character(e)]
       f$steps[i]=t[[1]]
  }
  
  
}
```

This is the new histogram
```r
h=split(f$steps,f$date)
StepsNew=sapply(h,sum)
hist(StepsNew)
```
![New Histogram](/plot3.png)

This is the new mean
```r
mean(StepsNew,na.rm=TRUE)
```
10766.19

This is the new median
```r
median(StepsNew,na.rm=TRUE)
```
10766.19

The values are the same as the initiate mean value.

##Activity pattern between weekdays and weekends

We gona indicate whether a given date is a weekday or weekend day.
```r
weekDay=as.Date(f$date)
weekDay=weekdays(weekDay)

newdata=cbind(f,weekDay)
levels(newdata$weekDay)<-list(weekday= c("Monday","Tuesday","Wednesday","Thursday","Friday"), weekend=c("Saturday","Sunday"))
```
We will now plot out the result
```r
v=newdata %>% group_by(interval,weekDay) %>% summarise(steps=mean(steps))
ggplot(v,aes(x=interval,y=steps),colour=weekDay) +geom_line()+facet_wrap(~weekDay, ncol = 1, nrow=2)
```
![Final Plot](/plot4.png)
