---
title: "Reproducible Research"
output: html_document
---

#### Assignment 1

##### STEP 1: Loading and preprocessing the data.

First, I set my working directory and I control if it exists. Then, I load the libraries that I will use for this assignment.

```{r, echo=TRUE}
setwd("C:/Documents and Settings/Sofia Cividini/Desktop")
file.exists("C:/Documents and Settings/Sofia Cividini/Desktop")
library(plyr)
library(lattice)
library(grid)
```

I load and process the data (data set: "activity.csv")

```{r, echo=TRUE}
activity <- read.csv("C:/Documents and Settings/Sofia Cividini/Desktop/activity.csv", header=TRUE,sep=",")
```

##### STEP 2: What is mean total number of steps taken per day ?

For this step:
* I select only the columns of my interest, that is 'steps' and 'date'. 
* I calculate the sum of the total number of steps taken per day.
* I make the histogram of the total number of steps taken each day.

```{r, echo=TRUE}
activity.1 <- activity[,1:2]
data.activity1 <- aggregate(.~date, FUN=sum, data=activity.1)
```
```{r, echo=TRUE}
hist(data.activity1$steps, xlab = "Total number of steps per day",
main = "Total number of steps per day from October to November 2012", 
col.main="bisque4", col = "beige", ylim=c(0,30), border = "bisque3")
```

* I calculate the total mean and median (taken per day) on all the days.
```{r,echo=TRUE}
steps.mean <- mean(data.activity1$steps)
steps.mean
steps.median <- median(data.activity1$steps)
steps.median
```

##### STEP 3: What is the average daily activity pattern ?

* I calculate the total mean of the steps (taken per day) within each 5-minute
interval on all the days.
* I create a continuos variable called 'time' which corresponds to the original discrete variable called 'interval' in order to adjust the x-axis for the next plot.
* I create the plot.

```{r,echo=TRUE}
data.activity4 <- aggregate(.~interval, FUN=mean, data=activity[,c(1,3)])
# I rename the column corresponding to the mean
names(data.activity4)[2] <- "mean"  
time <- c(1:288)
data.activity4 <- data.frame(data.activity4, time)
```
```{r,echo=TRUE}
par(cex="0.75")
plot(mean ~ time, data=data.activity4, type="l", col="steelblue3", xaxt = "n",
ylab="average number of the steps averaged across all days", col.main="tomato3",
xlab="5-minute intervals", main="Average number of the steps averaged across all days
at 5-minute intervals from October to November 2012")
axis(1, at=c(1,61,121,181,241), labels=c("0","500","1000","1500","2000"))
```

* I look at the maximum number of steps, and which is the 5-minute interval that
includes it.

```{r,echo=TRUE}
max.steps <- max(data.activity4$mean)
max.steps

data.activity5 <- subset(data.activity4, mean==max.steps)
data.activity5
```

##### STEP 4: Imputing missing values.

* I calculate and report the total number of missing value in the dataset.

```{r,echo=TRUE}
summary(activity)
```

<strong>Stategy used to fill in NAs.</strong>
* I calculate the daily mean of the number of steps taken per day on all days.
* Then, I use the daily mean of the day which follows the day with all missing values to fill in these NAs. Where it was not possible to use the day after, I used the daily mean of the day before (for example for 2012/11/30).

```{r,echo=TRUE}
data.activity2 <- aggregate(.~date, FUN=mean, data=activity.1)
# I rename the column corresponding to the mean
names(data.activity2)[2] <- "mean"
```
```{r,echo=TRUE}
data <- subset(data.activity2, date=="2012-10-02"|date=="2012-10-09"|
date=="2012-11-02"|date=="2012-11-05"|date=="2012-11-11"|date=="2012-11-12"|
date=="2012-11-15"|date=="2012-11-29")
data
```
```{r,echo=TRUE}

# I recode NA 
data.1.ott <- activity[1:288,2:3]
steps <- c(0.4375000)

data.8.ott <- activity[2017:2304,2:3]
steps <- c(44.4826389)

data.1.nov <- activity[8929:9216,2:3]
steps <- c(36.8055556)

data.4.nov <- activity[9793:10080,2:3]
steps <- c(36.2465278)

data.9.nov <- activity[11233:11520,2:3]
steps <- c(43.7777778)

data.10.nov <- activity[11521:11808,2:3]
steps <- c(37.3784722)

data.14.nov <- activity[12673:12960,2:3]
steps <- c(0.1423611)

data.30.nov <- activity[17281:17568,2:3]
steps <- c(24.4687500)
```

* I create a new data set without missing values.

```{r,echo=TRUE}
data.NA <- rbind(data.1.ott,data.8.ott,data.1.nov,data.4.nov,data.9.nov,
data.10.nov,data.14.nov,data.30.nov)
row.names(data.NA) <- NULL

activity.new <- activity[c(289:2016,2305:8928,9217:9792,10081:11232,
11809:12672,12961:17280), ]
row.names(activity.new) <- NULL

activity.new2 <- rbind(data.NA,activity.new)
activity.new2 <- activity.new2[order(activity.new2$date), ]
```

* I calculate the sum of the total number of steps taken per day.
* I make the histogram of the total number of steps taken each day.

```{r,echo=TRUE}
activity.new3 <- aggregate(.~date, FUN=sum, data=activity.new2)
```

```{r,echo=TRUE}
hist(activity.new3$steps, xlab = "Total number of steps per day",
main = "Total number of steps per day from October to November 2012 
(manipulated missing values)", col.main="bisque4", col = "beige", 
ylim=c(0,40), border = "bisque3")
```

* I calculate the total mean and median (taken per day) on all the days.

```{r,echo=TRUE}
steps.mean2 <- mean(activity.new3$steps)
steps.mean2
steps.median2 <- median(activity.new3$steps)
steps.median2
```

* I calculate the difference between the means and the meadians with and without NAs.

```{r,echo=TRUE}
diff.means <- steps.mean - steps.mean2
diff.means
diff.medians <- steps.median - steps.median2
diff.medians
```

##### STEP 5: Are there differences in activity patterns between weekdays and weekends?

* I associate a weekday to my dates. 

```{r,echo=TRUE}
weekdays <- weekdays(as.Date(activity.new2$date))
data.week <- data.frame(activity.new2, weekdays)
```

* I create the factor variable called 'daytype' and the corresponding data set.

```{r,echo=TRUE}
daytype <- c(rep("weekday",1440), rep("weekend",576))
data.p <- data.week[1:16128, ]
data.p <- data.frame(data.p, daytype)

daytype <- c("weekday")
data.p1 <- data.week[16129:17568, ]
data.p1 <- data.frame(data.p1,daytype)

data.week1 <- rbind(data.p, data.p1)
row.names(data.week1) <- NULL

```

* I calculate the average number of steps by interval and daytype on all days.

```{r,echo=TRUE}
data.week2 <- aggregate(.~interval*daytype, FUN=mean, data=data.week1[,c(1,3,5)])
# I rename the column corresponding to the mean
names(data.week2)[3] <- "mean"  
data.week2 <- edit(data.week2)
```

* I create another continuos variable called 'time1' which corresponds to the original discrete variable called 'interval' in order to adjust the x-axis for the next plot.
* I create the plot.

```{r,echo=TRUE}
time1 <- c(0:287)
data.week3 <- data.frame(data.week2, time1)
data.week3<- edit(data.week3)
```
```{r,echo=TRUE}
xyplot(mean ~ time1|daytype, data=data.week3, type="l", layout=c(1,2), xaxt="n",
ylab="Average number of steps", xlab="5-minute interval", col="steelblue3",
main=list("Average number of steps by weekday and weekend at 5-minute intervals
from October to November 2012 (manipulated missing values)", cex=0.90, col="bisque4"),
scales=list(x=list(at=seq(0,287, by=60), labels=seq(0,2000, by=500))))
```

