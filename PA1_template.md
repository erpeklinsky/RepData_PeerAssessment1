# Personal Activity Monitoring Data
### Reproducible Research: Course Project 1

An activity monitoring device collects data from an anonymous individual at 5 minute intervals through out the day. The data consists of two months of data collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### Loading and processing the data  

First, the data must be loaded into R and analyzed to determine how many observations 
and variables are in the data set.

```{r, echo = TRUE}
unzip(zipfile = "activity.zip")
data <- read.csv("activity.csv")
str(data)
```

There are a total of 17,568 observations in this dataset.  

The variables included in this dataset are:  

+ steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
+ date: The date on which the measurement was taken in YYYY-MM-DD format  
+ interval: Identifier for the 5-minute interval in which measurement was taken    

### What is the mean total number of steps taken per day?  

1. Determine the total number of steps taken each day.
2. Create a histogram to visualize the data.

```{r, echo = TRUE}
sum_eachday <- tapply(data$steps, data$date, sum, na.rm = T)
hist(sum_eachday, xlab = "Total Steps per Day", main = "Histogram of Total Steps per Day", breaks = 25, col = "orange")
```

3. Calculate the mean and median of the total number of steps taken per day.

```{r}
mean_perday <- round(mean(sum_eachday))
print(mean_perday)
median_perday <-round(median(sum_eachday))  
print(median_perday)
```

### What is the average daily activity pattern?  

1. Determine the average number of steps (across all days) taken for each 5 minute interval. Create a time series plot of the 5-minute interval and average number of 
steps taken (across all days).

```{r}
average_perinterval <- tapply(data$steps, data$interval, mean, na.rm = T)
plot(average_perinterval ~ unique(data$interval), xlab = "5-minute Intervals", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern", type = "l")
```

2. Identify the 5-minute interval that, on average of all days in the dataset, has the maximum number of steps.

```{r}
average_perinterval[which.max(average_perinterval)]
```

### Inputting Missing Values  

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs). First determine which variables contain "NA"

```{r}
totalnasteps <- is.na(data$steps)
sum(totalnasteps)

totalnadate <- is.na(data$date)
sum(totalnadate)

totalnainterval <- is.na(data$interval)
sum(totalnainterval)
```

Only the steps variable contains NA and there are 2,304.

2. In order to fill in the missing NA values in the steps variable, the mean (of steps) of the corresponding interval will be taken as the replacing value. 

3. A new data set will be created to fill in the missing ("NA") values.

```{r}
data2 <- data
for(i in 1:nrow(data)){
  if(is.na(data$steps[i])){
    data2$steps[i] <- average_perinterval[[as.character(data[i,"interval"])]]
  }
}
```

4. Create a histogram of total number of steps taken each day by first determining how many steps were taken each day with the new data set.Graph both the data sets side by side to get a better comparison.

```{r}
sum_eachday2 <- tapply(data2$steps, data2$date, sum, na.rm = T)
par(mfrow = c(1,2))
hist(sum_eachday, xlab = "Total Steps per Day", main = "With NAs", breaks = 25, col = "orange", ylim = c(0,15))
hist(sum_eachday2, xlab = "Total Steps per Day", main = "Without NAs", breaks = 25, col = "blue", ylim = c(0,15))
```

Calculate the mean and median of the total number of steps taken per day for the new data set.

```{r}
mean_perday2 <- round(mean(sum_eachday2))
print(mean_perday2)
median_perday2 <-round(median(sum_eachday2))  
print(median_perday2)
```

Comparing the mean and median of the data before removing the NAs

```{r}
compare <- data.frame(mean = c(mean_perday, mean_perday2), median = c(median_perday, median_perday2))
rownames(compare) <- c("With NAs", "Without NAs")
print(compare)
```

The new data set has a mean and median that are higher than the original data set. By inputting estimated data into the "NAs" the data is scewed higher than what it might be.

### Are the differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
data2$DOW = as.factor(ifelse(is.element(weekdays(as.Date(data2$date)), weekdays), "Weekday", "Weekend"))
data3 <- aggregate(steps ~ interval + DOW, data2, mean)

```

2. Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days. 

```{r}
library(lattice)
xyplot(data3$steps ~ data3$interval|data3$DOW, main = "Average Number of Steps by Interval", xlab = "Invertval", ylab = "Number of Steps", layout = c(1,2), type = "l")

```
