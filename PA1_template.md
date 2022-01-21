---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library(readr)
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
library(knitr)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
setwd('/home/shinobi/Documents')

activity_data <- read_csv("activity.csv")
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

## What is mean total number of steps taken per day?


```r
activity_data_day <-activity_data %>%
  group_by(date) %>%
  summarize(steps_date = sum(steps))

hist(activity_data_day$steps_date, xlab = "Steps", ylab = "Number of days",main = "Histogram steps by day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
activity_data_summary <- activity_data %>%
  group_by(date) %>%
  summarise(mean_steps = mean(steps, na.rm = TRUE),
            median_steps = median(steps, na.rm = TRUE))

activity_data_summary
```

```
## # A tibble: 61 x 3
##    date       mean_steps median_steps
##    <date>          <dbl>        <dbl>
##  1 2012-10-01    NaN               NA
##  2 2012-10-02      0.438            0
##  3 2012-10-03     39.4              0
##  4 2012-10-04     42.1              0
##  5 2012-10-05     46.2              0
##  6 2012-10-06     53.5              0
##  7 2012-10-07     38.2              0
##  8 2012-10-08    NaN               NA
##  9 2012-10-09     44.5              0
## 10 2012-10-10     34.4              0
## # … with 51 more rows
```


## What is the average daily activity pattern?


```r
with(activity_data_summary, plot(date, mean_steps, type = "l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
max_interval <- activity_data %>%
  group_by(interval) %>%
  summarise(mean_interval = mean(steps, na.rm = TRUE)) %>%
  filter(mean_interval == max(mean_interval)) %>%
  select(interval)

print(paste("Interval with maximum mean steps is ", as.character(max_interval)))
```

```
## [1] "Interval with maximum mean steps is  835"
```


## Imputing missing values

In order to clean values in activity dataset, imputer strategy that was selected is median imputation grouped by date. In order to clean all null values dplyr?s mutate functionality was used. Due to the fact that some days had only null values double check for null values had to be used.


```r
missing_values <- activity_data %>%
  summarize(total_number = n(),
            missing_number = sum(is.na(steps)))

print(paste("Total number of records in dataset is ",missing_values$total_number, " and missing numer is ", missing_values$missing_number, " which is ", round(missing_values$missing_number / missing_values$total_number * 100,2), "% of total records."))
```

```
## [1] "Total number of records in dataset is  17568  and missing numer is  2304  which is  13.11 % of total records."
```

```r
activity_data_imputed <- activity_data %>%
  #group_by(date) %>%
  mutate(steps = ifelse(is.na(steps), 
                        ifelse(is.na(median(steps, na.rm = TRUE)), 0, median(steps, na.rm = TRUE)), 
                        steps))

hist(activity_data_imputed$steps, xlab = "Steps", ylab = "Number of days",main = "Histogram steps by day on imputed values")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?


```r
activity_data_imputed_weekday <- activity_data_imputed %>%
  mutate(weekday = as.factor(ifelse(wday(date) %in% c(1,6), "weekends", "weekdays")))

weekend <- activity_data_imputed_weekday %>%
  filter(weekday == "weekends") %>%
  group_by(interval) %>%
  summarize(mean_steps = mean(steps))

weekday <- activity_data_imputed_weekday %>%
  filter(weekday == "weekdays") %>%
  group_by(interval) %>%
  summarize(mean_steps = mean(steps))

par(mfrow = c(2,1))
with(weekday, plot(interval, mean_steps, type = "l", main = "weekday", ylab = "Number of steps", col= "blue"))
with(weekend, plot(interval, mean_steps, type = "l", main = "weekend", ylab = "Number of steps", col= "blue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
