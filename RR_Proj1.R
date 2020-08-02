library(ggplot2)
library(dplyr)

## Read in the data to be processed and convert dates in date format

file_data <- "activity.csv"

data <- read.csv(file_data, header = TRUE, stringsAsFactors = FALSE)
data$date <- as.Date(as.character(data$date), "%Y-%m-%d")

##  calculate the number of steps taken per day

daily_data <- data %>%
        group_by(date) %>%
        summarize(daily_steps = sum(steps))

## Make a histogram of the total number of steps taken per day.

ggplot(daily_data, aes(daily_steps)) +
        geom_histogram(na.rm = TRUE, binwidth = 1000) +
        labs(title = "Number of Steps Taken per Day") +
        xlab("Daily Steps") +
        ylab("Number of Days") +
        theme(plot.title = element_text(hjust = 0.5))

## Calculate and report the mean and median of the total number of steps taken
## per day

daily_data %>%
        summarize(daily_mean = mean(daily_steps, na.rm = TRUE),
                  daily_median = median(daily_steps, na.rm = TRUE))

## Make a time series plot of the 5-minute internals and the average number
## of steps taken average across all days

int_data <- data %>%
        na.omit() %>%
        group_by(interval) %>%
        summarize(int_steps = mean(steps))

ggplot(int_data, aes(interval, int_steps)) +
        geom_line()

## Which 5 minutes interval, on average, contains the maximum number of steps

int_data %>%
        filter(int_steps == max(int_steps))

## Calculate and report the total number of missing values in the dataset

sapply(data, function(x) sum(is.na(x)))

## Replace each NA with the average of the non-NA for that interval across
## all other days

data_noNA <- data

for (i in 1:nrow(data)){
        if(is.na(data$steps[i])) {
                data_noNA$steps[i] <- 
                        int_data$int_steps[data_noNA$interval[i] 
                                           == int_data$interval]
        }
}

sapply(data_noNA, function(x) sum(is.na(x)))

##  calculate the number of steps taken per day with imputed data

daily_data2 <- data_noNA %>%
        group_by(date) %>%
        summarize(daily_steps = sum(steps))

## Make a histogram of the total number of steps taken per day with imputed data

ggplot(daily_data2, aes(daily_steps)) +
        geom_histogram(na.rm = TRUE, binwidth = 1000) +
        labs(title = "Number of Steps Taken per Day") +
        xlab("Daily Steps") +
        ylab("Number of Days") +
        theme(plot.title = element_text(hjust = 0.5))

## Calculate and report the mean and median of the total number of steps taken
## per day

daily_data2 %>%
        summarize(daily_mean = mean(daily_steps, na.rm = TRUE),
                  daily_median = median(daily_steps, na.rm = TRUE))

daily_data %>%
        summarize(daily_mean = mean(daily_steps, na.rm = TRUE),
                  daily_median = median(daily_steps, na.rm = TRUE))

## Create a new factor variable in the dataset with two levels "Weekday"
## and weekend indicating whether a given date

data_noNA$day_week <- ifelse(weekdays(data_noNA$date) %in% 
                            c("Saturday", "Sunday"), "weekend", "weekday")
data_noNA$day_week <- as.factor(data_noNA$day_week)

## Make a panel plot containing a time series plot of the 5-minute interval
## and the average number of steps taken across all weekdays and weekends

int_data2 <- data_noNA %>%
        group_by(day_week, interval) %>%
        summarize(int_steps = mean(steps))


ggplot(int_data2, aes(interval, int_steps)) +
        geom_line() +
        facet_grid(. ~ day_week) 
