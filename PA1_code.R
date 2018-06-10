setwd("C:/Users/shrut/datasciencecoursera/Reproducible_Research/Project_1")
raw_activity_data <- read.csv("data/activity.csv", header = TRUE, stringsAsFactors = FALSE)
head(raw_activity_data)

raw_activity_data$date <- as.POSIXct(raw_activity_data$date, format = "%Y-%m-%d")

activity_data <- subset(raw_activity_data, !is.na(raw_activity_data$steps))
total_data <- aggregate(steps ~ date, activity_data, sum)
names(total_data) <- c("date", "total_steps")
head(total_data)

hist(total_data$total_steps, col = "cadetblue3", xlab = "Number of Steps", 
     main = "Histogram: Total Steps per Day")

mean(total_data$total_steps)
median(total_data$total_steps)

avg_data <- aggregate(steps ~ interval, activity_data, mean)
names(avg_data) <- c("interval", "average")
head(avg_data)

plot(avg_data$interval, avg_data$average, type = "l", col = "cadetblue3",
     lwd = 2, xlab = "Interval in Minutes", ylab = "Avg Steps",
     main = "Average Steps per Interval")

maximum <- which(avg_data$average == max(avg_data$average))
max_interval <- avg_data[maximum, 1]
print(max_interval)

missing_count <- sum(is.na(raw_activity_data$steps))
print(missing_count)

missing_rows <- which(is.na(raw_activity_data$steps))
avg <- rep(mean(raw_activity_data$steps, na.rm= TRUE), times = length(missing_rows))
raw_activity_data[missing_rows, "steps"] <- avg

total_data_nm <- aggregate(raw_activity_data$steps, by = list(raw_activity_data$date),
                           FUN = sum)
names(total_data_nm) <- c("date", "total_steps")
head(total_data_nm)

hist(total_data_nm$total_steps, col = "cadetblue3", xlab = "Number of Steps", 
     main = "Histogram: Total Steps per Day (No Missing Values)")

mean(total_data_nm$total_steps)
median(total_data_nm$total_steps)



raw_activity_data <- data.frame(date = raw_activity_data$date,
                                weekday = tolower(weekdays(raw_activity_data$date)),
                                steps = raw_activity_data$steps,
                                interval = raw_activity_data$interval)
raw_activity_data <- cbind(raw_activity_data,
                           daytype = ifelse(raw_activity_data$weekday == "saturday" |
                                              raw_activity_data$weekday == "sunday", "weekend", "weekday"))
activity_data_labels <- data.frame(date = raw_activity_data$date,
                                   weekday = raw_activity_data$weekday,
                                   daytype = raw_activity_data$daytype,
                                   interval = raw_activity_data$interval,
                                   steps = raw_activity_data$steps)

library(lattice)
avg_data_labels <- aggregate(activity_data_labels$steps, by = list(activity_data_labels$daytype,
                                                                    activity_data_labels$weekday,
                                                                   activity_data_labels$interval), mean)
names(avg_data_labels) <- c("Day_Type", "Weekday", "Interval", "Avg")

xyplot(Avg ~ Interval | Day_Type, avg_data_labels, type = "l", lwd = 1,
       xlab = "Interval", ylab = "Number of Steps", layout = c(1,2))
