activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
# day dataframe with total steps per day
day_df <- aggregate(activity$steps, by = list(activity$date), sum)
names(day_df)[1] <- "day"
names(day_df)[2] <- "steps"
# histogram of steps
hist(day_df$steps, main = "Histogram of the total number of steps taken each day",
     xlab = "total number of steps taken each day")
# mean and median of steps
mean(day_df$steps, na.rm = TRUE)
median(day_df$steps, na.rm = TRUE)
# create total steps per interval dataframe
sum_int_df <- aggregate(activity$steps, by = list(activity$interval), sum, na.rm = TRUE, 
                         na.action = NULL)
names(sum_int_df)[1] <- "interval"
names(sum_int_df)[2] <- "steps"

# create mean steps per interval dataframe
mean_int_df <- aggregate(activity$steps, by = list(activity$interval), mean, na.rm = TRUE, 
                              na.action = NULL)
names(mean_int_df)[1] <- "interval"
names(mean_int_df)[2] <- "mean_steps"

#time series plot
plot(mean_int_df$interval, mean_int_df$mean_steps, type = "n", main = "Time Series Plot per 5-minute interval", 
     xlab = "5-minute intervals", ylab = "Average number of steps taken")
lines(mean_int_df$interval, mean_int_df$mean_steps, type = "l")

# max steps

mean_int_df[which.max(mean_int_df$mean_steps), 1]

# Inputting missing values
sum(is.na(activity$steps))

mis_df <- merge(activity, mean_int_df, by = "interval", sort = FALSE)  # merge dataframes
mis_df <- mis_df[with(mis_df, order(date, interval)), ]  # sort on date and interval (optional)
# replace in steps column NA with value in mean_steps column
mis_df$steps[is.na(mis_df$steps)] <- mis_df$mean_steps[is.na(mis_df$steps)]
mis_df$mean_steps <- NULL 

mis_df$steps <- round(mis_df$steps, digits = 0)

# New dataframe
activity_new <- mis_df[, c(2, 3, 1)]

# new day dataframe with total steps per day
newday_df <- aggregate(activity_new$steps, by = list(activity_new$date), sum)
names(newday_df)[1] <- "day"
names(newday_df)[2] <- "steps"
# histogram of steps
hist(newday_df$steps, main = "Histogram of the total number of steps taken each day",
     xlab = "total number of steps taken each day")
# mean and median of steps
mean(newday_df$steps, na.rm = TRUE)
median(newday_df$steps, na.rm = TRUE)

# hist plot showing differences

par(mfrow = c(1, 2))

hist(day_df$steps, main = "(with NA)", xlab = "total number of steps taken each day")

hist(newday_df$steps, main = "(NA replaced)", xlab = "total number of steps taken each day")

# new data frame

newday_df2 <- newday_df

# create a factor with the names of the days for all dates
newday_df2$weekdays <- factor(format(newday_df2$date, "%A"))

levels(newday_df2$weekdays)

# replace the levels
levels(newday_df2$weekdays) <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), weekend = c("Saturday", "Sunday"))

newday_df2_mean_int <- aggregate(newday_df2$steps, by = list(newday_df2$weekdays, newday_df2$interval), mean, na.rm = TRUE, na.action = NULL)
names(newday_df2_mean_int)[1] <- "weekday"
names(newday_df2_mean_int)[2] <- "interval"
names(newday_df2_mean_int)[3] <- "mean.steps"

library(lattice)
xyplot(newday_df2_mean_int$mean_steps ~ newday_df2_mean_int$interval | 
         newday_df2_mean_int$weekday, layout = c(1, 2), type = "l", xlab = "Interval", 
       ylab = "Number of steps")