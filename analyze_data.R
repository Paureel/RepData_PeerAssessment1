#----------------------------------------------------------#
# analyze_data.R                                           #
# Made for the Coursera course                             # 
# Author: Aurel Prosz                                      #
# Version: 1.0                                             #
# Date: 28.10.2017                                         #
#----------------------------------------------------------#

Sys.setlocale("LC_TIME", "English")

datax <- read.csv("activity.csv", sep = ",", 
     na.strings = "NA")
datax$date <- as.Date(datax$date)
#----------------------------------------------#
datax.sums <-  aggregate(x = datax["steps"],
     FUN = sum,
     by = list(Group.date = datax$date), 
     na.rm=TRUE)

hist(datax.sums$steps, col = "red", 
     xlab = "Total number of steps", 
     breaks = 20, 
     main = "Histogram of total number of steps per day")

numberofsteps.median <- median(datax.sums$steps)
numberofsteps.mean <- mean(datax.sums$steps)


datax.mean <-  aggregate(x = datax["steps"],
     FUN = mean,
     by = list(datax$interval), 
     na.rm=TRUE)
plot( datax.mean$Group.1, datax.mean$steps, type = 'l', xlab = "Interval", ylab = "Averaged number of steps", main = "The 5-minute interval and the average number of steps taken, averaged across all days")

datax.mean.max <- max(datax.mean$steps, na.rm = TRUE)
maxrow <- which(datax.mean$steps == datax.mean.max)

navalues <- nrow(datax) - sum(complete.cases(datax))

datax.new <- datax
for (i in c(1:length(datax$steps))){
  datax$steps[1]
  if(is.na(datax$steps[i]))
    {
    index <- datax$interval[i]
    datax.new$steps[i] <- datax.mean$steps[which(datax.mean$Group.1 == index)]
  
  }
  
}



datax.new.sums <-  aggregate(x = datax.new["steps"],
                         FUN = sum,
                         by = list(Group.date = datax.new$date), 
                         na.rm=TRUE)

hist(datax.new.sums$steps, col = "red", 
     xlab = "Total number of steps", 
     breaks = 20, 
     main = "Histogram of total number of steps per day based on the imputed dataset")

numberofsteps.new.median <- median(datax.new.sums$steps)
numberofsteps.new.mean <- mean(datax.new.sums$steps)


datax.new.mutated <- mutate(datax.new, days = weekdays(datax.new$date))
datax.new.mutated_2 <- mutate(datax.new.mutated, days_class = ifelse(as.character(datax.new.mutated$days) %in% c("Sunday", "Saturday"), "Weekend", "Weekday"))

weekdays <- datax.new.mutated_2[datax.new.mutated_2$days_class == "Weekday", ]
weekends <- datax.new.mutated_2[datax.new.mutated_2$days_class == "Weekend", ]

weekdays.mean <-  aggregate(x = weekdays["steps"],
                             FUN = mean,
                             by = list(interval = weekdays$interval), 
                             na.rm=TRUE)
weekdays.mean <- mutate(weekdays.mean, day = "weekday")

weekends.mean <-  aggregate(x = weekends["steps"],
                            FUN = mean,
                            by = list(interval = weekends$interval), 
                            na.rm=TRUE)
weekends.mean <- mutate(weekends.mean, day = "weekend")

binded <- rbind(weekdays.mean, weekends.mean)
xyplot(steps ~ interval | day, data = binded, type = "l")