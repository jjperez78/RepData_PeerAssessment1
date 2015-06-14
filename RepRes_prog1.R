# Libraries to load on at the begining
library(lubridate)
library(RColorBrewer)
library(lattice)
library(knitr)
library(dplyr)

# First we download the data and storage the moment we downloaded it.
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "Factivity.zip")
downloadDate <- format(Sys.time(), "%X %d-%m-%Y")

# Unzip the file and delete the zip file
unzip(zipfile = "Factivity.zip",exdir = ".")
file.remove("Factivity.zip")

# Load the data into the data fram activity.
# we expect three variables in this data frame: steps, date and interval
activity <- read.csv("activity.csv")

# Calculate the number of steps / each day usign dplyr

# Calculate the total number of steps. We correct also the names for each column
stepsDay <- summarize(group_by(activity,date),totalSteps = sum(steps,na.rm = TRUE))

# Generate the histogram for stepsDay
#   create a palette to represent the values

pal<-colorRampPalette(brewer.pal(12,"Paired"))
#   adjusting margins
par(mai=c(1.25,2.25,0.82,0.2))
# Create the plot
hist(stepsDay$totalSteps, col = pal(15),xlab="Total Steps / Day", main="Histogram. Total Steps per Day", nclass = 20)

# Now we calculate the mean/median of total steps per day using stepsDay
stepsDay_mean <- round(mean(stepsDay$totalSteps, na.rm = TRUE),digits=0)
stepsDay_median <- round(median(stepsDay$totalSteps, na.rm = TRUE),digits=0)

# Calculate the avg number of steps for each interval each day. We correct also the names for each column
AvgstepsInterval <- summarize(group_by(activity,interval),Avgsteps = mean(steps,na.rm = TRUE))

# Create the plot
plot(x=AvgstepsInterval$interval,y=AvgstepsInterval$Avgsteps,col=1,xlab="Interval in minutes",ylab="Average steps / day over period",main="Average steps / day for each 5 minutes interval", type="l")

# We calculate the interval with the maximum 
Interval_MaxAvgSteps <- AvgstepsInterval[AvgstepsInterval$Avgsteps == max(AvgstepsInterval$Avgsteps),"interval"]
Interval_MaxAvgSteps_Val <- AvgstepsInterval[AvgstepsInterval$Avgsteps == max(AvgstepsInterval$Avgsteps),"Avgsteps"]

# Calculate the number of NA presents on activity
Activity_NA <- sum(is.na(x = activity$steps))

# Now we fill the gaps using the average value when we find a NA.

# First we add a column with the avg number of steps for each interval
activityWithMeans<-merge(activity,AvgstepsInterval,by="interval")

# Search the NA rows and replace the value with the correspondent avg value
activityWithMeans[is.na(activityWithMeans$steps),"steps"]<-activityWithMeans[is.na(activityWithMeans$steps),"Avgsteps"]

# We select the appropiate columns and save it into a new variable 
activity_Filled <- arrange(select(activityWithMeans,3,1,2),date)

# Calculate again the total amount of steps per day, mean and median value of steps/day and we graph an histogram.

# Calculate the total number of steps. We correct also the names for each column
stepsDay_Fill <- summarize(group_by(activity_Filled,date),totalSteps = sum(steps,na.rm = TRUE))
# names(stepsDay_Fill) <- c("date","totalSteps")

# Generate the histogram for stepsDay
#   create a palette to represent the values
colors<-brewer.pal(12,"Paired")
pal<-colorRampPalette(colors)
#   adjusting margins
par(mai=c(1.25,2.25,0.82,0.2))
# Create the plot
hist(stepsDay_Fill$totalSteps, col = pal(15),xlab="Total Steps / Day", main="Histogram. Total Steps per Day", nclass = 20)

# Now we calculate the rest of statitics
# Now we calculate the mean/median of total steps per day using stepsDay
stepsDay_Fillmean <- round(mean(stepsDay_Fill$totalSteps, na.rm = TRUE),0)
stepsDay_Fillmedian <- round(median(stepsDay_Fill$totalSteps, na.rm = TRUE),0)

# The difference between these values and the original ones is
stepsDay_meanDiff <- stepsDay_Fillmean - stepsDay_mean
stepsDay_medianDiff <- stepsDay_Fillmedian - stepsDay_median

# Create a new factor using the weekday. The new factor has only two levels "Weekend" and "Weekday" and it is calculated from date
activity_FilledWD<-activity_Filled
activity_FilledWD$weekday<-as.POSIXlt(activity_FilledWD$date)$wday

# Update the values for weekdays and weekend days. In my configuration Monday is the first day of the week, so the weekdays will
# be numbers between 1 and 5 and weekend days will be 6 and 7
activity_FilledWD[activity_FilledWD$weekday>=6,"weekday"]<-"weekend"
activity_FilledWD[activity_FilledWD$weekday<=5,"weekday"]<-"weekday"

# Next we calculate the average number of steps for each interval depending on if it was weekday or weekend. I also adjust the names
Avgsteps_WDInterval <- summarize(group_by(activity_FilledWD,weekday,interval),Avgsteps = mean(steps,na.rm = TRUE))

# Print the plot
plot <- xyplot(Avgsteps~interval | weekday, Avgsteps_WDInterval,layout=c(1,2),type="l")
print(plot)
