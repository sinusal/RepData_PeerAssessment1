#1.downloading and unzipping file from web
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")
unzip(zipfile="./data/Dataset.zip",exdir="./data") #unzipping

#Reading activity file


activity<-read.csv("./data/activity.csv")
head(activity)


#2.What is mean total number of steps taken per day?
# create and print number of steps per day (SPD)
SPD <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(SPD) <- c("Date", "Steps")
SPD

# draw the histogram
png("Plot1.png", width=480,height=480,units="px",bg="transparent")

library(ggplot2)
g <- ggplot(SPD, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkblue", fill="lightblue")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,18,2))

dev.off()

#Mean and median of total number of steps taken per day
mean(SPD$Steps, na.rm=TRUE)    ## [1] 10766.19
median(SPD$Steps, na.rm=TRUE)  ## [1] 10765

#3.What is the average daily activity pattern?
# create table with steps per time (SPT)
SPT <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)
# variable time (more comprensible for the graph axis)
SPT$time <- SPT$interval/100
# draw the line plot
png("Plot2.png", width=480,height=480,units="px",bg="transparent")
h <- ggplot(SPT, aes(time, steps))
h+geom_line(col="blue")+ggtitle("Average steps per time interval")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))
dev.off()

# 5-minute interval with the maximum number of steps
# table for dplyr
library(dplyr)
ST <- tbl_df(SPT)
# find the column
ST %>% select(time, steps) %>% filter(steps==max(ST$steps))    
             ## A tibble: 1 x 2
             ##     time steps
             ##     <dbl> <dbl>
             ##   1  8.35  206.

#4.Imputing missing values
#total number of missing values in the dataset
# table for dplyr
ACT <- tbl_df(activity)
# find the column
ACT %>% filter(is.na(steps)) %>% summarize(missing_values = n())
            ## # A tibble: 1 x 1
            ##   missing_values
            ##            <int>
            ## 1           2304


#strategy for filling in all of the missing values in the dataset
# values without NA are imputed in a new column with the rounded values of the average 5-minute interval 
activity$CompleteSteps <- ifelse(is.na(activity$steps), 
                                 round(SPT$steps
                                       [match(activity$interval,
                                              SPT$interval)],0), 
                                 activity$steps)

#new dataset that is equal to the original dataset but with the missing data filled in
# new dataset activityAll
activityAll <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)
# see first 10 values of the new dataset
head(activityAll, n=10)

# histogram of the total number of steps taken each day
SPDAll <- aggregate(activityAll$steps, list(activityAll$date), FUN=sum)
colnames(SPDAll) <- c("Date", "Steps")
# draw the histogram
png("Plot3.png", width=480,height=480,units="px",bg="transparent")
g <- ggplot(SPDAll, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkgreen", fill="lightgreen")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))
dev.off()

# mean and median of total number of steps taken per day
mean(SPDAll$Steps)        ##[1] 10765.64
median(SPDAll$Steps)      ##[1] 10762


#5.Are there differences in activity patterns between weekdays and weekends?
# Create variable with date in correct format
activityAll$RealDate <- as.Date(activityAll$date, format = "%Y-%m-%d")
# create a variable with weekdays name
activityAll$weekday <- weekdays(activityAll$RealDate)
# create a new variable indicating weekday or weekend
activityAll$DayType <- ifelse(activityAll$weekday=='Saturday' | 
                                activityAll$weekday=='Sunday', 
                              'weekend','weekday')

head(activityAll, n=10)

# panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken
# create table with steps per time across weekdaydays or weekend days
SPTDT <- aggregate(steps~interval+DayType,data=activityAll,
                   FUN=mean,na.action=na.omit)
# variable time (more comprensible for the graph axis)
SPTDT$time <- SPT$interval/100
# draw the line plot
png("Plot4.png", width=2000,height=600,units="px",bg="transparent")
j <- ggplot(SPTDT, aes(time, steps))
j+geom_line(col="darkred")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)
dev.off()


