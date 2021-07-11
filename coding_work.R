library(dplyr)
library(ggplot2)

classes<-c("numeric","character","numeric")
activity_df<-read.csv("./data/activity.csv", colClasses=classes, header=TRUE)

activity_df$date<-as.Date(activity_df$date, "%Y-%m-%d")
str(activity_df)
head(activity_df)
summary(activity_df)

# dailySteps <- aggregate(activity_df$steps[!is.na(activity_df$steps)], by=list(activity_df$date[!is.na(activity_df$steps)]), FUN=sum)
# 
# names(dailySteps)[1]<-"date"
# names(dailySteps)[2]<-"steps"
# 
# ggplot(data=dailySteps, aes(x=date, y=steps, col="blue")) + geom_bar(stat="identity") +
#         scale_x_date(breaks="1 day",
#                      limits=c(as.Date("2012-10-01"),as.Date("2012-11-30"))) +
#         ylab("Steps") + xlab("Date") + 
#         theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# summary(dailySteps$steps)



dailySteps<-filter(activity_df,!is.na(steps)) %>% group_by(date) %>% summarize(steps=sum(steps))

ggplot(data=dailySteps, aes(x=date, y=steps)) + geom_bar(stat="identity", colour="blue", fill="blue", na.rm=TRUE) +
        scale_x_date(breaks="1 day",
                     limits=c(as.Date("2012-10-01"),as.Date("2012-11-30"))) +
        ylab("Steps") + xlab("Date") + 
        theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        ggtitle("Total Steps per Day")

#The above is really a barplot.  A histogram would be more like this.

hist(dailySteps$steps, col="green", breaks=length(unique(dailySteps$steps)), main="Histogram of Daily Steps",ylab="Count of Dates", xlab="Daily Steps")
rug(dailySteps$steps)
abline(v=median(dailySteps$steps),col="blue",lwd=4)
abline(v=mean(dailySteps$steps),col="red",lwd=2)


summary(dailySteps$steps)



intervalSteps<-filter(activity_df,!is.na(steps)) %>% group_by(interval) %>% summarize(steps=mean(steps), na.rm=TRUE)

ggplot(data=intervalSteps, aes(x=interval, y=steps)) + geom_point(color="blue") +
        geom_line(color="black") +
        ylab("Steps") + xlab("Interval") + 
        theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        ggtitle(("Average Steps per 5 Minute Time Interval"))

summary(intervalSteps$steps)
length(mrgActivity_df$steps[mrgActivity_df$steps==-999])
#Max steps on average in an interval are 206.170
maxIntervalSteps<-intervalSteps[intervalSteps$steps>205,]
maxIntervalSteps

head(arrange(intervalSteps,by_group=steps))
tail(arrange(intervalSteps,by_group=steps))

activity_df<-mutate(activity_df, dayOfWeek=weekdays(date))

dayIntMeanSteps<-filter(activity_df,!is.na(steps)) %>% group_by(dayOfWeek, interval) %>% summarize(meanSteps=mean(steps), na.rm=TRUE)

dayIntMeanSteps


mrgActivity_df<-left_join(activity_df,dayIntMeanSteps,by=c("dayOfWeek","interval"))
mrgActivity_df$imputeRow<-FALSE
mrgActivity_df$imputeRow[is.na(mrgActivity_df$steps)]<-TRUE
table(mrgActivity_df$imputeRow)
mrgActivity_df$steps[is.na(mrgActivity_df$steps)]<-mrgActivity_df$meanSteps[is.na(mrgActivity_df$steps)]

summary(mrgActivity_df[mrgActivity_df$imputeRow,])

mrgActivity_df<-select(mrgActivity_df, -c("na.rm","meanSteps", "imputeRow"))

intervalSteps<-mutate(mrgActivity_df, weekPart=as.factor(ifelse(dayOfWeek=="Saturday" | dayOfWeek=="Sunday","Weekend","Weekday"))) %>% group_by(weekPart,interval) %>% summarize(steps=mean(steps), na.rm=TRUE)
str(intervalSteps)

ggplot(data=intervalSteps, aes(x=interval, y=steps)) + geom_point(color="blue") +
        geom_line(color="black") + facet_grid(rows = vars(weekPart)) +
        ylab("Average Number of Steps") + xlab("Interval") +
        theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

