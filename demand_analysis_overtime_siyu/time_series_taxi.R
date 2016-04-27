library(lubridate)
library(gridExtra)
library(grid)
library(DataComputing)

####################################
######### read in the data #########
####################################

yellow <- read.csv("~/yellow.csv", comment.char="#")

####################################
########### aggregate the ##########
#########  count by 15 min #########
####################################
nMin = 15
yellow$count <- 1
yellow$pickup_datetime = strptime(yellow$pickup_datetime, "%Y-%m-%d %H:%M:%S")
yellow$dropoff_datetime = strptime(yellow$dropoff_datetime, "%Y-%m-%d %H:%M:%S")
agg_yellow_raw <- with(yellow,aggregate(as.vector(count), 
                             list(interval=pickup_datetime - as.numeric(pickup_datetime) %% (nMin * 60)),
                             FUN=sum))
time_range = range(agg_yellow_raw$interval)
agg_length = (as.numeric(time_range[2]) - as.numeric(time_range[1]))/(nMin * 60) + 1

agg_yellow_time <- data.frame(interval = time_range[1] + nMin * 60 * (0:(agg_length-1)))
agg_yellow <- merge(x = agg_yellow_time, y = agg_yellow_raw, 
                    by = "interval", all.x = TRUE, incomparables = 0)
agg_yellow$x[which(is.na(agg_yellow$x))] = 0
agg_yellow$day_of_week = format(agg_yellow$interval,'%A')
rm(agg_yellow_raw, agg_yellow_time)

###################################
######### plot the aggregated #####
############ data frame  ##########
###################################

png("../figure/data_agg_plot.png", width = 1600, height = 800)
ggplot(agg_yellow, aes(x = interval, y = x)) +
  geom_line() +
  ggtitle("Plot of Aggregated Counts (15-minute time intervals)") +
  labs(y = "Counts", x = "Time") 
dev.off()

############################
######### weekly trend #####
############################
agg_yellow <- agg_yellow %>%
  mutate(hour=hour(interval))

agg_yellow$day_of_week=factor(agg_yellow$day_of_week,levels = c("Monday", "Tuesday", "Wednesday", 
                  "Thursday", "Friday", 
                  "Saturday", "Sunday"))

png("../figure/week_trend.png", width = 1000, height = 300)
week_average <- agg_yellow %>%
  group_by(day_of_week)%>%
  summarise(avg=sum(x)/24)
week_trend <- agg_yellow %>%
  group_by(day_of_week,hour)%>%
  summarise(count=sum(x))%>%
  ggplot(aes(x=hour,y=count))+geom_line(aes(col=day_of_week))+facet_grid(.~day_of_week)+
  ggtitle("Plot of week trend") +
  labs(y = "Counts", x = "Hours") 

week_trend+geom_point(data=week_average,aes(x=12,y=avg,col=day_of_week))

dev.off()#returns the number and name of the new active device (after the specified device has been shut down)

##############################
###### plot daily demand #####
##############################

png("../figure/daily_demand.png", width = 1000, height = 500)
agg_yellow %>%
  mutate(day=day(interval))%>%
  group_by(day,day_of_week)%>%
  summarise(daily_count=sum(x)/(24*4))%>%
  ggplot(aes(x = day, y = daily_count)) + 
  geom_line() + ggtitle("Average Daily Demand in Sep") +
  labs(y = "Count in every 15 minutes", x="Day") +
  scale_x_continuous(breaks = seq(1, 30, by = 1))+
  geom_point(aes(x=day, y = daily_count,col= day_of_week))
dev.off()
