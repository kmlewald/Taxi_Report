library(lubridate)
library(gridExtra)
library(grid)
library(DataComputing)

####################################
######### read in the data #########
####################################

uber <- read.csv("~/stats133_final/uber.csv", quote="'")

####################################
########### aggregate the ##########
#########  count by 15 min #########
####################################
uber <- read.csv("~/Downloads/Code_Boiny_Gong/stats133_final/uber.csv", quote="'")
nMin = 15


colnames(uber) <- c("time","lat","lon","base","count")
uber$count <- 1
uber$time <- substring(uber$time, 2)

uber$time = strptime(uber$time, "%m/%d/%Y %H:%M:%S")
agg_uber_raw <- with(uber,aggregate(as.vector(count), 
                             list(interval=time - as.numeric(time) %% (nMin * 60)),
                             FUN=sum))
time_range = range(agg_uber_raw$interval)
agg_length = (as.numeric(time_range[2]) - as.numeric(time_range[1]))/(nMin * 60) + 1

agg_uber_time <- data.frame(interval = time_range[1] + nMin * 60 * (0:(agg_length-1)))
agg_uber <- merge(x = agg_uber_time, y = agg_uber_raw, 
                    by = "interval", all.x = TRUE, incomparables = 0)
agg_uber$x[which(is.na(agg_uber$x))] = 0
agg_uber$day_of_week = format(agg_uber$interval,'%A')
rm(agg_uber_raw, agg_uber_time)

###################################
######### plot the aggregated #####
############ data frame  ##########
###################################

png("../figure/data_agg_plot_uber.png", width = 1600, height = 800)
ggplot(agg_uber, aes(x = interval, y = x)) +
  geom_line() +
  ggtitle("Plot of Aggregated Counts (15-minute time intervals)") +
  labs(y = "Counts", x = "Time") 
dev.off()

############################
######### weekly trend #####
############################
agg_uber <- agg_uber %>%
  mutate(hour=hour(interval))

agg_uber$day_of_week=factor(agg_uber$day_of_week,levels = c("Monday", "Tuesday", "Wednesday", 
                  "Thursday", "Friday", 
                  "Saturday", "Sunday"))

png("../figure/week_trend_uber.png", width = 1000, height = 300)
week_average <- agg_uber %>%
  group_by(day_of_week)%>%
  summarise(avg=sum(x)/24)
week_trend <- agg_uber %>%
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

png("../figure/daily_demand_uber.png", width = 1000, height = 500)
agg_uber %>%
  mutate(day=day(interval))%>%
  group_by(day,day_of_week)%>%
  summarise(daily_count=sum(x)/(24*4))%>%
  ggplot(aes(x = day, y = daily_count)) + 
  geom_line() + ggtitle("Average Daily Demand in Sep") +
  labs(y = "Count in every 15 minutes", x="Day") +
  scale_x_continuous(breaks = seq(1, 30, by = 1))+
  geom_point(aes(x=day, y = daily_count,col= day_of_week))
dev.off()
