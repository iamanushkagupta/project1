library(tidyverse)
library(lubridate)
library(ggplot2)
getwd()
setwd("D:/DATA ANALYTICS CASE STUDY/2021_files/.csv files")
a1_2021 <- read_csv("202101-divvy-tripdata.csv")
a2_2021 <- read_csv("202102-divvy-tripdata.csv")
a3_2021 <- read_csv("202103-divvy-tripdata.csv")
a4_2021 <- read_csv("202104-divvy-tripdata.csv")
a5_2021 <- read_csv("202105-divvy-tripdata.csv")
a6_2021 <- read_csv("202106-divvy-tripdata.csv")
a7_2021 <- read_csv("202107-divvy-tripdata.csv")
a8_2021 <- read_csv("202108-divvy-tripdata.csv")
a9_2021 <- read_csv("202109-divvy-tripdata.csv")
a10_2021 <- read_csv("202110-divvy-tripdata.csv")
a11_2021 <- read_csv("202111-divvy-tripdata.csv")
a12_2021 <- read_csv("202112-divvy-tripdata.csv")
colnames(a1_2021)
colnames(a2_2021)
colnames(a3_2021)
colnames(a4_2021)
colnames(a5_2021)
colnames(a6_2021)
colnames(a7_2021)
colnames(a8_2021)
colnames(a9_2021)
colnames(a10_2021)
colnames(a11_2021)
colnames(a12_2021)
str(a1_2021)
str(a2_2021)
str(a3_2021)
str(a4_2021)
str(a5_2021)
str(a6_2021)
str(a7_2021)
str(a8_2021)
str(a9_2021)
str(a10_2021)
str(a11_2021)
str(a12_2021)
a1_2021 <-  mutate(a1_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
a2_2021 <-  mutate(a2_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
a3_2021 <-  mutate(a3_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
a4_2021 <-  mutate(a4_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
a5_2021 <-  mutate(a5_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
a6_2021 <-  mutate(a6_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
a7_2021 <-  mutate(a7_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
a8_2021 <-  mutate(a8_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
a9_2021 <-  mutate(a9_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
a10_2021 <-  mutate(a10_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
a11_2021 <-  mutate(a11_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
a12_2021 <-  mutate(a12_2021, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

all_trips <- bind_rows(a1_2021, a2_2021, a3_2021, a4_2021, a5_2021, a6_2021, a7_2021, a8_2021, a9_2021, a10_2021, a11_2021, a12_2021)

all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)
table(all_trips$member_casual)
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
str(all_trips)
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]


mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

summary(all_trips_v2$ride_length)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = "C:\\Users\\gupta\\OneDrive\\Desktop\\Ag_case_file\\avg_ride_length.csv")


