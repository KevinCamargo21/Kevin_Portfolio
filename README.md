# Capstone-Project

# Cyclistic: Members vs Casual Users 

## Project Overview

* The goal for this analysis is to find out how annual members and casual riders use Cyclistic bikes
differently.

* This analysis will help the marketing team develop a strategy to convert casual riders into annual members.

## Code and Resources Used
* **R Version:** 3.6.3
* **Packages:** tidyverse, janitor, skimr, formatR, readr, ggplot2, lubridate
* **Data Source:** Cyclistic trip data from Q2 2019 to Q1 2020

## Importing Datasets

`q2_2019 <- read.csv('Cyclistic_Trips_2019_Q2.csv')`

`q3_2019 <- read.csv('Cyclistic_Trips_2019_Q3.csv')`

`q4_2019 <- read.csv('Cyclistic_Trips_2019_Q4.csv')`

`q1_2020 <- read.csv('Cyclistic_Trips_2020_Q1.csv')`

## Comparing Column Names

`colnames(q2_2019)`

`colnames(q3_2019)`

`colnames(q4_2019)`

`colnames(q1_2020)`

## Data Cleaning 

### *Renaming Columns for Consistency*

`q4_2019 <- rename(q4_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid
                  ,started_at = start_time
                  ,ended_at = end_time
                  ,start_station_name = from_station_name 
                  ,start_station_id = from_station_id 
                  ,end_station_name = to_station_name 
                  ,end_station_id = to_station_id 
                  ,member_casual = usertype)`

`q3_2019 <- rename(q3_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid
                  ,started_at = start_time
                  ,ended_at = end_time
                  ,start_station_name = from_station_name
                  ,start_station_id = from_station_id
                  ,end_station_name = to_station_name
                  ,end_station_id = to_station_id
                  ,member_casual = usertype)`

`q2_2019 <- rename(q2_2019
                  ,ride_id = X01...Rental.Details.Rental.ID
                  ,rideable_type = X01...Rental.Details.Bike.ID
                  ,started_at = X01...Rental.Details.Local.Start.Time  
                  ,ended_at = X01...Rental.Details.Local.End.Time  
                  ,start_station_name = X03...Rental.Start.Station.Name
                  ,start_station_id = X03...Rental.Start.Station.ID
                  ,end_station_name = X02...Rental.End.Station.Name 
                  ,end_station_id = X02...Rental.End.Station.ID
                  ,member_casual = User.Type)`

`colnames(q1_2020)`

`colnames(q4_2019)`

`colnames(q3_2019)`

`colnames(q2_2019)`

### *Inspecting Dataframes*

`str(q1_2020)`

`str(q4_2019)`

`str(q3_2019)`

`str(q2_2019)`

### *Converting ride_id and rideable_type Columns to Characters*  

`q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))`

`q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))`

`q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))`

`str(q1_2020)`

`str(q4_2019)`

`str(q3_2019)`

`str(q2_2019)`

### *Stacking All Data Into One Dataframe*

`all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)`

### *Removing lat, long, birthyear, and gender Fields*

`all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, X01...Rental.Details.Duration.In.Seconds.Uncapped
            ,X05...Member.Details.Member.Birthday.Year, Member.Gender, tripduration))`

### *Inspecting New Dataframe*

`colnames(all_trips)`

`nrow(all_trips)`

`dim(all_trips)`

`head(all_trips)`

`tail(all_trips)`

`str(all_trips)`

`summary(all_trips)`

### *member_casual Column Consolidated From 4 to 2 Labels*

`all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual, 'Subscriber' = 'member', 'Customer' = 'casual'))`

`table(all_trips$member_casual)`

### *Adding month, day, year, and day_of_week Columns*

`all_trips$date <- as.Date(all_trips$started_at)`

`all_trips$month <- format(as.Date(all_trips$date), '%m')`

`all_trips$day <- format(as.Date(all_trips$date), '%d')`

`all_trips$year <- format(as.Date(all_trips$date), '%Y')`

`all_trips$day_of_week <- format(as.Date(all_trips$date), '%A')`

## Data Analysis 

### *Adding ride_length Column: Calculating ended_at - started_at Columns*

`all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)`

`str(all_trips)`

`summary(all_trips)`

### *Removing ride_length <0 and start_station_name = "HQ QR" (Bad Data)*

`all_trips_v2 <- all_trips[!(all_trips$start_station_name == 'HQ QR'| all_trips$ride_length <0),]`

### *ride_length Average, Midpoint, Min, and Max analysis*

`mean(all_trips_v2$ride_length)`

`median(all_trips_v2$ride_length)`

`max(all_trips_v2$ride_length)` 

`min(all_trips_v2$ride_length)`  

### *Comparing Average, Midpoint, Min, and Max: Members vs Casual* 

`aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)` 

`aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)` 

`aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)` 

`aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)`  

### *Average Ride Length by Day: Members vs Casual*

`aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)`

### *Fixing day_of_week Order* 

`all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday'
                                                                         ,'Friday', 'Saturday'))`

`aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)`

### *Analyzing Data by Type and Weekday*

`all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)`

## Data Visualization 

### *Bar Chart: Number of Rides, Rider Type, Day of Week*

`mindate <- min(all_trips_v2$year)`

`maxdate <- max(all_trips_v2$year)`

`all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = 'dodge') + 
  labs(y = 'Number of Rides', x = 'Day of Week', title = 'Members vs Casual Users', subtitle = 'Number of Rides Per Day', fill = 'User Type'
       ,caption = paste0('Data From: ', 'Q2 ', mindate, ' to ', 'Q1 ', maxdate)) + facet_wrap(~member_casual)`

![number_of_rides](https://user-images.githubusercontent.com/88723621/129129329-93393554-40be-4480-920c-33c3285153fd.png)

### *Bar Chart: Average Duration, Rider Type, Day of Week*

`all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = 'dodge') +
  labs(y = 'Average Ride Duration (sec)', x = 'Day of Week', title = 'Members vs Casual Users', subtitle = 'Average Ride Duration Per Day'
       ,fill = 'User Type', caption = paste0('Data From: ', 'Q2 ', mindate, ' to ', 'Q1 ', maxdate)) + facet_wrap(~member_casual)`
       
![average_ride_duration](https://user-images.githubusercontent.com/88723621/129129213-58863d51-bceb-4508-ad01-73bd35f86831.png)

## Exporting Dataframe to .csv

`counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)`

`write.csv(counts, file = '~/Desktop/Cyclistic_Trip_Data/avg_ride_length.csv')`

[avg_ride_length.csv](https://github.com/KevinCamargo21/Capstone-Project/files/6972442/avg_ride_length.csv)


 
