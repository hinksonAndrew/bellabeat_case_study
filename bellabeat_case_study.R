#load libraries
library(tidyverse)
library(lubridate)

#Dataframes / Tibbles

hourly_steps <- read.csv('Fitabase_Data_4.12.16-5.12.16/hourlySteps_merged.csv')
daily_activity <- read.csv('Fitabase_Data_4.12.16-5.12.16/dailyActivity_merged.csv')
daily_sleep <- read.csv('Fitabase_Data_4.12.16-5.12.16/sleepDay_merged.csv')

#hourly_steps
#convert ActivityHour to datetime
hourly_steps$ActivityHour <- mdy_hms(hourly_steps$ActivityHour, tz = "UTC")

#add column for active hour no date
hourly_steps$active_hour <- as.integer(format(hourly_steps$ActivityHour, "%H"))
#add column for date only
hourly_steps$active_date <- as.Date(hourly_steps$ActivityHour)

#inspecting hourly_steps
summary(hourly_steps)
str(hourly_steps)

#daily_sleep
#inspect daily_sleep
str(daily_sleep)

#convert SleepDay to consistent date format
daily_sleep$SleepDay <- parse_date_time(daily_sleep$SleepDay, orders = "mdy HMS")
#add a column just for date
daily_sleep$sleep_date <- as.Date(daily_sleep$SleepDay)

#inspect daily_sleep
str(daily_sleep)
summary(daily_sleep)
#count unique values
length(unique(daily_sleep$Id))


#Analysis
#find out the total and average steps per hour
steps_per_hour_avg_total <- hourly_steps %>% 
  group_by(active_hour) %>% 
  summarise(total_steps = sum(StepTotal),
            avg_steps = mean(StepTotal))

#find out the steps per day per user
steps_per_day <- hourly_steps %>% 
  group_by(Id, active_date) %>% 
  summarise(total_steps_per_day = sum(StepTotal))

#find out the average amount of sleep per user per day
daily_sleep_avg <- daily_sleep %>% 
  group_by(Id) %>% 
  summarise(avg_sleep = mean(TotalMinutesAsleep))

#Visualizations
#avg_steps per hour
ggplot(steps_per_hour_avg_total,
       aes(x = active_hour, y = avg_steps)) +
  geom_col() +
  scale_x_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 23)) +
  labs(
    title = "Average Steps Taken Per Hour",
    x = "Active Hour",
    y = "Average Steps Taken"
  )

#daily_sleep_avg - Average sleep per day per user
ggplot(daily_sleep_avg,
       aes(x = as.character(Id), y = avg_sleep)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))
                     
  