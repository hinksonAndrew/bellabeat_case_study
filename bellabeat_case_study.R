#load libraries
library(tidyverse)
library(lubridate)

#Dataframes / Tibbles

hourly_steps <- read.csv('Fitabase_Data_4.12.16-5.12.16/hourlySteps_merged.csv')
daily_sleep <- read.csv('Fitabase_Data_4.12.16-5.12.16/sleepDay_merged.csv')
daily_activity <- read.csv('Fitabase_Data_4.12.16-5.12.16/dailyActivity_merged.csv')

#checking how many unique id's in each df
n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)
n_distinct(hourly_steps$Id)

### hourly_steps

#convert ActivityHour to datetime
hourly_steps$ActivityHour <- mdy_hms(hourly_steps$ActivityHour, tz = "UTC")

#add column for active hour no date
hourly_steps$active_hour <- as.integer(format(hourly_steps$ActivityHour, "%H"))
#add column for date only
hourly_steps$active_date <- as.Date(hourly_steps$ActivityHour)

#inspecting hourly_steps
summary(hourly_steps)
str(hourly_steps)

### daily_sleep

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

## daily_activity
str(daily_activity)
summary(daily_activity)

#convert ActivityDate to a consistent date format
daily_activity$ActivityDate <- parse_date_time(daily_activity$ActivityDate, orders = "mdy")

#check
summary(daily_activity)

#Analysis
#find out the total and average steps per hour
steps_per_hour_avg_total <- hourly_steps %>% 
  group_by(active_hour) %>% 
  summarise(total_steps = sum(StepTotal),
            avg_steps = mean(StepTotal))

#find out the steps per day per user
steps_per_user <- hourly_steps %>% 
  group_by(Id) %>% 
  summarise(total_steps_per_day = sum(StepTotal),
            avg_steps_per_day = mean(StepTotal))

#find out the total sleep per user and the average sleep 
# per day.
sleep_per_user <- daily_sleep %>% 
  group_by(Id) %>% 
  summarise(user_sleep_total = sum(TotalMinutesAsleep),
            user_avg_sleep = mean(TotalMinutesAsleep))

#summarise each user id
avg_daily_activity <- daily_activity %>% 
  group_by(Id) %>% 
  summarise(total_steps = sum(TotalSteps),
            avg_steps = mean(TotalSteps),
            avg_distance = mean(TotalDistance))

### dataset merging for various analysis

#inner join to only return rows where left table 
# has matching key to right table
steps_sleep_per_user <- merge(sleep_per_user, steps_per_user, by = "Id")

### Visualizations

# very busy -- looking for better solution
# y is both min asleep and steps taken
# doesn't seem to be any correlation between the two
# small group 
ggplot(steps_sleep_per_user,
       aes(as.character(Id))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_line(aes(y = user_avg_sleep),linetype = "dashed", color = "blue", group = 1) +
  geom_point(aes(y = user_avg_sleep), color = "blue") +
  geom_line(aes(y = avg_steps_per_day), group = 1) +
  geom_point(aes(y = avg_steps_per_day)) +
  labs(
    title = "Average Sleep vs Average Steps",
    x = "User Id",
    y = "Avg Sleep / Avg Steps"
  )

#avg_sleep_per_user
ggplot(steps_sleep_per_user,
       aes(x = as.character(Id), y = user_avg_sleep)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_bar(stat = "identity")

#avg_steps_per_day
ggplot(steps_sleep_per_user, 
       aes(x = as.character(Id), y = avg_steps_per_day)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_bar(stat = "identity")

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
