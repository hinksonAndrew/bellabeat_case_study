#load librarys
library(tidyverse)
library(lubridate)

#create data frames

#daily_activity_4-12-16 thru 5-12-16
daily_activity_april <- read.csv('Fitabase_data_4.12.16-5.12.16/dailyActivity_merged.csv')
#sleep data 4-12-16 thru 5-12-16
sleep_day_april <- read.csv('Fitabase_data_4.12.16-5.12.16/sleepDay_merged.csv')

#taking a look at daily_activity_april
head(daily_activity_april)

#get the column names of daily_activity_april
colnames(daily_activity_april)

#get the column names of sleep_day_april
colnames(sleep_day_april)

#how many unique participants are in each?
n_distinct(daily_activity_april$Id)
n_distinct(sleep_day_april$Id)

#how many observations are in each?
nrow(daily_activity_april)
nrow(sleep_day_april)

#Quick summary stats

daily_activity_april %>%
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>% 
  summary()

sleep_day_april %>% 
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()

#Whats the relationship between steps taken
# in a day and sedentary minutes?
ggplot(daily_activity_april,
       aes(x = TotalSteps, y = SedentaryMinutes)) +
  geom_point()

#The more SedentaryMinutes the less steps
# a person takes and vice-versa (on average)

#Whats the relationship between minutes asleep
# and time in bed?
ggplot(sleep_day_april, 
       aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) +
  geom_point()

#Mostly linear where time in bed is equal to total
# time asleep but there are a few outliers.
# Would imagine its people just in bed but not asleep.

#Combine the sleep data and the daily activity data frames
combined_data <- merge(x = sleep_day_april, y = daily_activity_april, by = "Id", all = TRUE)

n_distinct(combined_data$Id)
nrow(combined_data)

ggplot(unique(combined_data),
       aes(x = TotalSteps, y = TotalMinutesAsleep)) +
  geom_point()
