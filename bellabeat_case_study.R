#load libraries
library(tidyverse)
library(lubridate)

#Dataframes / Tibbles
#create hourly_steps dataframe
hourly_steps <- read.csv('Fitabase_Data_4.12.16-5.12.16/hourlySteps_merged.csv')
daily_activity <- read.csv('Fitabase_Data_4.12.16-5.12.16/dailyActivity_merged.csv')

#hourly_steps
#convert ActivityHour to datetime
hourly_steps$ActivityHour <- mdy_hms(hourly_steps$ActivityHour, tz = "UTC")

#add column for active hour no date
hourly_steps$active_hour <- as.integer(format(hourly_steps$ActivityHour, "%H"))

#inspecting DF
summary(hourly_steps)
str(hourly_steps)

#daily_activity


#Analysis
#find out the total and average steps per hour
steps_per_hour_avg_total <- hourly_steps %>% 
  group_by(active_hour) %>% 
  summarise(total_steps = sum(StepTotal),
            avg_steps = mean(StepTotal))

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
