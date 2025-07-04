# install and load packages

install.packages(c("tidyverse", "lubridate", "dplyr", "tidyr", "janitor","ggplot2", "waffle", "corrplot"))

library(tidyverse)  # for data manipulation and visualization
library(lubridate)  # for working with dates and times
library(dplyr)      # for data manipulation
library(tidyr)      # for data tidying and reshaping
library(janitor)    # for data cleaning and formatting
library(ggplot2)    # for data visualization
library(waffle)     # for creating waffle charts
library(corrplot)   # for creating correlation plots

# import files

daily_activity <- read_csv("D:/Bellabeat Data 4.12.16-5.12.16/dailyActivity_merged.csv")
daily_sleep <- read_csv("D:/Bellabeat Data 4.12.16-5.12.16/sleepDay_merged.csv")
hourly_calories <- read_csv("D:/Bellabeat Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hourly_intensities <- read_csv("D:/Bellabeat Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
hourly_steps <- read_csv("D:/Bellabeat Data 4.12.16-5.12.16/hourlySteps_merged.csv")

# rename headers

daily_activity <- clean_names(daily_activity)
daily_sleep <- clean_names(daily_sleep)
hourly_calories <- clean_names(hourly_calories)
hourly_intensities <- clean_names(hourly_intensities)
hourly_steps <- clean_names(hourly_steps)

# show headers

colnames(daily_activity)
colnames(daily_sleep)
colnames(hourly_calories)
colnames(hourly_intensities)
colnames(hourly_steps)

# show data type and format

glimpse(daily_activity)
glimpse(daily_sleep)
glimpse(hourly_calories)
glimpse(hourly_intensities)
glimpse(hourly_steps)

# typecast columns: daily_activity

daily_activity <- daily_activity %>%
  mutate(id = as.character(id)) %>%
  mutate(activity_date = mdy(activity_date))

# typecast columns: daily_sleep

daily_sleep <- daily_sleep %>%
  mutate(id = as.character(id)) %>%
  mutate(sleep_day = mdy(sleep_day))

# typecast columns: hourly_calories

hourly_calories <- hourly_calories %>%
  mutate(id = as.character(id)) %>%
  mutate(activity_hour = mdy_hms(activity_hour))

# typecast columns: hourly_intensities

hourly_intensities <- hourly_intensities %>%
  mutate(id = as.character(id)) %>%
  mutate(activity_hour = mdy_hms(activity_hour))

# typecast columns: hourly_steps

hourly_steps <- hourly_steps %>%
  mutate(id = as.character(id)) %>%
  mutate(activity_hour = mdy_hms(activity_hour))

# rename headers: daily_activity

daily_activity <- daily_activity %>%
  rename(date = activity_date)

# rename headers: daily_sleep

daily_sleep <- daily_sleep %>%
  rename(date = sleep_day)

# rename headers: hourly_calories

hourly_calories <- hourly_calories %>%
  rename(date_time = activity_hour)

# rename headers: hourly_intensities

hourly_intensities <- hourly_intensities %>%
  rename(date_time = activity_hour)

# rename headers: hourly_steps

hourly_steps <- hourly_steps %>%
  rename(date_time = activity_hour)

# count incomplete observations

sum(!complete.cases(daily_activity))
sum(!complete.cases(daily_sleep))
sum(!complete.cases(hourly_calories))
sum(!complete.cases(hourly_intensities))
sum(!complete.cases(hourly_steps))

# count duplicated observations

sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_steps))

# remove duplicated observations

daily_sleep <- daily_sleep %>%
  distinct()

# count invalid observations

invalid_steps <- daily_activity %>%
  filter(total_steps==0)

nrow(invalid_steps)

# remove invalid observations

daily_activity <- daily_activity %>% 
  filter(total_steps!=0)

# create a column for total activity time

daily_activity_time <- daily_activity %>%
  mutate(total_minutes = very_active_minutes + fairly_active_minutes + lightly_active_minutes + sedentary_minutes)

# count invalid observations

invalid_time <- daily_activity_time %>%
  filter(total_minutes > 1440)

nrow(invalid_time)

# merge daily data frames

daily_activity_sleep <- inner_join(daily_activity, daily_sleep, by = c("id", "date"))

# merge hourly data frames

hourly_activity <- inner_join(hourly_calories, hourly_intensities, by = c("id", "date_time")) %>%
  inner_join(hourly_steps, by = c("id", "date_time"))

# count rows and users

nrow(daily_activity_sleep)
n_distinct(daily_activity_sleep$id)

nrow(hourly_activity)
n_distinct(hourly_activity$id)

# separate date and time in hourly_activity

hourly_activity <- hourly_activity %>%
  separate(col = date_time, into = c("date", "time"), sep = " ", remove = FALSE) %>%
  mutate(date = ymd(date), time = hms(time))

# add a column for day

daily_activity <- daily_activity %>%
  mutate(day = weekdays(date))

daily_activity_sleep <- daily_activity_sleep %>%
  mutate(day = weekdays(date))

hourly_activity <- hourly_activity %>%
  mutate(day = weekdays(date))

# add a column for non-sedentary activity

daily_activity <- daily_activity %>%
  mutate(total_active_minutes = very_active_minutes + fairly_active_minutes + lightly_active_minutes)

daily_activity_sleep <- daily_activity_sleep %>%
  mutate(total_active_minutes = very_active_minutes + fairly_active_minutes + lightly_active_minutes)

# add a column for awake time in bed

daily_activity_sleep <- daily_activity_sleep %>%
  mutate(total_minutes_awake = total_time_in_bed - total_minutes_asleep)

# remove columns

daily_activity <- daily_activity [, -c(4:10)]

daily_activity_sleep <- daily_activity_sleep [, -c(4:10)]

# sort columns

daily_activity <- daily_activity [, c(1, 2, 9, 8, 3, 10, 4:7)]

daily_activity_sleep <- daily_activity_sleep [, c(1, 2, 12, 8, 3, 13, 4:7, 11, 10, 14, 9)]

hourly_activity <- hourly_activity [, c(1:3, 9, 4:8)]

# sort rows

daily_activity <- daily_activity %>%
  arrange(id, date)

daily_activity_sleep <- daily_activity_sleep %>%
  arrange(id, date)

hourly_activity <- hourly_activity %>%
  arrange(id, date_time)

# preview daily_activity

head(daily_activity)

# preview daily_activity_sleep

head(daily_activity_sleep)

# preview hourly_activity

head(hourly_activity)



### USAGE RATE OF DEVICE FEATURES ###

# create a data frame

usage_by_features <- data.frame(device_feature = c("Activity Tracker", "Sleep Tracker"), no_of_users = c(n_distinct(daily_activity$id), n_distinct(daily_activity_sleep$id)))

# create a horizontal bar chart

ggplot(usage_by_features, aes(x = no_of_users, y = device_feature, fill = device_feature)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = no_of_users), position = position_stack(vjust = 0.5), color = "white", size = 10) +
  labs(x = "Number of Users", y = "Device Feature") + 
  scale_fill_manual(values = c("#946597", "#9A7BB3")) + 
  scale_y_discrete(labels = c("Activity\nTracker", "Sleep\nTracker")) +
  theme(panel.background = element_blank(), 
        axis.title.x = element_text(size = 25, color = "#6B2D7C", face = "bold"), 
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 25, color = c("#946597", "#9A7BB3")), 
        axis.ticks = element_blank(), 
        legend.position = "none")




### USER TYPE DISTRIBUTION ###

# create a data frame

user_type <- daily_activity %>%
  count(id, name = "days_used") %>%
  mutate(usage_level = cut(days_used, c(0, 10, 20, 31), labels = c("Low Usage", "Mid Usage", "High Usage"))) %>% 
  count(usage_level, name = "no_of_users") %>% 
  pivot_wider(names_from = usage_level, values_from = no_of_users) %>% 
  select("High Usage", "Mid Usage", "Low Usage")

# create a waffle chart

waffle(user_type, row = 3, size = 1, colors = c("#946597", "#9A7BB3", "#E3CBDA"), legend = "left") + 
  labs(caption = "1 Box = 1 User") + 
  guides(fill = guide_legend(title = "Usage Level", title.position = "top", title.hjust = 0.5, label.position = "bottom")) + 
  theme(plot.caption = element_text(size = 15, color = "#6B2D7C"), 
        legend.position = "top", 
        legend.title = element_text(size = 25, color = "#6B2D7C", face = "bold"), 
        legend.text = element_text(size = 15, color = "#6B2D7C"))




### USAGE RATE BY DAY IN A WEEK ###

# create a data frame

usage_by_day <- daily_activity %>%
  group_by(date, day) %>%
  summarise(no_of_users = n_distinct(id)) %>%
  group_by(day) %>%
  summarise(avg_no_of_users = round(mean(no_of_users), 1))

# create a vertical bar chart

ggplot(usage_by_day, aes(x = day, y = avg_no_of_users)) +
  geom_bar(aes(fill = day %in% c("Friday")), stat = "identity", width = 0.8) +
  geom_text(aes(label = avg_no_of_users), position = position_stack(vjust = 0.5), color = "white", size = 10, angle = 90, hjust = 0.5)  +
  labs(x = "Day", y = "Average Number of Users") +
  scale_fill_manual(values = c("#9A7BB3", "#946597"), guide = FALSE) +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  theme(panel.background = element_blank(),  
        panel.grid.major.x = element_blank(),  
        panel.grid.major.y = element_line(color = "grey"), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(color = "grey"),  
        axis.title.x = element_blank(),  
        axis.title.y = element_text(size = 25, color = "#6B2D7C", face = "bold"),  
        axis.text.x = element_text(size = 25, color = "#6B2D7C"),  
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        axis.line.x = element_line(size = 1))




### USAGE RATE BY HOUR IN A DAY ###

# create a data frame

usage_by_hour <- hourly_activity %>%
  group_by(date_time, hour = hour(date_time)) %>%
  summarise(no_of_users = n_distinct(id)) %>%
  group_by(hour) %>%
  summarise(avg_no_of_users = round(mean(no_of_users), 1))

# create a vertical bar chart

ggplot(usage_by_hour, aes(x = hour, y = avg_no_of_users)) +
  labs(x = "Hour", y = "Average Number of Users") +
  geom_line(color="#9A7BB3", size=5) +
  geom_point(color="#946597", size=10) +
  scale_x_continuous(breaks = seq(0, 23), labels = paste0(seq(0, 23))) +
  ylim(28, 32) +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "grey"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(color = "grey"),
        axis.title.x = element_text(size = 25, color = "#6B2D7C", face = "bold"),
        axis.title.y = element_text(size = 25, color = "#6B2D7C", face = "bold"),
        axis.text.x = element_text(size = 15, color = "#6B2D7C"),
        axis.text.y = element_text(size = 15, color = "#6B2D7C"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = 1))




### AVERAGE CALORIES BURNED BY HOUR ###

# create a data frame

calories_by_hour <- hourly_activity %>% 
  group_by(date_time, hour = hour(date_time)) %>% 
  summarise(calories_burned = mean(calories)) %>% 
  group_by(hour) %>% 
  summarise(avg_calories_burned = mean(calories_burned))

# compute max and min values for annotation

max_row <- which.max(calories_by_hour$avg_calories_burned)
min_row <- which.min(calories_by_hour$avg_calories_burned)

# create a line chart

ggplot(calories_by_hour, aes(x = hour, y = avg_calories_burned)) +
  geom_area(fill = "#FCC0C9", alpha = 0.7, color = "#C6687B", size = 5) +
  labs(x = "Hour", y = "Average Calories Burned") +
  scale_x_continuous(breaks = seq(0, 23), labels = paste0(seq(0, 23))) +
  ylim(0,170) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(color = "grey"),
        axis.title = element_text(size = 25, color = "#94384A", face = "bold"),
        axis.text = element_text(size = 15, color = "#94384A"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = 1),
        legend.position = "none") +
  
  # add annotation
  
  annotate("text", label = paste0("Highest: ", round(calories_by_hour$avg_calories_burned[max_row], 1)), x = calories_by_hour$hour[max_row], y = calories_by_hour$avg_calories_burned[max_row], vjust = -1, color = "#94384A", size = 6, fontface = "bold") +
  annotate("text", label = paste0("Lowest: ", round(calories_by_hour$avg_calories_burned[min_row], 1)), x = calories_by_hour$hour[min_row], y = calories_by_hour$avg_calories_burned[min_row]-5, vjust = 1, color = "#94384A", size = 6, fontface = "bold")





### AVERAGE STEPS TAKEN BY HOUR ###

# create a data frame

steps_by_hour <- hourly_activity %>%
  group_by(hour = hour(date_time), day = weekdays(date)) %>%
  reframe(avg_no_of_steps = mean(step_total))

# create heat map

ggplot(steps_by_hour, aes(x = hour, y = day, fill = avg_no_of_steps)) +
  geom_tile() + 
  labs(x = "Hour", y = "Day", fill = "Average Steps Taken") + 
  scale_x_continuous(breaks = seq(0, 23), labels = paste0(seq(0, 23))) + 
  scale_y_discrete(limits = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday")) + 
  scale_fill_gradient(low = "#FCC0C9", high = "#94384A", limits = c(2, 780)) + 
  guides(fill = guide_colorbar(barwidth = 20, title.position = "top", title.hjust = 0.5,label.position = "bottom")) + 
  theme(panel.background = element_blank(), panel.grid = element_blank(), 
        axis.title.x = element_text(size = 25, color = "#94384A", face = "bold"), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size = 15, color = "#94384A"), 
        axis.text.y = element_text(size = 25, color = "#94384A"), 
        axis.ticks.y = element_blank(), 
        legend.title = element_text(size = 25, color = "#94384A", face = "bold"), 
        legend.text = element_text(size = 12, color = "#94384A"), 
        legend.position = "top")





### AVERAGE ACTIVE TIME BY DAY ###

# create a data frame

active_time_by_day <- daily_activity %>%
  group_by(date, day) %>%
  summarise(across(c(very_active_minutes, fairly_active_minutes, lightly_active_minutes), mean)) %>%
  group_by(day) %>%
  summarise(across(c(very_active_minutes, fairly_active_minutes, lightly_active_minutes), ~ round(mean(.), 1), .names = "avg_{.col}")) %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "activity_intensity", values_to = "avg_active_minutes")

# compute overall average for annotation

overall_avg_active <- mean(daily_activity$total_active_minutes)

# create a vertical stacked bar chart 

ggplot(active_time_by_day, aes(x = day, y = avg_active_minutes, fill = activity_intensity)) +
  geom_bar(stat = "identity", width = 0.8) +
  labs(x = "Day", y = "Average Active Time (mins)", fill = "Activity Intensity") +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  scale_fill_manual(values = c("#EA7F8E", "#FCC0C9", "#B24459"), labels = c("Fairly Active", "Lightly Active", "Very Active")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, label.position = "bottom")) +
  theme(panel.background = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_line(color="grey"),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_line(color="grey"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25, color = "#94384A", face = "bold"),
        axis.text.x = element_text(size = 25, color = "#94384A"),
        axis.text = element_text(size = 15, color = "#94384A"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = 1),
        legend.position = "top",
        legend.title = element_text(size = 25, color = "#94384A", face = "bold"),
        legend.text = element_text(size = 15, color = "#94384A")) +
  
  # add annotation
  
  annotate("text", label = paste0("Overall Average: ", round(overall_avg_active, 1)), x = 7, y = overall_avg_active + 10, color = "#94384A", size = 5) +
  geom_hline(yintercept = overall_avg_active, color = "#94384A", linetype = "dashed", size = 1)



### AVERAGE TIME IN BED BY DAY ###

# create a data frame

bed_time_by_day <- daily_activity_sleep %>%
  group_by(date, day) %>%
  summarise(across(c(total_minutes_asleep, total_minutes_awake), mean)) %>%
  group_by(day) %>%
  summarise(across(c(total_minutes_asleep, total_minutes_awake), ~ round(mean(.), 1), .names = "avg_{.col}")) %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "asleep_awake", values_to = "avg_bedtime_minutes")

# compute overall average for annotation

overall_avg_bedtime <- mean(daily_activity_sleep$total_time_in_bed)

# create a vertical stacked bar chart

ggplot(bed_time_by_day, aes(x = day, y = avg_bedtime_minutes, fill = asleep_awake)) +
  geom_bar(stat = "identity", width = 0.8) +
  labs(x = "Day", y = "Average Time in Bed (mins)", fill = "Asleep or Awake?") +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  scale_fill_manual(values = c("#FCC0C9", "#B24459"), labels = c("     Time Asleep     ", "     Time Awake     ")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, label.position = "bottom")) +
  theme(panel.background = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_line(color="grey"),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_line(color="grey"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25, color = "#94384A", face = "bold"),
        axis.text.x = element_text(size = 25, color = "#94384A"),
        axis.text = element_text(size = 15, color = "#94384A"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = 1),
        legend.position = "top",
        legend.title = element_text(size = 25, color = "#94384A", face = "bold"),
        legend.text = element_text(size = 15, color = "#94384A")) +
  
  # add annotation
  
  annotate("text", label = paste0("Overall Average: ", round(overall_avg_bedtime, 1)), x = 1, y = overall_avg_bedtime + 18, color = "#94384A", size = 5) +
  geom_hline(yintercept = overall_avg_bedtime, color = "#94384A", linetype = "dashed", size = 1)



### CORRELATION: DAILY_ACTIVITY_SLEEP ###


# calculate correlation coefficients

corr_daily_activity_sleep <- daily_activity_sleep[, 4:13] %>%
  cor(method = "pearson")

# create a correlation plot

corrplot(corr_daily_activity_sleep, type = "upper", order = "hclust",
         col = colorRampPalette(c("#b2182b", "#ef8a62", "#fddbc7", "#d1e5f0", "#67a9cf", "#2166ac"), space = "rgb")(100),
         tl.col = "black",
         tl.cex = 0.8,
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.8,
         diag = FALSE,
         method = "circle")



### CORRELATION: HOURLY_ACTIVITY ###


# calculate correlation coefficients

corr_hourly_activity <- hourly_activity[, 6:9] %>%
  cor(method = "pearson")

# create a correlation plot

corrplot(corr_hourly_activity, type = "upper", order = "hclust",
         col = colorRampPalette(c("#b2182b", "#ef8a62", "#fddbc7", "#d1e5f0", "#67a9cf", "#2166ac"), space = "rgb")(100),
         tl.col = "black",
         tl.cex = 0.8,
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.8,
         diag = FALSE,
         method = "circle")



#### CORRELATION: DAILY SEDENTARY TIME AND SLEEP TIME ###

# compute correlation coefficient

cor_coef_1 <- cor(daily_activity_sleep$sedentary_minutes, daily_activity_sleep$total_minutes_asleep)

# create a scatter plot

ggplot(data = daily_activity_sleep, aes(x = sedentary_minutes, y = total_minutes_asleep, color = total_minutes_asleep)) + 
  geom_point(size = 6) + 
  geom_smooth(color = "#A5420B", size = 2) +
  labs(x = "Daily Sedentary Time (mins)", y = "Daily Sleep Time (mins)") +
  scale_color_gradient(low = "#962212", high = "#F09083") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "#D3D3D3"),
        panel.grid.minor = element_line(color="#D3D3D3"),
        axis.title = element_text(size = 25, color = "#962212", face = "bold"),
        axis.text = element_text(size = 15, color = "#962212"),
        axis.line = element_line(size = 1),
        legend.position = "none") +
  
  # add annotation
  
  annotate(geom = "text", label = paste0("correlation = ", round(cor_coef_1, 2)), x = 1250, y = 800, hjust = 1, vjust = 1, color = "#962212", size = 9)

# perform t-test

cor.test(daily_activity_sleep$sedentary_minutes, daily_activity_sleep$total_minutes_asleep, significance.level = 0.05)



### CORRELATION: DAILY ACTIVE TIME AND STEPS ###

# compute correlation coefficient

cor_coef_2 <- cor(daily_activity_sleep$total_active_minutes, daily_activity_sleep$total_steps)

# create a scatter plot

ggplot(data = daily_activity_sleep, aes(x = total_active_minutes, y = total_steps, color = total_steps)) + 
  geom_point(size = 6) + 
  geom_smooth(color = "#A5420B", size = 2) +
  labs(x = "Daily Active Time (mins)", y = "Daily Steps Taken") +
  scale_color_gradient(low = "#962212", high = "#F09083") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "#D3D3D3"),
        panel.grid.minor = element_line(color="#D3D3D3"),
        axis.title = element_text(size = 25, color = "#962212", face = "bold"),
        axis.text = element_text(size = 15, color = "#962212"),
        axis.line = element_line(size = 1),
        legend.position = "none") +
  
  # add annotation
  
  annotate(geom = "text", label = paste0("correlation = ", round(cor_coef_2, 2)), x = 540, y = 25000, hjust = 1, vjust = 1, color = "#962212", size = 9)

# perform t-test

cor.test(daily_activity_sleep$total_active_minutes, daily_activity_sleep$total_steps, significance.level = 0.05)



### CORRELATION: HOURLY STEPS AND CALORIES ###

# compute correlation coefficient

cor_coef_3 <- cor(hourly_activity$step_total, hourly_activity$calories)

# create a scatter plot

ggplot(data = hourly_activity, aes(x = step_total, y = calories, color = step_total)) + 
  geom_point(size = 6) + 
  geom_smooth(color = "#A5420B", size = 2) +
  labs(x = "Hourly Steps Taken", y = "Calories Burned (kcal)") +
  scale_color_gradient(low = "#962212", high = "#F09083") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "#D3D3D3"),
        panel.grid.minor = element_line(color="#D3D3D3"),
        axis.title = element_text(size = 25, color = "#962212", face = "bold"),
        axis.text = element_text(size = 15, color = "#962212"),
        axis.line = element_line(size = 1),
        legend.position = "none") +
  
  # add annotation
  
  annotate(geom = "text", label = paste0("correlation = ", round(cor_coef_3, 2)), x = 10500, y = 1000, hjust = 1, vjust = 1, color = "#962212", size = 9)

# perform t-test

cor.test(hourly_activity$step_total, hourly_activity$calories, significance.level = 0.05)



### CORRELATION: HOURLY INTENSITY LEVEL AND CALORIES ###

# compute correlation coefficient

cor_coef_4 <- cor(hourly_activity$average_intensity, hourly_activity$calories)

# create a scatter plot

ggplot(data = hourly_activity, aes(x = average_intensity, y = calories, color = average_intensity)) + 
  geom_point(size = 6) + 
  geom_smooth(color = "#A5420B", size = 2) +
  labs(x = "Average Intensity Level", y = "Calories Burned (kcal)") +
  scale_color_gradient(low = "#962212", high = "#F09083") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "#D3D3D3"),
        panel.grid.minor = element_line(color="#D3D3D3"),
        axis.title = element_text(size = 25, color = "#962212", face = "bold"),
        axis.text = element_text(size = 15, color = "#962212"),
        axis.line = element_line(size = 1),
        legend.position = "none") +
  
  # add annotation
  
  annotate(geom = "text", label = paste0("correlation = ", round(cor_coef_4, 2)), x = 3, y = 1000, hjust = 1, vjust = 1, color = "#962212", size = 9)

# perform t-test

cor.test(hourly_activity$average_intensity, hourly_activity$calories, significance.level = 0.05)

