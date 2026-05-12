library(tidyverse)
library(lubridate)

logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSq8sCLbN7Gk1WVCHG2rTKcyvYiCdYiMqBJAQurDNrVJkV_MM361DtHaR3KTDorBGXHnYUZSMvqa48Q/pub?gid=1103664544&single=true&output=csv")

bus_data <- logged_data %>%
  rename(
    timestamp = Timestamp,
    time_of_day = `What time of day was this bus trip?`,
    day_type = `Which day type was this observation recorded on?`,
    wait_minutes = `How many minutes did you wait for the bus?`,
    crowding = `How crowded was the bus when you boarded?`,
    on_time_status = `Was the bus on time based on the scheduled arrival?`,
    weather = `What was the weather like during this bus observation?`,
    people_waiting = `How many people were waiting at the bus stop?`
  ) %>%
  mutate(
    timestamp = dmy_hms(timestamp),
    wait_minutes = as.numeric(wait_minutes),
    people_waiting = as.numeric(people_waiting),
    time_of_day = factor(
      time_of_day,
      levels = c(
        "6:00-8:59 am",
        "9:00-11:59 am",
        "12:00-2:59 pm",
        "3:00-5:59 pm",
        "6:00 pm or later"
      )
    ),
    on_time_status = factor(
      on_time_status,
      levels = c("Early", "On time", "Late", "Not sure")
    )
  )

average_wait_by_time <- bus_data %>%
  group_by(time_of_day) %>%
  summarise(
    average_wait = mean(wait_minutes, na.rm = TRUE),
    number_of_observations = n()
  )

plot1 <- ggplot(average_wait_by_time, aes(x = time_of_day, y = average_wait)) +
  geom_col(fill = "#2B8CBE") +
  labs(
    title = "Average bus waiting time by time of day",
    x = "Time of day",
    y = "Average waiting time in minutes",
    caption = "Data collected from bus trip observations"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

plot1

ggsave("plot1.png", plot = plot1, width = 8, height = 5)


bus_status_data <- bus_data %>%
  filter(!is.na(on_time_status)) %>%
  filter(!is.na(wait_minutes)) %>%
  arrange(on_time_status)

plot2 <- ggplot(bus_status_data, aes(x = on_time_status, y = wait_minutes)) +
  geom_boxplot(fill = "#A6BDDB") +
  labs(
    title = "Bus waiting time by on-time status",
    x = "Bus on-time status",
    y = "Waiting time in minutes",
    caption = "This plot compares waiting times for early, on-time, late, and uncertain bus arrivals"
  ) +
  theme_minimal()

plot2

ggsave("plot2.png", plot = plot2, width = 8, height = 5)


bus_time_data <- bus_data %>%
  arrange(timestamp) %>%
  filter(!is.na(timestamp)) %>%
  filter(!is.na(wait_minutes))

plot3 <- ggplot(bus_time_data, aes(x = timestamp, y = wait_minutes)) +
  geom_point(color = "#045A8D") +
  geom_line(color = "#74A9CF") +
  labs(
    title = "Bus waiting time across observation times",
    x = "Observation timestamp",
    y = "Waiting time in minutes",
    caption = "Timestamp was converted into date-time values using lubridate"
  ) +
  theme_minimal()

plot3

ggsave("plot3.png", plot = plot3, width = 8, height = 5)
