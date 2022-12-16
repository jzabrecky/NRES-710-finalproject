#### NRES 710 Final Project
### Predictor variable comparisons
## 11.7.2022

#### Loading libraries & data ####

# Libraries
library(tidyverse)
library(ggplot2)
library(lme4)
library(lubridate)
library(gridExtra)

# Data
rus_metabolism <- read.csv("russian_metabolism.csv")
sac_metabolism <- read.csv("sacramento_metabolism.csv")
ynez_metabolism <- read.csv("santaynez_metabolism.csv")

# Adding identifier column
rus_metabolism <- rus_metabolism %>% 
  mutate(river = "Russian")
sac_metabolism <- sac_metabolism %>% 
  mutate(river = "Sacramento")
ynez_metabolism <- ynez_metabolism %>% 
  mutate(river = "Santa Ynez")

# Merging dataset into one
all_rivers <- rbind(rus_metabolism, sac_metabolism, ynez_metabolism)
all_rivers$date <- date(all_rivers$date)

#### Creating plots #####

# Velocity
velocity_plot <- ggplot(data = all_rivers, aes(x = date, y = velocity, color = river)) +
  geom_point(show.legend = FALSE) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               limits = as.Date(c("2007-10-01", "2017-01-01"))) +
  labs(y = "Velocity (m / s)", x = NULL) +
  theme_bw()

# Depth
depth_plot <- ggplot(data = all_rivers, aes(x = date, y = depth, color = river)) +
  geom_point(show.legend = FALSE) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               limits = as.Date(c("2007-10-01", "2017-01-01"))) +
  labs(y = "Depth (m)", x = NULL) +
  theme_bw()

# Discharge
discharge_plot <- ggplot(data = all_rivers, aes(x = date, y = discharge, color = river)) +
  geom_point(show.legend = TRUE) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               limits = as.Date(c("2007-10-01", "2017-01-01"))) +
  labs(color = "River", y = expression(paste("Discharge m"^"3"*" / s")), x = NULL) +
  theme_bw()

# adding legend to free space in this plot
discharge_plot <- discharge_plot + theme(legend.position = c(0.2, 0.6),
                       legend.title = element_text(size=10, face="bold"),
                       legend.background = element_rect(size=0.5, linetype="solid", color = "black"))

# Water Temperature
temperature_plot <- ggplot(data = all_rivers, aes(x = date, y = temp.water, color = river)) +
  geom_point(show.legend = FALSE) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               limits = as.Date(c("2007-10-01", "2017-01-01"))) +
  labs(y = expression("Temperature ("*~degree*C*")"), x = NULL, color = "River") +
  theme_bw()

# Day Length
day_plot <- ggplot(data = all_rivers, aes(x = date, y = day.length, color = river)) +
  geom_point(show.legend = FALSE) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               limits = as.Date(c("2007-10-01", "2017-01-01"))) +
  labs(y = "Hours of Daylight", x = NULL) +
  theme_bw()

# Solar Radiation
solar_plot <- ggplot(data = all_rivers, aes(x = date, y = shortwave, color = river)) +
  geom_point(show.legend = FALSE) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               limits = as.Date(c("2007-10-01", "2017-01-01"))) +
  labs(y = expression(paste("W / m"^"2")), x = NULL) +
  theme_bw()

combined_discharge <- grid.arrange(discharge_plot, depth_plot, velocity_plot, nrow = 3)
combined_temperature <- grid.arrange(temperature_plot, day_plot, solar_plot, nrow = 3)
combined <- grid.arrange(combined_discharge, combined_temperature, ncol = 2)
