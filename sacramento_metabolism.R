#### NRES 710 Final Project
### Modeling with each river separately (Sacramento River ver.)
## 11.8.2022

#### Loading libraries & data ####

# Libraries
library(tidyverse)
library(lubridate)
library(lme4)
library(car)

# Loading the dataset
sac_metabolism <- read.csv("sacramento_metabolism.csv")

# Inspecting relationships between variables
pairs(sac_metabolism[5:10])
cor(sac_metabolism[5:10])

# Log-transforming data for discharge and depth
sac_metabolism2 <- sac_metabolism %>% 
  mutate(discharge_log = log(discharge),
         depth_log = log(depth),
         velocity_log = log(velocity), 
         year = year(date))
sac_metabolism2 <- sac_metabolism2 %>% 
  select(year, site_name, GPP, ER, depth_log, temp.water, day.length, discharge_log, shortwave, velocity_log)

# Saving log-transformed as a new csv
#write.csv(sac_metabolism2, "sacramento_metabolism_final.csv", row.names = FALSE)

# Reinspecting relationships between variables
pairs(sac_metabolism2[5:10])
cor(sac_metabolism2[5:10])

#### Beginning modeling for GPP ####

# We do not have too many variables and I can think of reasons why each would relate to metabolism, 
# so to start I am going to test them all out with year included as a random effect
GPP_discharge <- lmer(GPP ~ discharge_log + (1 | year) + (1 | site_name), data = sac_metabolism2)
GPP_solar <- lmer(GPP ~ shortwave + (1 | year) + (1 | site_name), data = sac_metabolism2)
GPP_depth <- lmer(GPP ~ depth_log + (1 | year) + (1 | site_name), data = sac_metabolism2)
GPP_watertemp <- lmer(GPP ~ temp.water + (1 | year) + (1 | site_name), data = sac_metabolism2)
GPP_day <- lmer(GPP ~ day.length + (1 | year) + (1 | site_name), data = sac_metabolism2)
GPP_velocity <- lmer(GPP ~ velocity_log + (1 | year) + (1 | site_name), data = sac_metabolism2)

GPP_AIC_values <- data.frame(model = c("GPP_solar", "GPP_discharge", "GPP_depth", "GPP_watertemp", "GPP_day", "GPP_velocity"),
                             AIC = c(AIC(GPP_solar), AIC(GPP_discharge), AIC(GPP_depth), AIC(GPP_watertemp), AIC(GPP_day), AIC(GPP_velocity)))

# Day length was found to produce the best model (lowest AIC value)!
# Using cor() to see if there are any other variables I can add to improve the model
# that are not too correlated
cor(sac_metabolism2[5:10])

# Depth, discharge, and velocity are all options!
GPP_day_depth <- lmer(GPP ~ day.length + depth_log + (1 | year) + (1 | site_name), data = sac_metabolism2)
GPP_day_discharge <- lmer(GPP ~ day.length + discharge_log + (1 | year) + (1 | site_name), data = sac_metabolism2)
GPP_day_velocity <- lmer(GPP ~ day.length + velocity_log + (1 | year) + (1 | site_name), data = sac_metabolism2)

GPP_AIC_values <- rbind(GPP_AIC_values, c("GPP_day_depth", AIC(GPP_day_depth)))
GPP_AIC_values <- rbind(GPP_AIC_values, c("GPP_day_discharge", AIC(GPP_day_discharge)))
GPP_AIC_values <- rbind(GPP_AIC_values, c("GPP_day_velocity", AIC(GPP_day_velocity)))

# Depth with day length and depth produced the best model!
summary(GPP_day_depth)
vif(GPP_day_depth)

#### Beginning modeling for ER ####

# Going through all variables again with year included as a random effect
ER_discharge <- lmer(ER ~ discharge_log + (1 | year) + (1 | site_name), data = sac_metabolism2)
ER_solar <- lmer(ER ~ shortwave + (1 | year) + (1 | site_name), data = sac_metabolism2)
ER_depth <- lmer(ER ~ depth_log + (1 | year) + (1 | site_name), data = sac_metabolism2)
ER_watertemp <- lmer(ER ~ temp.water + (1 | year) + (1 | site_name), data = sac_metabolism2)
ER_day <- lmer(ER ~ day.length + (1 | year) + (1 | site_name), data = sac_metabolism2)
ER_velocity <- lmer(ER ~ velocity_log + (1 | year) + (1 | site_name), data = sac_metabolism2)

ER_AIC_values <- data.frame(model = c("ER_solar", "ER_discharge", "ER_depth", "ER_watertemp", "ER_day", "ER_velocity"),
                            AIC = c(AIC(ER_solar), AIC(ER_discharge), AIC(ER_depth), AIC(ER_watertemp), AIC(ER_day), AIC(ER_velocity)))

# Depth was found to produce the best model (lowest AIC value)!
# Using cor() to see if there are any other variables I can add to improve the model
# that are not too correlated
cor(sac_metabolism2[5:10])

# Depth, discharge, and velocity are all options!
ER_depth_solar <- lmer(ER ~ depth_log + shortwave + (1 | year) + (1 | site_name), data = sac_metabolism2)
ER_depth_watertemp <- lmer(ER ~ depth_log + temp.water + (1 | year) + (1 | site_name), data = sac_metabolism2)
ER_depth_day <- lmer(ER ~ depth_log + day.length + (1 | year) + (1 | site_name), data = sac_metabolism2)

ER_AIC_values <- rbind(ER_AIC_values, c("ER_depth_solar", AIC(ER_depth_solar)))
ER_AIC_values <- rbind(ER_AIC_values, c("ER_depth_watertemp", AIC(ER_depth_watertemp)))
ER_AIC_values <- rbind(ER_AIC_values, c("ER_depth_day", AIC(ER_depth_day)))

# Depth and hours of daylight produces the best model for ER!
summary(ER_depth_day)
vif(ER_depth_day)
