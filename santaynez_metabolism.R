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
ynez_metabolism <- read.csv("santaynez_metabolism.csv")

# Inspecting relationships between variables
pairs(ynez_metabolism[5:10])
cor(ynez_metabolism[5:10])

# Log-transforming data for discharge and depth
ynez_metabolism2 <- ynez_metabolism %>% 
  mutate(discharge_log = log(discharge),
         depth_log = log(depth),
         velocity_log = log(velocity), 
         year = year(date))
ynez_metabolism2 <- ynez_metabolism2 %>% 
  select(year, site_name, GPP, ER, depth_log, temp.water, day.length, discharge_log, shortwave, velocity_log)

# Saving log-transformed as a new csv
#write.csv(ynez_metabolism2, "santaynez_metabolism_final.csv", row.names = FALSE)

# Reinspecting relationships between variables
pairs(ynez_metabolism2[5:10])
cor(ynez_metabolism2[5:10])

#### Beginning modeling for GPP ####

# We do not have too many variables and I can think of reasons why each would relate to metabolism, 
# so to start I am going to test them all out with year included as a random effect
GPP_discharge <- lmer(GPP ~ discharge_log + (1 | year) + (1 | site_name), data = ynez_metabolism2)
GPP_solar <- lmer(GPP ~ shortwave + (1 | year) + (1 | site_name), data = ynez_metabolism2)
GPP_depth <- lmer(GPP ~ depth_log + (1 | year) + (1 | site_name), data = ynez_metabolism2)
GPP_watertemp <- lmer(GPP ~ temp.water + (1 | year) + (1 | site_name), data = ynez_metabolism2)
GPP_day <- lmer(GPP ~ day.length + (1 | year) + (1 | site_name), data = ynez_metabolism2)
GPP_velocity <- lmer(GPP ~ velocity_log + (1 | year) + (1 | site_name), data = ynez_metabolism2)

GPP_AIC_values <- data.frame(model = c("GPP_solar", "GPP_discharge", "GPP_depth", "GPP_watertemp", "GPP_day", "GPP_velocity"),
                             AIC = c(AIC(GPP_solar), AIC(GPP_discharge), AIC(GPP_depth), AIC(GPP_watertemp), AIC(GPP_day), AIC(GPP_velocity)))


# Solar (shortwave radiation) was found to produce the best model (lowest AIC value)!
# Using cor() to see if there are any other variables I can add to improve the model
# that are not too correlated
cor(ynez_metabolism2[5:10])

# Velocity, depth, discharge, and water temperature are all options
GPP_solar_velocity <- lmer(GPP ~ shortwave + velocity_log + (1 | year) + (1 | site_name), data = ynez_metabolism2)
GPP_solar_depth <- lmer(GPP ~ shortwave + depth_log + (1 | year) + (1 | site_name), data = ynez_metabolism2)
GPP_solar_discharge <- lmer(GPP ~ shortwave + discharge_log + (1 | year) + (1 | site_name), data = ynez_metabolism2)
GPP_solar_watertemp <- lmer(GPP ~ shortwave + temp.water + (1 | year) + (1 | site_name), data = ynez_metabolism2)

GPP_AIC_values <- rbind(GPP_AIC_values, c("GPP_solar_velocity", AIC(GPP_solar_velocity)))
GPP_AIC_values <- rbind(GPP_AIC_values, c("GPP_solar_depth", AIC(GPP_solar_depth)))
GPP_AIC_values <- rbind(GPP_AIC_values, c("GPP_solar_discharge", AIC(GPP_solar_discharge)))
GPP_AIC_values <- rbind(GPP_AIC_values, c("GPP_solar_watertemp", AIC(GPP_solar_watertemp)))

# Solar radiation and depth produced the best model. We can still try to add water temperature as it is
# < .8 correlated
cor(ynez_metabolism2[5:10])

GPP_solar_depth_watertemp <- lmer(GPP ~ depth_log + shortwave + temp.water + (1 | year) + (1 | site_name), data = ynez_metabolism2)

GPP_AIC_values <- rbind(GPP_AIC_values, c("GPP_depth_velocity_watertemp", AIC(GPP_solar_depth_watertemp)))

# Solar (shortwave radiation) depth, and water temperature were shown to produce the best model
summary(GPP_solar_depth_watertemp)
vif(GPP_solar_depth_watertemp)

#### Beginning modeling for ER ####

# Going through all variables again with year included as a random effect
ER_discharge <- lmer(ER ~ discharge_log + (1 | year) + (1 | site_name), data = ynez_metabolism2)
ER_solar <- lmer(ER ~ shortwave + (1 | year) + (1 | site_name), data = ynez_metabolism2)
ER_depth <- lmer(ER ~ depth_log + (1 | year) + (1 | site_name), data = ynez_metabolism2)
ER_watertemp <- lmer(ER ~ temp.water + (1 | year) + (1 | site_name), data = ynez_metabolism2)
ER_day <- lmer(ER ~ day.length + (1 | year) + (1 | site_name), data = ynez_metabolism2)
ER_velocity <- lmer(ER ~ velocity_log + (1 | year) + (1 | site_name), data = ynez_metabolism2)

ER_AIC_values <- data.frame(model = c("ER_solar", "ER_discharge", "ER_depth", "ER_watertemp", "ER_day", "ER_velocity"),
                            AIC = c(AIC(ER_solar), AIC(ER_discharge), AIC(ER_depth), AIC(ER_watertemp), AIC(ER_day), AIC(ER_velocity)))

# Depth was found to produce the best model (lowest AIC value)!
# Using cor() to see if there are any other variables I can add to improve the model
# that are not too correlated
cor(ynez_metabolism2[5:10])

# Day length, solar radiation, and water temperature are all options
ER_depth_day <- lmer(ER ~ depth_log + day.length + (1 | year) + (1 | site_name), data = ynez_metabolism2)
ER_depth_watertemp <- lmer(ER ~ depth_log + temp.water + (1 | year) + (1 | site_name), data = ynez_metabolism2)
ER_depth_solar <- lmer(ER ~ depth_log + shortwave + (1 | year) + (1 | site_name), data = ynez_metabolism2)

ER_AIC_values <- rbind(ER_AIC_values, c("ER_depth_day", AIC(ER_depth_day)))
ER_AIC_values <- rbind(ER_AIC_values, c("ER_depth_watertemp", AIC(ER_depth_watertemp)))
ER_AIC_values <- rbind(ER_AIC_values, c("ER_depth_solar", AIC(ER_depth_solar)))

# Discharge is actually also an option as correlation is > 7 but < 8
ER_depth_discharge <- lmer(ER ~ depth_log + discharge_log + (1 | year) + (1 | site_name), data = ynez_metabolism2)

ER_AIC_values <- rbind(ER_AIC_values, c("ER_depth_discharge", AIC(ER_depth_discharge)))

# Depth and water temperature were shown to produce the best model so far but we can still
# try variables including discharge, day length, and shortwave radiation as collinearity < 8
ER_depth_watertemp_discharge <- lmer(ER ~ depth_log + temp.water + discharge_log + (1 | year) + (1 | site_name), data = ynez_metabolism2)
ER_depth_watertemp_day <- lmer(ER ~ depth_log + temp.water + day.length + (1 | year) + (1 | site_name), data = ynez_metabolism2)
ER_depth_watertemp_solar <- lmer(ER ~ depth_log + temp.water + shortwave + (1 | year) + (1 | site_name), data = ynez_metabolism2)

ER_AIC_values <- rbind(ER_AIC_values, c("ER_depth_watertemp_discharge", AIC(ER_depth_watertemp_discharge)))
ER_AIC_values <- rbind(ER_AIC_values, c("ER_depth_watertemp_day", AIC(ER_depth_watertemp_day)))
ER_AIC_values <- rbind(ER_AIC_values, c("ER_depth_watertemp_solar", AIC(ER_depth_watertemp_solar)))

# Water temperature, depth, and discharge produces the best model so far
# And we can still try to add solar and hours of daylight individually
cor(ynez_metabolism2[5:10])

ER_depth_watertemp_discharge_solar <- lmer(ER ~ depth_log + temp.water + discharge_log + shortwave + (1 | year) + (1 | site_name), data = ynez_metabolism2)
ER_depth_watertemp_discharge_day <- lmer(ER ~ depth_log + temp.water + discharge_log + day.length + (1 | year) + (1 | site_name), data = ynez_metabolism2)

ER_AIC_values <- rbind(ER_AIC_values, c("ER_depth_watertemp_discharge_solar", AIC(ER_depth_watertemp_discharge_solar)))
ER_AIC_values <- rbind(ER_AIC_values, c("ER_depth_watertemp_discharge_day", AIC(ER_depth_watertemp_discharge_day)))

# Water temperature with depth and discharge only produces the best model
summary(ER_depth_watertemp_discharge)
vif(ER_depth_watertemp_discharge)

# However the vif() here is way too high, so we will stick with water temperature and depth
summary(ER_depth_watertemp)
vif(ER_depth_watertemp)