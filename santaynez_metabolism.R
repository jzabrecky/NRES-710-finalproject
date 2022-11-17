#### NRES 710 Final Project
### Modeling with each river separately (Sacramento River ver.)
## 11.8.2022

#### Loading libraries & data ####

# Libraries
library(tidyverse)
library(lubridate)
library(lme4)

# Loading the dataset
ynez_metabolism <- read.csv("santaynez_metabolism.csv")

# Inspecting relationships between variables
pairs(ynez_metabolism[2:9])
cor(ynez_metabolism[2:9])

# Log-transforming data for discharge and depth
ynez_metabolism2 <- ynez_metabolism %>% 
  mutate(discharge_log = log(discharge),
         depth_log = log(depth),
         velocity_log = log(velocity), 
         year = year(date))
ynez_metabolism2 <- ynez_metabolism2 %>% 
  select(year, GPP, ER, depth_log, temp.water, day.length, discharge_log, shortwave, velocity_log)

# Saving log-transformed as a new csv
#write.csv(ynez_metabolism2, "santaynez_metabolism_final.csv", row.names = FALSE)

# Reinspecting relationships between variables
pairs(ynez_metabolism2[2:9])
cor(ynez_metabolism2[2:9])

#### Beginning modeling for GPP ####

# We do not have too many variables and I can think of reasons why each would relate to metabolism, 
# so to start I am going to test them all out with year included as a random effect
GPP_discharge <- lmer(GPP ~ discharge_log + (1 | year), data = ynez_metabolism2)
GPP_solar <- lmer(GPP ~ shortwave + (1 | year), data = ynez_metabolism2)
GPP_depth <- lmer(GPP ~ depth_log + (1 | year), data = ynez_metabolism2)
GPP_watertemp <- lmer(GPP ~ temp.water + (1 | year), data = ynez_metabolism2)
GPP_day <- lmer(GPP ~ day.length + (1 | year), data = ynez_metabolism2)
GPP_velocity <- lmer(GPP ~ velocity_log + (1 | year), data = ynez_metabolism2)

GPP_AIC_values <- data.frame(model = c("GPP_solar", "GPP_discharge", "GPP_depth", "GPP_watertemp", "GPP_day", "GPP_velocity"),
                             AIC = c(AIC(GPP_solar), AIC(GPP_discharge), AIC(GPP_depth), AIC(GPP_watertemp), AIC(GPP_day), AIC(GPP_velocity)))


# Velocity was found to produce the best model (lowest AIC value)!
# Using cor() to see if there are any other variables I can add to improve the model
# that are not too correlated
cor(ynez_metabolism2[2:9])

# Day length, solar radiation, and water temperature are all options
GPP_velocity_day <- lmer(GPP ~ velocity_log + day.length + (1 | year), data = ynez_metabolism2)
GPP_velocity_watertemp <- lmer(GPP ~ velocity_log + temp.water + (1 | year), data = ynez_metabolism2)
GPP_velocity_solar <- lmer(GPP ~ velocity_log + shortwave + (1 | year), data = ynez_metabolism2)

GPP_AIC_values <- rbind(GPP_AIC_values, c("GPP_velocity_day", AIC(GPP_velocity_day)))
GPP_AIC_values <- rbind(GPP_AIC_values, c("GPP_velocity_watertemp", AIC(GPP_velocity_watertemp)))
GPP_AIC_values <- rbind(GPP_AIC_values, c("GPP_velocity_solar", AIC(GPP_velocity_solar)))

# Velocity and day length were shown to have the best models
summary(GPP_velocity_day)

#### Beginning modeling for ER ####

# Going through all variables again with year included as a random effect
ER_discharge <- lmer(ER ~ discharge_log + (1 | year), data = ynez_metabolism2)
ER_solar <- lmer(ER ~ shortwave + (1 | year), data = ynez_metabolism2)
ER_depth <- lmer(ER ~ depth_log + (1 | year), data = ynez_metabolism2)
ER_watertemp <- lmer(ER ~ temp.water + (1 | year), data = ynez_metabolism2)
ER_day <- lmer(ER ~ day.length + (1 | year), data = ynez_metabolism2)
ER_velocity <- lmer(ER ~ velocity_log + (1 | year), data = ynez_metabolism2)

ER_AIC_values <- data.frame(model = c("ER_solar", "ER_discharge", "ER_depth", "ER_watertemp", "ER_day", "ER_velocity"),
                            AIC = c(AIC(ER_solar), AIC(ER_discharge), AIC(ER_depth), AIC(ER_watertemp), AIC(ER_day), AIC(ER_velocity)))

# Velocity was found to produce the best model (lowest AIC value)!
# Using cor() to see if there are any other variables I can add to improve the model
# that are not too correlated
cor(ynez_metabolism2[2:9])

# Day length, solar radiation, and water temperature are all options
ER_velocity_day <- lmer(ER ~ velocity_log + day.length + (1 | year), data = ynez_metabolism2)
ER_velocity_watertemp <- lmer(ER ~ velocity_log + temp.water + (1 | year), data = ynez_metabolism2)
ER_velocity_solar <- lmer(ER ~ velocity_log + shortwave + (1 | year), data = ynez_metabolism2)

ER_AIC_values <- rbind(ER_AIC_values, c("ER_velocity_day", AIC(ER_velocity_day)))
ER_AIC_values <- rbind(ER_AIC_values, c("ER_velocity_watertemp", AIC(ER_velocity_watertemp)))
ER_AIC_values <- rbind(ER_AIC_values, c("ER_velocity_solar", AIC(ER_velocity_solar)))

# Velocity and water temperature were shown to have the best models
summary(ER_velocity_watertemp)
