#### NRES 710 Final Project
### Modeling with each river separately (Sacramento River ver.)
## 12.16.22

#### Loading libraries & data ####

# Libraries
library(lme4)
library(DHARMa)
library(gridExtra)

# Dataset
rus_metabolism <- read.csv("russian_metabolism_final.csv")
sac_metabolism <- read.csv("sacramento_metabolism_final.csv")
ynez_metabolism <- read.csv("santaynez_metabolism_final.csv")

#### Generating Diagnostic plots #####

## Russian Metabolism
rus_GPP_solar <- lmer(GPP ~ shortwave + (1 | year) + (1 | site_name), data = rus_metabolism)
rus_ER_depth_watertemp <- lmer(ER ~ depth_log + temp.water + (1 | year) + (1 | site_name), data = rus_metabolism)

rus_GPP_resids <- simulateResiduals(rus_GPP_solar)
rus_ER_resids <- simulateResiduals(rus_ER_depth_watertemp)

## Sacramento Metabolism
sac_GPP_day_depth <- lmer(GPP ~ day.length + depth_log + (1 | year) + (1 | site_name), data = sac_metabolism)
sac_ER_depth_day <- lmer(ER ~ depth_log + day.length + (1 | year) + (1 | site_name), data = sac_metabolism)

sac_GPP_resids <- simulateResiduals(sac_GPP_day_depth)
sac_ER_resids <- simulateResiduals(sac_ER_depth_day)

## Santa Ynez Metabolism
ynez_GPP_solar_depth_watertemp <- lmer(GPP ~ depth_log + shortwave + temp.water + (1 | year) + (1 | site_name), data = ynez_metabolism)
ynez_ER_depth_watertemp <- lmer(ER ~ depth_log + temp.water + (1 | year) + (1 | site_name), data = ynez_metabolism)
 
ynez_GPP_resids <- simulateResiduals(ynez_GPP_solar_depth_watertemp)
ynez_ER_resids <- simulateResiduals(ynez_ER_depth_watertemp)

## Plots
plot(rus_GPP_resids)
plot(rus_ER_resids)
plot(sac_GPP_resids)
plot(sac_ER_resids)
plot(ynez_GPP_resids)
plot(ynez_ER_resids)
