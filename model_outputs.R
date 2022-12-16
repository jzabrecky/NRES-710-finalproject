#### NRES 710 Final Project
### Creating model output figures
## 11.14.2022

#### Loading libraries & data ####

# Libraries
library(tidyverse)
library(ggplot2)
library(lme4)

# Data
rus_metabolism <- read.csv("russian_metabolism_final.csv")
sac_metabolism <- read.csv("sacramento_metabolism_final.csv")
ynez_metabolism <- read.csv("santaynez_metabolism_final.csv")

#### Re-creating models ####

russianGPP <- lmer(GPP ~ temp.water + velocity_log + (1 | year), data = rus_metabolism)
russianER <- lmer(ER ~ temp.water + depth_log + (1 | year), data = rus_metabolism)
sacramentoGPP <- lmer(GPP ~ day.length + depth_log + (1 | year), data = sac_metabolism)
sacramentoER <- lmer(ER ~ day.length + depth_log + (1 | year), data = sac_metabolism)
ynezGPP <- lmer(GPP ~ velocity_log + day.length + (1 | year), data = ynez_metabolism)
ynezER <- lmer(ER ~ velocity_log + temp.water + (1 | year), data = ynez_metabolism)

equation(ynezER)

#### Creating Plots ####
equatiomatic::extract_eq(ynezER)
plot(x= ynez_metabolism$velocity_log, y = ynez_metabolism$temp.water)
abline(ynezER)
?plot()

## Russian River

#GPP plot
rus_ER <- 

## Sacramento River

## Santa Ynez River