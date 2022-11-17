#### NRES 710 Final Project
### Playing around with data
## 11.7.2022

#### Loading libraries ####
library(tidyverse)
library(ggplot2)
library(lubridate)
library(gridExtra)

#### Loading and Cleaning Data ####

# Loading metabolism and site data from Appling et al. 2018
site_data <- read.table("appling_sitedata.tsv", header = TRUE)
metabolism <- read.table("appling_metabolism.tsv", header = TRUE)

# Filtering out our study sites

  # For the Russian River
  rus_metabolism <- metabolism %>% 
    filter(site_name == "nwis_11462500" | site_name == "nwis_11463000" |
             site_name == "nwis_11463682" | site_name == "nwis_11463980" |
             site_name == "nwis_11467000")
  
  # For the Sacramento River
  sac_metabolism <- metabolism %>% 
    filter(site_name == "nwis_11447650" | site_name == "nwis_11447890")
  
  # For the Santa Ynez River
  ynez_metabolism <- metabolism %>% 
    filter(site_name == "nwis_11128500" | site_name == "nwis_1112600")
  
  # Creating a data set with all three rivers
  mediterranean_metabolism <- metabolism %>% 
    filter(site_name == "nwis_11462500" | site_name == "nwis_11463000" |
           site_name == "nwis_11463682" | site_name == "nwis_11463980" |
           site_name == "nwis_11467000" | site_name == "nwis_11447650" | 
           site_name == "nwis_11447890" | site_name == "nwis_11128500" | 
           site_name == "nwis_1112600")
  
# Removing data that does not make sense (ie. negative GPP or positive ER)
rus_metabolism <- rus_metabolism %>% 
  filter(GPP >= 0 & ER <= 0)
sac_metabolism <- sac_metabolism %>% 
  filter(GPP >= 0 & ER <= 0)
ynez_metabolism <- ynez_metabolism %>% 
  filter(GPP >= 0 & ER <= 0)
mediterranean_metabolism <- mediterranean_metabolism %>% 
  filter(GPP >= 0 & ER <= 0)

# Dropping rows without any data for ER or GPP
rus_metabolism <- rus_metabolism %>% 
  drop_na(c("GPP", "ER"))
sac_metabolism <- sac_metabolism %>% 
  drop_na(c("GPP", "ER"))
ynez_metabolism <- ynez_metabolism %>% 
  drop_na(c("GPP", "ER"))
mediterranean_metabolism <- mediterranean_metabolism %>% 
  drop_na(c("GPP", "ER"))

# Function to convert date column from character to date
rus_metabolism$date <- date(rus_metabolism$date)
sac_metabolism$date <- date(sac_metabolism$date)
ynez_metabolism$date <- date(ynez_metabolism$date)
mediterranean_metabolism$date <- date(mediterranean_metabolism$date)

# Getting rid of columns I do not care about
rus_metabolism <- rus_metabolism %>% 
  select(date, GPP, ER, depth, temp.water, day.length, discharge, shortwave, velocity)
sac_metabolism <- sac_metabolism %>% 
  select(date, GPP, ER, depth, temp.water, day.length, discharge, shortwave, velocity)
ynez_metabolism <- ynez_metabolism %>% 
  select(date, GPP, ER, depth, temp.water, day.length, discharge, shortwave, velocity)
mediterranean_metabolism <- mediterranean_metabolism %>% 
  select(date, GPP, ER, depth, temp.water, day.length, discharge, shortwave, velocity)

# Exporting all created data frames to CSV
#write.csv(rus_metabolism, "russian_metabolism.csv", row.names = FALSE)
#write.csv(sac_metabolism, "sacramento_metabolism.csv", row.names = FALSE)
#write.csv(ynez_metabolism, "santaynez_metabolism.csv", row.names = FALSE)
#write.csv(mediterranean_metabolism, "mediterranean_metabolism.csv", row.names = FALSE)

#### Showing GPP and ER in graphs

# Need a column for month & day alone to make averages
rus_metabolism$month_day <- format(rus_metabolism$date, format="%m-%d")
sac_metabolism$month_day <- format(sac_metabolism$date, format="%m-%d")
ynez_metabolism$month_day <- format(ynez_metabolism$date, format="%m-%d")

# Averaging per date across sites
rus_metabolism_avg <- rus_metabolism %>% 
  group_by(month_day) %>% 
  summarize(GPP = mean(GPP, na.rm = TRUE),
            ER = mean(ER, na.rm = TRUE)) %>% 
  ungroup()

sac_metabolism_avg <- sac_metabolism %>% 
  group_by(month_day) %>% 
  summarize(GPP = mean(GPP, na.rm = TRUE),
            ER = mean(ER, na.rm = TRUE)) %>% 
  ungroup()

ynez_metabolism_avg <- ynez_metabolism %>% 
  group_by(month_day) %>% 
  summarize(GPP = mean(GPP, na.rm = TRUE),
            ER = mean(ER, na.rm = TRUE)) %>% 
  ungroup()

# Adding arbitrary year to month_day to convert it to a date and graph
rus_metabolism_avg$month_day <- as.Date(rus_metabolism_avg$month_day, c("%m-%d"))
sac_metabolism_avg$month_day <- as.Date(sac_metabolism_avg$month_day, c("%m-%d"))
ynez_metabolism_avg$month_day <- as.Date(ynez_metabolism_avg$month_day, c("%m-%d"))

# NPP calculation
rus_metabolism_avg <- rus_metabolism_avg %>% 
  mutate(NPP = GPP + ER)
sac_metabolism_avg <- sac_metabolism_avg %>% 
  mutate(NPP = GPP + ER)
ynez_metabolism_avg <- ynez_metabolism_avg %>% 
  mutate(NPP = GPP + ER)

# Getting total ER, GPP, NPP from averaged data set
annual <- function(metab_data) {
  x <- sum(metab_data$GPP)
  y <- sum(metab_data$ER)
  z <- sum(metab_data$NPP)
  df <- data.frame(GPP = x,
                 ER = y,
                 NPP = z)
}

rus_annual_stat <- annual(rus_metabolism_avg)
sac_annual_stat <- annual(sac_metabolism_avg)
ynez_annual_stat <- annual(ynez_metabolism_avg)

# Plotting data
rusplot <- ggplot(data = rus_metabolism_avg, aes(x = month_day)) +
  geom_point(aes(y = NPP), color = "gray") +
  geom_point(aes(y = GPP), color = "green", alpha = 0.7) +
  stat_smooth(aes(y = GPP), color = "green", method = "loess", span = 0.1) +
  geom_point(aes(y = ER), color = "red", alpha = 0.7) +
  stat_smooth(aes(y = ER), color = "red", method = "loess", span = 0.1) +
  geom_hline(yintercept = 0) +
  labs(subtitle = "Russian River", y = expression(paste("g C m"^"-2"*" d "^"-1")), x = "Month") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  theme(plot.subtitle= element_text(hjust = 0.5)) +
  annotate("text", x= ymd("2022-07-15"), y = 7, label = expression(paste("GGP = +805.1 g C m"^"-2"*" y "^"-1")), color = "darkgreen") +
  annotate("text", x= ymd("2022-07-15"), y = -7, label = expression(paste("ER = -947.7 g C m"^"-2"*" y "^"-1")), color = "darkred") +
  coord_cartesian(ylim = c(-18, 13))

rusplot

sacplot <- ggplot(data = sac_metabolism_avg, aes(x = month_day)) +
  geom_point(aes(y = NPP), color = "gray") +
  geom_point(aes(y = GPP), color = "green", alpha = 0.7) +
  stat_smooth(aes(y = GPP), color = "green", method = "loess", span = 0.1) +
  geom_point(aes(y = ER), color = "red", alpha = 0.7) +
  stat_smooth(aes(y = ER), color = "red", method = "loess", span = 0.1) +
  geom_hline(yintercept = 0) +
  labs(subtitle = "Sacramento River", y = expression(paste("g C m"^"-2"*" d "^"-1")), x = "Month") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  theme(plot.subtitle= element_text(hjust = 0.5)) +
  annotate("text", x= ymd("2022-07-15"), y = 6, label = expression(paste("GGP = +308.0 g C m"^"-2"*" y "^"-1")), color = "darkgreen") +
  annotate("text", x= ymd("2022-07-15"), y = -11, label = expression(paste("ER = -1994.9 g C m"^"-2"*" y "^"-1")), color = "darkred") +
  coord_cartesian(ylim = c(-18, 13))

sacplot

ynezplot <- ggplot(data = ynez_metabolism_avg, aes(x = month_day)) +
  geom_point(aes(y = NPP), color = "gray") +
  geom_point(aes(y = GPP), color = "green", alpha = 0.7) +
  stat_smooth(aes(y = GPP), color = "green", method = "loess", span = 0.1) +
  geom_point(aes(y = ER), color = "red", alpha = 0.7) +
  stat_smooth(aes(y = ER), color = "red", method = "loess", span = 0.1) +
  geom_hline(yintercept = 0) +
  labs(subtitle = "Santa Ynez River", y = expression(paste("g C m"^"-2"*" d "^"-1")), x = "Month") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  theme(plot.subtitle= element_text(hjust = 0.5)) +
  annotate("text", x= ymd("2022-04-15"), y = 12, label = expression(paste("GGP = +1841.5 g C m"^"-2"*" y "^"-1")), color = "darkgreen") +
  annotate("text", x= ymd("2022-10-15"), y = -2, label = expression(paste("ER = -3773.2 g C m"^"-2"*" y "^"-1")), color = "darkred") +
  coord_cartesian(ylim = c(-18, 13))

ynezplot

combined <- grid.arrange(rusplot, sacplot, ynezplot, ncol = 3)
