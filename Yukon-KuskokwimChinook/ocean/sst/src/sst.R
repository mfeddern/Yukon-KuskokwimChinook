# SST.R
# Erik Schoen
# 11-5-2021
# Download SST data and make a simple timeseries plot

# Load packages and read in data
library(tidyverse)
library(lubridate)
library(here)

setwd(here("sst"))

# Read in NOAA SST data downloaded from https://mattcallahan.shinyapps.io/NBS_SEBS_SST_MHW/
sst <- read_csv("BS-SST-2021-11-06.csv")

# Daily SST
# Prepare the data for plotting
sst2 <- sst %>%
  select(-X1) %>% # get rid of the row numbers
  mutate(Year = year(date),
         YearFactor = as.factor(Year),
         DOY = yday(date),
         CalDate = as_date(DOY)) %>%
  filter(Year <= 2020)
  
# Plot the data
sst.plot <- ggplot(data = sst2, aes(x = CalDate, y = meansst, color = Year)) +
  geom_line() +
  facet_grid(Ecosystem_sub ~ .) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b", name = NULL) +
  scale_y_continuous(name = "Sea Surface Temperature (˚C)") +
  theme_bw() +
  theme(legend.justification=c(1,1), legend.position=c(0.99,0.99), legend.title=element_blank())

sst.plot
ggsave("Daily SST.png", width = 6, height = 6)

# Mean Summer SST (JJA)
SummerSST <- sst2 %>%
  # Filter out all dates before June and after August
  filter(CalDate > ymd("1970-05-31"),
         CalDate < ymd("1970-09-01")) %>%
  group_by(Year, Ecosystem_sub) %>%
  summarize(MeanSST = mean(meansst, na.rm = T))

SummerSST.plot <- ggplot(data = SummerSST, aes(x = Year, y = MeanSST, color = Ecosystem_sub)) +
  geom_line() +
  scale_x_continuous(limits = c(1980, 2021)) +
  scale_y_continuous(name = "Sea Surface Temperature (˚C)") +
  theme_classic() +
  theme(legend.justification=c(0.1,0.99), legend.position=c(0.1,0.99), legend.title=element_blank())

SummerSST.plot
ggsave("Mean Summer SST.png", width = 4, height = 4)
