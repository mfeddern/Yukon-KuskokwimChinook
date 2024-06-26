# BreakupSimple.R
# Erik Schoen
# 11/5/2021
# Download breakup date data and create a simple timeseries plot

# Load packages and read in data
library(tidyverse)
library(lubridate)
library(here)

theme_set(theme_classic())
setwd(here("breakup"))

# Read in breakup dates for Yukon (at Dawson), Tanana (at Nenana), and Kuskokwim (at Bethel) from EPA website
# https://www.epa.gov/climate-indicators/alaskan-rivers

breakup <- read_csv("https://www.epa.gov/sites/default/files/2021-03/river-ice_fig-1.csv", skip = 6)

# reshape into long format for plotting

breakup.long <- breakup %>%
  pivot_longer(cols = c("Yukon River":"Kuskokwim River"), names_to = "River", values_to = "DOY") %>%
  mutate(Date = as_date(DOY))

# Plot the data
breakup.plot <- ggplot(data = breakup.long, aes(x = Year, y = Date, color = River)) +
  geom_line() +
  scale_x_continuous(limits = c(1980, 2021)) +
  scale_y_date(name = "Breakup date") +
  theme_classic() +
  theme(legend.justification=c(.433,1), legend.position=c(0.433,1), legend.title=element_blank())

breakup.plot
ggsave("Breakup Date.png", width = 4, height = 4)
