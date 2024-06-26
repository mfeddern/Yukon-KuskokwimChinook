# Breakup.R
# 8-25-22
# Erik Schoen

# Compile, summarize, and plot river ice breakup timing data

######## Load packages and read in data #######

library(tidyverse)
library(lubridate)

# Read in ice phenology data compiled by Fresh Eyes on Ice project (Arp and Cherry 2022)
# Citation: Chris Arp and Jessica Cherry. 2022. River and lake ice phenology data for Alaska and Northwest Canada 
# from 1882 to 2021. Arctic Data Center. doi:10.18739/A28W38388.
# Link: https://arcticdata.io/catalog/view/doi:10.18739/A28W38388

breakupRaw <- read_csv("breakup/data/AlaskaLakeRiverIcePhenologyRecords_FreshEyesonIce_APRFC_1882_2021.csv")
sitesAYK <- read_csv("breakup/data/sitesAYK.csv")

# Set study period start and end years
startYear = 1980
endYear = 2021

####### Select sites of interest #######

### Initial step to focus on sites in the AYK region:
# sitesAllRivers <- breakupRaw %>%
#   filter(Season == "break-up") %>%
#   filter(WaterbodyType == "river") %>%
#   # focus on breakup / ice free dates (not other categories such as "safe for boat")
#   filter(Event == "break-up" | Event == "ice free")  %>%
#   # eliminate redundant observations by site-year
#   select(WaterbodyName:Year) %>%
#   distinct() %>%
#   mutate(StudyPeriod = ifelse(Year %in% startYear:endYear, 1, 0)) %>%
#   group_by(WaterbodyName, NearestCommunity) %>%
#   summarize(YearsStudyPeriod = sum(StudyPeriod, na.rm = T),
#             YearsTotal = n()) %>%
#   filter(YearsTotal > 9) # remove sites with fewer than 10 years of data
# 
# write_csv(sitesAllRivers, "breakup/output/sitesAllRivers.csv")
### Manually edited this table to flag sites in the Arctic-Yukon-Kuskokwim region for further analysis

breakupAYK <- left_join(sitesAYK, breakupRaw, by = c("WaterbodyName", "NearestCommunity")) %>%
  filter(AYKRegion == T, # include only sites in the AYK region
         Event == "break-up")  %>% # focus on actual breakup dates (not other categories such as "safe for boat")
  rename(River = WaterbodyName) %>%
  mutate(Site = paste(River, "at", NearestCommunity),
         DateText = Date,
         Date = mdy(DateText),
         DateSameYear = as.Date(DOY, origin = ymd("2021-12-30")), # used for plotting
         StudyPeriod = ifelse(Year %in% startYear:endYear, 1, 0)) 

# write AYK dataset to csv for further analysis
write_csv(breakupAYK, "./breakup/output/breakupAYK.csv")

# Make plots for sites on Yukon and Kuskokwim mainstems
sitesYKmainstems <- filter(breakupAYK, River %in% c("Yukon River", "Kuskokwim River", "Unalakleet River")) %>%
  group_by(Site, River, NearestCommunity, Basin, Subbasin, Area, Source) %>%
  summarize(YearsStudyPeriod = sum(StudyPeriod, na.rm = T),
              YearsTotal = n()) %>%
  filter(YearsStudyPeriod > 19) %>% # remove sites with fewer than 20 years of data in the study period
  arrange(River, Subbasin, desc(YearsStudyPeriod)) # sort sites by years of data in the study period
  
# Expand the dataset to include NAs for missing site-years (so plots won't interpolate across missing values)
breakupYKmainstems.allYears <- breakupAYK %>%
  select(Site, Year, Date, DOY, DateSameYear) %>%
  complete(Site, Year) %>%
  right_join(sitesYKmainstems, by = c("Site"))

# Plot the data
breakupYKmainstems.plot <- ggplot(data = breakupYKmainstems.allYears, aes(x = Year, y = DateSameYear, color = Area,
                                                                          group = Site)) +
  geom_line() +
  scale_x_continuous(limits = c(1980, 2021)) +
  scale_y_date(name = "Breakup date") +
  facet_grid(Basin ~ .) 

breakupYKmainstems.plot
ggsave("./breakup/output/Breakup Date YK by subbasin.png", width = 8, height = 6)

# Best site for Lower Kusko = Bethel (complete 42 years)
# No single site in Lower Yukon with complete record during study period.
# Examine the available sites and see how the data gaps line up

lowerYukon <- breakupAYK %>%
  filter(Subbasin == "LowerYukon")

