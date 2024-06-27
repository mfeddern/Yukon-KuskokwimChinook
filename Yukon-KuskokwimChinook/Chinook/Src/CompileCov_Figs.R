library(dplyr)
library(nord)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(ggplot2)
library(cowplot)
library(MCMCvis)
library(HDInterval)
library(tidyverse)
library(dplyr)
library(rstan)
library(car)

##### Covariates UNSTANDARDIZED ####
yr_fst <- 1980
yr_last <- 2020
lumina<-nord(n = 6, palette = "lumina")

pollock_Biomass <-read.csv('ocean/EBS_Pollock.csv')
pollock_Biomass <-pollock_Biomass%>% 
  group_by(Year) %>%
  select(Age_3_Biomass)%>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(year = Year-4)%>%
  ungroup()%>%
  select(year, Age_3_Biomass)

pollock_Recruitment <-read.csv('ocean/EBS_Pollock.csv')
pollock_Recruitment <-pollock_Recruitment %>%
  group_by(Year) %>%
  select(Recruitment)%>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(year = Year-3)%>%
  ungroup()%>%
  select(year, Recruitment)

SST_winter <-read.csv('ocean/BS-SST-2023-09-28.csv')#reading in winter SST data
SST_winter <-SST_winter%>% 
  filter(Ecosystem_sub=='Southeastern Bering Sea')%>% #selecting region from dataset
  filter(month==1|month==2|month==3)%>% #selecting winter months
  group_by(year) %>%
  select(meansst)%>%
  summarise(winter_mean=mean(meansst))%>% #calculating annual mean grouped by year across months
  rename(Year=year)%>%
  mutate(year = Year-3)%>% ##assigning brood year offset: 3
  mutate(Winter_stand = 
           (winter_mean-mean(SST_winter$meansst))/sd(SST_winter$meansst))%>% #standardizing
  ungroup()

#adding two years of data to the dataset becuase the TS is two short. This only imacts ~5 observations
#an alternative option would be to give them a value of 0 

SST_winter_south <-SST_winter%>%
  add_row(data.frame(Year = c(1984, 1983),winter_mean=rep(SST_winter$winter_mean[1],2), 
                     year=c(1981,1980),Winter_stand=rep(SST_winter$Winter_stand[1],2)))%>%
  arrange(year)

SST_summer_north<-read.csv('ocean/NCEP_NCAR_SST.csv') 
SST_summer_north<-SST_summer_north%>%
  group_by(Year) %>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  select(EarlySummer_North)%>%
  mutate(year = Year-2)%>%
  ungroup()%>%
  select(year, EarlySummer_North)%>%
  mutate(region2 = "Yukon")%>%
  rename(EarlySummer=EarlySummer_North)

SST_summer_south<-read.csv('ocean/NCEP_NCAR_SST.csv') 
SST_summer_south<-SST_summer_south%>%
  group_by(Year) %>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  select(EarlySummer_South)%>%
  mutate(year = Year-2)%>%
  ungroup()%>%
  select(year, EarlySummer_South)%>%
  mutate(region2 = "Kuskokwim")%>%
  rename(EarlySummer=EarlySummer_South)

SST_summer<-SST_summer_south%>%
  bind_rows(SST_summer_north)


IceExtent <-read.csv('ocean/SeaIceIndices.csv') 
IceExtent<-IceExtent%>%
  group_by(Year) %>%
  select(ICIA)%>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(year = Year-3)%>%
  ungroup()%>%
  select(year, ICIA)

IceRetreat <-read.csv('ocean/SeaIceIndices.csv') 
IceRetreat<-IceRetreat%>%
  group_by(Year) %>%
  select(IRI)%>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(year = Year-2)%>%
  ungroup()%>%
  select(year, IRI)

uwind <-read.csv('ocean/uwindMonthly.csv') ### turning NPGO into an annual mean
uwind <- uwind%>%
  group_by(Year) %>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(uwind = mean(c(X6, X7, X8)))%>% #getting the average across the summer
  rename("year"='Year')%>%
  mutate(year = year-2)%>%
  select(year, uwind)%>%
  ungroup()

salmon <-read.csv('ocean/salmonabundanceOkeetal.csv') ### turning NPGO into an annual mean
chum <- salmon%>%
  group_by(Year) %>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  rename("year"='Year')%>%
  mutate(year = year-4)%>%
  select(year, chumTotal)%>%
  ungroup()

sizeUS<-read.csv('ocean/SizeTrend2016.csv')%>% #this dataset is derived from MARSSDFA
  mutate(year=Year-0)%>%#assigning brood year offset: 0
  rename(region2=Region)%>%
  rename(size=Size)%>%
  select(year, size, region2,size_stand)%>%
  mutate(region=ifelse(region2=="Yukon","Yukon (US)", "Kuskokwim"))
size<-sizeUS%>%select(-region)

marinecov<-read.csv('ocean/MarineTrend.csv')%>%
  mutate(year=Year)

breakupfull <-read.csv('breakup/output/breakupAYK.csv') 

Tanana <-  breakupfull%>%filter(Site  == "Tanana River at Nenana")%>%
  select(c("DOY", "Site", "Year"))%>%
  dplyr::group_by(Year, Site) %>%
  summarise(day = as.numeric(mean(DOY)))%>%
  mutate(region = "Yukon (US)")

Dawson <-  breakupfull%>%filter(Site  == "Yukon River at Dawson")%>%
  select(c("DOY", "Site", "Year"))%>%
  dplyr::group_by(Year, Site) %>%
  summarise(day = as.numeric(mean(DOY)))%>%
  mutate(region = "Yukon (CA)")


Kuskokwim <-  breakupfull%>%filter(Site  == "Kuskokwim River at Bethel")%>%
  select(c("DOY", "Site", "Year"))%>%
  dplyr::group_by(Year, Site) %>%
  summarise(day = as.numeric(mean(DOY)))%>%
  mutate(region = "Kuskokwim")


breakup2 <- Tanana %>% 
  mutate(year=Year-2)%>%
  bind_rows(Kuskokwim %>%mutate(year=Year-2))%>%
  bind_rows(Dawson %>% mutate(year=Year-2))%>%
  mutate(breakup2=day)%>%
  select(year, region, breakup2)
breakup2 <-data.frame(breakup2)%>%
  select(year, region, breakup2)


pops_allMets<- readRDS("precipitation/output/pops_allMets.rds")
pops_allMetsV3<- readRDS("precipitation/output/pops_allMets_v3.rds")
max5dprcp_stand <- RiverStand(pops_allMetsV3, 7, 39, 16)%>%
  #mutate(year = year-0)%>%
  mutate(max5dprcp_stand=stand)%>%
  mutate(pop=Population)%>%
  select(pop, year, max5dprcp_stand)

maxq_spawn <- pops_allMetsV3%>%
  select(year, maxq_spawn, Population)%>%
  mutate(year = year-0)%>%
  mutate(pop=Population)%>%
  mutate(maxq_spawn=maxq_spawn)%>%
  select(pop, year, maxq_spawn)

mean_swe_icu <- pops_allMetsV3%>%
  select(year, mean_swe, Population)%>%
  mutate(year = year-0)%>%
  rename(pop=Population)%>%
  rename(mean_swe_icu=mean_swe)%>%
  select(pop, year, mean_swe_icu)

mean_swe_rear <- pops_allMetsV3%>%
  select(year, mean_swe, Population)%>%
  mutate(year = year-1)%>%
  mutate(pop=Population)%>%
  mutate(mean_swe_rear=mean_swe)%>%
  select(pop, year, mean_swe_rear)

medq_rear <-pops_allMetsV3%>%
  select(year, medq_rear, Population)%>%
  mutate(year = year-1)%>%
  mutate(pop=Population)%>%
  select(pop, year, medq_rear)


mnprcp_rear<-pops_allMetsV3%>%
  select(year, mnprcp_rear, Population)%>%
  mutate(year = year-1)%>%
  mutate(pop=Population)%>%
  select(pop, year, mnprcp_rear)


maxDaily_migrate <-
  pops_allMets%>%
  select(year, maxDaily_migrate, Population)%>%
  mutate(year = year-0)%>%
  mutate(pop=Population)%>%
  select(pop, year, maxDaily_migrate)

maxDaily_spawn<-  pops_allMets%>%
  select(year, maxDaily_spawn, Population)%>%
  mutate(year = year-0)%>%
  mutate(pop=Population)%>%
  select(pop, year, maxDaily_spawn)

maxDaily_break<-  pops_allMets%>%
  select(year, maxDaily_migrate, Population)%>%
  mutate(year = year-2)%>%
  mutate(pop=Population,maxDaily_break=maxDaily_migrate)%>%
  select(pop, year, maxDaily_break)


maxWeekly_migrate<-  pops_allMets%>%
  select(year, maxWeekly_migrate, Population)%>%
  mutate(year = year-0)%>%
  mutate(pop=Population)%>%
  select(pop, year, maxWeekly_migrate)

cdd17 <- 
  pops_allMetsV3%>%
  select(year, cddGT17_migrate, Population)%>%
  mutate(year = year-0)%>%
  mutate(pop=Population)%>%
  select(pop, year,cddGT17_migrate)

cdd_rear <- pops_allMets%>%
  select(year, cdd_rear, Population)%>%
  mutate(year = year-0)%>%
  mutate(pop=Population)%>%
  select(pop, year,cdd_rear)


#### Making a full dataset with salmon and cov data ####
resid <- readRDS("Chinook/Output/posteriors/residualnc.rds")%>%
  rename(region2=region.1)

unstand_cov <-  merge(SST_summer,resid,  by=c('year', 'region2'))%>%
  merge(SST_winter, by=c('year'))%>%
  merge(marinecov, by='year')%>%
  merge(pollock_Biomass, by='year')%>%
  merge(pollock_Recruitment, by='year')%>%
  #merge(SST_summer, by=c('region2','year'))%>%
  merge(IceExtent, by='year')%>%
  merge(IceRetreat, by='year')%>%
  # merge(pinkchum, by='year')%>%
  #rename(region2=region2.x)%>%
  merge(size, by=c('region2','year'))%>%
  # merge(pink, by='year')%>%
  merge(chum, by='year')%>%
  #merge(vwind, by='year')%>%
  merge(uwind, by='year')%>%
  #merge(breakup, by=c('region','year'))%>%
  merge(breakup2, by=c('region','year'))%>%
  #merge(max5dprcp, by=c('pop','year'), all.x=FALSE)%>%
  merge(maxq_spawn, by=c('pop','year'))%>%
  merge(mean_swe_icu, by=c('pop','year'))%>%
  merge(mean_swe_rear, by=c('pop','year'))%>%
  merge(medq_rear, by=c('pop','year'))%>%
  #merge(mnprcp_rear, by=c('pop','year'))%>%
  merge(maxDaily_migrate, by=c('pop','year'))%>%
  merge(maxDaily_spawn, by=c('pop','year'))%>%
  #merge(maxDaily_break, by=c('pop','year'))%>%
  merge(maxWeekly_migrate, by=c('pop','year'))%>%
  merge(cdd17, by=c('pop','year'))%>%
  merge(cdd_rear, by=c('pop','year'))%>%
  arrange(pop, year)


saveRDS(unstand_cov , file = "Chinook/Output/posteriors/unstandcov3.rds")
##### Covariates NOT LAGGED ####


yr_fst <- 1980
yr_last<-2022

pollock_Biomass <-read.csv('ocean/EBS_Pollock.csv')
pollock_Biomass <-pollock_Biomass%>% 
  group_by(Year) %>%
  select(Age_3_Biomass)%>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(year = Year-0)%>%
  mutate(Age_3_Biomass_stand = 
           (Age_3_Biomass-mean(pollock_Biomass$Age_3_Biomass))/sd(pollock_Biomass$Age_3_Biomass))%>%
  mutate(covariate = 'Age 3+ Pollock Biomass')%>%
  rename(value=Age_3_Biomass_stand)%>%
  ungroup()%>%
  select(year, value,covariate)

pollock_Recruitment <-read.csv('ocean/EBS_Pollock.csv')
pollock_Recruitment <-pollock_Recruitment %>%
  group_by(Year) %>%
  select(Recruitment)%>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(year = Year-0)%>%
  mutate(Recruitment_stand = 
           (Recruitment-mean(pollock_Recruitment$Recruitment))/sd(pollock_Recruitment$Recruitment))%>%
  mutate(covariate = 'Pollock Recruits')%>%
  rename(value=Recruitment_stand)%>%
  ungroup()%>%
  select(year, value,covariate)

SST_winter <-read.csv('ocean/BS-SST-2023-09-28.csv')#reading in winter SST data
SST_winter <-SST_winter%>% 
  filter(Ecosystem_sub=='Southeastern Bering Sea')%>% #selecting region from dataset
  filter(month==1|month==2|month==3)%>% #selecting winter months
  group_by(year)%>%
  summarise(winter_mean=mean(meansst))#calculating annual mean grouped by year across months
SST_winter <-SST_winter%>%
  mutate(value = 
           (winter_mean-mean(SST_winter$winter_mean))/sd(SST_winter$winter_mean))%>% #standardizing
  mutate(covariate = 'Winter Sea Surface Temperature', region = 'Yukon (US)')%>%
  select(year, value, region,covariate)


SST_summer_north<-read.csv('ocean/NCEP_NCAR_SST.csv') 
SST_summer_north<-SST_summer_north%>%
  group_by(Year) %>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  select(EarlySummer_North)%>%
  mutate(year = Year-0)%>%
  mutate(EarlySummer_stand = 
           (EarlySummer_North-mean(SST_summer_north$EarlySummer_North))/sd(SST_summer_north$EarlySummer_North))%>%
  ungroup()%>%
  select(year, EarlySummer_stand)%>%
  mutate(region2 = "Yukon")

SST_summer_south<-read.csv('ocean/NCEP_NCAR_SST.csv') 
SST_summer_south<-SST_summer_south%>%
  group_by(Year) %>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  select(EarlySummer_South)%>%
  mutate(year = Year-0)%>%
  mutate(EarlySummer_stand = 
           (EarlySummer_South-mean(SST_summer_south$EarlySummer_South))/sd(SST_summer_south$EarlySummer_South))%>%
  ungroup()%>%
  select(year, EarlySummer_stand)%>%
  mutate(region2 = "Kuskokwim")

SST_summer<-SST_summer_south%>%
  bind_rows(SST_summer_north)%>%
  rename(value=EarlySummer_stand)%>%
  mutate(covariate = 'Summer Sea Surface Temperature')%>%
  mutate(region =replace(region2, region2 == 'Yukon', "Yukon (US)"))%>%
  bind_rows(filter(SST_summer_north,region2=="Yukon")%>%mutate(region =replace(region2, region2 == 'Yukon', "Yukon (CA)")))%>%
  select(year, region, value, covariate)

IceExtent <-read.csv('ocean/SeaIceIndices.csv') 
IceExtent<-IceExtent%>%
  group_by(Year) %>%
  select(ICIA)%>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(year = Year-0)%>%
  mutate(ICIA_stand = 
           (ICIA-mean(IceExtent$ICIA))/sd(IceExtent$ICIA))%>%
  mutate(covariate = 'Ice Cover Index', region = 'Yukon (US)')%>%
  rename(value = ICIA_stand)%>%
  ungroup()%>%
  select(year, value, region,covariate)

IceRetreat <-read.csv('ocean/SeaIceIndices.csv') 
IceRetreat<-IceRetreat%>%
  group_by(Year) %>%
  select(IRI)%>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(year = Year-0)%>%
  mutate(IRI_stand = 
           (IRI-mean(IceRetreat$IRI))/sd(IceRetreat$IRI))%>%
  ungroup()%>%
  mutate(covariate = 'Ice Retreat Index')%>%
  rename(value = IRI_stand)%>%
  mutate(region = "Yukon (US)")%>%
  select(year, value, covariate, region)

uwind <-read.csv('ocean/uwindMonthly.csv') ### turning NPGO into an annual mean
uwind <- uwind%>%
  group_by(Year) %>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(avg = mean(c(X6, X7, X8)))%>% #getting the average across the summer
  rename("year"='Year')%>%
  mutate(year = year-0)%>%
  select(year, avg)%>%
  ungroup()
uwind <- uwind%>%
  mutate(uwind_stand = 
           (avg-mean(uwind$avg))/sd(uwind$avg))%>%
  mutate(covariate = 'cross-shelf wind')%>%
  rename(value = uwind_stand)%>%
  mutate(region = "Yukon (US)")%>%
  select(year, value, covariate,region)%>%
  ungroup()

vwind <-read.csv('ocean/vwindMonthly.csv') ### turning NPGO into an annual mean
vwind <- vwind%>%
  group_by(Year) %>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(avg = mean(c(X1, X2,X3, X4, X5, X6, X7, X8, X9, X10, X11, X12)))%>%
  rename("year"='Year')%>%
  mutate(year = year-0)
vwind <- vwind%>%
  mutate(vwind_stand = 
           (avg-mean(vwind$avg))/sd(vwind$avg))%>%
  mutate(covariate = 'along-shelf wind')%>%
  rename(value = vwind_stand)%>%
  select(year, value, covariate)%>%
  ungroup()

salmon <-read.csv('ocean/salmonabundanceOkeetal.csv') ### turning NPGO into an annual mean
chum <- salmon%>%
  group_by(Year) %>%
  filter(Year >= yr_fst & Year <= yr_last)

chum <- chum%>%
  rename("year"='Year')%>%
  mutate(year = year-0)%>%
  mutate(chum_stand = 
           (chumTotal-mean(chum$chumTotal))/sd(chum$chumTotal))%>%
  mutate(covariate = 'Chum Salmon Abundance')%>%
  rename(value = chum_stand)%>%
  select(year, value, covariate)%>%
  ungroup()

pink <- salmon%>%
  rename(year=Year)%>%
  filter(year >= yr_fst & year <= yr_last)%>%
  mutate(year = year-0)%>%
  mutate(pink_stand = 
           (pinkTotal-mean(salmon$pinkTotal))/sd(salmon$pinkTotal))%>%
  mutate(covariate = 'Pink Salmon Abundance')%>%
  rename(value = pink_stand)%>%
  select(year, value, covariate)%>%
  ungroup()

pinkchum1  <- salmon%>%
  rename(year=Year)%>%
  mutate(year = year-0)%>%
  mutate(pinkchum=chumTotal+pinkTotal)
pinkchum  <- pinkchum1 %>%
  group_by(year) %>%
  mutate(pinkchum_stand = 
           (pinkchum-mean(pinkchum1$pinkchum))/sd(pinkchum1$pinkchum))%>%
  mutate(covariate = 'Pink and Chum Salmon Abundance')%>%
  rename(value = pinkchum_stand)%>%
  select(year, value, covariate)%>%
  ungroup()

Size<-read.csv('ocean/SizeTrend.csv')
size<-Size%>%
  rename(region2=Region, size=Size, year=Year)%>%
  mutate(covariate = 'Size')%>%
  rename(value = size)%>%
  mutate(region =replace(region2, region2 == 'Yukon', "Yukon (US)"))%>%
  bind_rows(filter(size,Region=="Yukon")%>%mutate(region =replace(Region, Region == 'Yukon', "Yukon (CA)")))%>%
  select(year, size_stand, region, value, covariate)


marine<-read.csv('ocean/MarineTrend.csv')
marinecov<-marine%>%
  mutate(year=Year)%>%
  select(year, marine)%>%
  add_column(marine_stand = 
               (marine[,2]-mean(marine$marine))/sd(marine$marine))%>%
  rename(value = marine_stand)%>%
  mutate(covariate = 'Marine Competitors',region = "Yukon (US)")%>%
  select(year,  value, covariate, region)


breakupfull <-read.csv('breakup/output/breakupAYK.csv') 

Tanana <-  breakupfull%>%filter(Site  == "Tanana River at Nenana")%>%
  select(c("DOY", "Site", "Year"))%>%
  dplyr::group_by(Year, Site) %>%
  summarise(day = as.numeric(mean(DOY)))%>%
  mutate(region = "Yukon (US)")%>%
  filter(Year >= yr_fst & Year <= yr_last)
sd(Tanana$day) 

Dawson <-  breakupfull%>%filter(Site  == "Yukon River at Dawson")%>%
  select(c("DOY", "Site", "Year"))%>%
  dplyr::group_by(Year, Site) %>%
  summarise(day = as.numeric(mean(DOY)))%>%
  mutate(region = "Yukon (CA)")%>%
  filter(Year >= yr_fst & Year <= yr_last)
sd(Dawson$day)

Kuskokwim <-  breakupfull%>%filter(Site  == "Kuskokwim River at Bethel")%>%
  select(c("DOY", "Site", "Year"))%>%
  dplyr::group_by(Year, Site) %>%
  summarise(day = as.numeric(mean(DOY)))%>%
  mutate(region = "Kuskokwim")%>%
  filter(Year >= yr_fst & Year <= yr_last)
sd(Kuskokwim$day)

breakup2 <- Tanana %>% mutate(breakup2 = (day - mean(Tanana$day))/sd(Tanana$day))%>%
  mutate(year=Year-0)%>%
  bind_rows(Kuskokwim %>% mutate(breakup2 = (day - mean(Kuskokwim$day))/sd(Kuskokwim$day))%>%
              mutate(year=Year-0))%>%
  bind_rows(Dawson %>% mutate(breakup2 = (day - mean(Dawson$day))/sd(Dawson$day))%>%
              mutate(year=Year-0))%>%
  select(year, region, breakup2)
breakup2 <-data.frame(breakup2)%>%
  rename(value=breakup2)%>%
  mutate(covariate = 'River Ice Breakup Date')%>%
  select(year, region, value,covariate)

RiverStand <- function(data, var, obs, rows) { #function to standardize by individual river/pop
  TP<-data.frame(Population =rep(NA, length(data$Population)))
  for(i in 1:length(unique(data$Population))){
    temp <- data%>%
      filter(Population==unique(data$Population)[i])
    x=as.numeric(unlist(temp[,var]))
    obs <- length(temp$Population)
    temp2 <- temp%>%
      mutate(stand =(x-mean(x))/sd(x))
    TP[((i-1)*obs)+1:(i*obs),1:rows]<- temp2
  }
  return(TP[1:length(data$Population),])
}



pops_hydroMets <- readRDS("precipitation/output/pops_hydroMets.rds")
colnames(pops_hydroMets)
max5dprcp_stand <- RiverStand(pops_hydroMets, 4, 39, 16)%>%
  mutate(year = year-0)%>%
  mutate(max5dprcp_stand=stand)%>%
  rename(pop=Population, region=Region)%>%
  mutate(covariate = 'Max 5-day Precipitation')%>%
  rename(value = max5dprcp_stand)%>%
  select(pop, year, value, region, covariate)

pops_allMets<- readRDS("precipitation/output/pops_allMets_v3.rds")

maxq_spawn_stand <- RiverStand(pops_allMets, 4, 39, 15)%>%
  mutate(year = year-0)%>%
  mutate(maxq_spawn_stand=stand)%>%
  rename(pop=Population, region=Region)%>%
  mutate(covariate = 'Max Streamflow Spawning')%>%
  rename(value =  maxq_spawn_stand)%>%
  select(pop, year,value, region, covariate)


mean_swe_icu <- RiverStand(filter(pops_allMets, year<2019), 9, 39, 15)%>%
  mutate(year = year-0)%>%
  mutate(mean_swe_icu=stand)%>%
  rename(pop=Population, region=Region)%>%
  mutate(covariate = 'Snowpack (snow-water equivalent) Incubation')%>%
  rename(value =  mean_swe_icu)%>%
  select(pop, year, value,region, covariate)

mean_swe_rear <- RiverStand(filter(pops_allMets, year<2019), 9, 39, 15)%>%
  mutate(year = year-0)%>%
  mutate(mean_swe_rear=stand)%>%
  rename(pop=Population, region=Region)%>%
  mutate(covariate = 'Snowpack (snow-water equivalent) Rearing')%>%
  rename(value =  mean_swe_rear)%>%
  select(pop, year, value, region)

medq_rear_stand <- RiverStand(pops_allMets, 5, 39, 15)%>%
  mutate(year = year-0)%>%
  mutate(medq_rear_stand=stand)%>%
  rename(pop=Population, region=Region)%>%
  mutate(covariate = 'Median Daily Streamflow')%>%
  rename(value =  medq_rear_stand)%>%
  select(pop, year, value, region, covariate)


mnprcp_rear_stand <- RiverStand(filter(pops_allMets, year<2019), 8, 39, 15)%>%
  mutate(year = year-0)%>%
  mutate(mnprcp_rear_stand=stand)%>%
  mutate(pop=Population)%>%
  mutate(covariate = 'Median Precipitation Rearing')%>%
  rename(value =  mnprcp_rear_stand, region=Region)%>%
  select(pop, year, value, region,covariate)


maxDaily_migrate_stand <- RiverStand(pops_allMets, 11, 39, 15)%>%
  mutate(year = year-0)%>%
  mutate(maxDaily_migrate_stand=stand)%>%
  rename(pop=Population, region=Region, value=maxDaily_migrate_stand)%>%
  mutate(covariate = 'Maximum Daily Temperature Migration')%>%
  select(pop, year, value, region, covariate)


maxDaily_spawn_stand <- RiverStand(pops_allMets, 10, 39, 15)%>%
  mutate(year = year-0)%>%
  mutate(maxDaily_spawn_stand=stand)%>%
  rename(pop=Population, region=Region, value=maxDaily_spawn_stand)%>%
  mutate(covariate = 'Maximum Daily Temperature Spawning')%>%
  select(pop, year, value, region, covariate)


cdd17_stand <- RiverStand(pops_allMets, 12, 39, 15)%>%
  mutate(year = year-0)%>%
  mutate(cdd17_stand=stand)%>%
  rename(pop=Population, region=Region, value =cdd17_stand)%>%
  mutate(covariate = "Migration CDD above 17C")%>%
  select(pop, year, value, region, covariate)

cdd_rear_stand <- RiverStand(pops_allMets, 10, 39, 13)%>%
  select(Population, Region, year, stand)%>%
  mutate(year = year-1,region=Region, pop=Population, value=stand)%>%
  mutate(covariate = "Daily stream temp rearing")%>%
  select(pop, year, value,  region,covariate)



unlag_cov<- bind_rows(SST_winter,marinecov,pollock_Biomass, IceExtent,pollock_Recruitment,IceRetreat,
                      pink, chum, pinkchum, vwind, uwind, SST_summer, size,breakup2,maxq_spawn_stand,
                      mean_swe_icu, mean_swe_rear,medq_rear_stand,
                      mnprcp_rear_stand, maxDaily_migrate_stand, maxDaily_spawn_stand,
                      cdd17_stand, cdd_rear_stand)

saveRDS(unlag_cov, file = "Chinook/Output/posteriors/unlagcov.rds")


full.dat.wide.cov<-readRDS("Chinook/Output/posteriors/full.dat.wide.cov.offset.rds")
colnames(full.dat.wide.cov)
pop_assignments<- unique(full.dat.wide.cov%>%select(pop,region2,region))

unlag_cov_corr<-merge(pop_assignments, SST_summer%>%rename(region2=region,summer_sst=value)%>%select(-covariate), by =c('region2'))%>%
  merge(SST_winter%>%select(-covariate,-region)%>%rename(wintersst=value), by=c('year'))%>%
  merge(marinecov%>%rename(marinecov=value)%>%select(-covariate), by='year')%>%
  merge(IceExtent%>%rename(ICIA=value)%>%select(-covariate), by='year')%>%
  merge(Size%>%rename(region2=Region,year=Year), by=c('region2','year'))%>%
  merge(uwind%>%rename(uwind=value)%>%select(-covariate, -region), by='year')%>%
  merge(breakup2%>%rename(breakup=value)%>%select(-covariate), by=c('region','year'))%>%
  merge(max5dprcp_stand%>%rename(max5dprcp=value)%>%select(-covariate, -region), by=c('pop','year'))%>%
  merge(maxq_spawn_stand%>%rename(maxq_spawn=value)%>%select(-covariate, -region), by=c('pop','year'))%>%
  merge(mean_swe_icu%>%rename(mean_swe_icu=value)%>%select(-covariate, -region), by=c('pop','year'))%>%
  merge(medq_rear_stand%>%rename(medq_rear=value)%>%select(-covariate, -region), by=c('pop','year'))%>%
  merge(mnprcp_rear_stand%>%rename(mnprcp_rear=value)%>%select(-covariate, -region), by=c('pop','year'))%>%
  merge(maxDaily_spawn_stand%>%rename(maxDaily_spawn=value)%>%select(-covariate, -region), by=c('pop','year'))%>%
  merge(maxDaily_migrate_stand%>%rename(maxDaily_migrate=value)%>%select(-covariate, -region), by=c('pop','year'))%>%
  merge(cdd_rear_stand%>%rename(cdd_rear_stand=value)%>%select(-covariate, -region), by=c('pop','year'))%>%
  arrange(pop, year)



panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  his <- hist(x, plot = FALSE)
  breaks <- his$breaks
  nB <- length(breaks)
  y <- his$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = rgb(0, 1, 1, alpha = 0.5), ...)
  # lines(density(x), col = 2, lwd = 2) # Uncomment to add density lines
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

labs3<-c("Winter \n SST",
         "Sea \n Ice \n Cover",
         "Summer \n SST \n NCEP",
         "Body Size",
         "Marine \n DFA \n Trend",
         "u-wind",
         "River Ice \n Breakup",
         "Max \n Streamflow \n Spawn",
         "Median \n Discharge  \n Rear",
         "Cum. Degree \n Days Rear",
         "Max Daily \n Temp Migrate")

pdf(file = "Chinook/Output/Figures/Supplement/FigS17_CovsCorrModelNoLag.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10)
pairs(~ wintersst+
        ICIA+
        summer_sst+
        size_stand+
        marinecov+
        uwind+
        breakup+
        maxq_spawn+
        medq_rear+
        maxDaily_migrate+
        cdd_rear_stand, 
      data = unlag_cov_corr,
      labels=labs3, 
      upper.panel = panel.cor,         # Disabling the upper panel
      # diag.panel = panel.hist,
      lwd=3,col= 'grey', pch=16,cex=0.5,font.labels=2,
      lower.panel = panel.smooth)
dev.off()

colnames(unlag_cov_corr)

#### Figure 7 Cov TS ####

unlag <-unlag_cov%>%
  filter(covariate=="Winter Sea Surface Temperature" |covariate=="Marine Competitors"
         |covariate=="Ice Cover Index"|covariate=="cross-shelf wind"
         |covariate=="Median Daily Streamflow" |covariate=="Maximum Daily Temperature Migration"  
         |covariate=='River Ice Breakup Date'|covariate=="Max Streamflow Spawning"
         |covariate=="Daily stream temp rearing")

covplot <- unlag_cov%>%
  filter(covariate=="Summer Sea Surface Temperature"|covariate=="Size"
         |covariate=="Winter Sea Surface Temperature" |covariate=="Marine Competitors"
         |covariate=="Ice Cover Index"|covariate=="cross-shelf wind"
         |covariate=="Median Daily Streamflow" |covariate=="Maximum Daily Temperature Migration"  
         |covariate=='River Ice Breakup Date'|covariate=="Max Streamflow Spawning"
         |covariate=="Daily stream temp rearing")%>%
  group_by(covariate, year, region) %>%
  summarise(value = mean(value))



levels <- c("Max Streamflow Spawning","Daily stream temp rearing","Median Daily Streamflow",
            'River Ice Breakup Date',"Ice Cover Index","Summer Sea Surface Temperature","cross-shelf wind",
            "Winter Sea Surface Temperature","Marine Competitors","Size",
            "Maximum Daily Temperature Migration")

covplot$covariate = factor(covplot$covariate, 
                           levels = levels)
levels(covplot$covariate) <- levels

unlag$covariate = factor(unlag$covariate, 
                         levels = levels)
levels(unlag$covariate) <- levels

covTS<-ggplot(data = covplot,
              aes(x =year, y = value, group = region))+
  #group=region)) +
  facet_wrap(.~covariate, ncol = 3, labeller = label_wrap_gen(25), scales="free_y") +
  geom_line(data=unlag,aes(group=pop, col='Watershed Conditions'))+
  geom_line(aes(col=region), lwd=0.9)+
  #ggtitle(thetas$Covar.Name, subtitle = thetas$Lifestage) +
  scale_color_manual(breaks=c("Kuskokwim", "Yukon (CA)", "Yukon (US)",'Watershed Conditions'),
                     labels=c("Kuskokwim", "Yukon (CA)", "Yukon (US)",'Watershed Conditions'),
                     values = c(lumina[3],lumina[4],lumina[7],'grey80'))+
  #geom_smooth(col='black')+
  #geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(name ="Standardized Trend" )+
  scale_x_continuous(name = "Year")+
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()+
  theme(legend.position = c(0.9, 0),
        legend.justification = c(1, 0))+
  labs(col = "Region")
covTS
pdf(file = "Chinook/Output/Figures/MainText/Figure6_covtimeseries.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 8)
covTS
dev.off()
