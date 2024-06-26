library(dplyr)
library(nord)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(ggplot2)
library(cowplot)
library(Cairo)
library(MCMCvis)
library(HDInterval)
library(reshape2)
library(tidyverse)
library(dplyr)
library(rstan)
library(car)
#### Organizing data into single dataset ####

#Here we compile the salmon dataset, evaluate what type of error is associated with each dataset
#and convert error into the appropriate type for the model. In this case the model needs error in 
#log space so we convert all error types to a CV. We also assign a CV for observations that do not
# have one to 0.3. We create a "region" variable that is assigned based on Yukon (US), Yukon (CA), 
#and "Kuskokwim". We also assign 'region2' which assigns a factor for the mainstem "Yukon" or 
#"Kuskokwim". We also filter out years that there are now observations at the begining of the TS
#relevant equations CV=SD/mean; 
#CV on original scale = sqrt(exp(sigma^2)-1) where sigma is SD of log transformed data
#We need variables that are: Spawner and recruit data in non-log space, and error estimates
#in log space
CV.assump<- 0.4 # change this value to change what unobserved error should be assumed as

cs.dat <-read.csv('Chinook/Data/chenasalchaSRA.csv')%>% #dataset for Chena and Salcha
  filter(quantity == "spawners"|quantity == "recruits")%>% #selecting spawners and recruits
  mutate(region='Yukon (US)')%>% #adding a region factor
  mutate(region2='Yukon')%>% #adding mainstem factor
  rename(error ='cv') %>% #renaming error type
  mutate(errortype='cv')%>% #adding an error type category for keeping track of conversions
  mutate(CV = error)%>%  #mutating a cv column (this is the )
  mutate(SD = mean * CV) #converting cv into an SD cv = sd/mean
cs.dat<-cs.dat%>%mutate(SDlog=sqrt(log(CV^2+1)))

ag.dat <-read.csv('Chinook/Data/giasaandreafskySR.csv')%>% #dataset for Gisasa and Andreafsky
  filter(quantity == "spawners"|quantity == "recruits")%>% #getting recruit and spawner data
  mutate(region='Yukon (US)')%>% #adding a region
  mutate(region2='Yukon')%>% #adding a mainstem factor
  rename(error ='sd') %>% #renaming for a single error column
  mutate(errortype='sd')%>% #assigning the type of error to keep track
  mutate(SD = error)%>% #creating an sd column
  mutate(CV = SD/mean) #calculating CV cv = sd/mean
ag.dat["CV"][is.na(ag.dat["CV"])]<- CV.assump #assigning assumed Cv to the NAs
ag.dat<-ag.dat%>%mutate(SDlog=sqrt(log(CV^2+1))) #getting SD on log scale

g.dat <-read.csv('Chinook/Data/goodnewsSR.csv')%>% #dataset for the Goodnews river
  mutate(region='Kuskokwim')%>% #assigning region
  mutate(region2='Kuskokwim')%>% #assigning mainstem
  mutate(SD = NA)%>% #assigning error
  mutate(CV = CV.assump)%>%
  filter(year >=1991&year <=2009)%>%
  mutate(SDlog=sqrt(log(CV^2+1))) #removing years with no observations or incomplete observations

#Ben et al. fit six model types in their 2021 paper. Here we use the model categorized as "SSM-VM"
# this was the "full" state-space model that estimated the covariance matrix on recruitment residuals
# and included time-varying maturity. The work also included two explorations in the vulnerability 
# schedule (scenario = 'vuln') and weighting of age composition (scenario = 'ess'). Here we use the 
# base scenario, which is the primary scenario presented in the paper

kusk.dat1 <-read.csv('Chinook/Data/kusko-multi-SRA-ests.csv')%>% #Kuskokwim SRA dataset from Ben
  filter(model_id == "SSM-VM" & scenario=='base')%>% #selecting the model and scenario to use, "SSM-VM" and "base"
  filter(quantity == "logR"|quantity == "S")%>% #filtering the recruit and spawner data
  select(quantity, pop, year, mean, sd) #selecting the variables we need

#recruitment data is in log scale 
kusk.dat2 <- kusk.dat1 %>% 
  filter(quantity == "logR")%>% #selecting recruit data
  mutate(CV = sqrt(exp(sd^2)-1))%>% #creating a CV on the original scale
  mutate(mean2 =exp(mean))%>% #creating recruit estimate on original scale
  mutate(SDlog = sd)%>%
  select(quantity, pop, year, mean2, SDlog,CV)%>% #selecting variables to keep
  mutate(quantity='recruits')%>% #assigning a quantity
  rename(mean = 'mean2')%>%
  mutate(errortype='sd log scale')%>%
  mutate(SD=CV*mean) #creating SD on original scale



kusk.dat3 <- kusk.dat1 %>%
  filter(quantity == "S")%>% #getting appropriate spawner data
  mutate(quantity='spawners')%>% #mutating for consistency with other data
  rename(error='sd')%>% #error is sd, non log
  mutate(errortype ='sd')%>%
  mutate(quantity='spawners')%>%
  mutate(SD=error)%>%
  mutate(CV = SD/mean)%>% #estimating CV
  mutate(SDlog=sqrt(log(CV^2+1)))%>% #calculating error in log space
  bind_rows(kusk.dat2)%>% #combining with the recruit data
  mutate(region='Kuskokwim')%>%
  mutate(region2='Kuskokwim')



#filtering years to get rid of missing data by population
Aniak<-kusk.dat3 %>%
  filter(pop == "Aniak" & year>=1983)
George<-kusk.dat3 %>%
  filter(pop == "George" & year>=1996)
Holitna<-kusk.dat3 %>%
  filter(pop == "Holitna" & year>=1978)
Holokuk<-kusk.dat3 %>%
  filter(pop == "Holokuk" & year>=1993)
Kisaralik<-kusk.dat3 %>%
  filter(pop == "Kisaralik" & year>=1978)
Kogrukluk<-kusk.dat3 %>%
  filter(pop == "Kogrukluk" & year>=1976)
Kwethluk<-kusk.dat3 %>%
  filter(pop == "Kwethluk" & year>=1992)
Oskawalik<-kusk.dat3 %>%
  filter(pop == "Oskawalik" & year>=1987)
Pitka<-kusk.dat3 %>%
  filter(pop == "Pitka" & year>=2000)
Swift<-kusk.dat3 %>%
  filter(pop == "Swift" & year>=1977)
Takotna<-kusk.dat3 %>%
  filter(pop == "Takotna" & year>=1996)
Tatlawiksuk<-kusk.dat3 %>%
  filter(pop == "Tatlawiksuk" & year>=1999)
Tuluksak<-kusk.dat3 %>%
  filter(pop == "Tuluksak" & year>=1991)

kusk.dat<- Aniak %>%
  bind_rows(Tuluksak, Tatlawiksuk,Takotna,Swift,Pitka,Oskawalik,Kwethluk,
            Kogrukluk,Kisaralik,Holokuk,Holitna,George)


yuk.dat1 <-read.csv('Chinook/Data/yukon-multi-SRA-ests.csv')%>% #reading in Yukon CA data
  filter(quantity == "logR"|quantity == "S")%>%
  select(quantity, pop, year, mean, sd)
yuk.dat2 <- yuk.dat1 %>%
  filter(quantity == "logR")%>% #getting the recruit data
  mutate(CV = sqrt(exp(sd^2)-1))%>% #creating CV on original scale from log SD
  mutate(mean2 =exp(mean))%>% #calculating the recruit estimate in non log space
  mutate(SDlog = sd)%>% #original sd is in log space. renaming
  select(quantity, pop, year, mean2, SDlog,CV)%>% #selecting data we need
  mutate(quantity='recruits')%>% #creating a quantity column
  rename(mean = 'mean2')%>% #renaming the nonlog data
  mutate(errortype='sd log scale')%>% 
  mutate(SD=CV*mean) #creating a non log SD
yuk.dat <- yuk.dat1 %>%
  filter(quantity == "S")%>% #selecting the spawner data
  rename(error ='sd') %>% 
  mutate(errortype='sd')%>%
  mutate(quantity='spawners')%>%
  mutate(SD=error)%>% # error is sd in non log space
  mutate(CV = SD/mean)%>% #calculating CV
  mutate(SDlog=sqrt(log(CV^2+1)))%>% #calculating SD in log space
  bind_rows(yuk.dat2)%>% #combining with recruit dat
  mutate(region='Yukon (CA)')%>% #adding region
  mutate(region2='Yukon') #adding mainstem

dat.long <- yuk.dat %>% #combining all dataset that now have same columns
  bind_rows(kusk.dat) %>%
  bind_rows(g.dat)%>%
  bind_rows(cs.dat)%>%
  bind_rows(ag.dat)

full.dat.wide.CV <-dat.long%>% #creating a wide dataset for errors
  select(quantity, pop, year, SDlog, region, region2)%>%
  pivot_wider(names_from = quantity, 
              values_from = SDlog)%>%
  rename(rec.SDlog=recruits)%>%
  rename(spawn.SDlog =spawners)

full.dat.wide <-dat.long%>% #creating a wide dataset for actual data
  select(quantity, pop, year, mean, region, region2)%>%
  pivot_wider(names_from = quantity, 
              values_from = mean)%>%
  left_join(full.dat.wide.CV) #merging columns with the CV data

full.dat.wide<- full.dat.wide[complete.cases(full.dat.wide), ] #ensuring we have complete cases for stan

##### Making MARSS dataset ####
#In this section we make the MARSS dataset that will be used in the MASRSSDFA code. We need
# chum, pink, pollock 3+, and pollock recruits. We don't add the lags here and insted add them to 
# the latent trend when the data is re-read in (this makes it easier to change to 3 or 4 without
#have to rerun the MARSS code)
#assigning year first and last is the time period over which the data is standardized
yr_fst <- 1970 
yr_last <- 2022
pollock <-read.csv('ocean/EBS_Pollock.csv')%>%
  filter(Year >= yr_fst & Year <= yr_last) #pollock datasets for EBS
salmon <-read.csv('ocean/salmonabundanceOkeetal.csv') ### reading in the salmon datasets
chumMARSS <- salmon%>%
  rename("year"='Year')%>%
  mutate(chum_stand = 
           (chumTotal-mean(salmon$chumTotal))/sd(salmon$chumTotal))%>% #standardizng for mean 0 and sd 1
  select(year, chum_stand)%>%
  ungroup()

pinkMARSS <- salmon%>%
  rename("year"='Year')%>%
  mutate(pink_stand = 
           (pinkTotal-mean(salmon$pinkTotal))/sd(salmon$pinkTotal))%>%
  select(year, pink_stand)%>%
  ungroup()

pollock_BiomassMARSS <-pollock%>% 
  select(Year, Age_3_Biomass)%>%
  mutate(year = Year)%>%
  mutate(Age_3_Biomass_stand = 
           (Age_3_Biomass-mean(pollock$Age_3_Biomass))/sd(pollock$Age_3_Biomass))%>%
  ungroup()%>%
  select(year, Age_3_Biomass_stand)

pollock_RecruitmentMARSS <-pollock%>%
  select(Recruitment, Year)%>%
  rename(year = Year)%>%
  mutate(Recruitment_stand = 
           (Recruitment-mean(pollock$Recruitment))/sd(pollock$Recruitment))%>%
  ungroup()%>%
  select(year, Recruitment_stand)

marineMARSS <- merge(pollock_BiomassMARSS, pollock_RecruitmentMARSS, by ='year', all.x = TRUE)%>%
  merge(pinkMARSS, by='year', all.x = TRUE)%>%
  merge(chumMARSS, by='year', all.x = TRUE)
saveRDS(marineMARSS, file = "Chinook/Output/posteriors/marine.rds")

#### Adding Covariates  ####
#### Ocean Covariates ####

SST_dat <-read.csv('ocean/BS-SST-2023-09-28.csv')#reading in winter SST data
SST_winter <-SST_dat%>% 
  filter(Ecosystem_sub=='Southeastern Bering Sea')%>% #selecting region from dataset
  filter(month==1|month==2|month==3)%>% #selecting winter months
  group_by(year) %>%
  select(meansst)%>%
  summarise(winter_mean=mean(meansst))#calculating annual mean grouped by year across months

SST_winter <-SST_winter%>% 
  rename(Year=year)%>%
  mutate(year = Year-3)%>% ##assigning brood year offset: 3
  mutate(Winter_stand = 
           (winter_mean-mean(SST_winter$winter_mean))/sd(SST_winter$winter_mean))%>% #standardizing
  ungroup()

#adding two years of data to the dataset becuase the TS is two short. This only imacts ~8 observations
#an alternative option would be to give them a value of 0 

SST_winter_south <-SST_winter%>%
  add_row(data.frame(Year = c(1984, 1983, 1982, 1981, 1980),winter_mean=rep(0,5), 
            year=c(1981,1980,1979, 1978, 1977),Winter_stand=rep(0,5)))%>%
    arrange(year)

#we will also look into including the same dataset for summer but not eliminate teh ncep
#dataset 

SST_summer_south <-SST_dat%>% 
  filter(Ecosystem_sub=='Southeastern Bering Sea')%>% #selecting region from dataset
  filter(month==6|month==7|month==8)%>% #selecting winter months
  group_by(year) %>%
  select(meansst)%>%
  summarise(summer_south_mean=mean(meansst))#calculating annual mean grouped by year across months

SST_summer_south <-SST_summer_south%>% 
  rename(Year=year)%>%
  mutate(year = Year-2)%>% ##assigning brood year offset: 3
  mutate(summer_stand_alt = 
           (summer_south_mean-mean(SST_summer_south$summer_south_mean))/sd(SST_summer_south$summer_south_mean))%>% #standardizing
  select(Year,year, summer_stand_alt)%>%
  add_row(data.frame(Year = c(1984,1983, 1982, 1981,1980),summer_stand_alt=rep(0,5), 
                     year=c(1982,1981,1980, 1979, 1978)))%>%
  mutate(region2="Kuskokwim")%>%
  ungroup()

SST_summer_north <-SST_dat%>% 
  filter(Ecosystem_sub=='Northern Bering Sea')%>% #selecting region from dataset
  filter(month==6|month==7|month==8)%>% #selecting winter months
  group_by(year) %>%
  select(meansst)%>%
  summarise(summer_north_mean=mean(meansst))#calculating annual mean grouped by year across months

SST_summer_north <-SST_summer_north%>% 
  rename(Year=year)%>%
  mutate(year = Year-2)%>% ##assigning brood year offset: 3
  mutate(summer_stand_alt = 
           (summer_north_mean-mean(SST_summer_north$summer_north_mean))/sd(SST_summer_north$summer_north_mean))%>% #standardizing
  select(Year,year, summer_stand_alt)%>%
  add_row(data.frame(Year = c(1984,1983, 1982),summer_stand_alt=rep(0,3), 
                     year=c(1982,1981,1980)))%>%
  mutate(region2="Yukon")%>%
  ungroup()

SST_summerALT<-SST_summer_south%>%
  bind_rows(SST_summer_north)%>%
  arrange(year)


#add_row(data.frame(Year = c(1984, 1983),winter_mean=rep(SST_winter$winter_mean[1],2), 
#           year=c(1981,1980),Winter_stand=rep(SST_winter$Winter_stand[1],2)))%>%
#  arrange(year)
#
#for summer SST we have two regions based on Yasumiishi et al. and we follow the methods described
#in that paper to get SST data. We calculate a "north" variable for Yukon and a "south" variable for the 
#Kuskokwim that are more associated with the environment of outmigration

SST_summer<-read.csv('ocean/NCEP_NCAR_SST.csv') #data from NCEP/NCAR
SST_summer_north<-SST_summer%>%
  group_by(Year) %>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  select(EarlySummer_North)%>%
  mutate(year = Year-2)%>%#assigning brood year offset: 2
  mutate(EarlySummer_stand = 
           (EarlySummer_North-mean(SST_summer$EarlySummer_North))/sd(SST_summer$EarlySummer_North))%>%
  ungroup()%>%
  select(year, EarlySummer_stand)%>%
  mutate(region2 = "Yukon")

SST_summer_south<-SST_summer%>%
  group_by(Year) %>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  select(EarlySummer_South)%>%
  mutate(year = Year-2)%>% #assigning brood year offset: 2
  mutate(EarlySummer_stand = 
           (EarlySummer_South-mean(SST_summer$EarlySummer_South))/sd(SST_summer$EarlySummer_South))%>%
  ungroup()%>%
  select(year, EarlySummer_stand)%>%
  mutate(region2 = "Kuskokwim")

SST_summer<-SST_summer_south%>%
  bind_rows(SST_summer_north)

#The next few marine variables are a bit more straightforward

IceExtent <-read.csv('ocean/SeaIceIndices.csv') 
IceExtent<-IceExtent%>%
  group_by(Year) %>%
  select(ICIA)%>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(year = Year-2)%>%#assigning brood year offset: 2
  mutate(ICIA_stand = 
           (ICIA-mean(IceExtent$ICIA))/sd(IceExtent$ICIA))%>% #standardizing
  ungroup()%>%
  select(year, ICIA_stand)

IceRetreat <-read.csv('ocean/SeaIceIndices.csv') 
IceRetreat<-IceRetreat%>%
  group_by(Year) %>%
  select(IRI)%>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(year = Year-2)%>% #assigning brood year offset: 2
  mutate(IRI_stand = 
           (IRI-mean(IceRetreat$IRI))/sd(IceRetreat$IRI))%>% #standardizing
  ungroup()%>%
  select(year, IRI_stand)

uwind <-read.csv('ocean/uwindMonthly.csv') ### turning NPGO into an annual mean
uwind <- uwind%>%
  group_by(Year) %>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(avg = mean(c(X6, X7, X8)))%>% #getting the average across the summer
  rename("year"='Year')%>%
  mutate(year = year-2)#assigning brood year offset: 2
uwind <- uwind%>%
  mutate(uwind_stand=avg-mean(uwind$avg)/sd(uwind$avg))%>%
  select(year, uwind_stand)%>%
  ungroup()

vwind <-read.csv('ocean/vwindMonthly.csv') ### turning NPGO into an annual mean
vwind <- vwind%>%
  group_by(Year) %>%
  filter(Year >= yr_fst & Year <= yr_last)%>%
  mutate(avg = mean(c(X6, X7, X8)))%>%
  rename("year"='Year')%>%
  mutate(year = year-2)#assigning brood year offset: 2
vwind <- vwind%>%
  mutate(vwind_stand = (avg-mean(vwind$avg))/sd(vwind$avg))%>% #standardizing
  select(year, vwind_stand)%>%
  ungroup()

sizeCA<-read.csv('ocean/YKCA_femalesize.csv')%>%
  select(sampleYear, Length)%>%
  mutate(size_stand=(Length-mean(Length))/sd(Length),
         region2="Yukon", region="Yukon (CA)")%>%
  rename(size=Length, year=sampleYear)
sd(sizeCA$Length)

sizeUS<-read.csv('ocean/SizeTrend2016.csv')%>% #this dataset is derived from MARSSDFA
  mutate(year=Year-0)%>%#assigning brood year offset: 0
  rename(region2=Region)%>%
  rename(size=Size)%>%
  select(year, size, region2,size_stand)%>%
  mutate(region=ifelse(region2=="Yukon","Yukon (US)", "Kuskokwim"))
size<-sizeUS%>%select(-region)
sizeALT<-sizeUS%>%add_row(sizeCA)%>%rename(size_alt=size_stand)

marine<-read.csv('ocean/MarineTrend.csv') #this is the DFA latent trend
marinecov<-marine%>%
  mutate(year=Year-3)%>% #assigning brood year offset: 3 (can also test 4)
  select(year, marine)%>%
  add_column(marine_stand =(marine[,2]-mean(marine$marine))/sd(marine$marine))%>%
  select(year, marine_stand)


####Freshwater Covariates ####

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
  mutate(year=Year-2)%>%
  bind_rows(Kuskokwim %>% mutate(breakup2 = (day - mean(Kuskokwim$day))/sd(Kuskokwim$day))%>%
              mutate(year=Year-2))%>%
  bind_rows(Dawson %>% mutate(breakup2 = (day - mean(Dawson$day))/sd(Dawson$day))%>%
              mutate(year=Year-2))%>%
  select(year, region, breakup2)
breakup2 <-data.frame(breakup2)%>%
  select(year, region, breakup2)

# We now use Becky's datasets which need to be standardized within pop  
#This function takes a dataset "data" and adds a new column to it "stand" where a specific FW variable "var" is standardized
# by population. It essentially loops over populations standardizing a particular variable

RiverStand <- function(data, var) { 
  TP<-data.frame(Population =rep(NA, length(data$Population)))
  for(i in 1:length(unique(data$Population))){ #looping over population i
    temp <- data%>%
      filter(Population==unique(data$Population)[i]) #filter data by pop
    x=as.numeric(unlist(temp[,var])) #select just the data from the var column
    obs <- length(temp$Population)#number of observations just for the selected population
    temp2 <- temp%>%
      mutate(stand =(x-mean(x))/sd(x)) #standardize the selected variable and add it to the temp dataframe as its own column
    TP[((i-1)*obs)+1:(i*obs),1:(ncol(data)+1)]<- temp2#this fills in TP at the point pop i-i (previous pop) data ends to the length of this pop
  }
  return(TP[1:length(data$Population),]) #return TP - a dataframe with the original data plus a new stand column
}


pops_allMets<- readRDS("precipitation/output/pops_allMets.rds")
pops_allMetsV3<- readRDS("precipitation/output/pops_allMets_v3.rds")

maxDaily_migrate_stand <- RiverStand(pops_allMetsV3, which(colnames(pops_allMetsV3) == "maxDaily_migrate"))%>% 
  mutate(year = year-0,maxDaily_migrate_stand=stand,pop=Population)%>% #assigning brood year offset: 0
  select(pop, year, maxDaily_migrate_stand) #selecting the columns we need 

pops_allMetsV3%>%select(maxDaily_migrate,Population)%>%
 # group_by(Population)%>%
  summarise(sd=sd(maxDaily_migrate))

maxDaily_spawn_stand <- RiverStand(pops_allMetsV3, which(colnames(pops_allMets) == "maxDaily_spawn"))%>% 
  mutate(year = year-0,maxDaily_spawn_stand=stand,pop=Population)%>% #assigning brood year offset: 0
  select(pop, year, maxDaily_spawn_stand) #selecting the columns we need 


medq_rear_stand <- RiverStand(pops_allMetsV3, which(colnames(pops_allMetsV3) == "medq_rear"))%>% 
  mutate(year = year-1,medq_rear_stand=stand,pop=Population)%>% #assigning brood year offset: 0
  select(pop, year, medq_rear_stand) #selecting the columns we need 


max5dprcp_stand <- RiverStand(filter(pops_allMetsV3, year<2019), which(colnames(pops_allMetsV3) == "max5dprcp_spawn"))%>%
  mutate(year = year-1, max5dprcp_stand=stand,pop=Population)%>% #assigning brood year offset: 0
  select(pop, year, max5dprcp_stand)

pops_allMetsV3%>%select(max5dprcp_spawn,Population)%>%
  # group_by(Population)%>%
  summarise(sd=sd(na.omit(max5dprcp_spawn)))

maxq_spawn_stand <- RiverStand(pops_allMetsV3,which(colnames(pops_allMetsV3) == "maxq_spawn"))%>%
  mutate(year = year-0,maxq_spawn_stand=stand,pop=Population)%>%#assigning brood year offset: 0
  select(pop, year, maxq_spawn_stand)

mean_swe_icu <- RiverStand(filter(pops_allMetsV3, year<2019), which(colnames(pops_allMetsV3) == "mean_swe"))%>%
  mutate(year = year-0,mean_swe_icu=stand,pop=Population)%>%#assigning brood year offset: 0
  select(pop, year, mean_swe_icu)

mean_swe_rear <- RiverStand(filter(pops_allMetsV3, year<2019), which(colnames(pops_allMetsV3) == "mean_swe"))%>%
  mutate(year = year-1,mean_swe_rear=stand,pop=Population)%>%#assigning brood year offset: 1
  select(pop, year, mean_swe_rear)


mnprcp_rear_stand <- RiverStand(filter(pops_allMetsV3, year<2019),which(colnames(pops_allMetsV3) == "mnprcp_rear"))%>%
  mutate(year = year-1,mnprcp_rear_stand=stand,pop=Population)%>%#assigning brood year offset: 1
  select(pop, year, mnprcp_rear_stand)

cdd_rear_stand <- RiverStand(pops_allMetsV3,which(colnames(pops_allMetsV3) == "cdd_rear"))%>%
  mutate(year = year-1, pop=Population, cdd_rear_stand=stand)%>%
  select(pop, year, cdd_rear_stand)



##### Simulated Covariates ####

#here we simulate some data that we fit to the model to test what the results look like with random covariates
#we will add this to same dataframe as the rest of the covariates so we can easily select swap out the covariates
# to the actual data

sim<- data.frame(year = IceRetreat$year)
nsim<-11

for(i in 1:nsim){
  sim[,i]<-rnorm(length(IceRetreat$year),0,1)
  sim[,1]<-IceRetreat$year
}

#### Making a full dataset with salmon and cov data ####
#here we combine all of the standardized, offset data, into a single dataframe with the salmon data so we can 
#easily select columns when running the model
full.dat.wide.cov <- merge(full.dat.wide, SST_summer, by =c('region2','year'))%>%
  merge(SST_summerALT, by=c('region2','year'))%>%
  merge(SST_winter_south, by=c('year'))%>%
  merge(marinecov, by='year')%>%
  merge(marineMARSS%>%mutate(year=year+3), by='year')%>% #all the datasets that went into the MARSS model with an offset
  merge(IceExtent, by='year')%>%
  merge(IceRetreat, by='year')%>%
  merge(size, by=c('region2','year'))%>%
  merge(vwind, by='year')%>%
  merge(uwind, by='year')%>%
  merge(breakup2, by=c('region','year'))%>%
  merge(max5dprcp_stand, by=c('pop','year'))%>%
  merge(maxq_spawn_stand, by=c('pop','year'))%>%
  merge(mean_swe_icu, by=c('pop','year'))%>%
  merge(mean_swe_rear, by=c('pop','year'))%>%
  merge(medq_rear_stand, by=c('pop','year'))%>%
  merge(mnprcp_rear_stand, by=c('pop','year'))%>%
  merge(maxDaily_spawn_stand, by=c('pop','year'))%>%
  merge(maxDaily_migrate_stand, by=c('pop','year'))%>%
  merge(cdd_rear_stand, by=c('pop','year'))%>%
  merge(sim, by=c('year'))%>% #simulated covariates
  arrange(pop, year)



#### Calculating vif for covariates included in the model####
lm <- lm(log(spawners/recruits)~
           marine_stand+ 
           size_stand+
           Winter_stand+
           # summer_stand_alt+
           EarlySummer_stand+
           uwind_stand+
           ICIA_stand+
           breakup2+
           maxq_spawn_stand+
           medq_rear_stand+
           # mnprcp_rear_stand+
           cdd_rear_stand+
           maxDaily_migrate_stand, data=full.dat.wide.cov)
vif(lm)
summary(lm)


#### Covariate correlation #####

## this is for the covariate plot with everything contained

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

labs1<-c(
       "River Ice \n Breakup",
       "Max 5-day \n precip",
       "Max \n Streamflow \n Spawn",
       "Snowpack \n Incubation",
       "Snowpack \n Rearing",
       "Median \n Discharge  \n Rear",
       "Median \n Precip Rear",
       "Max Daily \n Temp Spawn",
       "Max Daily \n Temp Migrate",
       "Cum. Degree \n Days Rear")

pdf(file = "Chinook/Output/Figures/Supplement/FigS14_CovsCorrFW.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)
pairs(~breakup2+
        max5dprcp_stand+
        maxq_spawn_stand+
        mean_swe_icu+
        mean_swe_rear+
        medq_rear_stand+
        mnprcp_rear_stand+
        maxDaily_spawn_stand+
        maxDaily_migrate_stand+
        cdd_rear_stand, 
      data = full.dat.wide.cov,
      labels=labs1, 
      upper.panel = panel.cor,         # Disabling the upper panel
     # diag.panel = panel.hist,
      lwd=3,col= 'grey', pch=16,cex=0.5,font.labels=2,
      lower.panel = panel.smooth)
dev.off()
labs2<- c("Winter \n SST",
         "Pollock \n Age-3", 
          "Pollock \n Rec.",
          "Summer \n SST \n NCEP",
          "Summer \n SST",
          "Body Size",
          "Chum \n Salmon",
          "Pink \n Salmon",
          "v-wind",
          "u-wind",
          "Sea \n Ice \n Cover",
          "Sea \n Ice \n Retreat")
pdf(file = "Chinook/Output/Figures/Supplement/FigS15_CovsCorrMar.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)
pairs(~Winter_stand+
        Age_3_Biomass_stand+
        Recruitment_stand+
        EarlySummer_stand+
        summer_stand_alt+
        size+
        # marine_stand+
        chum_stand+
        pink_stand+
        vwind_stand+
        uwind_stand+
        ICIA_stand+
        IRI_stand,
      data = full.dat.wide.cov,
      labels=labs2, 
      upper.panel = panel.cor,         # Disabling the upper panel
      # diag.panel = panel.hist,
      lwd=3,col= 'grey', pch=16,cex=0.5,font.labels=2,
      lower.panel = panel.smooth)
dev.off()

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

pdf(file = "Chinook/Output/Figures/Supplement/FigS16_CovsCorrModel.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10)
pairs(~ Winter_stand+
        ICIA_stand+
        EarlySummer_stand+
        size+
        marine_stand+
        uwind_stand+
        breakup2+
        maxq_spawn_stand+
        medq_rear_stand+
        maxDaily_migrate_stand+
        cdd_rear_stand, 
      data = full.dat.wide.cov,
      labels=labs3, 
      upper.panel = panel.cor,         # Disabling the upper panel
      # diag.panel = panel.hist,
      lwd=3,col= 'grey', pch=16,cex=0.5,font.labels=2,
      lower.panel = panel.smooth)
dev.off()

#TS summary for Table S1

full.dat.wide.cov%>%count(pop)

print(full.dat.wide.cov %>%
        group_by(pop) %>%
        summarise(yfirst=min(year), ylast=max(year)), n=26)

print(full.dat.wide.cov %>%
        group_by(pop) %>%
        summarise(y1=max(year)), n=26)

yearsumm <- full.dat.wide.cov %>%
  group_by(pop) %>%
  summarise(yfirst=min(year), ylast=max(year))
full.dat.wide.cov %>%filter(pop=="Goodnews")
saveRDS(full.dat.wide.cov, file = "Chinook/Output/posteriors/full.dat.wide.cov.offset.rds")
saveRDS(yearsumm, file = "Chinook/Output/posteriors/yearsumm.rds")
