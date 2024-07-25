library(dplyr)
library(nord)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(ggplot2)
library(cowplot)
library(Cairo)


#### Organizing data into single dataset ####
c.dat <-read.csv('Chinook/Data/chenasalchaSRA.csv')%>%
  filter(quantity == "spawners"|quantity == "recruits")%>%
  mutate(region='Yukon (US)')%>%
  rename(error ='cv') %>%
  mutate(errortype='cv')%>%
  mutate(CV = error)%>%
  mutate(SD = mean * CV)%>%
  filter(pop=="Chena"& year>1985)

s.dat <-read.csv('Chinook/Data/chenasalchaSRA.csv')%>%
  filter(quantity == "spawners"|quantity == "recruits")%>%
  mutate(region='Yukon (US)')%>%
  rename(error ='cv') %>%
  mutate(errortype='cv')%>%
  mutate(CV = error)%>%
  mutate(SD = mean * CV)%>%
  filter(pop=="Salcha"& year>1986)

cs.dat<-rbind(s.dat,c.dat)

ag.dat <-read.csv('Chinook/Data/giasaandreafskySR.csv')%>%
  filter(quantity == "spawners"|quantity == "recruits")%>%
  mutate(region='Yukon (US)')%>%
  rename(error ='sd') %>%
  mutate(errortype='sd')%>%
  mutate(SD = error)%>%
  mutate(CV = SD/mean)

g.dat <-read.csv('Chinook/Data/goodnewsSR.csv')%>%
  mutate(region='Kuskokwim')%>%
  mutate(SD = NA)%>%
  mutate(CV = NA)

kusk.dat1 <-read.csv('Chinook/Data/kusko-multi-SRA-ests.csv')%>%
  filter(model_id == "SSM-VM" & scenario=='base')%>%
  filter(quantity == "logR"|quantity == "S")%>%
  select(quantity, pop, year, mean, sd)
kusk.dat2 <- kusk.dat1 %>%
  filter(quantity == "logR")%>%
  mutate(CV = sqrt(exp(sd^2)-1))%>%
  mutate(mean2 =exp(mean+0.5*sd)-1)%>%
  select(quantity, pop, year, mean2, sd, CV)%>%
  mutate(quantity='recruits')%>%
  rename(mean = 'mean2')%>%
  rename(error ='sd') %>%
  mutate(errortype='sd log scale')%>%
  mutate(SD=CV*mean)
kusk.dat <- kusk.dat1 %>%
  filter(quantity == "S")%>%
  mutate(quantity='spawners')%>%
  rename(error='sd')%>%
  mutate(errortype ='sd')%>%
  mutate(quantity='spawners')%>%
  mutate(SD=error)%>%
  mutate(CV = SD/mean)%>%
  bind_rows(kusk.dat2)%>%
  mutate(region='Kuskokwim')

yuk.dat1 <-read.csv('Chinook/Data/yukon-multi-SRA-ests.csv')%>%
  filter(quantity == "logR"|quantity == "S")%>%
  select(quantity, pop, year, mean, sd)
yuk.dat2 <- yuk.dat1 %>%
  filter(quantity == "logR")%>%
  mutate(CV = sqrt(exp(sd^2)-1))%>%
  mutate(mean2 =exp(mean+0.5*sd)-1)%>%
  select(quantity, pop, year, mean2, sd,CV)%>%
  mutate(quantity='recruits')%>%
  rename(mean = 'mean2')%>%
  rename(error ='sd') %>%
  mutate(errortype='sd log scale')%>%
  mutate(SD=CV*mean)
yuk.dat <- yuk.dat1 %>%
  filter(quantity == "S")%>%
  rename(error ='sd') %>%
  mutate(errortype='sd')%>%
  mutate(quantity='spawners')%>%
  mutate(SD=error)%>%
  mutate(CV = SD/mean)%>%
  bind_rows(yuk.dat2)%>%
  mutate(region='Yukon (CA)')

full.dat.long <- yuk.dat %>%
  bind_rows(kusk.dat)%>%
  bind_rows(g.dat)%>%
  bind_rows(cs.dat)%>%
  bind_rows(ag.dat)

full.dat.wide.mean <-full.dat.long%>%
  select(quantity, pop, year, mean, region)%>%
  pivot_wider(names_from = quantity, 
              values_from = mean)%>%
  mutate(ln.sr = log(recruits/spawners))%>%
  mutate(recth = recruits/1000)%>%
  mutate(spawnth = spawners/1000)

full.dat.wide.CV <-full.dat.long%>%
  select(quantity, pop, year, CV, region)%>%
  pivot_wider(names_from = quantity, 
              values_from = CV)%>%
  rename(rec.CV=recruits)%>%
  rename(spawn.CV =spawners)


full.dat.wide <-full.dat.long%>%
  select(quantity, pop, year, mean, region)%>%
  pivot_wider(names_from = quantity, 
              values_from = mean)%>%
  mutate(ln.sr = log(recruits/spawners))%>%
  mutate(recth = recruits/1000)%>%
  mutate(spawnth = spawners/1000)%>%
  left_join(full.dat.wide.CV)%>%
  mutate(sr.CV= sqrt(rec.CV^2+spawn.CV^2))



yearsumm<-readRDS("Chinook/Output/posteriors/yearsumm.rds")%>%
  left_join(full.dat.wide %>%
              select(pop, year, region, spawners,recruits)%>%
              group_by(pop) %>%
              summarise(y1=min(year)))%>%
  left_join(full.dat.wide %>%
              select(pop, region)%>%
              distinct())


print(full.dat.wide %>% filter(pop=="Chena"),n=41)
#### Rearranging Levels ####

pops.regions <- c("Aniak","George","Goodnews", "Holitna",
                  "Holokuk", "Kisaralik","Kogrukluk","Kwethluk",
                  "Oskawalik","Pitka", "Swift",      
                  "Takotna","Tatlawiksuk","Tuluksak",
                  
                  "Carmacks","LwrMain","MidMain",    
                  "Pelly","Stewart","Teslin",      
                  "UprLksMain","White-Donjek",
                  
                  "EFAndreafsky","Chena","Gisasa",
                  "Salcha")

full.dat.wide.levels <- data.frame(full.dat.wide)
full.dat.wide.levels$pop <-factor(full.dat.wide.levels$pop, levels=pops.regions) 

yearsumm.levels<-data.frame(yearsumm)
yearsumm.levels$pop <-factor(yearsumm.levels$pop, levels=pops.regions) 

lev<- c("Aniak","George","Goodnews", "Holitna",
        "Holokuk", "Kisaralik","Kogrukluk","Kwethluk",
        "Oskawalik","Pitka", "Swift",      
        "Takotna","Tatlawiksuk","Tuluksak",
        
        "Carmacks","Lower Mainstem","Mid Mainstem",    
        "Pelly","Stewart","Teslin",      
        "Upper Lakes Mainstem","White-Donjek",
        
        "Andreafsky East Fork","Chena","Gisasa",
        "Salcha")

levels(full.dat.wide.levels$pop) <- lev
levels(yearsumm.levels$pop) <- lev



##### Plotting Productivity Time series ####
lumina<-nord(n = 4, palette = "lumina")[4:2]

full.dat.wide.levels$sr.CV*full.dat.wide.levels$ln.sr

prod.time <- ggplot(data = full.dat.wide.levels, aes(x = year, y = ln.sr, color=region)) +
 geom_ribbon(alpha=0.2,linetype = 0,aes(ymin=ln.sr-sr.CV*ln.sr, ymax=ln.sr+sr.CV*ln.sr,lwd=0))+
  geom_point(size=0.75, aes(lty=region, col=region, shape=region), cex=2) +
  geom_line() +
  scale_color_manual(values = lumina)+
  geom_hline(yintercept = 0, lty = "dotted") +
  facet_wrap(.~pop, scales='free',ncol = 3) +
  scale_x_continuous(name = "Brood year", breaks = seq(1980, 2010, by = 10)) +
  expand_limits(x = c(1979,2011)) +
  scale_y_continuous(name = "Productivity Index (ln[R/S])")+
  theme_bw()+
  labs(col = "Subegion",shape = "Subegion",lty = "Subegion")
prod.time


##### Plotting Recruitment versus Spawning Abundance ####


lumina2 <- nord(n = 14, palette = "lumina")
frost <- nord(n = 10, palette = "frost")
prarie <- nord(n = 9, palette = "afternoon_prarie")


spawn.v.rec <- ggplot(data = full.dat.wide.levels, aes(x = spawnth, y = recth, color=year)) +
  geom_point() +
  scale_color_gradient(low=lumina2[1], high=lumina2[13], name="Brood Year")+
  facet_wrap(.~pop, scales='free', ncol = 3)+
  scale_x_continuous(name = "Spawning Abundance (thousands)") +
  scale_y_continuous(name = "Recruitment (thousands)")
spawn.v.rec

pdf(file = "Chinook/Output/Figures/Supplement/FigS2_RecruitsvsSpawners.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 10)
spawn.v.rec
dev.off()

#### Spawning Abundance Time Series ####
lumina2 <- nord(n = 14, palette = "lumina")
unique(full.dat.wide.levels$pop)

spawn.time <- ggplot(data = full.dat.wide.levels, aes(x = year, y = spawnth, color=region)) +
  geom_ribbon(alpha=0.2,linetype = 0,aes(ymin=spawnth-spawn.CV*spawnth, ymax=spawnth+spawn.CV*spawnth))+
  geom_point(size=1.5, aes(col=region, shape=region)) +
  geom_line(aes(lty=region)) +
  scale_color_manual(values = c("#8D8EB1", "#3A7398","#222B4C"))+
 # geom_hline(yintercept = 0, lty = "dotted") +
  facet_wrap(.~pop,scale='free', ncol = 3) +
  scale_x_continuous(name = "Return Year", breaks = seq(1980, 2010, by = 10)) +
  expand_limits(x = c(1979,2011)) +
  scale_y_continuous(name = "Spawner Abundance (thousands)")+
  geom_rect(aes(xmin = y1, xmax = yfirst, ymin = -Inf, ymax = Inf),
            alpha = 0.3, fill = lumina[1],
            data = yearsumm.levels,
            inherit.aes = FALSE) +
  theme_bw()+
  labs(col = "Subregion",shape = "Subregion",lty = "Subregion")
spawn.time

pdf(file = "Chinook/Output/Figures/Supplement/FigS1_SpawnerTimeSeries.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 10)
spawn.time
dev.off()

#### Spawn Versus Productivity ####

spawn.v.prod <- ggplot(data = full.dat.wide.levels, aes(x = spawnth, y = ln.sr, color=year)) +
  geom_point() +
  geom_smooth(aes(group=pop), method='lm',formula=y~x, col=lumina2[14])+
  scale_color_gradient(low=lumina2[1], high=lumina2[13],name="Brood Year")+
  facet_wrap(.~pop, scales='free', ncol = 3)+
  scale_x_continuous(name = "Spawner Abundance (thousands)") +
  scale_y_continuous(name = 'Productivity Index (ln[R/S])')
spawn.v.prod

pdf(file = "Chinook/Output/Figures/Supplement/FigS3_ProductivityvsSpawners.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 10)
spawn.v.prod
dev.off()

