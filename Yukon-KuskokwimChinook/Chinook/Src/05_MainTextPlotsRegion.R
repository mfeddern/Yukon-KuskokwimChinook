####DATA####
library(FSA)
library(nord)
library(bayesplot)
library(ggpubr)
library(nord)
library(ggplot2)
library(dplyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(nord)
library(tidyr)

lumina<-nord(n = 6, palette = "lumina")
#bhfit <- readRDS("bhfit.rda")
group.posteriors<-readRDS("Chinook/Output/posteriors/group.posteriors.rds")
thetas<-readRDS("Chinook/Output/posteriors/thetas.posteriors.rds")
parameter.posteriors<-readRDS("Chinook/Output/posteriors/parameter.posteriors.rds")
dat<-readRDS("Chinook/Output/posteriors/full.dat.wide.cov.offset.rds")
recruits<-readRDS("Chinook/Output/posteriors/recruits.rds")
unstand <- readRDS("Chinook/Output/posteriors/unstandcov3.rds")
residual <- readRDS("Chinook/Output/posteriors/residualcov.rds")
residualnc <- readRDS("Chinook/Output/posteriors/residualnc.rds")
unlag_cov <- readRDS( "Chinook/Output/posteriors/unlagcov.rds")
mean_cov <- readRDS( "Chinook/Output/posteriors/mean.theta.rds")

# Define the conditions and replacement values
conditions <- c("Size", "Ice Cover Index")
replacement_values <- c("Body Size", "Sea Ice Cover")
group.posteriors$Covar.Name <- replace(group.posteriors$Covar.Name, group.posteriors$Covar.Name %in% conditions, replacement_values)
thetas$Covar.Name <- replace(thetas$Covar.Name, thetas$Covar.Name %in% conditions, replacement_values)



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

full.dat.wide.levels <- data.frame(dat)
full.dat.wide.levels$pop <-factor(full.dat.wide.levels$pop, levels=pops.regions) 

levels(full.dat.wide.levels$pop) <- 
  c("Aniak","George","Goodnews", "Holitna",
    "Holokuk", "Kisaralik","Kogrukluk","Kwethluk",
    "Oskawalik","Pitka", "Swift",      
    "Takotna","Tatlawiksuk","Tuluksak",
    
    "Carmacks","Lower Mainstem","Mid Mainstem",    
    "Pelly","Stewart","Teslin",      
    "Upper Lakes Mainstem","White-Donjek",
    
    "Andreafsky East Fork","Chena","Gisasa",
    "Salcha")


##### Figure 2 Plotting Productivity Time series ####
lumina<-nord(n = 8, palette = "lumina")
full.dat.wide.levels$sr.CV*full.dat.wide.levels$ln.sr
prod.time <- ggplot(data = full.dat.wide.levels, aes(x = year, y = log(recruits/spawners), color=region)) +
  geom_ribbon(alpha=0.2,linetype = 0,aes(ymin=(log(recruits/spawners)-2*sqrt(spawn.SDlog^2+rec.SDlog^2)), ymax=log(recruits/spawners)+2*sqrt(spawn.SDlog^2+rec.SDlog^2)))+
  geom_point(size=0.75) +
  geom_line() +
  scale_color_manual(values =  c(lumina[3],lumina[4],lumina[7]), name="Subregion")+
  geom_hline(yintercept = 0, lty = "dotted") +
  facet_wrap(.~pop, scales="free_y",ncol = 3) +
  scale_x_continuous(name = "Brood year", breaks = seq(1980, 2010, by = 10)) +
  expand_limits(x = c(1979,2011)) +
  theme_bw()+
  theme(legend.position = c(0.9, -0.05),
        legend.justification = c(1, 0),
        #text=element_text(size=20), #change font size of all text
        axis.text=element_text(size=12), #change font size of axis text
        axis.title=element_text(size=14), #change font size of axis titles
        plot.title=element_text(size=14), #change font size of plot title
        legend.text=element_text(size=14), #change font size of legend text
        strip.text = element_text(size=14), #facet label
        legend.title=element_text(size=16))+
  scale_y_continuous(name = "Productivity Index (ln[R/S])")

pdf(file = "Chinook/Output/Figures/MainText/Figure2_ProductivityTimeSeries.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 10)
prod.time
dev.off()
##### Figure 3 Mean Theta Plots #####

### Rearranging Levels ###
pops.regions <- c("Aniak","George","Goodnews", "Holitna","Holokuk", "Kisaralik","Kogrukluk","Kwethluk","Oskawalik","Pitka", "Swift", "Takotna","Tatlawiksuk","Tuluksak",
                  "EFAndreafsky","Chena","Gisasa","Salcha",
                  "Carmacks","LwrMain","MidMain","Pelly","Stewart","Teslin","UprLksMain","White-Donjek")

df.levels <- data.frame(thetas)
df.levels$Covar.Name <- replace(df.levels$Covar.Name, df.levels$Covar.Name %in% conditions, replacement_values)

df.levels$pop <-factor(df.levels$pop, levels=pops.regions) 

levels(df.levels$pop) <- 
  c("Aniak","George","Goodnews", "Holitna","Holokuk", "Kisaralik","Kogrukluk","Kwethluk","Oskawalik","Pitka", "Swift","Takotna","Tatlawiksuk","Tuluksak",
    "Andreafsky East Fork","Chena","Gisasa","Salcha",
    "Carmacks","Lower Mainstem","Mid Mainstem", "Pelly","Stewart","Teslin", "Upper Lakes Mainstem","White-Donjek")

cov.lifestage  <- 
  c('Maximum daily streamflow',#incubation
    'Daily stream temp',"Median daily streamflow",
    "River Ice Breakup Date", # smolt outmigration
    'Sea Ice Cover', 'Summer Sea Surface Temperature',
    "Cross-shelf wind","Winter Sea Surface Temperature",
    "Marine Competitors", #early marine
     "Maximum Daily Migration Temp",
    "Body Size")



group.levels <- data.frame(group.posteriors)
group.levels$Covar.Name <-factor(group.levels$Covar.Name, levels=cov.lifestage) 
levels(group.levels$Covar.Name) <- cov.lifestage
mean_cov$Covar.Name <-factor(mean_cov$Covar.Name, levels=cov.lifestage) 
levels(mean_cov$Covar.Name) <- cov.lifestage
pop.levels <- data.frame(thetas)
pop.levels$Covar.Name <-factor(pop.levels$Covar.Name, levels=cov.lifestage) 

levels(pop.levels$Covar.Name) <- cov.lifestage


### Creating Plots ###
### Figure 3a Mean Effect ###
Param.Name <- unique(group.levels$Param.Name)
breaks <- c("Incubation","Juvenile Rearing" ,"Outmigration & Early Marine","Marine Residence","Adult Migration","Spawning", " ")
lumina2<-nord(n = 7, palette = "lumina")
Aurora2<-hcl.colors(n = 6, palette = "dynamic")
names.params <- unique(group.levels$Param)

colnames(group.levels)
tableS7<-group.levels%>%filter(Param=="mu_coef")%>%
  select(Covar.Name, Region, mean, lower.80, upper.80)%>%
  mutate(mean=mean*100,lower.80=lower.80*100, upper.80=upper.80*100)
write.csv(tableS7,"Chinook/Output/Tables/TableS7.csv")


colnames(group.levels)
tablePop<-df.levels%>%
  select(Covar.Name, region,pop, mean, lower.80, upper.80)%>%
  mutate(mean=mean*100,lower.80=lower.80*100, upper.80=upper.80*100)
write.csv(tablePop,"Chinook/Output/Tables/TableS7Population.csv")


### Figure 3c Mean Effect ###
dodge<-0.1
Param.Name <- unique(group.levels$Param.Name)
regions<-unique(group.posteriors$Region)
Kuskokwim<-ggplot(data = group.levels%>%filter(Param==names.params[1], Region==regions[1]),
              aes(x =mean, y = reorder(Covar.Name, desc(Covar.Name)), group=Lifestage)) +
  ggtitle(str_wrap(paste("A. Kuskokwim"), width = 20))+
  scale_color_manual(values = Aurora2, breaks =breaks)+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(name = "Covariate", labels = function(y) str_wrap(y, width = 20))+
  scale_x_continuous(name = "Covariate Coefficient")+
  geom_errorbar(aes(xmin=lower.80, xmax=upper.80),width = 0, size=0.25,
                position=position_dodge(width=dodge))+
  geom_errorbar(aes(xmin=lower.50, xmax=upper.50, col=Lifestage),
                width = 0, size=1.25, position=position_dodge(width=dodge))+
  geom_point(aes(col = Lifestage),size = 2.5, position=position_dodge(width=dodge))+
  theme_bw()+
 # xlim(c(-1,0.5))+
  theme(plot.title = element_text(hjust = 0.5, size=16),
        plot.subtitle = element_text(hjust = 0.5, size=14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14, hjust = 0.5),
        legend.position="none"
  )+
  labs(col = "Region")

YukonUS<-ggplot(data = group.levels%>%filter(Param==names.params[1], Region==regions[3]),
                  aes(x =mean, y = reorder(Covar.Name, desc(Covar.Name)), group=Lifestage)) +
  ggtitle(str_wrap(paste("B. Yukon (US)"), width = 20))+
  scale_color_manual(values = Aurora2, breaks =breaks)+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(name = "Covariate", labels = function(y) str_wrap(y, width = 20))+
  scale_x_continuous(name = "Covariate Coefficient")+
  geom_errorbar(aes(xmin=lower.80, xmax=upper.80),width = 0, size=0.25,
                position=position_dodge(width=dodge))+
  geom_errorbar(aes(xmin=lower.50, xmax=upper.50, col=Lifestage),
                width = 0, size=1.25, position=position_dodge(width=dodge))+
  geom_point(aes(col = Lifestage),size = 2.5, position=position_dodge(width=dodge))+
  theme_bw()+
#  xlim(c(-1.25,0.5))+
  theme(plot.title = element_text(hjust = 0.5, size=16),
        plot.subtitle = element_text(hjust = 0.5, size=14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y =element_blank(),
        legend.text=element_text(size=14,),
        legend.title=element_text(size=14),
        legend.position="none"
  )+
  labs(col = "Region")


YukonCA<- ggplot(data = group.levels%>%filter(Param==names.params[1], Region==regions[2]),
       aes(x =mean, y = reorder(Covar.Name, desc(Covar.Name)), group=Lifestage)) +
  ggtitle(str_wrap(paste("C. Yukon (CA)"), width = 20))+
  scale_color_manual(values = Aurora2, breaks =breaks)+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(name = "Covariate", labels = function(y) str_wrap(y, width = 20))+
  scale_x_continuous(name = "Covariate Coefficient")+
  geom_errorbar(aes(xmin=lower.80, xmax=upper.80),width = 0, size=0.25,
                position=position_dodge(width=dodge))+
  geom_errorbar(aes(xmin=lower.50, xmax=upper.50, col=Lifestage),
                width = 0, size=1.25, position=position_dodge(width=dodge))+
  geom_point(aes(col = Lifestage),size = 2.5, position=position_dodge(width=dodge))+
  theme_bw()+
  #xlim(c(-1,0.5))+
  theme(plot.title = element_text(hjust = 0.5, size=16),
        plot.subtitle = element_text(hjust = 0.5, size=14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y =element_blank(),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16)
  )+
  labs(col = "Region")

arranged <- ggarrange(Kuskokwim,YukonUS,YukonCA, widths=c(2,1.25,2.5),ncol = 3, nrow = 1)


pdf(file = "Chinook/Output/Figures/MainText/Figure3_MeanCov.pdf",   # The directory you want to save the file in
    width = 14, # The width of the plot in inches
    height = 7)
annotate_figure(arranged,bottom = text_grob("Covariate Coefficient", size=16))
dev.off()


#### Figuer 4 Theta By Watershed Plots #####

npop<-length(unique(full.dat.wide.cov$pop))
levels <- c("Maximum daily streamflow","Daily stream temp",'Snowpack (snow-water equivalent)',
            "Median daily streamflow", "River Ice Breakup Date",
            "Sea Ice Cover",  "Summer Sea Surface Temperature",
            "Cross-shelf wind",'Winter Sea Surface Temperature',
            'Pollock Biomass',"North Pacific Chum", "Marine Competitors",
            "Migration CDD above 17","Maximum Weekly Migration Temp", "Maximum Daily Migration Temp",
            "Body Size")


df.levels$Covar.Name = factor(df.levels$Covar.Name, 
                         levels = levels)
lumina<-nord(n = 8, palette = "lumina")

thetaplot<- ggplot(data = df.levels,
         aes(x =mean, y = reorder(pop, desc(pop)), group = region)) +
    facet_wrap(.~Covar.Name, ncol = 4,scales='free_x', labeller = label_wrap_gen(18)) +
    #ggtitle(str_wrap(thetas$Covar.Name, width = 15)) +
    scale_color_manual(values = c(lumina[3],lumina[4],lumina[7]), name='Subregion')+
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_discrete(name = "Population Unit")+
    scale_x_continuous(name = "Covariate Coefficient")+
    geom_errorbar(aes(xmin=lower.80, xmax=upper.80),width = 0, size=0.25)+
    geom_errorbar(aes(xmin=lower.50, xmax=upper.50,col=region), width = 0, size=0.75)+
    theme_bw()+
    labs(col = "Region")+
  theme(legend.position = c(1, 0.1),
        legend.justification = c(1, 0.05),
        plot.title = element_text(hjust = 0.5, size=16),
        plot.subtitle = element_text(hjust = 0.5, size=14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size=11),
        axis.text.y = element_text(size = 11),
       # axis.text.x = element_text(size = 10),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))+
    geom_point(aes(col=region))

t1 <-annotate_figure(thetaplot, right = text_grob("Incubation & Juvenile Rearing", size=12, rot=270, hjust=2.25, vjust=1))
t2 <-annotate_figure(t1, right = text_grob("Early Marine", size=12, rot=270, vjust=3))
t3 <-annotate_figure(t2, right = text_grob("Adult Marine & Spawning Migration", size=12, rot=270, hjust=-0.9, vjust=4))


pdf(file = "Chinook/Output/Figures/MainText/Figure4_ThetaPop.pdf",   # The directory you want to save the file in
    width = 8.5, # The width of the plot in inches
    height = 13)
t3
#thetaplot
dev.off()

#### Figuer S18 Model Fits #####

recruits.levels <- data.frame(recruits)
recruits.levels$pop<-factor(recruits.levels$pop, levels=pops.regions) 

fitplot<- ggplot(data = recruits.levels,
                   aes(x =year, y = log(pred), group = region)) +
  facet_wrap(.~pop,scales='free', ncol = 3) +
  #ggtitle(thetas$Covar.Name, subtitle = thetas$Lifestage) +
  scale_color_manual(values = c(lumina[3],lumina[4],lumina[7]))+
  #geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(name ="Log Recruitment" )+
  scale_x_continuous(name = "Brood Year")+
  geom_errorbar(aes(ymin=log(recruits)-2*rec.SDlog, ymax=log(recruits)+2*rec.SDlog),
                width = 0, size=0.25)+
  geom_point(aes(y=log(recruits),x=year, col=region))+
  
  geom_ribbon(alpha=0.2,linetype = 0,aes(ymin=log(lower.95), ymax=log(upper.95)))+
  geom_line(aes(y=log(pred),x=year, col=region))+
  theme_bw()+  
  theme(legend.position = c(0.9, -0.025),
                     legend.justification = c(1, 0))+
  labs(col = "Region")

pdf(file = "Chinook/Output/Figures/Supplement/FigS18_modelfits.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 10)
fitplot
dev.off()

####Figure S21 UNSATND ####
covunstand <- unstand%>%select(pop, year, region, EarlySummer,
                         medq_rear,
                         size,
                         Winter_stand,
                         # Age_3_Biomass_stand,
                         #chum_stand,
                         uwind,
                         marine,
                         # mean_swe_rear,
                         ICIA,
                         breakup2,
                         maxq_spawn,
                         # cdd17_stand,
                         # maxWeekly_migrate_stand,
                         maxDaily_migrate,
                         cdd_rear
) %>%
  rename("Summer Sea Surface Temperature"=EarlySummer,
         "Median Daily Streamflow"=medq_rear,
         "Size"=size,
         "First Marine Winter SST"=Winter_stand,
         #"Age 3+ Pollock Biomass"=mean(Age_3_Biomass_stand),
         #"North Pacific Chum"=mean(chum_stand),
         "cross-shelf wind"=uwind,
         "Marine Competitors"=marine,
         "Ice Cover Index"=ICIA,
         #"Snowpack (snow-water equivalent)" = mean_swe_rear,
         "River Ice Breakup Date"=breakup2,
         "Maximum Daily Streamflow"=maxq_spawn,
         #"Migration CDD above 17C"=cdd17_stand,
         #"Maximum Weekly Migration Temp"= maxWeekly_migrate_stand,
         "Maximum Daily Migration Temp"=maxDaily_migrate,
         "Daily stream temp rearing"=cdd_rear)%>%
  pivot_longer(
    cols = c("Summer Sea Surface Temperature",
             "Ice Cover Index",
             #   "Snowpack (snow-water equivalent)",
             "Size",
             # "North Pacific Chum",
             "First Marine Winter SST",
             "Marine Competitors",
             #"Age 3+ Pollock Biomass",
             "cross-shelf wind",
             "River Ice Breakup Date",
             "Maximum Daily Streamflow",
             "Median Daily Streamflow",
             #"Migration CDD above 17C",
             #"Maximum Weekly Migration Temp",
             "Maximum Daily Migration Temp",
             "Daily stream temp rearing"),
    names_to = "covariate",
    values_to = "value",
    values_drop_na = TRUE
  )

covariates<- covunstand %>% #this code is for the standardized plots
  merge(select(residualnc,residual.nps, residual,year, pop), by = c('year', 'pop'))
cov_lon <- covariates
cov_lon$covariate = factor(cov_lon$covariate, 
                           levels = levels)
levels(cov_lon$covariate) <- levels

residPlot <- ggplot(data = cov_lon,
                    aes(x =value , y = residual,
                        group=region)) +
  facet_wrap(.~covariate,scales='free', ncol = 4, labeller = label_wrap_gen(18) ) +
  geom_point(col='grey')+
  #ggtitle(thetas$Covar.Name, subtitle = thetas$Lifestage) +
  scale_color_manual(values = c(lumina[3],lumina[4],lumina[7]))+
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 5),aes(col=region))+
  #geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(name ="Productivity (Ricker Residual)" )+
  scale_x_continuous(name = "Covariate Value")+
  theme_bw()+
  #ylim(c(-2,2))+
  theme(legend.position = c(0.9, 0.08),
        legend.justification = c(1, 0))+
  labs(col = "Region")


pdf(file = "Chinook/Output/Figures/Supplement/FigureS21_UNSTAND_residuals.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 10)
residPlot
dev.off()



#### Figure 6 Cov TS ####

unlag_cov$covariate <- replace(unlag_cov$covariate, unlag_cov$covariate %in% conditions[1], replacement_values[1])
unlag_cov$covariate <- replace(unlag_cov$covariate, unlag_cov$covariate %in% conditions[2], replacement_values[2])

unlag <-unlag_cov%>%
  filter(covariate=="Winter Sea Surface Temperature" |covariate=="Marine Competitors"
         |covariate=="Sea Ice Cover"|covariate=="cross-shelf wind"
         |covariate=="Median Daily Streamflow" |covariate=="Maximum Daily Temperature Migration"  
         |covariate=='River Ice Breakup Date'|covariate=="Max Streamflow Spawning"
         |covariate=="Daily stream temp rearing")

covplot <- unlag_cov%>%
  filter(covariate=="Summer Sea Surface Temperature"|covariate=="Body Size"
         |covariate=="Winter Sea Surface Temperature" |covariate=="Marine Competitors"
         |covariate=="Sea Ice Cover"|covariate=="cross-shelf wind"
         |covariate=="Median Daily Streamflow" |covariate=="Maximum Daily Temperature Migration"  
         |covariate=='River Ice Breakup Date'|covariate=="Max Streamflow Spawning"
         |covariate=="Daily stream temp rearing")%>%
  group_by(covariate, year, region) %>%
  summarise(value = mean(value))


levels <- c("Max Streamflow Spawning","Daily stream temp rearing","Median Daily Streamflow",
            'River Ice Breakup Date',"Sea Ice Cover","Summer Sea Surface Temperature","cross-shelf wind",
            "Winter Sea Surface Temperature","Marine Competitors","Body Size",
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
  geom_line(data=unlag,aes(group=pop, col='Watershed Conditions'),alpha=0.4)+
  geom_line(aes(col=region), lwd=0.9)+
  #ggtitle(thetas$Covar.Name, subtitle = thetas$Lifestage) +
  scale_color_manual(breaks=c("Kuskokwim", "Yukon (CA)", "Yukon (US)",'Watershed Conditions'),
                     labels=c("Kuskokwim", "Yukon (CA)", "Yukon (US)",'Watershed Conditions'),
                     values = c(lumina[3],lumina[4],lumina[7],'grey80'))+
  #geom_smooth(col='black')+
  #geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(name ="Standardized Trend" )+
  scale_x_continuous(name = "Calendar Year")+
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()+
  theme(legend.position = c(0.95, 0),
        legend.justification = c(1, 0),
                plot.title = element_text(hjust = 0.5, size=16),
        plot.subtitle = element_text(hjust = 0.5, size=14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size=14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
       # axis.text.x = element_text(size = 10),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16))+
  labs(col = "Region")
covTS
pdf(file = "Chinook/Output/Figures/MainText/Figure6_covtimeseries.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 8)
covTS
dev.off()

#### Figure S19 Residual Migration Temp ####
covmig <- unstand%>%select(pop, year, region, 
                          # cddGT17_migrate,
                          # maxWeekly_migrate,
                               maxDaily_migrate) %>%
  rename(#"Migration CDD above 17C"=cddGT17_migrate,
         #"Maximum Weekly Migration Temp"= maxWeekly_migrate,
         "Maximum Daily Migration Temp"=maxDaily_migrate)%>%
  pivot_longer(
    cols = c(#"Migration CDD above 17C",
#"Maximum Weekly Migration Temp",
             "Maximum Daily Migration Temp"),
    names_to = "covariate",
    values_to = "value",
    values_drop_na = TRUE
  )

covariates<- covmig%>% #this code is for the standardized plots
  merge(select(residualnc,residual,year, pop), by = c('year', 'pop'))
cov_lon <- covariates
cov_lon$covariate = factor(cov_lon$covariate, 
                           levels = levels)
levels(cov_lon$covariate) <- levels

#### Figure 5 ####
migration_temps_plot<-ggplot(data = cov_lon,
       aes(x =value , y = residual,
           group=region)) +
 # facet_wrap(.~covariate,scales='free', ncol = 3, labeller = label_wrap_gen(18) ) +
  geom_point(aes(col=region),alpha=0.3)+
  #ggtitle(thetas$Covar.Name, subtitle = thetas$Lifestage) +
  scale_color_manual(values = c(lumina[3],lumina[4],lumina[7]))+
  geom_smooth(aes(col=region))+
  #geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(name ="Ricker Residual" )+
  scale_x_continuous(name = "Degrees Celsius")+
  theme_bw()+
  #ylim(c(-2,2))+
  theme(legend.position = c(0.4, 0.01),
        legend.justification = c(1, 0))+
  labs(col = "Region")
migration_temps_plot

pdf(file = "Chinook/Output/Figures/MainText/Figure5_MigrationTemp.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height =4)
migration_temps_plot
dev.off()
