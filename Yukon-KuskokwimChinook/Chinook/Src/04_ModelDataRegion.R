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
library(flextable)

### Prepping stan variables ###

#pulling in full standardized datasets
full.dat.wide.cov<-readRDS("Chinook/Output/posteriors/full.dat.wide.cov.offset.rds")

spawn.dat <- full.dat.wide.cov$spawners #selecting spawner data
rec.dat <- full.dat.wide.cov$recruits #selecting recruit data
NS <-length(unique(full.dat.wide.cov$pop))  #number of stocks
S <- as.numeric(as.factor(full.dat.wide.cov$pop)) # stock pointer vector
R <- as.numeric(as.factor(unique(full.dat.wide.cov%>%select(pop, region))$region)) # region pointer for pop
NR<-length(unique(full.dat.wide.cov%>%select(region))$region)
N <- length(full.dat.wide.cov$spawners) #total number of observations
ln_rec <- log(rec.dat)
ln_spawn <- log(spawn.dat)

Region.names<-unique(full.dat.wide.cov%>%select(region))$region
#final model includes covairates (n = 11): 1. Winter_stand (winter SST), 2. EarlySummer_stand (summer SST), 
#3. medq_rear_stand (median discharge), 4. size (size DFA trend), 5. marine (marine competitors DFA trend),
#6. uwind_stand (cross-shelf wind), 7. ICIA_stand (ice cover index),
# 8. breakup2 (river ice break up), 9. maxq_spawn_stand (max discharge), 
# 10. maxDaily_migrate_stand (migration temp), 11. cdd_rear_stand (degree days rearing)

#selecting covariates to include in model:
covariates<- full.dat.wide.cov%>% 
    select(#ProbWinter_stand,
      Winter_stand,
     # Age_3_Biomass_stand
     # Recruitment_stand
         EarlySummer_stand,
         medq_rear_stand,
    size_stand,
     marine_stand,
      # chum_stand
      #vwind_stand,
           uwind_stand,
           ICIA_stand,
          breakup2,
      # max5dprcp_stand,
               maxq_spawn_stand,
      #mean_swe_rear,
      # mean_swe_icu,
      # mnprcp_rear_stand,
     maxDaily_migrate_stand,
    #maxWeekly_migrate_stand, 
    # cdd17_stand,
    #    maxDaily_spawn_stand,
              cdd_rear_stand
         )
#covariates<- full.dat.wide.cov%>% 
#  select( )
sim.covariates<- full.dat.wide.cov%>% select(V2,V3,V4,V5,V6) #selecting simulated covs
nsim<- ncol(sim.covariates) # total number of covariates

error.rec <- full.dat.wide.cov$rec.SDlog #recruit error
error.spawn <- full.dat.wide.cov$spawn.SDlog #spawn error
n.covars<- ncol(covariates) # total number of covariates

#Assigning names to covariates to generate output for plots 
names.covars <- c(  'Winter Sea Surface Temperature',
                  'Summer Sea Surface Temperature',
                  'Median daily streamflow',
                   "Body Size",
                  "Marine Competitors",
                   "Cross-shelf wind",
                  'Sea Ice Cover',
                'River Ice Breakup Date',
                      'Maximum daily streamflow',
                'Maximum Daily Migration Temp',
                "Daily stream temp"
                  )

#check to make sure the correct names and covariates are assigned
ncol(covariates)
length(names.covars)


#### STAN data #### 
data <- list(NS = NS,#number of stocks
             S = S, #population assignment
             N = N,# total observations
             R=R,
             NR=NR,
          ncovars=ncol(covariates), #use this when running the null model
       #     ncovars=n.covars, #number of covariates
             spawn = spawn.dat,#spawner data
             ln_rec = log(rec.dat),#recruit data
           covars = covariates, # covariate data
             error_rec = error.rec, # error for recruits
             error_spawn = error.spawn #error for spawenrs
)
datasim <- list(NS = NS,
             S = S, #population assignment
             N = N,# total observations
             R=R,
             NR=NR,
             ncovars=nsim, #number of covariates
             spawn = spawn.dat,#spawner data
             ln_rec = log(rec.dat),#recruit data
             covars = sim.covariates, # covariate data
             error_rec = error.rec, # error for recruits
             error_spawn = error.spawn #error for spawners
             )

#####Assinging Stan Conditions ####


total_iterations <- 10000
max_treedepth <-  13
n_chains <-  3
n_cores <- 3
adapt_delta <- 0.95
warmups<-3000

####Fitting the Model ####
bhfit <- stan(
  file = here::here("Chinook/Src/ModelSTANregion.stan"),
  data = data, #swap this for datasim object when running sim data
  chains = n_chains,
  warmup = warmups,
  iter = total_iterations,
  cores = n_cores,
  refresh = 250,
  control = list(max_treedepth = max_treedepth,
                 adapt_delta = adapt_delta)
)


saveRDS(bhfit, file = "bhfitESR.rda") # saving model bject

bhfit <- readRDS("bhfit.rda") #reading in an already run model object


### Reviewing model output ###
MCMCsummary(bhfit,params = c("mu_coef"))
fit.theta<- MCMCsummary(bhfit,params = c("theta"))
fit.theta
MCMCsummary(bhfit,params = c("sigma_oe"))
post.all =data.frame(summary(bhfit, prob=c(0.025, 0.25,0.75, 0.975, 0.1, 0.9))$summary)

#### Creating Data Frame with Model Posteriors #####
###CURRY - note this section is all for putting dataframes together to export for the plot scripts
npop<-length(unique(full.dat.wide.cov$pop)) #assingning the number of populations
n<- length(names.covars) #number of covar names
thetas<-data.frame(summary(bhfit,pars = c("theta"), prob=c(0.025, 0.25,0.75, 0.975, 0.1,0.9))$summary) #extracting theta posteriors
pred<-data.frame(summary(bhfit,pars = c("pred"), prob=c(0.025, 0.25,0.75, 0.975, 0.1, 0.9))$summary) #extracting posteriors for predicted recruitment

#assigning lifestages to generate plots

lifestage <- c( "Marine Residence", #'Marine Food Web',
                 "Outmigration & Early Marine", #'Marine Food Web',
                 #   "Outmigration & Early Marine", #'Marine Food Web',
                "Juvenile Rearing",#'median daily discharge',
                "Spawning", #"Female Size"
                 "Marine Residence", #'Marine Food Web'
         #   "Marine Residence", #'Marine Food Web'
         "Outmigration & Early Marine", #'Marine Food Web',
         "Outmigration & Early Marine", #'Marine Food Web',
         "Outmigration & Early Marine", #'River Ice Breakup'
              #"Incubation", #'Maximum 5-day precipitation'
               "Incubation",#'Maximum daily streamflow',
               #"Juvenile Rearing", #'Snowpack (snow-water equivalent)',
             # "Incubation", #'Snowpack (snow-water equivalent)',
             # "Juvenile Rearing",#'Average daily precipitation',
             "Adult Migration",#'Migration Maximum daily stream temperature ',
              # "Spawning",  #'Spawing Maximum daily stream temperature ',
   "Juvenile Rearing" # "Juvenile Growth Potential
              ) 

#lifestage<- c(" "," "," "," "," "," ", " ") #use for sim data
#names.covars <- c("Covariate 1 ", "Covariate 2 ","Covariate 7 ","Covariate 4 ",
#                  "Covariate 5 ","Covariate 6 ", "Covariate 3 ") #use for sim data


#names.covars<-c("Covariate 1","Covariate 2","Covariate 3","Covariate 4",  "Covariate 5")
n.covars<- 11
#restitching data to posteriors for plots for pop effects
df<-data.frame(NA) #empty dataframe
df <- thetas%>% #filling it with posteriors
  add_column(pop = rep(unique(full.dat.wide.cov$pop), each = n.covars))%>% #and population names
  add_column(Covar.Name = rep(names.covars, npop))%>% #and covariate names
  #add_column(Lifestage = rep(lifestage, npop))%>% #adding lifestages
  right_join(distinct(select(full.dat.wide.cov, c(pop, region))), by='pop') 



df<-  rename(df, lower.95='X2.5.',
             upper.95='X97.5.', lower.50='X25.',upper.50='X75.',
             lower.80='X10.',upper.80='X90.' ) #renaming posterior data
thetas<-df

#dataframe restitching data to posteriors for plots for group/region effects

group <- c("mu_coef", "mu_coef_rep", "sigma_coef") #names for effects
group.long <- c("Mean Covariate Effect", "Distribution of Mean Covariate Effect", "Standard Deviation of Covariate Effect") #plot labels for params
reg<-unique(full.dat.wide.cov%>%select(region2))$region2
reg<-unique(full.dat.wide.cov%>%select(region))$region
group.fit<- data.frame(summary(bhfit,pars = group,  probs = c(0.025, 0.25,0.75, 0.975, 0.9, 0.1))$summary) #extracting posteriors
group.level<- group.fit%>%
    add_column(Param = rep(group, each=length(names.covars)*NR))%>%
    add_column(Param.Name = rep(group.long, each=length(names.covars)*NR))%>%
    add_column(Covar.Name = rep(names.covars, length(unique(group))*NR))%>%
    add_column(Lifestage = rep(lifestage, length(unique(group))*NR))%>%
  add_column(Region = rep(reg, each=length(names.covars), times=length(group)))

group.posteriors <-rename(group.level,lower.95='X2.5.',
                          upper.95='X97.5.', lower.50='X25.',upper.50='X75.',
                          lower.80='X10.',upper.80='X90.' )


#dataframe for ricker posteriors
params <- c("alpha", "beta")
param.fit<- data.frame(summary(bhfit,pars = params, prob=c(0.025, 0.975, 0.75, 0.25, 0.9, 0.1))$summary)
parameters<- param.fit%>%
  add_column(Param = rep(params, each=npop))%>%
  add_column(pop = rep(unique(full.dat.wide.cov$pop), length(params)))%>%
   right_join(distinct(select(full.dat.wide.cov, c(pop, region))), by='pop')


parameter.posteriors <-rename(parameters, lower.95='X2.5.',
                              upper.95='X97.5.',lower.50='X25.',upper.50='X75.',
                              lower.80='X10.',upper.80='X90.'  )


#dataframe with data spawn and rec with pred spawn and rec
pred<-data.frame(summary(bhfit,pars = c("pred"), prob=c(0.025, 0.25,0.75, 0.975, 0.1, 0.9))$summary)
spawn_pred =data.frame(summary(bhfit,pars = c("spawn_pred"), prob=c(0.025, 0.25,0.75, 0.975, 0.1, 0.9))$summary)

spawn_pred =select(data.frame(summary(bhfit,pars = c("spawn_pred"), prob=c(0.025, 0.25,0.75, 0.975, 0.1, 0.9))$summary), mean)
pred<-  rename(pred, lower.95='X2.5.',
               upper.95='X97.5.', lower.50='X25.',upper.50='X75.',
               lower.80='X10.',upper.80='X90.', 'pred'=mean)
pred <- pred%>%
  add_column(pop = full.dat.wide.cov$pop)%>%
  add_column(region = full.dat.wide.cov$region)%>%
  add_column(recruits = full.dat.wide.cov$recruits)%>%
  add_column(spawners = full.dat.wide.cov$spawners)%>%
  add_column(spawn_pred = spawn_pred[,1])%>%
  add_column(year = full.dat.wide.cov$year)%>%
  add_column(rec.SDlog = full.dat.wide.cov$rec.SDlog)

#claculating posteriors
recpred <- pred%>%
  add_column(residual = log(pred$spawners/pred$recruits)-log(pred$spawn_pred/pred$pred))%>%
#add_column(residual2 = (pred$spawners/pred$recruits)-(pred$spawn_pred/pred$pred))%>%
  add_column(residual3 = log(pred$recruits)-log(pred$pred))


# dataframe for mean across the entire region 
mu.theta <- extract(bhfit, pars=c('mu_coef'), permuted = TRUE, inc_warmup = FALSE,
                   include = TRUE)$mu_coef
mean.theta <- data.frame()

for(i in 1:n.covars){
 temp<-apply(mu.theta[,,i],1,mean)
df<- data.frame(t(hdi(temp, credMass = 0.80)))%>%
   data.frame(t(hdi(temp, credMass = 0.5)))%>%
   add_column(mean = mean(temp))%>%
  add_column(Lifestage=rep(lifestage[i],1))%>%
   # add_column(pop=unique(full.dat.wide$pop))%>%
   add_column(Covar.Name=rep(names.covars[i],1))%>%
  add_column(Param="Mean Covariate Effect")
 mean.theta <- rbind(mean.theta,df)
}

mu.theta.rep <- extract(bhfit, pars=c("mu_coef_rep"), permuted = TRUE, inc_warmup = FALSE,
                    include = TRUE)$mu_coef
mean.theta.rep <- data.frame()
for(i in 1:n.covars){
  temp<-apply(mu.theta.rep[,,i],1,mean)
  df<- data.frame(t(hdi(temp, credMass = 0.80)))%>%
    data.frame(t(hdi(temp, credMass = 0.5)))%>%
    add_column(mean = mean(temp))%>%
    add_column(Lifestage=rep(lifestage[i],1))%>%
    # add_column(pop=unique(full.dat.wide$pop))%>%
    add_column(Covar.Name=rep(names.covars[i],1))%>%
    add_column(Param="Distribution of Mean Covariate Effect")
  mean.theta.rep <- rbind(mean.theta.rep,df)
}
mean.theta<-mean.theta%>%add_row(mean.theta.rep)%>%
  rename(lower.50='lower.1',upper.50='upper.1',
lower.80='lower',upper.80='upper' )
#saving posteriors dataframes for plots
saveRDS(mean.theta, file = "Chinook/Output/posteriors/mean.theta.rds")
saveRDS(group.posteriors, file = "Chinook/Output/posteriors/group.posteriors.rds")
saveRDS(thetas, file = "Chinook/Output/posteriors/thetas.posteriors.rds")
saveRDS(parameter.posteriors, file = "Chinook/Output/posteriors/parameter.posteriors.rds")
saveRDS(full.dat.wide.cov, file = "Chinook/Output/posteriors/full.dat.wide.cov.rds")
#saveRDS(gammas, file = "Chinook/Output/posteriors/gammas.rds")
saveRDS(recpred, file = "Chinook/Output/posteriors/recruits.rds")

#calculating p value for posterior predictive chack
sum(ln_rec- data.frame(summary(bhfit,pars = c("ln_rec_new"))$summary)$mean>0)/
  (sum(ln_rec- data.frame(summary(bhfit,pars = c("ln_rec_new"))$summary)$mean>0)+
     sum(ln_rec- data.frame(summary(bhfit,pars = c("ln_rec_new"))$summary)$mean<0))

#### Regional effect size ####

mu_spawn <-rowMeans(extract(bhfit, pars=c('spawn_pred'), permuted = TRUE, inc_warmup = FALSE,
                   include = TRUE)$spawn_pred)
mu.alpha <- extract(bhfit, pars=c('mu_alpha'), permuted = TRUE, inc_warmup = FALSE,
                                     include = TRUE)
mu.beta <- rowMeans(extract(bhfit, pars=c('beta'), permuted = TRUE, inc_warmup = FALSE,
                            include = TRUE)$beta)
mu.coef <- extract(bhfit, pars=c('mu_coef'), permuted = TRUE, inc_warmup = FALSE,
                    include = TRUE)
effect.null.rec<-array(dim = c(total_iterations-warmups*n_chains,1))
effect.rec<-array(dim = c((total_iterations-warmups)*n_chains, NR,n.covars))
effect<-array(dim = c((total_iterations-warmups)*n_chains, NR,n.covars))
for(j in 1:n.covars){
  for(i in 1:NR){
  effect.null.rec <- mu_spawn*exp(mu.alpha$mu_alpha-mu_spawn*mu.beta)
    effect.rec[,i,j] <- mu_spawn*exp(mu.alpha$mu_alpha-mu_spawn*mu.beta+mu.coef$mu_coef[,i,j]*apply(covariates,2,sd)[j])
    effect[,i,j] <- ((effect.rec[,i,j]-effect.null.rec)/effect.null.rec)*100
    
    }
}
effectsummaryReg <- data.frame()
for(i in 1:n.covars){
  for(j in 1:NR){
  df <- data.frame(t(hdi(effect[,j,i], credMass = 0.80)))%>%
    data.frame(t(hdi(effect[,j,i], credMass = 0.5)))%>%
    add_column(mean = median(effect[,j,i]))%>%
   # add_column(pop=unique(full.dat.wide$pop))%>%
    add_column(Covar.Name=rep(names.covars[i],1))%>%
    add_column(Region=rep(Region.names[j],1))
  effectsummaryReg <- rbind(effectsummaryReg,df)
  }
}
lifest <-c("Outmigration & Early Marine","Outmigration & Early Marine","Juvenile Rearing","Spawning","Marine Residence" ,
           "Outmigration & Early Marine","Outmigration & Early Marine","Outmigration & Early Marine",
           "Incubation","Adult Migration","Juvenile Rearing")
effectsummaryReg <-round(effectsummaryReg[,1:5],2)%>%
  add_column(effectsummaryReg[,6:7])%>%
    add_column(Lifestage = rep(lifest, each = length(unique(group))))

saveRDS(effectsummaryReg, file = "Chinook/Output/posteriors/effectsummaryReg.rds")
effectsummaryReg <- effectsummaryReg%>%
  select('Covar.Name', "Region", 'mean','lower','upper', 'lower.1','upper.1')%>%
  rename('Lower 80%' =lower, 'Upper 80%'=upper, 
         'Lower 50%'=lower.1, 'Upper 50%'=upper.1,
         "Mean"=mean, Covariate="Covar.Name")

pdf(file = "Chinook/Output/EffectSummaryRegional.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 10)
plot(effectsummaryReg%>%regulartable())
dev.off()

effectsummaryMean <- data.frame()
for(i in 1:n.covars){
  temp<-apply(effect[,,i],1,mean)
  df<- data.frame(t(hdi(temp, credMass = 0.80)))%>%
    data.frame(t(hdi(temp, credMass = 0.5)))%>%
    add_column(mean = median(temp))%>%
    add_column(Lifestage=rep(lifestage[i],1))%>%
    # add_column(pop=unique(full.dat.wide$pop))%>%
    add_column(Covar.Name=rep(names.covars[i],1))
  effectsummaryMean <- rbind(effectsummaryMean,df)
}

effectsummaryMean <-round(effectsummaryMean[,1:5],2)%>%
add_column(effectsummaryMean[,6:7])
effectsummaryMean <- effectsummaryMean%>%
  select('Covar.Name', "Lifestage", 'mean','lower','upper', 'lower.1','upper.1')%>%
  rename('Lower 80%' =lower, 'Upper 80%'=upper, 
         'Lower 50%'=lower.1, 'Upper 50%'=upper.1,
         "Posterior \n Median"=mean, Covariate="Covar.Name")
pdf(file = "Chinook/Output/EffectSummary.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
height = 3)
plot(effectsummaryMean%>%regulartable())
dev.off()


write.csv(effectsummaryReg,"Chinook/Output/Tables/EffectSummaryRegional.csv")



#### Population effect size ####

mean_spawn<- full.dat.wide.cov%>%
  group_by(pop)%>%
  summarise(mean_spawn = mean(spawners))

alpha <- extract(bhfit, pars=c('alpha'), permuted = TRUE, inc_warmup = FALSE,
                 include = TRUE)$alpha
beta <-extract(bhfit, pars=c('beta'), permuted = TRUE, inc_warmup = FALSE,
               include = TRUE)$beta
theta <- extract(bhfit, pars=c('theta'), permuted = TRUE, inc_warmup = FALSE,
                 include = TRUE)
effect.null.rec<-array(dim = c((total_iterations-warmups)*n_chains,npop))
effect.rec<-array(dim = c((total_iterations-warmups)*n_chains, npop, n.covars))
effect<-array(dim = c((total_iterations-warmups)*n_chains, npop, n.covars))
j<-1
i<- 1
for(j in 1:n.covars){
  for(i in 1:npop){
    effect.null.rec <- mu_spawn*exp(mu.alpha$mu_alpha-mu_spawn*mu.beta)
    effect.rec[,i,j] <- mu_spawn*exp(mu.alpha$mu_alpha-mu_spawn*mu.beta+theta$theta[,i,j]*apply(covariates,2,sd)[j])
    effect[,i,j] <- ((effect.rec[,i,j]-effect.null.rec)/effect.null.rec)*100
    
  }
}

pop.name=unique(full.dat.wide.cov$pop)
effectsummaryPop <- data.frame()
for(i in 1:11){
  for(j in 1:npop){
    df <- data.frame(t(hdi(effect[,j,i], credMass = 0.80)))%>%
      data.frame(t(hdi(effect[,j,i], credMass = 0.5)))%>%
      add_column(mean = median(effect[,j,i]))%>%
      # add_column(pop=unique(full.dat.wide$pop))%>%
      add_column(Covar.Name=rep(names.covars[i],1))%>%
       add_column(pop=rep(pop.name[j],1))
  effectsummaryPop <- rbind(effectsummaryPop,df)
}}

effectsummaryPop <-round(effectsummaryPop[,1:5],2)%>%
  add_column(effectsummaryPop[,6:7])
saveRDS(effectsummaryPop, file = "Chinook/Output/posteriors/effectsummaryPop.rds")


effectsummaryPop <- effectsummaryPop%>%
  select('Covar.Name', "pop", 'mean','lower','upper', 'lower.1','upper.1')%>%
  rename('Lower 80%' =lower, 'Upper 80%'=upper, 
         'Lower 50%'=lower.1, 'Upper 50%'=upper.1,
         "Posterior \n Mean"=mean, Covariate="Covar.Name", Population=pop)
pdf(file = "Chinook/Output/EffectSummaryPopulation.pdf",   # The directory you want to save the file in
    width =5, # The width of the plot in inches
    height = 80)
plot(effectsummaryPop%>%regulartable())
dev.off()
write.csv(effectsum,"Chinook/Output/Tables/effectsummaryPop.csv")

##### No cov mod residuals #####
bhfit <-readRDS("bhfitNC.rda")
pred<-data.frame(summary(bhfit,pars = c("pred"), prob=c(0.025, 0.25,0.75, 0.975, 0.1, 0.9))$summary)


pred<-  rename(pred, lower.95='X2.5.',
               upper.95='X97.5.', lower.50='X25.',upper.50='X75.',
               lower.80='X10.',upper.80='X90.'  )

residual <- pred%>%
  add_column(pop = full.dat.wide.cov$pop)%>%
  add_column(region = full.dat.wide.cov$region)%>%
  add_column(region = full.dat.wide.cov$region2)%>%
  add_column(recruits = full.dat.wide.cov$recruits)%>%
  add_column(year = full.dat.wide.cov$year)%>%
  add_column(rec.SDlog = full.dat.wide.cov$rec.SDlog)%>%
  add_column(spawners = full.dat.wide.cov$spawners)%>%
  add_column(spawn_pred=data.frame(summary(bhfit,pars = c("spawn_pred"), prob=c(0.025, 0.25,0.75, 0.975))$summary)$mean)

residual <- residual%>%  
add_column(residual.nps = log(residual$spawners/residual$recruits)-log(residual$spawn_pred/residual$mean))%>%
add_column(residual = log(residual$recruits)-log(residual$mean))


saveRDS(residual, file = "Chinook/Output/posteriors/residualnc.rds")


#### Spawner Abundance effect size ####
beta<-data.frame(summary(bhfit,pars = c("beta"), prob=c(0.025, 0.25,0.75, 0.975, 0.1, 0.9))$summary)
spawn.pred<-data.frame(summary(bhfit,pars = c("spawn_pred"), prob=c(0.025, 0.25,0.75, 0.975, 0.1, 0.9))$summary)
sd.spawn<-data.frame(summary(bhfit,pars = c("spawn_pred"), prob=c(0.025, 0.25,0.75, 0.975, 0.1, 0.9))$summary)%>%
  add_column(pop = full.dat.wide.cov$pop)%>%
  add_column(region = full.dat.wide.cov$region)%>%
  add_column(year = full.dat.wide.cov$year)%>%group_by(pop)%>%summarise(sd=sd(mean))%>%
  add_column(beta = beta$mean)

spawn_effect <-data.frame(effect=(-1*rnorm(1000,beta$mean,beta$sd))*rep(sd.spawn$sd, each=1000)*100)%>%
  add_column(pop = rep(unique(full.dat.wide.cov$pop), each=1000))
spawn_effect <-spawn_effect%>%filter(effect>-200)

effectsummarySpawn<- data.frame()
for(i in 1:npop){
temp<-unlist(spawn_effect%>%filter(pop==unique(spawn_effect$pop)[i])%>%select(effect))
  df<- data.frame(t(hdi(temp, credMass = 0.80)))%>%
    data.frame(t(hdi(temp, credMass = 0.5)))
  effectsummarySpawn <- rbind(effectsummarySpawn,df)
}

effectsummary <-data.frame(effectsummarySpawn%>%mutate(median=(lower.1+upper.1)/2))%>%
  add_column(data.frame(sd.spawn=sd.spawn$sd))%>%
    add_column(pop = unique(full.dat.wide.cov$pop))

#effectsummary <-data.frame(spawn_effect%>%group_by(pop)%>%summarise(median=median(effect)))%>%
#  add_column(effectsummarySpawn)%>%
#  add_column(data.frame(sd.spawn=sd.spawn$sd))

col_order <- c("pop", "sd.spawn", "median",
               "lower.1", "upper.1", "lower", "upper")
effectsummary <- effectsummary[, col_order]
effectsummary

effectsummary<-round(effectsummary[,2:7],2)
effectsum<-cbind(Population = unique(full.dat.wide.cov$pop),effectsummary)

effectsum <- effectsum%>%
  rename('Lower 80%' =lower, 'Upper 80%'=upper, 
         'Lower 50%'=lower.1, 'Upper 50%'=upper.1,
         "Posterior \n Mean"=median, "Spawner \n Abundance SD"=sd.spawn)
plot(effectsum%>%regulartable())
write.csv(effectsum,"Chinook/Output/Tables/EffectSizeSpawners.csv")
colMeans(effectsum[,2:7])
### FULL yk effect ####
effectsummarySpawn<- data.frame()
for(i in 1:1){
temp<-unlist(spawn_effect%>%select(effect))
  df<- data.frame(t(hdi(temp, credMass = 0.80)))%>%
    data.frame(t(hdi(temp, credMass = 0.5)))
  effectsummarySpawn <- rbind(effectsummarySpawn,df)
}

effectsummary <-effectsummarySpawn%>%
  add_column(data.frame(effect=-mean(beta$mean*sd.spawn$sd)))%>%
  add_column(data.frame(sd.spawn=mean(sd.spawn$sd)))

effectsummary<-round(effectsummary[,1:6],2)

effectsummary <- effectsummary%>%
  rename('Lower 80%' =lower, 'Upper 80%'=upper, 
         'Lower 50%'=lower.1, 'Upper 50%'=upper.1,
         "Posterior \n Mean"=effect, "Spawner \n Abundance SD"=sd.spawn)
pdf(file = "Chinook/Output/EffectSummarySpawnYK.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 10)
plot(effectsummary%>%regulartable())
dev.off()

