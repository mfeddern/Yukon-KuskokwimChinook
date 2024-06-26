
ASL <-read.csv('ocean/ASL_summary_byFWSWage.csv') 
breakup <-read.csv('breakup/output/breakupAYK.csv') 

library(MARSS)
library(ggplot2)
library(forecast)
library(broom)
library(nord)
head(data)
unique(data$ASLProjectType)
#### Updating Size Data ####
YukonUpdate<-read.csv('ocean/YukonSizeUpdate/YukonSizeUpdated.csv')%>%
  group_by(Location, Sample.Year,Age.Fresh,Age.Salt)%>%
  summarise(mean=mean(Length), sd=sd(Length), n=n())%>%
  mutate(ASLProjectType="escapement", Species='chinook',SASAP.Region="Yukon")%>%
  rename(Salt.Water.Age=Age.Salt,	Fresh.Water.Age=Age.Fresh, LocationID=Location,sampleYear=Sample.Year)

KuskokwimUpdate<-read.csv('ocean/KuskokwimSizeUpdate/KuskokwimSizeUpdated.csv')%>%
  group_by(Location, Sample.Year,Age.Fresh,Age.Salt)%>%
  summarise(mean=mean(Length), sd=sd(Length), n=n())%>%
  mutate(ASLProjectType="escapement", Species='chinook',SASAP.Region="Kuskokwim")%>%
  mutate(LocationID =replace(Location, Location == 'Salmon River Weir', "Salmon River (Aniak)"))%>%
  rename(Salt.Water.Age=Age.Salt,	Fresh.Water.Age=Age.Fresh, sampleYear=Sample.Year)
  
  
  

####Kuskokwim####
data<-ASL%>%
  bind_rows(KuskokwimUpdate, YukonUpdate)

Kuskokwim <- data%>%
  filter(ASLProjectType == "escapement"& Species =="chinook")%>%
  filter(SASAP.Region == "Kuskokwim")%>%
  group_by(LocationID, sampleYear) %>%
  summarise(sum = sum(mean*n), n1=sum(n))%>%
  mutate(mean =sum/n1)%>%
  
  arrange(sampleYear)%>%
  select(c("LocationID", "sampleYear", "mean","n1"))

p<-ggplot(data = Kuskokwim, aes(x=sampleYear, y=n1)) +
  geom_bar(stat="identity")+
  facet_wrap(~LocationID) +
  xlab("Year") + ylab("Count")
p


Kuskokwim_dat <- Kuskokwim%>%
    filter(LocationID != "Nyac Weir" & LocationID != "Kisaralik River" &
             LocationID != "Salmon River (Pitka Fork)"& LocationID != "Aniak River")
locations <-unique(Kuskokwim_dat$LocationID)

Kuskokwim_dat <- Kuskokwim_dat%>%
  select(c("LocationID", "sampleYear", "mean"))%>%
pivot_wider(names_from = LocationID, 
            values_from = mean,id_expand=FALSE)

 years <-unique(Kuskokwim$sampleYear) 
 unique(Kuskokwim$LocationID) 



##Setting up data
yr_frst <- 1980
yr_last <- 2016
Kuskokwim_dat <- Kuskokwim_dat[Kuskokwim_dat[, "sampleYear"] >= yr_frst & 
                                 Kuskokwim_dat[, "sampleYear"] <= yr_last,]


## get only the phytoplankton
dat_1980 <- Kuskokwim_dat[,locations]

## transpose data so time goes across columns
dat_1980 <- t(dat_1980)
## get number of time series
N_ts <- dim(dat_1980)[1]
## get length of time series
TT <- dim(dat_1980)[2] 
## 'ZZ' is loadings matrix

Z_vals <- list("z11", 
               "z21",
               "z31",
               "z41",
               "z51",
               "z61",
               "z71",
               "z81",
               "z91")
ZZ <- matrix(Z_vals, nrow = N_ts, ncol = 1, byrow = TRUE)
ZZ
y_bar <- apply(dat_1980, 1, mean, na.rm = TRUE)
sd<- apply(dat_1980, 1, sd, na.rm = TRUE)
dat <- (dat_1980 - y_bar)/sd
rownames(dat) <- rownames(dat_1980)
## 'aa' is the offset/scaling
aa <- "zero"
## 'DD' and 'd' are for covariates
DD <- "zero"  # matrix(0,mm,1)
dd <- "zero"  # matrix(0,1,wk_last)
## 'RR' is var-cov matrix for obs errors
RR <- "diagonal and unequal"

## number of processes
mm <- 1
## 'BB' is identity: 1's along the diagonal & 0's elsewhere
BB <- "identity"  # diag(mm)
## 'uu' is a column vector of 0's
uu <- "zero"  # matrix(0, mm, 1)
## 'CC' and 'cc' are for covariates
CC <- "zero"  # matrix(0, mm, 1)
cc <- "zero"  # matrix(0, 1, wk_last)
## 'QQ' is identity
QQ <-"diagonal and equal"  # diag(mm)

## list with specifications for model vectors/matrices
mod_list <- list(Z = ZZ, A = aa, D = DD, d = dd, R = RR,
                 B = BB, U = uu, C = CC, c = cc, Q = QQ)
## list with model inits
init_list <- list(x0 = matrix(rep(0, mm), mm, 1))
## list with model control parameters
con_list <- list(maxit = 3000, allow.degen = TRUE)

## fit MARSS
dfa_1 <- MARSS(y = dat, model = mod_list, inits = init_list, control = con_list)

## get the estimated ZZ
Z_est <- coef(dfa_1, type="matrix")$Z
## Code for plotting one trend 
mm=1
Z_est <- coef(dfa_1, type="matrix")$Z
H_inv <- 1
Z_rot = Z_est %*% H_inv   
proc_rot = solve(H_inv) %*% dfa_1$states
ylbl <-locations
w_ts <- seq(dim(dat)[2])

## plot the processes

clr<-nord(n = 15, palette = "lumina")[5:15]
pdf(file = "Chinook/Output/Figures/Supplement/FigS7_KuskoDFASize.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 7)
layout(matrix(c(1,2),mm,2),widths=c(2,1))
par(mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
clr<-nord(n = 15, palette = "lumina")[5:15]
for(i in 1:mm) {
  ylm <- c(-1,1)*max(abs(proc_rot[i,]))
  ## set up plot area
  plot(w_ts,proc_rot[i,], type="n", bty="L",
       ylim=ylm, xlab="", ylab="", xaxt="n")
  ## draw zero-line
  abline(h=0, col="gray")
  ## plot trend line
  lines(w_ts,proc_rot[i,], lwd=2)
  lines(w_ts,proc_rot[i,], lwd=2)
  ## add panel labels
  mtext(paste("Kuskokwim Trend"), side=3, line=0.5)
  axis(1,(0:dim(dat_1980)[2])+1,yr_frst+0:dim(dat_1980)[2])
}
## plot the loadings
minZ <- 0
ylm <- c(-1,1)*max(abs(Z_rot))
for(i in 1:mm) {
  plot(c(1:N_ts)[abs(Z_rot[,i])>minZ], as.vector(Z_rot[abs(Z_rot[,i])>minZ,i]), type="h",
       lwd=2, xlab="", ylab="", xaxt="n", ylim=ylm, xlim=c(0.5,N_ts+0.5), col=clr)
  for(j in 1:N_ts) {
    if(Z_rot[j,i] > minZ) {text(j, -0.03, ylbl[j], srt=90, adj=1, cex=1.2, col=clr[j])}
    if(Z_rot[j,i] < -minZ) {text(j, 0.03, ylbl[j], srt=90, adj=0, cex=1.2, col=clr[j])}
    abline(h=0, lwd=1.5, col="gray")
  } 
  mtext(paste("Factor loadings on state",i),side=3,line=0.5)
  
}
dev.off()

get_DFA_fits <- function(MLEobj, dd = NULL, alpha = 0.05) {
  ## empty list for results
  fits <- list()
  ## extra stuff for var() calcs
  Ey <- MARSS:::MARSShatyt(MLEobj)
  ## model params
  ZZ <- coef(MLEobj, type="matrix")$Z
  ## number of obs ts
  nn <- dim(Ey$ytT)[1]
  ## number of time steps
  TT <- dim(Ey$ytT)[2]
  ## get the inverse of the rotation matrix
  H_inv <- varimax(ZZ)$rotmat
  ## check for covars
  if(!is.null(dd)) {
    DD <- coef(MLEobj, type = "matrix")$D
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states + DD %*% dd
  } else {
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states
  }
  ## Var in model fits
  VtT <- MARSSkfss(MLEobj)$VtT
  VV <- NULL
  for(tt in 1:TT) {
    RZVZ <- coef(MLEobj, type = "matrix")$R - ZZ %*% VtT[,,tt] %*% t(ZZ)
    SS <- Ey$yxtT[,,tt] - Ey$ytT[,tt,drop = FALSE] %*% t(MLEobj$states[,tt,drop = FALSE])
    VV <- cbind(VV, diag(RZVZ + SS %*% t(ZZ) + ZZ %*% t(SS)))
  }
  SE <- sqrt(VV)
  ## upper & lower (1-alpha)% CI
  fits$up <- qnorm(1-alpha/2)*SE + fits$ex
  fits$lo <- qnorm(alpha/2)*SE + fits$ex
  return(fits)
}

## get model fits & CI's
mod_fit <- get_DFA_fits(dfa_1)
## plot the fits
ylbl <- locations
d <- residuals(dfa_1, type="tt1")
d$.conf.low <- d$.fitted+qnorm(0.05/2)*d$.sigma
d$.conf.up <- d$.fitted-qnorm(0.05/2)*d$.sigma
d <- d %>%
  mutate(Year = t+yr_frst-1)

pdf(file = "Chinook/Output/Figures/Supplement/FigS8_KuskoDFASizeFit.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 6)

p<-ggplot(data = d) +
  geom_line(aes(Year, .fitted)) +
  geom_point(aes(Year, value)) +
  geom_ribbon(aes(x = Year, ymin = .conf.low, ymax = .conf.up), linetype = 2, alpha = 0.1) +
  facet_wrap(~.rownames) +
  xlab("Year") + ylab("Standardized Size")
p
dev.off()

Kuskokwim.size <- data.frame(Size = proc_rot[1,])%>%
  add_column(Year = seq(yr_frst,yr_last,1), Region = 'Kuskokwim')%>%
  add_column(size_stand = 
               (proc_rot[1,]-mean(proc_rot[1,]))/sd(proc_rot[1,]))


#### Yukon US ####

Yukon <- data%>%
  filter(ASLProjectType == "escapement"& Species =="chinook")%>%
  filter(SASAP.Region == "Yukon")%>%
  group_by(LocationID, sampleYear) %>%
  summarise(sum = sum(mean*n), n1=sum(n))%>%
  mutate(mean =sum/n1)%>%
  arrange(sampleYear)%>%
  select(c("LocationID", "sampleYear", "mean","n1"))

years <-unique(Yukon$sampleYear) 
unique(Yukon$LocationID) 

Yukon.US <- Yukon%>%
filter(LocationID == "Salcha River" | LocationID == "Andreafsky River"|
         LocationID == "Andreafsky River"|LocationID == "Anvik River" |
         LocationID == "Stevens Village (Village/City)"|
         LocationID == "Chena River" |LocationID == "Andreafsky River (East Fork)" |
         LocationID == "Nulato River" |LocationID == "Gisasa River" |
         LocationID == "Jim River"|LocationID == "Koyukuk River"|
         LocationID == "Chandalar River"| LocationID == "Henshaw Creek" |
         LocationID == "Chulitna River" |LocationID == "Tozitna River" |
         LocationID == "Barton Creek" |LocationID == "Goodpaster River"|
         LocationID == "Chatanika (Village/City)"|LocationID == "Kateel River"|
         LocationID == "Tanana (Village/City)"|LocationID == "Tanana River"
         )

p<-ggplot(data = Yukon.US, aes(x=sampleYear, y=n1)) +
            geom_bar(stat="identity")+
            facet_wrap(~LocationID) +
  xlab("Year") + ylab("Count")
p

Yukon.US <- Yukon%>%
  filter(LocationID == "Salcha River" | LocationID == "Andreafsky River"|
           LocationID == "Anvik River" |
           LocationID == "Chena River" |LocationID == "Andreafsky River (East Fork)" |
           LocationID == "Gisasa River" |
           LocationID == "Henshaw Creek" |
           LocationID == "Tozitna River" |
          LocationID == "Big Salmon River" |
           LocationID == "Little Salmon River - Yukon" |LocationID == "Nisutlin River"  |
          LocationID == "Tatchun Creek"
  )




Yukon_dat_US <- Yukon.US%>%
  select(c("LocationID", "sampleYear", "mean"))%>%
  pivot_wider(names_from = LocationID, 
              values_from = mean,id_expand=FALSE)


##Setting up data
yr_frst <- 1980
yr_last <- 2016

Yukon_dat_US  <- Yukon_dat_US [Yukon_dat_US [, "sampleYear"] >= yr_frst & 
                                 Yukon_dat_US [, "sampleYear"] <= yr_last,]

locations <-unique(Yukon.US$LocationID)

## get only the phytoplankton
dat_1980 <- Yukon_dat_US[,locations]

## transpose data so time goes across columns
dat_1980 <- t(dat_1980)
## get number of time series
N_ts <- dim(dat_1980)[1]
## get length of time series
TT <- dim(dat_1980)[2] 
## 'ZZ' is loadings matrix
Z_vals <- list("z11", 
               "z21",
               "z31",
               "z41",
               "z51",
               "z61",
               "z71",
               "z81",
               "z91",
               "z101",
               "z111",
               "z121")
ZZ <- matrix(Z_vals, nrow = N_ts, ncol = 1, byrow = TRUE)
ZZ

y_bar <- apply(dat_1980, 1, mean, na.rm = TRUE)
sd<- apply(dat_1980, 1, sd, na.rm = TRUE)
dat <- (dat_1980 - y_bar)/sd
rownames(dat) <- rownames(dat_1980)
## 'aa' is the offset/scaling
aa <- "zero"
## 'DD' and 'd' are for covariates
DD <- "zero"  # matrix(0,mm,1)
dd <- "zero"  # matrix(0,1,wk_last)
## 'RR' is var-cov matrix for obs errors
RR <- "diagonal and unequal"

## number of processes
mm <- 1
## 'BB' is identity: 1's along the diagonal & 0's elsewhere
BB <- "identity"  # diag(mm)
## 'uu' is a column vector of 0's
uu <- "zero"  # matrix(0, mm, 1)
## 'CC' and 'cc' are for covariates
CC <- "zero"  # matrix(0, mm, 1)
cc <- "zero"  # matrix(0, 1, wk_last)
## 'QQ' is identity
QQ <- "diagonal and equal" # diag(mm)

## list with specifications for model vectors/matrices
mod_list <- list(Z = ZZ, A = aa, D = DD, d = dd, R = RR,
                 B = BB, U = uu, C = CC, c = cc, Q = QQ)
## list with model inits
init_list <- list(x0 = matrix(rep(0, mm), mm, 1))
## list with model control parameters
con_list <- list(maxit = 3000, allow.degen = TRUE)

## fit MARSS
dfa_1 <- MARSS(y = dat, model = mod_list, inits = init_list, control = con_list)

## get the estimated ZZ
Z_est <- coef(dfa_1, type="matrix")$Z
## Code for plotting one trend 
H_inv <- 1
Z_rot = Z_est %*% H_inv   
proc_rot = solve(H_inv) %*% dfa_1$states
ylbl <-locations
w_ts <- seq(dim(dat)[2])
## plot the processes

pdf(file = "Chinook/Output/Figures/Supplement/FigS9_YukonDFASize.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 7)

layout(matrix(c(1,2),mm,2),widths=c(2,1))
par(mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))


clr<-nord(n = 15, palette = "lumina")[5:15]

for(i in 1:mm) {
  ylm <- c(-1,1)*max(abs(proc_rot[i,]))
  ## set up plot area
  plot(w_ts,proc_rot[i,], type="n", bty="L",
       ylim=ylm, xlab="", ylab="", xaxt="n")
  ## draw zero-line
  abline(h=0, col="gray")
  ## plot trend line
  lines(w_ts,proc_rot[i,], lwd=2)
  lines(w_ts,proc_rot[i,], lwd=2)
  ## add panel labels
  mtext(paste("Yukon Trend"), side=3, line=0.5)
  axis(1,(0:dim(dat_1980)[2])+1,yr_frst+0:dim(dat_1980)[2])
}
## plot the loadings
minZ <- 0
ylm <- c(-1,1)*max(abs(Z_rot))
for(i in 1:mm) {
  plot(c(1:N_ts)[abs(Z_rot[,i])>minZ], as.vector(Z_rot[abs(Z_rot[,i])>minZ,i]), type="h",
       lwd=2, xlab="", ylab="", xaxt="n", ylim=ylm, xlim=c(0.5,N_ts+0.5), col=clr)
  for(j in 1:N_ts) {
    if(Z_rot[j,i] > minZ) {text(j, -0.03, ylbl[j], srt=90, adj=1, cex=1.2, col=clr[j])}
    if(Z_rot[j,i] < -minZ) {text(j, 0.03, ylbl[j], srt=90, adj=0, cex=1.2, col=clr[j])}
    abline(h=0, lwd=1.5, col="gray")
  } 
  mtext(paste("Factor loadings on state",i),side=3,line=0.5)
  
}
dev.off()

get_DFA_fits <- function(MLEobj, dd = NULL, alpha = 0.05) {
  ## empty list for results
  fits <- list()
  ## extra stuff for var() calcs
  Ey <- MARSS:::MARSShatyt(MLEobj)
  ## model params
  ZZ <- coef(MLEobj, type="matrix")$Z
  ## number of obs ts
  nn <- dim(Ey$ytT)[1]
  ## number of time steps
  TT <- dim(Ey$ytT)[2]
  ## get the inverse of the rotation matrix
  H_inv <- varimax(ZZ)$rotmat
  ## check for covars
  if(!is.null(dd)) {
    DD <- coef(MLEobj, type = "matrix")$D
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states + DD %*% dd
  } else {
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states
  }
  ## Var in model fits
  VtT <- MARSSkfss(MLEobj)$VtT
  VV <- NULL
  for(tt in 1:TT) {
    RZVZ <- coef(MLEobj, type = "matrix")$R - ZZ %*% VtT[,,tt] %*% t(ZZ)
    SS <- Ey$yxtT[,,tt] - Ey$ytT[,tt,drop = FALSE] %*% t(MLEobj$states[,tt,drop = FALSE])
    VV <- cbind(VV, diag(RZVZ + SS %*% t(ZZ) + ZZ %*% t(SS)))
  }
  SE <- sqrt(VV)
  ## upper & lower (1-alpha)% CI
  fits$up <- qnorm(1-alpha/2)*SE + fits$ex
  fits$lo <- qnorm(alpha/2)*SE + fits$ex
  return(fits)
}

## get model fits & CI's
mod_fit <- get_DFA_fits(dfa_1)
## plot the fits
ylbl <- locations
d <- residuals(dfa_1, type="tt1")
d$.conf.low <- d$.fitted+qnorm(0.05/2)*d$.sigma
d$.conf.up <- d$.fitted-qnorm(0.05/2)*d$.sigma
d <- d %>%
  mutate(Year = t+yr_frst-1)

pdf(file = "Chinook/Output/Figures/Supplement/FigS10_YukonDFASizeFit.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 6)
p<-ggplot(data = d) +
  geom_line(aes(Year, .fitted)) +
  geom_point(aes(Year, value)) +
  geom_ribbon(aes(x = Year, ymin = .conf.low, ymax = .conf.up), linetype = 2, alpha = 0.1) +
  facet_wrap(~.rownames) +
  xlab("Year") + ylab("Standardized Size")
p

dev.off()

Yukon.size <- data.frame(Size = proc_rot[1,])%>%
  add_column(Year = seq(yr_frst,yr_last,1), Region = 'Yukon')%>%
  add_column(size_stand = 
               (proc_rot[1,]-mean(proc_rot[1,]))/sd(proc_rot[1,]))

#### Write output file ####

size <- rbind(Yukon.size, Kuskokwim.size)

write.csv(size, "ocean/SizeTrend2016.csv")


#######Marine DFA  ####
marine <-readRDS("Chinook/Output/posteriors/marine.rds")



##Setting up data
yr_frst <- 1983
yr_last <- 2020

marine.bw  <- marine[marine[, "year"] >= yr_frst & 
                       marine[, "year"] <= yr_last,]

covs <-c("Age_3_Biomass_stand","Recruitment_stand",      
         "pink_stand","chum_stand")

## get only the phytoplankton
marine.bw <- marine.bw[,covs]

## transpose data so time goes across columns
marine.bw  <- t(marine.bw)
## get number of time series
N_ts <- dim(marine.bw)[1]
## get length of time series
TT <- dim(marine.bw)[2] 
## 'ZZ' is loadings matrix
Z_vals <- list("z1", 
               "z2",
               "z3",
               "z4"
               )
ZZ <- matrix(Z_vals, nrow = N_ts, ncol = 1, byrow = TRUE)
ZZ
dat <- marine.bw 
rownames(dat) <- rownames(marine.bw)
## 'aa' is the offset/scaling
aa <- "zero"
## 'DD' and 'd' are for covariates
DD <- "zero"  # matrix(0,mm,1)
dd <- "zero"  # matrix(0,1,wk_last)
## 'RR' is var-cov matrix for obs errors
RR <- "diagonal and unequal"

## number of processes
mm <- 1
## 'BB' is identity: 1's along the diagonal & 0's elsewhere
BB <- "identity"  # diag(mm)
## 'uu' is a column vector of 0's
uu <- "zero"  # matrix(0, mm, 1)
## 'CC' and 'cc' are for covariates
CC <- "zero"  # matrix(0, mm, 1)
cc <- "zero"  # matrix(0, 1, wk_last)
## 'QQ' is identity
QQ <- "unconstrained"  # diag(mm)

## list with specifications for model vectors/matrices
mod_list <- list(Z = ZZ, A = aa, D = DD, d = dd, R = RR,
                 B = BB, U = uu, C = CC, c = cc, Q = QQ)
## list with model inits
init_list <- list(x0 = matrix(rep(0, mm), mm, 1))
## list with model control parameters
con_list <- list(maxit = 3000, allow.degen = TRUE)

## fit MARSS
dfa_1 <- MARSS(y = dat, model = mod_list, inits = init_list, control = con_list)

## get the estimated ZZ
Z_est <- coef(dfa_1, type="matrix")$Z
## Code for plotting one trend 
H_inv <- 1
Z_rot = Z_est %*% H_inv   
proc_rot = solve(H_inv) %*% dfa_1$states
ylbl <-c('EBS walleye pollock 3+',
         'EBS walleye pollock recruits',      
         'North Pacific pink salmon',
         'North Pacific chum salmon')
w_ts <- seq(dim(dat)[2])

pdf(file = "Chinook/Output/Figures/Supplement/FigS11_MarineComptDFA.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 6)
## plot the processes

layout(matrix(c(1,2),mm,2),widths=c(1,1))
par(mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))


clr<-rep(nord(n = 5, palette = "lumina")[2:5])

for(i in 1:mm) {
  ylm <- c(-1,1)*max(abs(proc_rot[i,]))
  ## set up plot area
  plot(w_ts,proc_rot[i,], type="n", bty="L",
       ylim=ylm, xlab="", ylab="", xaxt="n")
  ## draw zero-line
  abline(h=0, col="gray")
  ## plot trend line
  lines(w_ts,proc_rot[i,], lwd=2)
  lines(w_ts,proc_rot[i,], lwd=2)
  ## add panel labels
  mtext(paste("Marine Trend"), side=3, line=0.5)
  axis(1,(0:dim(marine.bw )[2])+1,yr_frst+0:dim(marine.bw)[2])
}
## plot the loadings
minZ <- 0
ylm <- c(-1,1)*max(abs(Z_rot))
for(i in 1:mm) {
  plot(c(1:N_ts)[abs(Z_rot[,i])>minZ], as.vector(Z_rot[abs(Z_rot[,i])>minZ,i]), type="h",
       lwd=2, xlab="", ylab="", xaxt="n", ylim=ylm, xlim=c(0.5,N_ts+0.5), col=clr)
  for(j in 1:N_ts) {
    if(Z_rot[j,i] > minZ) {text(j, -0.03, ylbl[j], srt=90, adj=1, cex=1.2, col=clr[j])}
    if(Z_rot[j,i] < -minZ) {text(j, 0.03, ylbl[j], srt=90, adj=0, cex=1.2, col=clr[j])}
    abline(h=0, lwd=1.5, col="gray")
  } 
  mtext(paste("Factor loadings on state",i),side=3,line=0.5)
  
}

dev.off()
get_DFA_fits <- function(MLEobj, dd = NULL, alpha = 0.05) {
  ## empty list for results
  fits <- list()
  ## extra stuff for var() calcs
  Ey <- MARSS:::MARSShatyt(MLEobj)
  ## model params
  ZZ <- coef(MLEobj, type="matrix")$Z
  ## number of obs ts
  nn <- dim(Ey$ytT)[1]
  ## number of time steps
  TT <- dim(Ey$ytT)[2]
  ## get the inverse of the rotation matrix
  H_inv <- varimax(ZZ)$rotmat
  ## check for covars
  if(!is.null(dd)) {
    DD <- coef(MLEobj, type = "matrix")$D
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states + DD %*% dd
  } else {
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states
  }
  ## Var in model fits
  VtT <- MARSSkfss(MLEobj)$VtT
  VV <- NULL
  for(tt in 1:TT) {
    RZVZ <- coef(MLEobj, type = "matrix")$R - ZZ %*% VtT[,,tt] %*% t(ZZ)
    SS <- Ey$yxtT[,,tt] - Ey$ytT[,tt,drop = FALSE] %*% t(MLEobj$states[,tt,drop = FALSE])
    VV <- cbind(VV, diag(RZVZ + SS %*% t(ZZ) + ZZ %*% t(SS)))
  }
  SE <- sqrt(VV)
  ## upper & lower (1-alpha)% CI
  fits$up <- qnorm(1-alpha/2)*SE + fits$ex
  fits$lo <- qnorm(alpha/2)*SE + fits$ex
  return(fits)
}

## get model fits & CI's
mod_fit <- get_DFA_fits(dfa_1)
## plot the fits

d <- residuals(dfa_1, type="tt1")
d$.conf.low <- d$.fitted+qnorm(0.05/2)*d$.sigma
d$.conf.up <- d$.fitted-qnorm(0.05/2)*d$.sigma
d <- d %>%
  mutate(Year = t+yr_frst-1)%>%
  mutate(label=rep(ylbl, each=38))

pdf(file = "Chinook/Output/Figures/Supplement/FigS12_MarineComptDFAFit.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 6)
p<-ggplot(data = d) +
  geom_line(aes(Year, .fitted)) +
  geom_point(aes(Year, value)) +
  geom_ribbon(aes(x = Year, ymin = .conf.low, ymax = .conf.up), linetype = 2, alpha = 0.1) +
  facet_wrap(~label,
             labeller = labeller(label = label_wrap_gen(width = 25))) +
  xlab("Year") + ylab("Abundance")
p
dev.off()



MarineCov <- data.frame(marine = proc_rot[1,])%>%
  add_column(Year = seq(yr_frst,yr_last,1))


#### Write out put file ####
write.csv(MarineCov, "ocean/MarineTrend.csv")
