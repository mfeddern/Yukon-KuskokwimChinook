model{
  for (y in nages+fage:nyrs+nages-1) {
    log.R[y] ~ dnorm(log.R.mean2[y],tau.R)
    R[y] <- exp(log.R[y])
    #log.R.mean1[y] <- log(S[y-lage]) + lnalpha - lnalpha/Req * S[y-lage]
    log.R.mean1[y] <- log(S[y-lage]) + lnalpha - beta * S[y-lage]
    #log.R.mean1[y] <- log(S[y-lage]) + lnalpha - log(1+beta * S[y-lage])
    #log.R.mean1[y] <- lnalpha + log(S[y-lage]) - w*lnalpha/Req*S[y-lage]-(1-w)*log(1+ (alpha-1)/Req*S[y-lage])
    log.resid[y] <- log(R[y]) - log.R.mean1[y]
    RPS[y] <- R[y]/S[y-lage]
  }
  log.R.mean2[nages+fage] <- log.R.mean1[nages+fage] + phi * log.resid.0
  for (y in nages+fage+1:nyrs+nages-1) {
    log.R.mean2[y] <- log.R.mean1[y] + phi * log.resid[y-1]
  }
  #w ~ dbern(0.5)
  lnalpha ~ dnorm(0,1.0E-6) T(0,)
  beta ~ dnorm(0,1.0E-6)%_%I(0,) 
  #beta <- lnalpha/Req
  #Req <- exp(lnReq)
  Req <- lnalpha/beta
  #lnReq <- log(Req)
  #lnReq ~ dnorm(0.0,1e-6)%_%I(0,30)           
  tau.R ~ dgamma(0.001,0.001)        
  log.resid.0 ~ dnorm(0,tau.red)%_%I(-5,5)
  alpha <- exp(lnalpha)
  tau.red <- tau.R * (1-phi*phi)
  sigma.R <- 1 / sqrt(tau.R)
  sigma.red <- 1 / sqrt(tau.red)
  phi ~ dunif(-1,1)
  
  # BROOD YEAR RETURNS W/O SR LINK DRAWN FROM COMMON LOGNORMAL DISTN
  mean.log.R0 ~ dnorm(0,1.0E-6)%_%I(0,30)  #normal prior on mean log recruitment for initial brood years without stock recruit link (no information on escapement)       
  tau.R0 ~ dgamma(0.1,0.1)           #inverse gamma prior on error standard deviation on recruitments without stock recruit link         
  sigma.R0 <- 1/sqrt(tau.R0)
  for (y in 1:lage) { 
    log.R[y] ~ dnorm(mean.log.R0,tau.R0) 
    R[y] <- exp(log.R[y]) 
  }
  
  # GENERATE MATURITY SCHEDULES
  D.scale ~ dunif(0,1)
  D.sum <- 1 / (D.scale * D.scale)
  pi[1] <- prob[1]
  prob[1] ~ dbeta(1,1)
  prob[2] ~ dbeta(1,1)
  prob[3] ~ dbeta(1,1)
  pi[2] <- prob[2] * (1 - pi[1])
  pi[3] <- prob[3] * (1 - pi[1] - pi[2])
  pi[4] <- 1 - pi[1] - pi[2] - pi[3]
  for (a in 1:nages) {
    gamma[a] <- D.sum * pi[a]
    for (y in 1:nyrs+nages-1) {                                                    
      g[y,a] ~ dgamma(gamma[a],0.1)
      pi.ta[y,a] <- g[y,a]/sum(g[y,])
    }
  }
  
  # CALCULATE THE NUMBERS AT AGE MATRIX
  for(t in 1:nyrs){
    for(a in 1:nages){
      N.ta[t,a]<-R[t+nages-a]*pi.ta[t+nages-a,a]
    }
  }
  
  # MULTINOMIAL SCALE SAMPLING ON TOTAL ANNUAL RETURN N
  # INDEX t IS CALENDAR YEAR
  for (t in 1:nyrs) {
    N[t] <- sum(N.ta[t,1:nages])
    for (a in 1:nages) {
      q[t,a] <- N.ta[t,a] / N[t]
    }
    n[t] <- sum(x[t,1:nages])
    x[t,1:nages] ~ dmulti(q[t,],n[t])
  }
  
  #Proportion of fish going up north river
  Pnr.scale ~ dunif(0,1)
  Pnr.sum <- 1 / (Pnr.scale * Pnr.scale)
  Pnr.pi ~ dbeta(1,1)
  #shape1.Pnr ~ dgamma(0.001,0.001)
  #shape2.Pnr ~ dgamma(0.001,0.001)
  shape1.Pnr <- Pnr.pi*Pnr.sum
  shape2.Pnr <- (1-Pnr.pi)*Pnr.sum
  for (t in 1:nyrs){
    Pnr[t] ~ dbeta(shape1.Pnr,shape2.Pnr)%_%I(0.01,0.975)
    shape1.Pnr.obs[t] <- Pnr[t]*(Pnr[t]*(1-Pnr[t])/pow(obs.cv.Pnr[t]*Pnr[t],2)-1)
    shape2.Pnr.obs[t] <- (1-Pnr[t])*(Pnr[t]*(1-Pnr[t])/pow(obs.cv.Pnr[t]*Pnr[t],2)-1)
    obs.Pnr[t] ~ dbeta(shape1.Pnr.obs[t],shape2.Pnr.obs[t])
  }
  
  # APPLY (SMALL, KNOWN) HARVEST BELOW SONAR TO GET INRIVER RETURN
  # HARVEST ABOVE SONAR IS ESTIMATED, AND CAN BE LARGE
  
  for (t in 1:nyrs) {                     
    sigma.H.com[t]<-sqrt(log(pow(obs.cv.Hcom[t],2)+1))
    tau.H.com[t]<-1/pow(sigma.H.com[t],2)
    mu.H.com[t] ~ dbeta(1,1)
    H.com[t] <- N[t] * mu.H.com[t]
    log.H.com[t] <- log(H.com[t])
    obs.Hcom[t] ~ dlnorm(log.H.com[t],tau.H.com[t])  
    
    sigma.H.sub[t]<-sqrt(log(pow(obs.cv.Hsub[t],2)+1))
    tau.H.sub[t]<-1/pow(sigma.H.sub[t],2)
    mu.H.sub[t] ~ dbeta(shape1.mu.sub,shape2.mu.sub)
    H.sub[t] <- N[t] * (1-mu.H.com[t]) * mu.H.sub[t]
    log.H.sub[t] <- log(H.sub[t])
    obs.Hsub[t] ~ dlnorm(log.H.sub[t],tau.H.sub[t])
    
    log.cpcom[t] <- log(q.cpcom*N[t])
    obs.cpcom[t] ~ dlnorm(log.cpcom[t], tau.cpcom)
    
    inriver[t] <- N[t] - H.com[t] - H.sub[t]
    log.test[t] <- log(inriver[t]*q.test)
    obs.test[t] ~ dlnorm(log.test[t],tau.test) 
    
    mu.H.sport[t] ~ dbeta(shape1.mu.sport,shape2.mu.sport)
    sigma.H.sport[t]<-sqrt(log(pow(obs.cv.Hsport[t],2)+1))
    tau.H.sport[t]<-1/pow(sigma.H.sport[t],2)
    H.sport[t] <- inriver[t] * mu.H.sport[t]
    log.H.sport[t] <- log(H.sport[t])
    obs.Hsport[t] ~ dlnorm(log.H.sport[t],tau.H.sport[t])
    
    inriver.NR[t] <- inriver[t]*(1-mu.H.sport[t])*Pnr[t]
    sigma.tower[t]<-sqrt(log(pow(obs.cv.tower[t],2)+1))
    tau.tower[t]<-1/pow(sigma.tower[t],2)		
    log.tower[t] <- log(inriver.NR[t])
    obs.tower[t] ~ dlnorm(log.tower[t],tau.tower[t])
    log.air.NR[t] <- log(inriver.NR[t]*q.air[1])
    obs.air.NR[t] ~ dlnorm(log.air.NR[t],tau.air[1]) 
    
    inriver.UR[t] <- inriver[t]*(1-mu.H.sport[t])*(1-Pnr[t])
    sigma.wier[t]<-sqrt(log(pow(obs.cv.wier[t],2)+1))
    tau.wier[t]<-1/pow(sigma.wier[t],2)		
    log.wier[t] <- log(inriver.UR[t])
    obs.wier[t] ~ dlnorm(log.wier[t],tau.wier[t])
    log.air.UR[t] <- log(inriver.UR[t]*q.air[2])
    obs.air.UR[t] ~ dlnorm(log.air.UR[t],tau.air[2]) 
    log.air.OWR[t] <- log(inriver.UR[t]*q.air[3])
    obs.air.OWR[t] ~ dlnorm(log.air.OWR[t],tau.air[3]) 
    
    S[t] <- inriver[t]*(1-mu.H.sport[t])
  }
  #shape1.mu.sub ~ dgamma(0.001,0.001)
  #shape2.mu.sub ~ dgamma(0.001,0.001)
  mu.sub.pi ~ dbeta(1,1)
  mu.sub.scale ~ dunif(0,1)
  mu.sub.sum <- 1/(mu.sub.scale*mu.sub.scale)
  shape1.mu.sub <- mu.sub.pi*mu.sub.sum
  shape2.mu.sub <- (1-mu.sub.pi)*mu.sub.sum
  
  #shape1.mu.sport ~ dgamma(0.001,0.001)
  #shape2.mu.sport ~ dgamma(0.001,0.001)
  mu.sport.pi ~ dbeta(1,1)
  mu.sport.scale ~ dunif(0,1)
  mu.sport.sum <- 1/(mu.sport.scale*mu.sport.scale)
  shape1.mu.sport <- mu.sport.pi*mu.sport.sum
  shape2.mu.sport <- (1-mu.sport.pi)*mu.sport.sum
  
  sigma.test<-1/pow(tau.test,2)
  tau.test ~ dgamma(0.001,0.001)	
  q.test ~ dbeta(1,1)
  
  sigma.cpcom <- 1/pow(tau.cpcom, 2)
  tau.cpcom ~ dgamma(0.001, 0.001)
  q.cpcom ~ dbeta(1, 1)
  
  for (i in 1:3){
    tau.air[i] ~ dgamma(0.001,0.001)
    sigma.air[i]<-1/pow(tau.air[i],2)
    q.air[i] ~ dbeta(1,1)
  }
}