nc <- readRDS("Chinook/Output/posteriors/residualnc.rds")

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
effect.rec<-array(dim = c((total_iterations-warmups)*n_chains, npop, 11))
effect<-array(dim = c((total_iterations-warmups)*n_chains, npop, 11))
j<-1
i<- 1
for(j in 1:11){
  for(i in 1:npop){
    for (y in 1:N){
  effect.null.rec[,j] <- mean_spawn$mean_spawn[j]*exp(alpha[,j]-mean_spawn$mean_spawn[j]*beta[,j])
  effect.rec[,i,j] <- mean_spawn$mean_spawn[j]*exp(alpha[,j]-mean_spawn$mean_spawn[j]*beta[,j]+theta$theta[,i,j]*apply(covariates,2,sd)[j])
  }
}
  effect[,,j] <- ((effect.rec[,i,j]-effect.null.rec[,j])/effect.null.rec[,j])*100
}

effectsummary <- data.frame()
for(i in 1:11){
  df <- data.frame(t(hdi(effect[,i,], credMass = 0.95)))%>%
    data.frame(t(hdi(effect[,i], credMass = 0.5)))%>%
    add_column(mean = mean(effect[,i]))%>%
    # add_column(pop=unique(full.dat.wide$pop))%>%
    add_column(Covar.Name=rep(names.covars[i],1))
  effectsummary <- rbind(effectsummary,df)
}