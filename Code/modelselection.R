################################################################################
################################################################################
#code is similar to em_clust except additional lines for overall log likelihood#
################################################################################

overall.logelike <- function(loglike){
  part.of.overall.ll<-matrix(c(rep(0,nrow(loglike)*1)),nrow(loglike),1)
  for(i in 1:nrow(loglike))#warning: do not use length instead of nrow
    {
    part.of.overall.ll[i,1]<-sum(mix.wgts*exp(loglike[i,]))
    }
  overall.ll<-colSums(log(part.of.overall.ll))
  return(overall.ll)
}


################################################################################
traj <- load("trajectorysample.R")

max.classes <- 10
list.loglike<-vector("list", max.classes)
for(c in 1:max.classes)
{
  no.states  <- 4
  no.classes <- c
  list.counts <- count.trans.per.sub(traj,no.states)
  no.subs <- length(list.counts)
  gamma <- matrix(c(rep(0,no.subs*no.classes)),no.subs,no.classes)
  gamma.est <- matrix(c(rep(0,nrow(gamma)*ncol(gamma))), nrow(gamma),ncol(gamma))
  
  for(k in 1:nrow(gamma))
    gamma[k,] <- runif(no.classes)
  
  gamma <- normalise.gam(gamma)
  mix.wgts <- mix.wgts.fun(gamma)
  list.mats <- est.trans.mat(gamma,list.counts)
  loglike <- computeloglike(list.counts,list.mats)
  overall.ll <- overall.logelike(loglike)
  gamma.est <-gamma.est.fun(gamma)
  gamma.est <- normalise.gam(gamma.est)
  
  
######################################################################
#########################iteration####################################
######################################################################
 
  #convergence criterion and initiation:
  no.iter<-1
  no.iter <- no.iter + 1 ;
  
  no.classes<-ncol(gamma)
  delta.gamma <- gamma - gamma.est
  dg.squared <- delta.gamma * delta.gamma  # elementwise multiplication
  rms.delta.gamma <- sqrt( sum(dg.squared)/ (no.subs*no.classes) ) 
  
  
  #iteration
  
  gamma<-gamma.est
  
  eps<-0.000001
  no.iter<-2
  
  while (isTRUE(rms.delta.gamma>=eps)) {
    mix.wgts<-mix.wgts.fun(gamma)
    list.mats<-est.trans.mat(gamma,list.counts)
    loglike<-computeloglike(list.counts,list.mats)
    overall.ll <- overall.logelike(loglike)
    gamma.est<-gamma.est.fun(gamma)
    gamma.est<-normalise.gam(gamma.est)
    
    no.iter <- no.iter + 1 ;
    delta.gamma <- gamma - gamma.est
    dg.squared <- delta.gamma * delta.gamma  
    rms.delta.gamma <- sqrt( sum(dg.squared)/ (no.subs*no.classes) ) 
    
    gamma<-gamma.est
    
  }
  
  
  list.loglike[[c]]<-overall.ll
}



##############################################################################################
##############################################################################################
#####################      Criteria for number of clusters      ##############################
##############################################################################################
##############################################################################################

list.negll <- vector("list", max.classes)
list.aic <- vector("list", max.classes)
list.bic <- vector("list", max.classes)
list.aicc <- vector("list", max.classes)
for(i in 1:max.classes)
{ 
  k<-i*(no.states)*(no.states-1)
  list.negll[[i]] <- (-1)*(list.loglike[[i]])
  list.aic[[i]]<-(-1)*(list.loglike[[i]]) + 2*k
  list.bic[[i]]<-(-2)*(list.loglike[[i]]) + k*log(no.subs)
  #list.bic[[i]]<-(-2)*(list.loglike.aic[[i]]) + 2*k*log(log(no.subs))
  list.aicc[[i]]<-(-2)*(list.loglike[[i]]) + 2*k*(no.subs/(no.subs-k-1))
} 

plot(unlist(list.negll))
plot(unlist(list.aic))
plot(unlist(list.bic))
plot(unlist(list.aicc))
