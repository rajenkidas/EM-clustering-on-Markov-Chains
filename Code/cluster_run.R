
set.seed(4115)

no.states  <- 4
no.classes <- 4
list.counts<-count.trans.per.sub(traj,no.states)
no.subs<-length(list.counts)
gamma<- matrix(c(rep(0,no.subs*no.classes)),no.subs,no.classes)
gamma.est<-matrix(c(rep(0,nrow(gamma)*ncol(gamma))), nrow(gamma),ncol(gamma))

for(k in 1:nrow(gamma))
  gamma[k,]<-runif(no.classes)

gamma<-normalise.gam(gamma)
mix.wgts<-mix.wgts.fun(gamma)
list.mats<-est.trans.mat(gamma,list.counts)
loglike<-computeloglike(list.counts,list.mats)
gamma.est<-gamma.est.fun(gamma)
gamma.est<-normalise.gam(gamma.est)


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
  gamma.est<-gamma.est.fun(gamma)
  gamma.est<-normalise.gam(gamma.est)
  
  no.iter <- no.iter + 1 ;
  delta.gamma <- gamma - gamma.est
  dg.squared <- delta.gamma * delta.gamma  
  rms.delta.gamma <- sqrt( sum(dg.squared)/ (no.subs*no.classes) ) 
  
  gamma<-gamma.est
  
}

####################################iter ends#############################
if(any(duplicated(gamma[1,]))==TRUE)
{print("There is a repeat")}else
  {gamma<-gamma[,order(gamma[1,])]}


#assigning 1's to where the probability to be in the cluster is maximum
#3s to when equal

for(i in 1: nrow(gamma))
  gamma[i,which(gamma[i,]==max(gamma[i,]))]<-1

#gamma tab is gamma with an extra column showing the cluster number per row
gamma.tab<-as.data.frame(gamma)
gamma.tab$clust<-0

for(i in 1:nrow(gamma))
{
  for(k in 1:ncol(gamma))
  {
    if(isTRUE(gamma[i,k]==1))
      gamma.tab$clust[i]<-k
    
  }
}



