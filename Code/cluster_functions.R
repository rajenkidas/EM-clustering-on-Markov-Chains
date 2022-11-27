

####################TRAJECTORIES CREATED#####################
#count of transitions per subject where no.states is the number of states
#traj is the dataset with the columns subjects and state i.e. the sequence
#of states can be found from here
#no.states<-4 #depending on the no. of non na transtype states kept.
#############################################################
############### F U N C T I O N S ###########################
#############################################################
count.trans.per.sub <- function(traj,no.states)
{
  
  list.counts <- vector("list",length(traj))
  for(k in 1:length(traj)){
    mat <- matrix(c(rep(0,no.states*no.states)),no.states,no.states)
    for(i in 2:length(traj[[k]])){
      mat[traj[[k]][i-1],traj[[k]][i]] <- (mat[traj[[k]][i-1],traj[[k]][i]])+1
    }
    list.counts[[k]] <- mat
  }
  return(list.counts)
}

#we have gamma: n rows- no of subs, m columns- m is the number of clusters
#pi or mixture weights: mix.wgts
#no.classes<-2 # no of clusters taken

normalise.gam <- function(gamma){
  for(i in 1: nrow(gamma))
    gamma[i,] <- gamma[i,]/sum(gamma[i,])
  return(gamma)
}


mix.wgts.fun <- function(gamma)
{
  no.sub <- nrow(gamma)
  mix.wgts <- colSums(gamma)/no.sub
  return(mix.wgts)
}

#estimate the transition matrices
est.trans.mat <- function(gamma,list.counts)
{
  no.classes <- ncol(gamma)
  no.subs <- nrow(gamma)
  no.states <- nrow(list.counts[[1]])
  list.mats <- vector("list",no.classes)
  for(i in 1: no.classes)
  {
    # Accumulate a weightd sum of counts for the i-th class i.e.
    # counts are multiplied by gammas of a class and added up
    # and normalised to get estimated transition matrix
    M <- matrix(c(rep(0,no.states*no.states)),no.states,no.states)
    for(j in 1:no.subs)
    {
      M <- gamma[j,i]*list.counts[[j]] + M
    }
    # Normalise the rows
    for( k  in 1:no.states)
    {
      M[k,] <- M[k,] / sum(M[k,])
    }
    list.mats[[i]] <- M
  }
  
  
  return(list.mats)
}


#using count matrices per subject and estimated trans 
#matrix per component we find the loglikelihood here

computeloglike <- function(list.counts,list.mats)
{
  no.classes <- length(list.mats)
  no.subs <- length(list.counts)
  no.states <- nrow(list.counts[[1]])
  loglike <- matrix(c(rep(0,no.subs*no.classes)),no.subs,no.classes)
  for(i in 1:no.subs) #loops runs for number of subjects/ no of count matrices
    for(j in 1:no.classes) #loop runs for number of clusters/no of estimated trans matrices
    {
      prod <- list.counts[[i]]*log(list.mats[[j]]) #elementwise multiplication; could have converted 
      #the mats to vectors to achieve the same and for better clarity
      loglike[i,j] <- sum(prod, na.rm = TRUE)
    }
  return(loglike)
}


gamma.est.fun <- function(gamma){
  for(i in 1:nrow(gamma))
    for(j in 1:ncol(gamma))
    {gamma.est[i,j] <- mix.wgts[j]*exp(loglike[i,j])}
  return(gamma.est)}





