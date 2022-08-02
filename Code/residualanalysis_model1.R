library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Import data
# Conversion to single digit state to denote combination of mood and pain
data$state <- 0
data$state <- 5*(data$Mood)-data$painSeverity + 1

# Building a count matrix that contains total number of transitions between two states
# We have 5*5 = 25 states
count_mat <- matrix(c(rep(0,25*25)),25,25)
for(i in 2: length(data$state))
  {count_mat[(data$state[i]),(data$state[i-1])] <- 
  (count_mat[(data$state[i]),(data$state[i-1])])+1}


# Recording sum of transitions per row
nTransitionsSeen=rep(0, 25)
for(j in 1:25)
  nTransitionsSeen[j]<-sum(count_mat[j,])

# Calculating transition probability matrix by dividing each row count by the row sum
trans_prob=matrix(c(rep(0,625)),25,25)
for(i in 1:25)
  trans_prob[i,]<-count_mat[i,]/nTransitionsSeen[i]


# Probability matrix that is to be used to find residual: MODEL 1
prob_res1=matrix(c(rep(0,625)),25,25)
for(i in 1:25)
  for(j in 1:25)
{
  if(i==j){prob_res1[i,j]<-trans_prob[i,j]}
  else
  {prob_res1[i,j]<-(1-trans_prob[i,j])/24}
}


# Expectation matrix
exp_mat1=matrix(c(rep(0,625)),25,25)
for(i in 1:25)
  for(j in 1:25)
    exp_mat1[i,j]<-nTransitionsSeen[i]*prob_res1[i,j]


# Residual matrix 1
res1=matrix(c(rep(0,625)),25,25)
for(i in 1:25)
  for(j in 1:25)
  {
    res1[i,j]<-((count_mat[i,j]-exp_mat1[i,j]))/sqrt(exp_mat1[i,j])
    }

########################################################################################
########                               Plotting                                #########
########################################################################################


library(RColorBrewer)
library(ggpubr)
library(reshape2)
library(colorspace) 



statelabs <- c("M:1 P:5","M:1 P:4","M:1 P:3","M:1 P:2","M:1 P:1",
               "M:2 P:5","M:2 P:4","M:2 P:3","M:2 P:2","M:2 P:1",
               "M:3 P:5","M:3 P:4","M:3 P:3","M:3 P:2","M:3 P:1",
               "M:4 P:5","M:4 P:4","M:4 P:3","M:4 P:2","M:4 P:1",
               "M:5 P:5","M:5 P:4","M:5 P:3","M:5 P:2","M:5 P:1")

# Scatter plot
plot(exp_mat1, res1, xlab = 'Expected Value', ylab='Residual')


# Data frame for ggplot
rownames(res1) <- statelabs
colnames(res1) <- statelabs
p1 <- melt((res1)) 
colnames(p1) <- c("From", "To", "Residual")

# Heatmap
ggplot(p1, aes(x = To, y = From)) + 
  geom_tile(aes(fill = Residual)) + 
  theme(panel.background = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_text(angle=270,hjust=1))+
  scale_x_discrete(labels=statelabs)+
  scale_y_discrete(labels=statelabs)+
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   rev=TRUE,
                                   p1 = 0.5,p2=0.25, p3 = 1, p4=0.5,
                                   breaks=seq(-50,300, by=100)
  )


# Histogram
g<-p1$Residual

mean<-mean(g)
sd<-sd(g)

h<-hist(g, breaks=30, 
        xlab= 'Residual', 
        main= NULL
        #main ='Normal curve over histogram of residuals'
)
xfit<-seq(min(g), max(g))
yfit<-dnorm(xfit, mean=mean, sd=sd)
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col='blue')

