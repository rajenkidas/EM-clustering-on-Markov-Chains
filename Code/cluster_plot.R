#STEP 3

#data to be used: traj, gamma and gamma est
#libraries
library(ggplot2)
library(tidyverse)
library(pheatmap)
library(reshape2) #melt
library(RColorBrewer)
library(rcartocolor)
library(colorspace) #to add different mid point to colour scheme
library(ggpubr) #ggarrange
library(cowplot) #plotgrid
library(expm) #exponential


#list index numbers in cluster k (remember, index numbers are not the user ids)
class.index<-vector("list",no.classes)
for (k in 1:no.classes) {
  class.index[[k]]<-which(gamma[,k]==1)
}

list.trans.per.cluster<-list.mats 

#Stationary distribution

P<-(list.trans.per.cluster[[1]])%^%100
Q<-(list.trans.per.cluster[[2]])%^%100
R<-(list.trans.per.cluster[[3]])%^%100
S<-(list.trans.per.cluster[[4]])%^%100

stat.dis<-data.frame()
stat.dis<-rbind( P[1,], Q[1,], R[1,], S[1,])
#colnames(stat.dis)<-c('1','2','3','4')
rownames(stat.dis)<-c('A','B','C','D')

rownames(stat.dis)<-c('A','B','C','D')
colnames(stat.dis) <- c("G L", "G H", "B L", "B H")

library(reshape2)
df <- melt(stat.dis)  #the function melt reshapes it from wide to long
#df$rowid <- 1:4

library(viridis)
library(hrbrthemes)



levels(df$Var1)<-c('1','2','3','4')
df$Interv<- c('No')

statdist1<-ggplot(df, aes(fill=(factor(Var1)), y=value, x=Var2)) + 
  geom_bar(position="dodge", stat="identity")+
  xlab("State (Mood, Pain)")+ylab("Probability")+
  scale_x_discrete(labels=statelabs)+
  scale_fill_viridis_d(name='Cluster',end=0.8)+
  theme(panel.background = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size=12))
  theme_minimal_hgrid()

statdist2<-ggplot(df, aes(fill=factor(Var1), 
                          y=value, x=Var2)) + 
  geom_bar(stat="identity") +
  xlab(" ")+ylab("Probability")+
  scale_x_discrete(labels=statelabs)+
  facet_wrap(~factor(Var1))+
  scale_fill_viridis_d(name='Cluster',end=0.8, 
                       labels=c('1','2','3','4'))+
  theme(panel.background = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size=12),
        strip.text = element_blank()
        )
#+coord_flip()
ggarrange(statdist2,statdist1, common.legend = TRUE,nrow=2)


Breaks <- seq(0.1, 0.6, length = 50)
statelabs <- c("G L",
               "G H",
               "B L",
               "B H")


for(i in 1:no.classes){
  rownames(list.trans.per.cluster[[i]])<-statelabs
  colnames(list.trans.per.cluster[[i]])<-statelabs}



c1<-melt(list.trans.per.cluster[[1]])
colnames(c1)<-c("From", "To", "Probability")


c2<-melt(list.trans.per.cluster[[2]])
colnames(c2)<-c("From", "To", "Probability")


c3<-melt(list.trans.per.cluster[[3]])
colnames(c3)<-c("From", "To", "Probability")



c4<-melt(list.trans.per.cluster[[4]])
colnames(c4)<-c("From", "To", "Probability")


c1$clust<- c("Cluster 1")
c2$clust<- c("Cluster 2")
c3$clust<- c("Cluster 3")
c4$clust<- c("Cluster 4")
clusters_combined<-rbind(c1, c2, c3, c4)


#plot (change the cluster number in the square brackets in first line)
#NOTE: the axes of to and from in the plots. (For ease in reading.)


ggplot(clusters_combined, aes(x = To, y = From)) + 
  geom_tile(aes(fill = Probability)) + 
  ylab('From State (Mood, Pain)') +
  xlab('To State (Mood, Pain)') +
  theme(panel.background = element_blank(), 
        axis.title.x.top = element_text("To"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size=14),
        axis.text = element_text(size=14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.background = element_rect(colour="red", fill="white"),
        strip.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5)
        )+
  #coord_equal() +
  scale_x_discrete(labels=statelabs) +
  scale_y_discrete(labels=statelabs) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0.25,
                                   rev=TRUE,
                                   p1 = 0.5,p2=0.25, p3 = 1, p4=0.5,
                                   breaks=seq(0.25, 0.6, by=0.15)
  ) +
  facet_wrap(~clust) #+ggtitle(label = "Transition of state (Mood, Pain)") + 



