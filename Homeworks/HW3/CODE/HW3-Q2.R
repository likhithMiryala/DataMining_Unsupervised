###########################################################################
## M Likhith Kumar 
## Created: mar 19,2018
###########################################################################

rm(list=ls())

setwd("C:/Users/bunny/Desktop/sem 2/STA 2/codes2")  #setting current work directory

library(ggplot2)

load("C:/Users/bunny/Desktop/sem 2/STA 2/HW3/primate.scpulae.rdata")

dim(primate.scapulae)

primate_scap<-data.frame(primate.scapulae)
primate_scap<-primate_scap[-c(10,11)]

cluster_com<-hclust(dist(primate_scap),method="complete")
plot(cluster_com)

cluster_sin<-hclust(dist(primate_scap),method="single")
plot(cluster_sin)

cluster_ave<-hclust(dist(primate_scap),method="average")
plot(cluster_ave)

clustercut_com <- cutree(cluster_com, 5)
clustercut_com_tab<-table(primate.scapulae$classdigit, clustercut_com)

clustercut_sin <- cutree(cluster_sin, 5)
clustercut_sin_tab<-table(primate.scapulae$classdigit, clustercut_sin)

clustercut_ave <- cutree(cluster_ave, 5)
clustercut_ave_tab<-table(primate.scapulae$classdigit, clustercut_ave)

rate_com<-1-sum(diag(clustercut_com_tab))/sum(clustercut_com_tab)
rate_sin<-1-sum(diag(clustercut_sin_tab))/sum(clustercut_sin_tab)
rate_ave<-1-sum(diag(clustercut_ave_tab))/sum(clustercut_ave_tab)

rate_com
rate_sin
rate_ave


############################ b ############################
sum(is.na(primate_scap))

for(i in 1:ncol(primate_scap)){
  primate_scap[is.na(primate_scap[,i]), i] <- mean(primate_scap[,i], na.rm = TRUE)
}

km.out <- kmeans(primate_scap,5)
#str(primate_scap)

1-sum(diag(table(primate.scapulae$classdigit,km.out$cluster)))/sum(table(primate.scapulae$classdigit,km.out$cluster))

