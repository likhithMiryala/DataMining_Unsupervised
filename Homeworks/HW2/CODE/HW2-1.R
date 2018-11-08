###########################################################################
## M Likhith Kumar 
## Created: mar 04,2018
###########################################################################

rm(list=ls())

setwd("C:/Users/bunny/Desktop/sem 2/STA 2/codes2")  #setting current work directory

library(ggplot2)
?USArrests

colnames(USArrests)
head(USArrests)

#USArrests1<-setNames(USArrests,c("statename","murder","assault","urbanpop","rape"))


##########  a  #############
cluster1<-hclust(dist(USArrests),method="complete")
plot(cluster1)


#########  b  ##############
clustercut <- cutree(cluster1, 3)

clust_1 <- clustercut[clustercut == 1]
clust_2 <- clustercut[clustercut == 2] 
clust_3 <- clustercut[clustercut == 3]

names(clust_1)
names(clust_2)
names(clust_3)

table(clustercut )

ggplot(USArrests, aes(UrbanPop,Assault, color = clustercut)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustercut)

ggplot(USArrests, aes(Assault,Rape, color = clustercut)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustercut)

######## c ############

sd(scale(USArrests))
scaled_data<-scale(USArrests)
head(scaled_data)
cluster_sd<-hclust(dist(scaled_data),method="complete")
plot(cluster_sd)

######## d ##########
cluster_sd_cut<-cutree(cluster_sd, 3)

table(clustercut,cluster_sd_cut)

#ggplot(scaled_data, aes(Assault,Rape, color = cluster_sd_Cut)) + 
 # geom_point(alpha = 0.4, size = 3.5) + geom_point(col = cluster_sd_Cut)
