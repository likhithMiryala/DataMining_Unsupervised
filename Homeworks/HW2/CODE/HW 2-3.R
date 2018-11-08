###########################################################################
## M Likhith Kumar 
## Created: mar 04,2018
###########################################################################

rm(list=ls())

getwd()
setwd("~/")


setwd("C:/Users/bunny/Desktop/sem 2/STA 2/codes2")  #setting current work directory

library(ggplot2)

########## a ###########

genes_data<-read.csv("C:/Users/bunny/Desktop/sem 2/STA 2/codes2/Ch10Ex11.CSV",header=F)

dim(genes_data)


######### b ##########
dissimilarity <- 1 - cor(genes_data)
distance <- as.dist(dissimilarity)

par(mfrow=c(1,1))

cluster_com<-hclust(distance,method="complete")
plot(cluster_com)

cluster_sin<-hclust(distance,method="single")
plot(cluster_sin)

cluster_ave<-hclust(distance,method="average")
plot(cluster_ave)

########## c ##########
apply(genes_data , 2, mean)
apply(genes_data , 2, var)

pr.out =prcomp (genes_data , scale =TRUE)
names(pr.out)

pr.out$center
pr.out$scale
head(pr.out$rotation)
dim(pr.out$x)

biplot(pr.out,scale=0)

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot (pr.out , scale =0)

pr.var =pr.out$sdev ^2
pr.var

pve=pr.var/sum(pr.var )
pve

