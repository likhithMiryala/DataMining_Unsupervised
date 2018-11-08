###########################################################################
## M Likhith Kumar 
## Created: mar 19,2018
###########################################################################

rm(list=ls())

setwd("C:/Users/bunny/Desktop/sem 2/STA 2/codes2")  #setting current work directory

install.packages("ElemStatLearn")
install.packages("kohonen")
install.packages("phyclust")
library(ElemStatLearn)
library(kohonen)
library(phyclust)

data(nci)
nci_data<-scale(nci)
####################

km.out_two<-kmeans(nci_data,2)
names(km.out_two)
similar.save.2<-c()
for(i in c(0.3,0.67,1,2,3))
{
  self_organizing_map.2<-som(nci_data,grid=somgrid(2,1,"hexagonal"),radius=i)
  similar.2<-RRand(km.out_two$cluster, self_organizing_map.2$unit.classif)$Rand
  similar.save.2<-c(similar.save.2,similar.2)
}
similar.save.2
plot(c(0.3,0.67,1,2,3),similar.save.2,lty=2,type="b")

km.out_five<-kmeans(nci_data,5)
names(km.out_five)
similar.save.5<-c()
for(i in c(0.3,0.67,1,2,3))
{
  self_organizing_map.5<-som(nci_data,grid=somgrid(5,1,"hexagonal"),radius=i)
  similar.5<-RRand(km.out_five$cluster, self_organizing_map.5$unit.classif)$Rand
  similar.save.5<-c(similar.save.5,similar.5)
}
similar.save.5
plot(c(0.3,0.67,1,2,3),similar.save.5,lty=2,type="b")

km.out_ten<-kmeans(nci_data,10)
names(km.out_ten)
similar.save.10<-c()
for(i in c(0.3,0.67,1,2,3))
{
  self_organizing_map.10<-som(nci_data,grid=somgrid(5,2,"hexagonal"),radius=i)
  similar.10<-RRand(km.out_ten$cluster, self_organizing_map.10$unit.classif)$Rand
  similar.save.10<-c(similar.save.10,similar.10)
}
similar.save.10
plot(c(0.3,0.67,1,2,3),similar.save.10,lty=2,type="b")

km.out_twenty<-kmeans(nci_data,20)
names(km.out_twenty)
similar.save.20<-c()
for(i in c(0.3,0.67,1,2,3))
{
  self_organizing_map.20<-som(nci_data,grid=somgrid(5,4"hexagonal"),radius=i)
  similar.20<-RRand(km.out_twenty$cluster, self_organizing_map.20$unit.classif)$Rand
  similar.save.20<-c(similar.save.20,similar.20)
}
similar.save.20
plot(similar.save.20)
plot(c(0.3,0.67,1,2,3),similar.save.20,lty=2,type="b")

