###########################################################################
## M Likhith Kumar 
## Created: mar 19,2018
###########################################################################

rm(list=ls())

setwd("C:/Users/bunny/Desktop/sem 2/STA 2/codes2")  #setting current work directory

library(ggplot2)
library(GGally)
#install.packages("GGally")

load("C:/Users/bunny/Desktop/sem 2/STA 2/HW3/SwissBankNotes.rdata")

dim(SwissBankNotes)

swiss_notes<-SwissBankNotes
swiss_notes$Y<-NA

names(swiss_notes)
notes1<-swiss_notes[1:100,]
notes2<-swiss_notes[101:200,]

notes1$Y<-1
notes2$Y<-0

notes<-rbind(notes1,notes2)

dim(notes1)
dim(notes2)
##################################################
pr.out<-prcomp(notes1)
df_out <- as.data.frame(pr.out$x)
names(df_out)
#df_out$group<- sapply( strsplit(as.character(row.names(df)), "_"), "[[", 1 )
x11()
plot(df_out,col=c('firebrick'))
#ggpairs(df_out,columns=1:6)
p<-ggplot(df_out,aes(x=PC1,y=PC2))
p<-p+geom_point()
p

#################################################
pr_cf<-prcomp(notes2)
df_cf <- as.data.frame(pr_cf$x)
head(df_cf)
plot(df_cf,col=c('blue'))
ggpairs(df_cf,columns=1:6)
p1<-ggplot(df_cf,aes(x=PC1,y=PC2))
p1<-p+geom_point()
p1

####################################################
pr_tot<-prcomp(SwissBankNotes)
df_tot <- as.data.frame(pr_tot$x)
df_tot$Y = as.factor(as.character(notes$Y))
ggpairs(df_tot,columns=1:6 , ggplot2::aes(colour=Y))
str(notes)


p2<-ggplot(df_tot,aes(x=PC1,y=PC2))
p2<-p2+geom_point(aes(colour=Y))
p2


