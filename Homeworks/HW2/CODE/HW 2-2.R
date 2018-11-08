###########################################################################
## M Likhith Kumar 
## Created: mar 04,2018
###########################################################################

rm(list=ls())

setwd("C:/Users/bunny/Desktop/sem 2/STA 2/codes2")  #setting current work directory

library(ggplot2)

############# a ############

rand_data<-matrix(rnorm(20*3*50,mean=0,sd=0.001),ncol=50)
dim(rand_data)

#Adding mean shift
rand_data[1:20 ,1]=rand_data[1:20 ,1]+5
rand_data[21:40 ,2]=rand_data[21:40 ,2]-4
rand_data[21:40 ,1]=rand_data[21:40 ,1]+1
rand_data[41:60 ,1]=rand_data[41:60 ,1] -2

lables_true <- c(rep(1,20),rep(2,20),rep(3,20))

#rand_data[lables_true==2,]= rand_data[lables_true==2,] - .7
#rand_data[lables_true==3,]= rand_data[lables_true==3,] + .7

#rand_data[1:20 ,2]=1
#rand_data[21:40 ,1]=2
#rand_data[21:40 ,2]=2
#rand_data[41:60 ,1]=1



########### b ###########

pr.out =prcomp (rand_data )

#cols=function(vec){
# + cols=rainbow(length(unique(vec)))
# + return(cols[as.numeric(as.factor(vec))])
#}

biplot(pr.out,scale=0)

par(mfrow =c(1,2))
plot(pr.out$x [,1:2], col =1:3, pch =12,xlab ="First principal component ",ylab="second principal component")
#plot(pr.out$x[,c(1,3) ], col =1:3, pch =12,xlab ="first principal component",ylab="second principal component") 



############### c ##########
km.out <- kmeans(rand_data,3, nstart = 20)
table(lables_true, km.out$cluster)

######### d #########
km.out2<-kmeans(rand_data,2,nstart=20)
km.out2$cluster

table(lables_true, km.out2$cluster)
plot(rand_data, col =(km.out2$cluster +1) , main="K-Means Clustering Results with K=2", xlab ="", ylab="", pch =20, cex =2)


######### e ########

km.out4<-kmeans(rand_data,4,nstart=20)
km.out4$cluster  

table(lables_true, km.out4$cluster)
plot(rand_data, col =(km.out4$cluster +1) , main="K-Means Clustering Results with K=4", xlab ="", ylab="", pch =20, cex =2)

######### f #########

km.out.pc<-kmeans(pr.out$x [,1:2],3,nstart=20)

table(lables_true, km.out.pc$cluster)
plot(rand_data, col =(km.out.pc$cluster +1) , main="K-Means Clustering Results with PC", xlab ="", ylab="", pch =20, cex =2)

######### g ########

scaled_rand_data<-scale(rand_data)

km.out.scaled<-kmeans(scaled_rand_data,3,nstart=20)
km.out.scaled$cluster  

table(lables_true, km.out.scaled$cluster)
plot(rand_data, col =(km.out.scaled$cluster +1) , main="K-Means Clustering Results with scale", xlab ="", ylab="", pch =20, cex =2)


