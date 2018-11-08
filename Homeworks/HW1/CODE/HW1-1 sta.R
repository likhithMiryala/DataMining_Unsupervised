###########################################################################
## M Likhith Kumar 
## Created: feb 20,2018
###########################################################################
rm(list==ls())
setwd("Users/bunny/Desktop/sem 2/STA 2/codes2")  #setting current work directory

install.packages('philentropy')
B = matrix(c(4,5,0,5,1,0,3,2,0,3,4,3,1,2,1,0,2,0,1,3,0,4,5,3), ncol=8, nrow=3, byrow=TRUE)
B[B>0] = 1
library(philentropy)
distance(B, method="cosine")

distance(B, method="jaccard")

