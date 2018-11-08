###########################################################################
## M Likhith Kumar 
## Created: feb 20, 2012
###########################################################################
rm(list=ls())

setwd("Users/bunny/Desktop/sem 2/STA 2/codes2")  #setting current work directory

install.packages("arules")
install.packages("ElemStatLearn")
install.packages("MASS")

library(arules)
library(ElemStatLearn)
library(MASS)

?Boston

data(Boston)
head(Boston)
summary(Boston)

################### a ###################

hist(Boston$crim)
hist(Boston$zn)
hist(Boston$indus)
hist(Boston$chas)
hist(Boston$nox)
hist(Boston$rm)
hist(Boston$age)
hist(Boston$dis)
hist(Boston$rad)
hist(Boston$tax)
hist(Boston$ptratio)
hist(Boston$black)
hist(Boston$lstat)
hist(Boston$medv)


Boston[["crim"]] <- ordered(cut(Boston[["crim"]], c(0,40,100)), labels = c("highc", "lowc"))
Boston$crim

Boston[["zn"]] <- ordered(cut(Boston[["zn"]], c(-1,15,101)), labels = c("expen", "cheap"))

Boston[["indus"]] <- ordered(cut(Boston[["indus"]], c(0,12,30)), labels = c("lowp", "highp"))

Boston[["chas"]] <- ordered(cut(Boston[["chas"]], c(-1,0.5,1.1)), labels = c("bound", "nbound"))

Boston[["nox"]] <- ordered(cut(Boston[["nox"]], c(0,0.5,0.9)), labels = c("lowc", "highc"))

Boston[["rm"]] <- ordered(cut(Boston[["rm"]], c(3,6.5,9)), labels = c("lowr", "highr"))

Boston[["age"]] <- ordered(cut(Boston[["age"]], c(0,25,100)), labels = c("Young", "old"))
summary(Boston)

Boston[["dis"]] <- ordered(cut(Boston[["dis"]], c(1,5,13)), labels = c("near", "far"))

Boston[["rad"]] <- ordered(cut(Boston[["rad"]], c(0,10,25)), labels = c("reach", "unreach"))

Boston[["tax"]] <- ordered(cut(Boston[["tax"]], c(180,400,720)), labels = c("full", "low"))
?Boston

Boston[["ptratio"]] <- ordered(cut(Boston[["ptratio"]], c(10,17,23)), labels = c("low", "high"))

Boston[["black"]] <- ordered(cut(Boston[["black"]], c(0,350,400)), labels = c("lessB", "highB"))

Boston[["lstat"]] <- ordered(cut(Boston[["lstat"]], c(0,16,40)), labels = c("dense", "few"))

Boston[["medv"]] <- ordered(cut(Boston[["medv"]], c(3,25,50)), labels = c("highval", "lowval"))

?transactionInfo

Bostonmat <- as(Boston, "transactions")
summary(Bostonmat)

########## b #############

x11()
itemFrequencyPlot(Bostonmat, support = 0.02, cex.names = 0.8)

rules <- apriori(Bostonmat, parameter = list(support = 0.001, confidence = 0.6))

######### C and D #############

summary(rules)
rulescrimelow <- subset(rules, subset = rhs  %in%  "crim=lowc")
inspect(rulescrimelow)
rulescrimehigh <- subset(rules, subset = rhs  %in%  "dis=near")
inspect(rulescrimehigh)

rulescrimelow
rulescrimehigh

inspect(head(sort(rulescrimelow, by = "confidence"), n = 3))
inspect(head(sort(rulescrimehigh, by = "confidence"), n = 3))

inspect(head(sort(rulescrimelow, by = "lift"), n = 3))
inspect(head(sort(rulescrimehigh, by = "lift"), n = 3))


#regression model. 
subset = data.frame(lapply(subset, function(x) as.numeric(as.character(x))))
model = lm(ptratio ~ ., subset)
summary(model)
#####
