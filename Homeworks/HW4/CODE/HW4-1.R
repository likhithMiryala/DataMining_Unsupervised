###########################################################################
## M Likhith Kumar 
## Created: March 27
###########################################################################
rm(list=ls())
setwd("/Users/bunny/Desktop/sem 2/STA 2/HW4")  #setting current work directory

#remove.packages("gRbase")

library(gRbase)
library(bnlearn)
library(gRain)
#library(RHugin)
library(Rgraphviz)
library(ggm)

?gRbase
data(cad1)
?cad1

cad<-list(~sex, ~smoker|sex, ~suffheartf, ~inherit|smoker, ~hyperchol|suffheartf:smoker, ~cad|inherit:hyperchol)
cad_dag<-dagList(cad)

if (require(Rgraphviz)) {
  plot(cad_dag)
}

########### calculating the factors values from cad1 dataset ###########
sex_fem<-dim(data.frame(which(cad1$Sex=='Female')))
sex_mal<-dim(data.frame(which(cad1$Sex=='Male')))
suff_y<-dim(data.frame(which(cad1$SuffHeartF=='Yes')))
suff_y
suff_n<-dim(data.frame(which(cad1$SuffHeartF=='No')))
suff_n
smok_y<-dim(data.frame(which(cad1$Smoker=='Yes')))
smok_y
smok_n<-dim(data.frame(which(cad1$Smoker=='No')))
smok_n
inher_y<-dim(data.frame(which(cad1$Inherit=='Yes')))
inher_y
inher_n<-dim(data.frame(which(cad1$Inherit=='No')))
inher_n
hyper_y<-dim(data.frame(which(cad1$Inherit=='Yes')))
hyper_y
hyper_n<-dim(data.frame(which(cad1$Inherit=='No')))
hyper_n
cad_y<-dim(data.frame(which(cad1$CAD=='Yes')))
cad_y
cad_n<-dim(data.frame(which(cad1$CAD=='No')))
cad_n

####################### cp tables ################################
?cptable

yn <- c("yes", "no")
sx<-c("male","female")
s <- cptable(~sex, values = c(80, 20), levels =sx)
s.s <- cptable(~smoker|sex, values = c(185, 51, 189, 47), levels = yn )
shf <- cptable(~suffheartf, values = c(29, 71), levels = yn )
i.s <- cptable(~inherit|smoker, values = c(118,108,185,51), levels = yn )
h.s <- ortable(~hyperchol|suffheartf:smoker, levels = yn)
c.ih <- ortable(~cad|inherit:hyperchol, levels = yn)

plist <- compileCPT(list(s, s.s, shf, i.s, h.s,c.ih))
grn1 <- grain(plist)

############### infering conditinal probababilites values ################
plist$sex
plist$smoker
plist$suffheartf
plist$inherit
plist$hyperchol
plist$cad

##################### dseparations for the graph ##############3
dSep(as(cad_dag,"matrix"),'sex', 'smoker',NULL)
dSep(as(cad_dag,"matrix"),'smoker', 'hyperchol',NULL)
dSep(as(cad_dag,"matrix"),'cad', 'suffheartf','smoker')
dSep(as(cad_dag,"matrix"),'inherit','suffheartf',NULL)
dSep(as(cad_dag,"matrix"),'smoker','cad',c("inherit","hyperchol"))

############################### b ###################################

grn1c<-compile(grn1)
summary(grn1c)

grn1cp<-propagate(grn1c)
summary(grn1cp)

###################### previous values ##########################
querygrain(grn1c, nodes = c("suffheartf", "cad"), type = "marginal")
querygrain(grn1c, nodes = c("suffheartf", "cad"), type = "joint")
querygrain(grn1c, nodes = c("suffheartf", "cad"), type = "conditional")

grn1c.find <- setFinding(grn1c, nodes = c("sex", "hyperchol"),  states = c("female", "yes"))
summary(grn1c.find)

############## probability values after considering evidence ###########
querygrain(grn1c.find, nodes = c("suffheartf", "cad"), type="joint")
querygrain(grn1c.find, nodes = c("suffheartf", "cad"), type = "conditional")
querygrain(grn1c.find, nodes = c("suffheartf", "cad"), type = "marginal")
###############################  c  ##################################

sim.find_five <- simulate(grn1c.find, nsim = 5)
head(sim.find_five)

xtabs(~ smoker + cad, data = sim.find_five) / nrow(sim.find_five)

predict(grn1c, response = c("smoker", "cad"), newdata = sim.find_five, predictors = c("sex", "hyperchol", "suffheartf" , "inherit"),type = "class")

predict(grn1c, response = c("smoker", "cad"), newdata = sim.find_five, predictors = c("sex", "hyperchol", "suffheartf" , "inherit"),type = "dist")

############################### d  ###################################
sim.find_500 <- simulate(grn1c, nsim = 500)
save(sim.find_500, file="simulate_500.Rdata")

head(sim.find_500)

xtabs(~ smoker + cad, data=sim.find_500) / nrow(sim.find_500)

predict_500_class<-predict(grn1c, response = c("smoker", "cad"), newdata = sim.find_500, predictors = c("sex", "hyperchol", "suffheartf" , "inherit"),type = "class")

predict_500_dist<-predict(grn1c, response = c("smoker", "cad"), newdata = sim.find_500, predictors = c("sex", "hyperchol", "suffheartf" , "inherit"),type = "dist")


table(predict_500_class$pred$cad, sim.find_500$cad)

table(predict_500_class$pred$smoker, sim.find_500$smoker)






