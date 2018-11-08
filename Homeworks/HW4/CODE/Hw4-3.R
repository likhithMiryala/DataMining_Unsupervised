###########################################################################
## M Likhith Kumar 
## Created: April 9, 2018
###########################################################################
setwd("/Users/bunny/Desktop/sem 2/STA 2/HW4")  #setting current work directory

install.packages("gRbase")
install.packages("bnlearn")
install.packages("Rgraphviz")
install.packages("dagitty")

source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

library(dagitty)
library(gRbase)
library(bnlearn)

##################################################
## Specify the DAG
##################################################
g <- list(~A, ~C|A, ~B, ~E|B, ~D|A:B, ~F|A:C:E, ~G|D:E, ~H|F:G )
chestdag <- dagList(g)

if (require(Rgraphviz)) {
  plot(chestdag)
}

##################################################
## Inquire about d-separation
##################################################

######## C and G ############

dSep(as(chestdag, "matrix"), "C", "G", NULL)

####### C and E ############

dSep(as(chestdag, "matrix"), "C", "E",NULL)

####### c, E, evidence G ###

dSep(as(chestdag, "matrix"), "C", "E","G")

######## d ##########

dSep(as(chestdag, "matrix"), "A", "G",c("D","E"))

####### e ###########

dSep(as(chestdag, "matrix"), "A", "G","D")
















