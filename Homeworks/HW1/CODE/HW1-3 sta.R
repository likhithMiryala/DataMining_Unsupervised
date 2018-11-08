###########################################################################
## M Likhith Kumar 
## Created: feb 20,2018
###########################################################################
rm(list=ls())
setwd("Users/bunny/Desktop/sem 2/STA 2/codes2")  #setting current work directory

demographic = c("Sex", "Martial_Status", "Age", "Education", "Occupation", "Income", "Years_In_BayArea", "Dual_Incomes", "Numbers_in_Household", 
                "Number_of_Children", "Householder_Status", "Type_of_Home", "Ethinic_Classification", "Language_in_Home")
N = 9409
Sex = sample(c(1,2), N,replace = T)
Martial_Status = sample(seq(1,5), N, replace = T)
Age = sample(seq(1,7), N, replace = T)
Education = sample(seq(1,6), N, replace = T)
Occupation = sample(seq(1,9), N, replace = T)
Income = sample(seq(1,9), N, replace = T)
Years_In_BayArea = sample(seq(1,5), N, replace = T)
Dual_Incomes = sample(seq(1,3), N, replace = T)
Numbers_in_Household = sample(seq(1,9), N, replace = T)
Number_of_Children = sample(seq(1,9), N, replace = T)
Householder_Status = sample(seq(1,3), N, replace = T)
Type_of_Home = sample(seq(1,5), N, replace = T)
Ethinic_Classification = sample(seq(1,8), N, replace = T) 
Language_in_Home = sample(seq(1,3), N, replace = T)

train_sample = data.frame(Sex, Martial_Status, Age, Education, Occupation, Income, Years_In_BayArea, Dual_Incomes, Numbers_in_Household, 
                             Number_of_Children, Householder_Status, Type_of_Home, Ethinic_Classification, Language_in_Home)
names(train_sample) = demographic
train_sample$target = 1
rm(Sex, Martial_Status, Age, Education, Occupation, Income, Years_In_BayArea, Dual_Incomes, Number_of_Children, Numbers_in_Household, 
   Householder_Status, Type_of_Home, Ethinic_Classification, Language_in_Home)

refer_sample = train_sample
for(i in 1:ncol(refer_sample)){
  refer_sample[,i] = sample(refer_sample[,i], nrow(refer_sample), replace = F)
}
refer_sample$target = 0
total_data = rbind(refer_sample, train_sample); rm(refer_sample, train_sample)
# few cols are categorical. Changing them. 
total_data$Sex = as.factor(as.character(total_data$Sex))
total_data$Martial_Status = as.factor(as.character(total_data$Martial_Status))
total_data$Occupation = as.factor(as.character(total_data$Occupation))
total_data$Dual_Incomes = as.factor(as.character(total_data$Dual_Incomes))
total_data$Householder_Status = as.factor(as.character(total_data$Householder_Status))
total_data$Type_of_Home = as.factor(as.character(total_data$Type_of_Home))
total_data$Ethinic_Classification = as.factor(as.character(total_data$Ethinic_Classification))
total_data$Language_in_Home = as.factor(as.character(total_data$Language_in_Home))

library(rpart)
library(rattle)
total_data$target = as.factor(as.character(total_data$target))
model = rpart(target~., total_data)
summary(model)
pred = predict(model, total_data[,-c(15)])
pred


