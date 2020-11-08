#########################################################
## data Mining I HW2
## Subhashchandra Babu Madineni   UBIT = 50373860
## Created on 24th Sept
## Edited: 
#########################################################
rm(list = ls())



#########################################################
# Loading th dataset
#########################################################
setwd("C:/University At Buffalo Fall 2020 Classes/EAS 506-CDA 541Stastical Data Mining/R for STA/HW2")

dataset = read.csv("cereal.csv", sep = ',', header = TRUE, stringsAsFactors = TRUE)
dim(dataset)

#########################################################
# 1)A Removing the unwanted rows
#########################################################

dataset <- dataset[,-1]


#########################################################
# 1)A Splitting the dataset into training and test sets
#########################################################
set.seed(234)
independent_dataset <-sample(2, nrow(dataset),replace = TRUE, prob = c(0.8, 0.2))

train_set <- dataset[independent_dataset == 1,]
test_set <- dataset[independent_dataset == 2,]



#########################################################
# 1)A Fitting the linear model to the dataset
#########################################################


regressor = lm(formula = rating~., data = train_set)
summary(regressor)

#########################################################
# 1)A Calculating the MSE -  Mean squared Error
#########################################################
#install.packages('Metrics')
library(Metrics)

mse(test_set$rating,predict(regressor, newdata = test_set))     # Test_MSE = 9.890283e-14
reg.summary_lm = summary(regressor)



#install.packages('leaps')
library(leaps)

#########################################################
# 1)B Applying The Forward  Subset Selection Method
#########################################################

regfit.fwd <- regsubsets(rating~., data = dataset, nvmax = 14, method = "forward")



#########################################################
# 1)A Analysing the summaries for Forward Selection
#########################################################
reg.summary_fwd = summary(regfit.fwd)
par(mfrow = c(1,1))
plot(regfit.fwd,scale="bic")


#install.packages("ggvis")
library(ggvis)
par(mfrow=c(2,2))
plot(reg.summary_fwd$rss ,xlab="Number of Variables ",ylab="RSS",type="l")

plot(reg.summary_fwd$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")

points(11,reg.summary_fwd$adjr2[11], col="red",cex=2,pch=20)

plot(reg.summary_fwd$cp ,xlab="Number of Variables ",ylab="Cp", type='l')

points(10,reg.summary_fwd$cp [10],col="red",cex=2,pch=20)
plot(reg.summary_fwd$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
points(9,reg.summary_fwd$bic [9],col="red",cex=2,pch=20)


#########################################################
# 1)C  Applying The Exhaustive Subset Selection Method
#########################################################

regfit.exhaustice_subset <- regsubsets(rating~., data = dataset, nvmax = 14, method = "exhaustive")


#########################################################
# 1)A Analysing the summaries for Exhaustive Subset Selection
#########################################################
reg.summary_exhaustive = summary(regfit.exhaustice_subset)
par(mfrow = c(1,1))
plot(regfit.exhaustice_subset,scale="bic")


#install.packages("ggvis")
library(ggvis)
par(mfrow=c(2,2))
plot(reg.summary_exhaustive$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(reg.summary_exhaustive$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
points(11,reg.summary_exhaustive$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary_exhaustive$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
points(10,reg.summary_exhaustive$cp [10],col="red",cex=2,pch=20)
plot(reg.summary_exhaustive$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
points(9,reg.summary_exhaustive$bic [9],col="red",cex=2,pch=20)





