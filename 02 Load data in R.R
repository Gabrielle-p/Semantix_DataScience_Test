################################################################################

#Date: 2019-07-11 
#Subject: Semantix Data Science Test
#Data: https://archive.ics.uci.edu/ml/datasets/bank+marketing
#Candidate: Gabrielle Provost

#Script 1
#Load data

################################################################################

###############################################
#Install if needed and load various packages
############################################

#Required for everything
#install.packages('data.table') 
library(data.table)

#Required for Q1A
#install.packages('dplyr')
library('dplyr')

#Required for everything
#install.packages('ggplot2')
library('ggplot2')

#Required for Q1B, Q5, Q6
#install.packages('RColorBrewer')
library('RColorBrewer')

#Required for Q1B
#install.packages('randomForest')
library('randomForest')

#Requires for Q1B
#install.packages('ROCR')
library(ROCR)



############################################################################
#Load data into R (No need to connect to SQL, original data is clean enough)
#############################################################################

#Update the file path with the location of the file on your machine
bank<-read.csv("/Documents/Work/Semantix/01 Data Science test/bank/bank-full.csv", header=TRUE, sep = ";")
bank_DT<-data.table(bank)

#or pull directly using URL

bank_DT <- fread("curl https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip | tar -xf- --to-stdout *bank.csv")
bank<-as.data.frame(bank_DT)

#Check data
head(bank)

summary(bank)
#Based on pday and previous stats, most people were not previously contacted about previous campaigns
#All other stats appear reasonable




