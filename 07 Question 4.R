################################################################################

#Date: 2019-07-11 
#Subject: Semantix Data Science Test
#Data: https://archive.ics.uci.edu/ml/datasets/bank+marketing
#Candidate: Gabrielle Provost

#Script 6
#Question 4
#Raw statistics on campaign success vs poutcome

#Pre-requisite
#Having run script 1 named "02 Load data in R.R"

################################################################################



################################################################################
#Question 4: Is the result of previous campaign revelant to future campaign?
#O resultado da campanha anterior tem relevância na campanha atual?
################################################################################

library(ggplot2)

#Look at the matrix of current campaign success v previous results
table(bank[,c("poutcome",'y')])

#Pull success rate
table(bank[,c("poutcome",'y')])[,2]/rowSums(table(bank[,c("poutcome",'y')]))
#Highly correlated

#Interestingly, the unknown rate is lower than failure. Could this have to do
#with capacity to reach the client in first place?

#Check if they are contact with duration 0
bank_DT[duration==0,,]
#Only 3

#Duration of calls

qplot(bank_DT[,duration,], geom="histogram",col='red') 

#See if there is a correlation between very short calls and poutcome
#Perhaps very short comm. are voicemails or wrong numbers
bank_DT[duration<=20,.N,]
bank$shortcall<-sapply(bank$duration,function(x){if(x<=20) 1 else 0})
table(bank[,c('poutcome','shortcall')])
table(bank[,c('poutcome','shortcall')])[,2]/rowSums(table(bank[,c('poutcome','shortcall')]))
#Not really


#Check statistical significance using T test

failure_set<-sapply(bank_DT[poutcome=='failure',y,],function(x){if(x=='yes') 1 else 0})
unknown_set<-sapply(bank_DT[poutcome=='unknown',y,],function(x){if(x=='yes') 1 else 0})
success_set<-sapply(bank_DT[poutcome=='success',y,],function(x){if(x=='yes') 1 else 0})
other_set<-sapply(bank_DT[poutcome=='other',y,],function(x){if(x=='yes') 1 else 0})

#T test

t_test_results<-lapply(list(success=success_set,failure=failure_set,other=other_set),function(x){t.test(x,unknown_set)})

t_test_summ<-as.data.frame(lapply(t_test_results,function(x){unlist(x[c('statistic','parameter','p.value')])}))

View(t_test_summ)
