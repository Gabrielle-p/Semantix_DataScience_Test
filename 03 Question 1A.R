################################################################################

#Date: 2019-07-11 
#Subject: Semantix Data Science Test
#Data: https://archive.ics.uci.edu/ml/datasets/bank+marketing
#Candidate: Gabrielle Provost

#Script 2
#Question 1 A)
#Raw statistics on loan rates

#Pre-requisite
#Having run script 1 named "02 Load data in R.R"

################################################################################

################################################
#Question 1
################################################

###############################
#Part A: Simple stats first
###############################

#Housing loans by job
Housing_N<-bank_DT[,.(n_housing_loans=sum(sapply(housing,function(x){if(x=='yes'){1}else{0}}))),by=job]

#Personal loans by job
Pers_N<-bank_DT[,.(n_pers_loans=sum(sapply(loan,function(x){if(x=='yes'){1}else{0}}))),by=job]

#Total nb of client by job
Job_N<-bank_DT[,.(n_clients=.N),by=job]

#Loan (either housing or personal), binary choice per customer
Loan_Bin_N<-bank_DT[,.(n_loan_client=sum(mapply(function(x,y){if(x=='yes' | y=='yes'){1}else{0}},housing,loan))),by=job]

#Aggregate all stats
Summary_Job<-data.table(Reduce(function(...) left_join(...), list(Job_N,Housing_N,Pers_N,Loan_Bin_N)))
Summary_Job[,`:=`(
  housing_rate=n_housing_loans/n_clients,
  pers_loan_rate=n_pers_loans/n_clients,
  loan_affinity_bin=n_loan_client/n_clients, 
  overall_loan_rate=(n_pers_loans+n_housing_loans)/n_clients
) 
,]
#Show results, ordered by the probability of a client having either loan (binomial, they either have a loan or not)
View(Summary_Job[order(loan_affinity_bin,decreasing=TRUE)])


