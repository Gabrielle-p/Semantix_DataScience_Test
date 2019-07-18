################################################################################

#Date: 2019-07-11 
#Subject: Semantix Data Science Test
#Data: https://archive.ics.uci.edu/ml/datasets/bank+marketing
#Candidate: Gabrielle Provost

#Script 3
#Question 1 B)
#Multi-variable model on loan rate

#Pre-requisite
#Having run script 1 named "02 Load data in R.R"

################################################################################

########################################################################
#Part B: Multi-variable model; which jobs drive people to take up
#          a loan once we account for other variables
#######################################################################

#library(ggplot2)

#Variables that will be used are : Age, Job, Marital, Education, Balance

#Default is left out given that is highly correlated with a loan and refinancing it in the future, it is likely be self-fulling

table(bank[,c('default','housing')])
table(bank[,c('default','housing')])/rowSums(table(bank[,c('default','housing')]))
#Little different in housing

table(bank[,c('default','loan')])
table(bank[,c('default','loan')])/rowSums(table(bank[,c('default','loan')]))
#Significant difference in personal loan

#Default will be left out regardless to hinge on the safer side




###################
#Arrange variables
###################

#Create groups for age and also a capped version (Refer to one-way plot RE why)
hist(bank$age)
qplot(bank$age, geom='histogram')
#little exposure before 20 and after 60

bank$age_group<-sapply(bank$age, function(x) {
  if (x<=25) 25
  else if (x>60) 65
  else ceiling(x/5)*5
})

hist(bank$age_group)  
qplot(bank$age_group, geom='histogram')

bank$age_capped<-sapply(bank$age, function(x) {if (x>=70) 70 else if (x<18) 18 else x})
hist(bank$age_capped) 

#For balance, we will take the log and excludes observation with balance <0 when training model (relation too strong to response, self-fulnissness is suspected)

hist(bank$balance)

sum(bank$balance<0)/nrow(bank) #8% of obs.
sum(bank$balance<10)/nrow(bank) #18% of obs.
sum(bank$balance>0 & bank$balance<10)/nrow(bank) #2.3% of obs.
#minimum bound set at 10 to avoid -inf

bank$balance_log<-sapply(bank$balance,function(x){
  if (x<=10) log(10)
  else log(x)})

hist(bank$balance_log)

#Bank balance log grouped
bank$balance_log_grouped<-floor(bank$balance_log)
hist(bank$balance_log_grouped)

#Create a binomial version of housing and loan to ease computing

bank$housing_bin<-sapply(bank$housing,function(x){if(x=='yes') 1 else 0})

bank$loan_bin<-sapply(bank$loan,function(x){if(x=='yes') 1 else 0})



#########################################
#Create proper one-way plot to see the correlation between loan rates and each variable
#########################################

f_one_way_plot_data <- function(factor,response,data) { 
  factor_values<-unique(data[,factor])
  
  if (length(factor_values)<30)
  {
    factor_values<-factor_values[order(factor_values)]
  } else {
    factor_values<-seq(from=min(factor_values, na.rm=TRUE), to=max(factor_values, na.rm=TRUE), by=(max(factor_values, na.rm=TRUE)-min(factor_values, na.rm=TRUE))/20)
  }
  
  y<-c(0,factor_values)
  y<-y[-length(y)]
  
  f_one_way_plot<- function(x,y,factor) {
    if (is.factor(data[,factor])) {
      mean(data[which(data[,factor]==x),response], na.rm=TRUE)
    } else{
      mean(data[which(data[,factor]<=x & data[,factor]>y),response], na.rm=TRUE)
    }
  }
  
  one_way_plot_y<-mapply(f_one_way_plot,factor_values,y,factor)
  
  data.frame(x_values=factor_values, y_values=one_way_plot_y)
}



##############################
#One way plot of housing rate in function of all other variables
##############################

factor<-'age'
one_way_plot_data<-f_one_way_plot_data(factor,'housing_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Housing Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Housing Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))

#second degree relation with mortgage
#impact plateaus around 70, consider capping values?


factor<-'age_group'
one_way_plot_data<-f_one_way_plot_data(factor,'housing_bin', bank)
#plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Housing Rate')
#axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Housing Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'balance_log'
one_way_plot_data<-f_one_way_plot_data(factor, 'housing_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Housing Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)
#Decreasing, to be more precise, we would need to create different bands, relation stronger in higher values
#Test second degree in glm

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Housing Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'job'
one_way_plot_data<-f_one_way_plot_data(factor,'housing_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Housing Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)
#Seem significant, unknown is very low and probably self fulfilling. A bank wouldn't give a loan without
#knowing employment details and this data appears to be most recent info at the time data was pulled.
#Should be removed.

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Housing Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))



factor<-'marital'
one_way_plot_data<-f_one_way_plot_data(factor, 'housing_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Housing Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)
#Lower for single, highest for married, makes sense.

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Housing Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'education'
one_way_plot_data<-f_one_way_plot_data(factor, 'housing_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Housing Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)
#Secondary higher; not obvious why. Perhaps wealthy enough to buy a house, but tertiary are 
#wealthier and hence do not need a loan to buy a property?

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Housing Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))




#############################
#One way plot on loan rate
#############################

factor<-'age'
one_way_plot_data<-f_one_way_plot_data(factor,'loan_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Personal Loan Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)
#Correlation not as strong as for mortgage. 

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Pers. Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'age_group'
one_way_plot_data<-f_one_way_plot_data(factor,'loan_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Personal Loan Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)
#Correlation not as strong as for mortgage. 

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Pers. Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'age_capped'
one_way_plot_data<-f_one_way_plot_data(factor,'loan_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Personal Loan Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)
#Correlation not as strong as for mortgage. 

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Pers. Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'balance_log'
one_way_plot_data<-f_one_way_plot_data(factor, 'loan_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Personal Loan Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)
#strong decreasing correlation
#First degree only needed for this model

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Pers. Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'balance_log_grouped'
one_way_plot_data<-f_one_way_plot_data(factor, 'loan_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Personal Loan Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Pers. Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'job'
one_way_plot_data<-f_one_way_plot_data(factor,'loan_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Personal Loan Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)
#Strong correlation, unknown is again very low and likely to be self-fulfilling

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Pers. Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'marital'
one_way_plot_data<-f_one_way_plot_data(factor, 'loan_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Personal Loan Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)
#Divorced is the higest, sensible. Single is the lowest, less obvious but potential correlation with age.

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Pers. Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'education'
one_way_plot_data<-f_one_way_plot_data(factor, 'loan_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Personal Loan Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)
#Secondary still the highest. Not obvious why, potential correlation with jobs which is more likely to have a causation effect on the variable.

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Pers. Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))




###################################################
###################################################

#Logistics regression will be run first despite limited data cleansing/banding, etc.
#No interactions will be considered
#Easy interpretation of coefficients

###################################################
###################################################


#Randomize train vs test observations. Remove self-fulfilling obs.
set.seed(19591001)
rand_idx<-runif(nrow(bank_DT[balance>=0 & job!='unknown' ,,]),0,1)
train_idx<-which(rand_idx<=0.9)
test_idx<-which(rand_idx>0.9)


#Removing bank balance below zero as it seems self-fulling with loan and unknown employment
train_data_glm<-bank[bank$balance>=0 & bank$job!='unknown',][train_idx,]
train_data_glm$housing<-as.factor(train_data_glm$housing)
train_data_glm$housing_bin<-as.factor(train_data_glm$housing_bin)
train_data_glm$loan<-as.factor(train_data_glm$loan)
train_data_glm$loan_bin<-as.factor(train_data_glm$loan_bin)

#Test data
test_data_glm<-bank[bank$balance>=0 & bank$job!='unknown',][test_idx,]
test_data_glm$housing<-as.factor(test_data_glm$housing)
test_data_glm$housing_bin<-as.factor(test_data_glm$housing_bin)
test_data_glm$loan<-as.factor(test_data_glm$loan)
test_data_glm$loan_bin<-as.factor(test_data_glm$loan_bin)

#All eligible data
bank_cleansed<-bank[bank$balance>=0 & bank$job!='unknown',]

#Housing model

#Few options were tested, we will choose the age_capped variable to allow granularity where there is more exposure but avoid decreasing the odds of over 70 without further evidence.
#We also accept the second degree on balance, behaviour seems more representative

housing_logistic<-glm(housing_bin~age_capped+I(age_capped^2)+job+marital+education+balance_log+I(balance_log^2),family=binomial(link='logit'),data=train_data_glm)

summary(housing_logistic)
anova(housing_logistic, test="Chisq")

#Most variables significant, overall model not very predictive however

#Check curve for age and balance
#age
curve(0.1277943*x-0.0019477*x^2-2.0940310,18,70)
#Reasonable

#Balance
curve(0.3359755*x-0.0281415*x^2-2.0940310,2,10)
#Reasonable
View(summary(housing_logistic)$coefficients)


#Personal loan model
loan_logistic<-glm(loan_bin~age_capped+I(age_capped^2)+job+marital+education+balance_log,family=binomial(link='logit'),data=train_data_glm)
summary(loan_logistic)
anova(loan_logistic, test="Chisq")
#Number of significant variable, model performance even weaker than housing (although expected given the lower loan rate)

#Check curve for age and balance
#age
curve(0.0325453*x-0.0004706*x^2-1.5544109,18,70)
#Reasonable

#Balance
curve(-0.0976934*x-2.0940310,2,10)
#Reasonable
View(summary(loan_logistic)$coefficients)


#############################
#Check accuracy of test set
#############################

## Predicting Test Data
housing_logistics_pred <- predict(housing_logistic,newdata=test_data_glm,type='response')
housing_logistics_pred_bin<-ifelse(housing_logistics_pred > 0.5,1,0)

#confusion matrix
conf_housing<-matrix(numeric(4),2,2)
colnames(conf_housing)<-c('Response_1','Response_0')
rownames(conf_housing)<-c('Pred_1','Pred_0')

#True Positive
conf_housing[1,1]<-sum(housing_logistics_pred_bin==1&test_data_glm$housing_bin==1)
#False Positive
conf_housing[1,2]<-sum(housing_logistics_pred_bin==1&test_data_glm$housing_bin==0)
#True negative
conf_housing[2,2]<-sum(housing_logistics_pred_bin==0&test_data_glm$housing_bin==0)
#False negative
conf_housing[2,1]<-sum(housing_logistics_pred_bin==0&test_data_glm$housing_bin==1)
#Check matrix
conf_housing


loan_logistics_pred <- predict(loan_logistic,newdata=test_data_glm,type='response')
#Confusion matrix would be useless, loan rate is too low

## ROC Curve and calculating the area under the curve(AUC)
#install.packages('ROCR')
library(ROCR)

#housing
ROCRpred <- prediction(housing_logistics_pred, test_data_glm$housing_bin)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))
performance(ROCRpred, measure = "auc") #69%

#Pers Loan
ROCRpred <- prediction(loan_logistics_pred, test_data_glm$loan_bin)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))
performance(ROCRpred, measure = "auc") #61%
#Sampling adjustment for rare events would be needed to improve performance on this model

###############################
#Pull odds multiplier charts
###############################

#Housing

housing_logis_coeff<-summary(housing_logistic)$coefficients
job_coeff<-housing_logis_coeff[grepl('job',rownames(housing_logis_coeff)),'Estimate']
job_coeff<-c(job_coeff, setNames(0,'jobadmin.'))

mean_job_coeff<-sum(job_coeff*sapply(gsub("job", "", names(job_coeff)),function(x){sum(bank_cleansed$job==x)}))/nrow(bank_cleansed)


#Put in a data.frame
job_coeff_adj<-data.frame(job=gsub("job", "", names(job_coeff)),mult=exp(job_coeff-mean_job_coeff))
job_coeff_adj$job<-factor(job_coeff_adj$job, levels=job_coeff_adj$job[order(job_coeff_adj$mult)])

#Plot job multiplier vs the average person
Multiplier_plot_housing_logistic_job<-
  ggplot(data=job_coeff_adj)+
  geom_bar(aes(x=job,y=mult-1,fill=mult), stat='identity',show.legend=FALSE)+
  scale_y_continuous(labels =function(x){paste0(round(x+1,1),'x')})+
  theme_bw()+
  theme(text = element_text(size=20), axis.title.y = element_blank())+
  ylab('Multiplier on odds to have a mortgage') +
  coord_flip()+
  scale_fill_distiller(palette = "YlOrRd", direction=1) 

#Check the plot
Multiplier_plot_housing_logistic_job


#Pers Loan

loan_logis_coeff<-summary(loan_logistic)$coefficients
job_coeff_loan<-loan_logis_coeff[grepl('job',rownames(loan_logis_coeff)),'Estimate']
job_coeff_loan<-c(job_coeff_loan, setNames(0,'jobadmin.'))

mean_job_coeff_loan<-sum(job_coeff_loan*sapply(gsub("job", "", names(job_coeff_loan)),function(x){sum(bank_cleansed$job==x)}))/nrow(bank_cleansed)


#Put in a data.frame
job_coeff_adj_loan<-data.frame(job=gsub("job", "", names(job_coeff_loan)),mult=exp(job_coeff_loan-mean_job_coeff_loan))
job_coeff_adj_loan$job<-factor(job_coeff_adj_loan$job, levels=job_coeff_adj_loan$job[order(job_coeff_adj_loan$mult)])

#Plot job multiplier vs the average person
Multiplier_plot_loan_logistic_job<-
  ggplot(data=job_coeff_adj_loan)+
  geom_bar(aes(x=job,y=mult-1,fill=mult), stat='identity',show.legend=FALSE)+
  scale_y_continuous(labels =function(x){paste0(round(x+1,1),'x')})+
  theme_bw()+
  theme(text = element_text(size=20), axis.title.y = element_blank())+
  ylab('Multiplier on odds to have a pers. loan') +
  coord_flip()+
  scale_fill_distiller(palette = "YlOrRd", direction=1) 

#Check the plot 
Multiplier_plot_loan_logistic_job

##########################################
#End of results needed for presentation
#########################################


######################################################################
#####################################################################
#Random forest 

#These results aren't used in the presentation but were run 
#to compare the logistics model results

####################################################################
#####################################################################


#install.packages("randomForest")
library(randomForest)


###################################################

#Data issues observed on personal loan model 
#to do with young people <25 and retirees

#those are somehow picked up as very likely
#to take up loan but data error is assumed

####################################################

#Issue on Personal Loan model with Retirees
#Lots of young retirees with unexpected behaviour
#50% of retiree <60
bank_DT[job=='retired',.N,floor(age/5)*5][order(floor)]

#Loan rate super high for yound retirees
factor='age'
one_way_plot_data<-f_one_way_plot_data(factor,'loan_bin',bank_cleansed[bank_cleansed$job=='retired',])
ggplot(data=one_way_plot_data)+
   geom_bar(aes(x_values,y_values), stat='identity')+
   xlab(factor)+
   ylab('Pers. Loan Rate')+
   scale_y_continuous(labels = scales::percent) +
   theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


#Due to unexpected retired behaviour, a new variable will be create.
#Many retirees <60, 50% incl. people in their 20s...
#Their behaviour is different and therefore they will be split out for pers. loan

bank_cleansed$job_new<-mapply(function(x,y){
  if (x=='retired' & y<40) 'Retired under 40' else as.character(x)
}, bank_cleansed$job, bank_cleansed$age)

bank_cleansed$job_new <- as.factor(bank_cleansed$job_new)

#Floor and ceil age 
bank_cleansed$age_group2<-sapply(bank_cleansed$age, function(x) { if(x<25) 25 else if (x>65) 65 else x })


#Last try, retiree under 40 removed
#bank_cleansed<-bank_cleansed[-which(bank_cleansed$job_new=='Retired under 40'),]

#After a few test, the strange behaviour cannot be removed, logistic regression will
#be used for personal loan

#RF overfits the behaviour for Personal loan

######################################################################

#Train/test index
set.seed(19591001)
random_idx<-runif(nrow(bank_cleansed),0,1)
train<-which(random_idx<=0.9)
test<-which(random_idx>0.9)

#Train models

set.seed(20180206)

housing_predictors<-c('age','job','marital','education','balance_log')
RF_Housing<-randomForest(x=bank_cleansed[train,housing_predictors],y=bank_cleansed[train,'housing'],xtest=bank_cleansed[test,housing_predictors],ytest=bank_cleansed[test,'housing'], keep.forest=TRUE)

set.seed(20180206)
predictors_loan<-c('age','job','marital','education','balance_log')
RF_Loan<-randomForest(x=bank_cleansed[train,predictors_loan],y=bank_cleansed[train,'loan'],xtest=bank_cleansed[test,predictors_loan],ytest=bank_cleansed[test,'loan'], keep.forest=TRUE, nodesize=100)




######################################

#Set up Partial plots code

########################################

#Take a random sample of 10,000 rows
set.seed(176198365)
pplot_sample_position<-sample(1:nrow(bank_cleansed),10000)




#Create function

f_pplot <- function(pplot_factor,model,data) { 
  factor_values<-unique(data[,pplot_factor])

  if (length(factor_values)<30) {
    factor_values<-factor_values[order(factor_values)]
  } else{
    factor_values<-seq(from=min(factor_values), to=max(factor_values), by=(max(factor_values)-min(factor_values))/20)
  }
  
  
  f_pplot_factor<- function(x,factor) {
    pplot_sample_data[,factor]<-x
    pred<-predict(model,pplot_sample_data,'prob')
    mean(pred[,'yes'])
  }
  
  pplot_factor_y<-sapply(factor_values, f_pplot_factor, pplot_factor)
  
  data.frame(x_values=factor_values, y_values=pplot_factor_y)
}




###############################

#Partial plot on housing loan

###############################

pplot_sample_data<-bank_cleansed[pplot_sample_position, housing_predictors]

#What is the benchmark housing_rate?

sum(bank_cleansed[,'housing_bin']==1)/nrow(bank_cleansed)
#Response 0.543427

factor<-'balance_log'
pplot_data<-f_pplot(factor, RF_Housing, bank_cleansed)
plot(pplot_data, type="h", xaxt="n", main=paste0('Partial plot ', factor), xlab=factor, ylab='Housing Rate')
axis(1, at=pplot_data$x_values, labels=pplot_data$x_values)
#View(pplot_data)

ggplot(data=pplot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Mortgage Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'marital'
pplot_data<-f_pplot(factor, RF_Housing, bank_cleansed)
plot(pplot_data, type="h", xaxt="n", main=paste0('Partial plot ', factor), xlab=factor, ylab='Casual count')
axis(1, at=pplot_data$x_values, labels=pplot_data$x_values)
#View(pplot_data)

ggplot(data=pplot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Mortgage Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))

factor<-'age'
pplot_data<-f_pplot(factor, RF_Housing, bank_cleansed)
plot(pplot_data, type="h", xaxt="n", main=paste0('Partial plot ', factor), xlab=factor, ylab='Casual count')
axis(1, at=pplot_data$x_values, labels=pplot_data$x_values)
#View(pplot_data)

ggplot(data=pplot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Mortgage Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))

factor<-'job'
pplot_data<-f_pplot(factor, RF_Housing, bank_cleansed)
plot(pplot_data, type="h", xaxt="n", main=paste0('Partial plot ', factor), xlab=factor, ylab='Casual count')
axis(1, at=pplot_data$x_values, labels=pplot_data$x_values)
View(pplot_data)

ggplot(data=pplot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Mortgage Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'education'
pplot_data<-f_pplot(factor, RF_Housing, bank_cleansed)
plot(pplot_data, type="h", xaxt="n", main=paste0('Partial plot ', factor), xlab=factor, ylab='Casual count')
axis(1, at=pplot_data$x_values, labels=pplot_data$x_values)
#View(pplot_data)

ggplot(data=pplot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Mortgage Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))

#########################################
#Housing Loan model from RF looks robust

##########################################




###########################
#Partial plot on pers loan

############################

pplot_sample_data<-bank_cleansed[pplot_sample_position, predictors_loan]



#What is the benchmark Personal Loan rate?
sum(bank_cleansed$loan_bin==1)/nrow(bank_cleansed)
#Response 0.1465222



factor<-'balance_log'
pplot_data<-f_pplot(factor, RF_Loan, bank_cleansed)
plot(pplot_data, type="h", xaxt="n", main=paste0('Partial plot ', factor), xlab=factor, ylab='Personal Loan Rate')
axis(1, at=pplot_data$x_values, labels=pplot_data$x_values)
View(pplot_data)
#Looks overfit, especially on large values

ggplot(data=pplot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Pers. Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'marital'
pplot_data<-f_pplot(factor, RF_Loan, bank_cleansed)
plot(pplot_data, type="h", xaxt="n", main=paste0('Partial plot ', factor), xlab=factor, ylab='Personal Loan Rate')
axis(1, at=pplot_data$x_values, labels=pplot_data$x_values)
#View(pplot_data)

ggplot(data=pplot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Pers. Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))


factor<-'age'
pplot_data<-f_pplot(factor, RF_Loan,bank_cleansed)
plot(pplot_data, type="h", xaxt="n", main=paste0('Partial plot ', factor), xlab=factor, ylab='Personal Loan Rate')
axis(1, at=pplot_data$x_values, labels=pplot_data$x_values)
#View(pplot_data)

ggplot(data=pplot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Pers. Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))
#!!! Very high loan rate on young age looks wrong



factor<-'job'
pplot_data<-f_pplot(factor, RF_Loan,bank_cleansed)

ggplot(data=pplot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))

#!!! Retirees too high, looks to be picking up on outliers
View(pplot_data)



factor<-'education'
pplot_data<-f_pplot(factor, RF_Loan, bank_cleansed)
plot(pplot_data, type="h", xaxt="n", main=paste0('Partial plot ', factor), xlab=factor, ylab='Personal Loan Rate')
axis(1, at=pplot_data$x_values, labels=pplot_data$x_values)
View(pplot_data)

ggplot(data=pplot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))

###########################
# RF on personal loan is deemed overfit, some data inconsistensies are picked up on
#Model WILL NOT BE USED 
##########################




##############################
#Trouble shooting, one-way plot on retired only and personal_loan rate
#############################


#####
#One way plot on loan rate
#####



trouble_shoot_data<-bank[bank$job=='retired',]

factor<-'age'
one_way_plot_data<-f_one_way_plot_data(factor,'loan_bin', trouble_shoot_data)
 
ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Personal Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20))


#Why are there some retired 20 years old? This could be the problem

hist(trouble_shoot_data$age)
qplot(trouble_shoot_data$age, geom="histogram") 

View(bank_DT[job=='retired',.N,age][order(age)])




factor<-'balance_log'
one_way_plot_data<-f_one_way_plot_data(factor, 'loan_bin',trouble_shoot_data)
ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Personal Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20))




factor<-'marital'
one_way_plot_data<-f_one_way_plot_data(factor, 'loan_bin',trouble_shoot_data)
ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Personal Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20))




factor<-'education'
one_way_plot_data<-f_one_way_plot_data(factor, 'loan_bin',trouble_shoot_data)
ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('Personal Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20))




