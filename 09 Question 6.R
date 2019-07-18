################################################################################

#Date: 2019-07-11 
#Subject: Semantix Data Science Test
#Data: https://archive.ics.uci.edu/ml/datasets/bank+marketing
#Candidate: Gabrielle Provost

#Script 8
#Question 6
#Statistics on default rates

#Pre-requisite
#Having run script 1 named "02 Load data in R.R"

################################################################################


####################################

#Question 6
#Quais são as características mais proeminentes de um cliente que possua
#empréstimo imobiliário?


#Profile of clients with mortgage

#pull relevant output from Question 2B

######################################

##########
# Age
##########


bank_DT[,age_group:=sapply(age, function(x) {
  if (x<=25) 25
  else if (x>60) 65
  else ceiling(x/5)*5
})]

#Age
#All clients
age_dist_all<-bank_DT[,.(All_Prop=.N/nrow(bank_DT)),age_group][order(age_group)]

#Mortgage
age_dist_housing<-bank_DT[housing=='yes',.(Housing_Prop=.N/nrow(bank_DT[housing=='yes',,])),age_group][order(age_group)]

H_Age_Summ<-merge(age_dist_all,age_dist_housing)[,.(age_group,Housing_Prop,All_Prop,Affinity=Housing_Prop/All_Prop),]
H_Age_Summ[,Affinity_Format:=paste0(round(Affinity,1),'x')]

#Plot
ggplot(data=H_Age_Summ)+
  geom_bar(aes(x=age_group,y=Housing_Prop, fill=Housing_Prop),stat='identity', show.legend = FALSE)+
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks=seq(25,65,5))+
  labs(x='Age',y='Proportion of clients (with mortgage)')+
  theme(text = element_text(size=20),panel.background = element_rect(fill='white',colour='white'),
        axis.title.y = element_blank()) +  
  scale_fill_distiller(palette = "YlOrRd", direction=1) +
  geom_text(aes(y=Housing_Prop+0.01,x=age_group, label=Affinity_Format),size=5)




#Balance
qplot(bank$balance,geom='histogram')


bank_DT[,balance_group:=sapply(balance, function(x){
  if(x<0) 'Under 0' else
    if(x==0) '0' else
      if(x>0 & x<100) '0-100' else
        if(x>=100 & x<250) '100-250' else
          if(x>=250 & x<500) '250-500' else
            if(x>=500 & x<1000) '500-1,000' else
              if(x>=1000 & x<2000) '1,000-2,000' else
                if(x>=2000 & x<5000) '2,000-5,000' else
                  '5,000 +'
})]

bank_DT[,balance_group:=factor(balance_group, levels = c('Under 0', '0' ,'0-100', '100-250', '250-500','500-1,000','1,000-2,000','2,000-5,000','5,000 +'))]




balance_dist_all<-bank_DT[,.(All_Prop=.N/nrow(bank_DT)),balance_group][order(balance_group)]
balance_dist_housing<-bank_DT[housing=='yes',.(Housing_Prop=.N/nrow(bank_DT[housing=='yes',,])),balance_group][order(balance_group)]
H_Balance_Summ<-merge(balance_dist_all,balance_dist_housing,by='balance_group')[,.(balance_group,All_Prop,Housing_Prop, Affinity=Housing_Prop/All_Prop)]

H_Balance_Summ[,Affinity_Format:=paste0(round(Affinity,1),'x')]

ggplot(data=H_Balance_Summ)+
  geom_bar(aes(x=balance_group,y=Housing_Prop, fill=Housing_Prop),stat='identity', show.legend = FALSE)+
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  ylab('Proportion of clients (with mortgage)')+  
  theme(text = element_text(size=20),panel.background = element_rect(fill='white',colour='white'),
        axis.title.y = element_blank()) +  
  scale_fill_distiller(palette = "YlOrRd", direction=1) +
  geom_text(aes(y=Housing_Prop+0.01,x=balance_group, label=Affinity_Format),size=5)

#########################
#Balance log
########################
factor<-'balance_log'
one_way_plot_data<-f_one_way_plot_data(factor, 'housing_bin',bank)


ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values, fill=y_values), stat='identity')+
  xlab(factor)+
  ylab('Pers. Loan Rate')+
  scale_y_continuous(labels = scales::percent) +
  # theme(text = element_text(size=20),axis.text.x = element_text(angle = 90)) +  
  theme(text = element_text(size=20),panel.background = element_rect(fill='white',colour='white')) +  
  scale_fill_distiller(palette = "YlOrRd", direction=1) 


#Not very interesting

######################
#Marital Status
######################

bank_DT[,marital:=factor(marital, levels = c('single', 'married' ,'divorced'))]

#All clients
marital_dist_all<-bank_DT[,.(All_Prop=.N/nrow(bank_DT)),marital][order(marital)]

#Mortgage
marital_dist_housing<-bank_DT[housing=='yes',.(Housing_Prop=.N/nrow(bank_DT[housing=='yes',,])),marital][order(marital)]

H_Marital_Summ<-merge(marital_dist_all,marital_dist_housing)[,.(marital,Housing_Prop,All_Prop,Affinity=Housing_Prop/All_Prop),]
H_Marital_Summ[,Affinity_Format:=paste0(round(Affinity,1),'x')]



ggplot(data=H_Marital_Summ)+
  geom_bar(aes(x=marital,y=Housing_Prop, fill=Housing_Prop),stat='identity', show.legend = FALSE)+
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  ylab('Proportion of clients (with mortgage)')+  
  theme(text = element_text(size=25),panel.background = element_rect(fill='white',colour='white'),
        axis.title.y = element_blank()) +  
  scale_fill_distiller(palette = "YlOrRd", direction=1) +
  geom_text(aes(y=Housing_Prop+0.03,x=marital, label=Affinity_Format),size=8)


######################
#Job
######################

#All clients
job_dist_all<-bank_DT[,.(All_Prop=.N/nrow(bank_DT)),job][order(job)]

#Mortgage
job_dist_housing<-bank_DT[housing=='yes',.(Housing_Prop=.N/nrow(bank_DT[housing=='yes',,])),job][order(job)]

H_Job_Summ<-merge(job_dist_all,job_dist_housing)[,.(job,Housing_Prop,All_Prop,Affinity=Housing_Prop/All_Prop),]
H_Job_Summ[,Affinity_Format:=paste0(round(Affinity,1),'x')]

H_Job_Summ[,job:=factor(job, levels = H_Job_Summ$job[order(H_Job_Summ$Housing_Prop)])]



ggplot(data=H_Job_Summ[order(Housing_Prop)])+
  geom_bar(aes(x=job,y=Housing_Prop, fill=Housing_Prop),stat='identity', show.legend = FALSE)+
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  ylab('Proportion of clients (with mortgage)')+  
  theme(text = element_text(size=20),panel.background = element_rect(fill='white',colour='white'),
        axis.title.y = element_blank()) +  
  scale_fill_distiller(palette = "YlOrRd", direction=1) +
  geom_text(aes(y=Housing_Prop+0.01,x=job, label=Affinity_Format),size=5)


#########################
# Education
#########################

#All clients
educ_dist_all<-bank_DT[,.(All_Prop=.N/nrow(bank_DT)),education][order(education)]

#Mortgage
educ_dist_housing<-bank_DT[housing=='yes',.(Housing_Prop=.N/nrow(bank_DT[housing=='yes',,])),education][order(education)]

H_Educ_Summ<-merge(educ_dist_all,educ_dist_housing)[,.(education,Housing_Prop,All_Prop,Affinity=Housing_Prop/All_Prop),]
H_Educ_Summ[,Affinity_Format:=paste0(round(Affinity,1),'x')]


#should we instead compare to people without mortgage given so many have a mortgage?


ggplot(data=H_Educ_Summ[order(Housing_Prop)])+
  geom_bar(aes(x=education,y=Housing_Prop, fill=Housing_Prop),stat='identity', show.legend = FALSE)+
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  ylab('Proportion of clients (with mortgage)')+  
  theme(text = element_text(size=25),panel.background = element_rect(fill='white',colour='white'),
        axis.title.y = element_blank()) +  
  scale_fill_distiller(palette = "YlOrRd", direction=1) +
  geom_text(aes(y=Housing_Prop+0.03,x=education, label=Affinity_Format),size=7)





#######################

# Relevant outputs from question 2B
#Multiplier effect on odds to have a mortgage

########################

#model
summary(housing_logistic)

#Check random forest importance just for record
RF_Housing$importance
#To remain consistent with Q1, use logistic regression results


#Multiplier chart from housing logistics regression with job coefficients
Multiplier_plot_housing_logistic_job


#Code can be found further below again for reference


#Create nice plots with age and balance_log curves

#Age multiplier impact on odds to have a mortgage; compared to default baseline
ggplot(data.frame(x=c(18, 70)), aes(x)) + 
  stat_function(fun=function(x){exp(0.1277943*x-0.0019477*x^2-2.0940310)},size=2, col='light coral') +
  ylab("Multiplier on odds to have a mortgage")+
  xlab("Age")+
  scale_y_continuous(labels =function(x){paste0(round(x,1),'x')})+
  theme_bw()+
  theme(text = element_text(size=20)) 



#log balance multiplier impact on odds to have a mortgage; compared to default baseline

ggplot(data.frame(x=c(2, 10)), aes(x)) + 
  stat_function(fun=function(x){exp(0.3359755*x-0.0281415*x^2-2.0940310)},size=2, col='light coral') +
  ylab("Multiplier on odds to have a mortgage")+
  xlab("Log of balance")+
  scale_y_continuous(labels =function(x){paste0(round(x,1),'x')})+
  theme_bw()+
  theme(text = element_text(size=20))


#Scale the mulplier on graph to be compared to the average person (available in the model)

#Calculate newly scaled b0
b0_age<--(0.1277943*mean(bank_cleansed$age)-0.0019477*mean(bank_cleansed$age)^2)
b0_log_bal<--(0.3359755*mean(bank_cleansed$balance_log)-0.0281415*(mean(bank_cleansed$balance_log)^2))

#plot Age
ggplot(data.frame(x=c(18, 70)), aes(x)) + 
  stat_function(fun=function(x){exp(0.1277943*x-0.0019477*x^2+b0_age)},size=2, col='light coral') +
  ylab("Multiplier on odds to have a mortgage")+
  xlab("Age")+
  scale_y_continuous(limits=c(0,1.2),breaks=seq(0,1.2,0.15),labels =function(x){paste0(round(x,1),'x')})+
  theme_bw()+
  theme(text = element_text(size=20))

#Plot log balance
ggplot(data.frame(x=c(2, 10)), aes(x)) + 
  stat_function(fun=function(x){exp(0.3359755*x-0.0281415*x^2+b0_log_bal)},size=2, col='light coral') +
  ylab("Multiplier on odds to have a mortgage")+
  xlab("Log of balance")+
  scale_y_continuous(labels =function(x){paste0(round(x,1),'x')})+
  theme_bw()+
  theme(text = element_text(size=20))




##################################################

#Reference only, Q1B code on job multiplier plot

########################################################

housing_logis_coeff<-summary(housing_logistic)$coefficients
job_coeff<-housing_logis_coeff[grepl('job',rownames(housing_logis_coeff)),'Estimate']
job_coeff<-c(job_coeff, setNames(0,'jobadmin.'))

mean_job_coeff<-sum(job_coeff*sapply(gsub("job", "", names(job_coeff)),function(x){sum(bank_cleansed$job==x)}))/nrow(bank_cleansed)


#Put in a data.frame
job_coeff_adj<-data.frame(job=gsub("job", "", names(job_coeff)),mult=exp(job_coeff-mean_job_coeff))
job_coeff_adj$job<-factor(job_coeff_adj$job, levels=job_coeff_adj$job[order(job_coeff_adj$mult)])

#Plot job multiplier vs the average person
ggplot(data=job_coeff_adj)+
  geom_bar(aes(x=job,y=mult-1,fill=mult), stat='identity',show.legend=FALSE)+
  scale_y_continuous(labels =function(x){paste0(round(x+1,1),'x')})+
  theme_bw()+
  theme(text = element_text(size=20), axis.title.y = element_blank())+
  ylab('Multiplier on odds to have a mortgage') +
  coord_flip()+
  scale_fill_distiller(palette = "YlOrRd", direction=1) 











