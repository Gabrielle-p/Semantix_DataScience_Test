################################################################################

#Date: 2019-07-11 
#Subject: Semantix Data Science Test
#Data: https://archive.ics.uci.edu/ml/datasets/bank+marketing
#Candidate: Gabrielle Provost

#Script 4
#Question 2
#Raw statistics on campaign success vs number of contacts

#Pre-requisite
#Having run script 1 named "02 Load data in R.R"

################################################################################


##########################
#Question 2: Fazendo uma relação entre número de contatos e sucesso da campanha quais são os pontos relevantes a serem observados?
##########################

#install.library('ggplot2')
library(ggplot2)

#################################
#Below is the code to generate the results used in the presentation.
#Further below is a  second section with code testing different alternatives before
#selecting the results used for the presentation.
####################################

#########
#Check distribution of number of contacts
#########

qplot(bank$campaign, geom="histogram") 

#See the distribution in numbers
sapply(sort(unique(bank$campaign)),function(x){y<-sum(bank$campaign==x) 
return(c(x,y)) })
#Campaign will be grouped by 5 after 15


#########
#Summarise and aggregate data
##########
Question2<-bank_DT[order(campaign),.(client_nbr=.N, nbr_success=sum(sapply(y,function(x){if(x=='yes') 1 else 0})), success_rate=sum(sapply(y,function(x){if(x=='yes') 1 else 0}))/.N),.(campaign_group=sapply(campaign,function(x){if(x>=15) floor(x/5)*5 else x}))]


##########
#Visualise
##########


ggplot(data=as.data.frame(Question2)) + 
  geom_line(aes(x=campaign_group,y=success_rate), stat='identity', color='blue', size=1.5) +
  geom_bar(aes(x=campaign_group, y=client_nbr/100000), stat='identity') +
  scale_y_continuous(sec.axis = sec_axis(~.*100000, name='Client Count'), name='Success Rate')
#Data still needs further smoothing

#add trend line (Other type of curves are tested in the section below)
#Polynomial 6 degree was providing the best fit
fit <- lm(success_rate~poly(campaign_group,6,raw=TRUE),data=Question2)
Question2$trend_line<-predict(fit, data.frame(campaign_group=Question2$campaign_group))


#Visualise with trend line polynomial only

ggplot(data=as.data.frame(Question2)) + 
  geom_bar(aes(x=campaign_group, y=client_nbr/100000), stat='identity', fill='light grey') +
  geom_line(aes(x=campaign_group, y=trend_line), stat='identity', color='darkolivegreen3', size=1.5)+
  scale_y_continuous(sec.axis = sec_axis(~.*100000, name='Client Count'), name='Success Rate',labels = scales::percent) +
  geom_point(aes(x=campaign_group,y=success_rate), stat='identity', color='lightcoral', size=1.5) +
  xlab('Number of contacts during campaign') +
  theme_bw()+
  xlim(0,40)+
  theme(text = element_text(size=20))

#Pull trend line equation
View(summary(fit)$coefficients)

#Create table with trend line values
View(Question2[,c("campaign_group","trend_line")])






################################################################
#Section 2 
#Different tested alternatives
#Not needed to recreate results
##################################################################


#######
#First data grouping, grouped by 5 if contact >=15
#########

ggplot(data=as.data.frame(Question2)) + 
  geom_line(aes(x=campaign_group,y=success_rate), stat='identity', color='blue', size=1.5) +
  geom_bar(aes(x=campaign_group, y=client_nbr/100000), stat='identity') +
  scale_y_continuous(sec.axis = sec_axis(~.*100000, name='Client Count'), name='Success Rate')
#Data still needs further smoothing

#add trend line
fit <- lm(success_rate~poly(campaign_group,6,raw=TRUE),data=Question2)
Question2$trend_line<-predict(fit, data.frame(campaign_group=Question2$campaign_group))

#add inv prop. trend line 2 degree
Question2$campaign_inv<-1/Question2$campaign_group
fit_inv_group_2degree <- lm(success_rate~poly(campaign_inv,2,raw=TRUE),data=Question2)
Question2$trend_line_inv_group_2degree<-predict(fit_inv_group_2degree, data.frame(campaign_inv=Question2$campaign_inv))

#add inv prop. trend line 6 degree
fit_inv_group_6degree <- lm(success_rate~poly(campaign_inv,6,raw=TRUE),data=Question2)
Question2$trend_line_inv_group_6degree<-predict(fit_inv_group_6degree, data.frame(campaign_inv=Question2$campaign_inv))

#Visualise with different trend lines

ggplot(data=as.data.frame(Question2)) + 
  geom_line(aes(x=campaign_group,y=success_rate), stat='identity', color='blue', size=1.5) +
  geom_bar(aes(x=campaign_group, y=client_nbr/100000), stat='identity') +
  geom_line(aes(x=campaign_group, y=trend_line), stat='identity', color='red', size=1.5)+
  geom_line(aes(x=campaign_group, y=trend_line_inv_group_2degree), stat='identity', color='green', size=1.5)+
  geom_line(aes(x=campaign_group, y=trend_line_inv_group_6degree), stat='identity', color='gold', size=1.5)+
  scale_y_continuous(sec.axis = sec_axis(~.*100000, name='Client Count'), name='Success Rate')
#Normal 6 degree polynomial is best



##########
#Second data grouping, grouped by 5 for all contacts >=5
#########

Question2_b<-bank_DT[order(campaign),.(client_nbr=.N, nbr_success=sum(sapply(y,function(x){if(x=='yes') 1 else 0})), success_rate=sum(sapply(y,function(x){if(x=='yes') 1 else 0}))/.N),.(campaign_group=sapply(campaign,function(x){if(x>=5) floor(x/5)*5 else x}))]

ggplot(data=as.data.frame(Question2_b)) + 
  geom_bar(aes(x=campaign_group, y=client_nbr/100000), stat='identity') +
  geom_line(aes(x=campaign_group,y=success_rate), stat='identity', color='blue', size=1.5) +
  scale_y_continuous(sec.axis = sec_axis(~.*100000, name='Client Count'), name='Success Rate')
#Smoother, but still no as smooth as a trend


##########
#Third data grouping, no grouping, raw data used
#########

Question2_c<-bank_DT[order(campaign),.(client_nbr=.N, nbr_success=sum(sapply(y,function(x){if(x=='yes') 1 else 0})), success_rate=sum(sapply(y,function(x){if(x=='yes') 1 else 0}))/.N),campaign]

ggplot(data=as.data.frame(Question2_c)) + 
  geom_bar(aes(x=campaign, y=client_nbr/100000), stat='identity') +
  geom_point(aes(x=campaign,y=success_rate), stat='identity', color='blue', size=1.5) +
  scale_y_continuous(sec.axis = sec_axis(~.*100000, name='Client Count'), name='Success Rate')

fit_c <- lm(success_rate~poly(campaign,6,raw=TRUE),data=Question2_c)
Question2_c$trend_line<-predict(fit_c, data.frame(campaign=Question2_c$campaign))
#View(summary(fit_c)$coefficients)

#Try different types of trend lines

#Make 0 slighly above 0 to fit an exponential trend
Question2_c$success_rate_adj<-sapply(Question2_c$success_rate,function(x){if(x==0) 0.0000001 else x})

fit_log<-lm(log(success_rate_adj)~poly(campaign,1,raw=TRUE),data=Question2_c)
Question2_c$trend_line_exp<-exp(predict(fit_log, data.frame(campaign=Question2_c$campaign)))

#Inverse proportion, 1 degree
Question2_c$inv_campaign<-1/Question2_c$campaign
fit_inverse_prop<-lm(success_rate~inv_campaign,data=Question2_c)
Question2_c$trend_line_inv<-predict(fit_inverse_prop, inv_campaign=Question2_c$inv_campaign)

#Inverse proportion, 2 degree
fit_inverse_prop_2degree<-lm(success_rate~inv_campaign+I(inv_campaign^2),data=Question2_c)
Question2_c$trend_line_inv_2degree<-predict(fit_inverse_prop_2degree, inv_campaign=Question2_c$inv_campaign)

#Inverse proportion, 6 degree
fit_inverse_prop_6degree<-lm(success_rate~inv_campaign+I(inv_campaign^6),data=Question2_c)
Question2_c$trend_line_inv_6degree<-predict(fit_inverse_prop_6degree, inv_campaign=Question2_c$inv_campaign)



#Visualise all trend lines

ggplot(data=as.data.frame(Question2_c)) + 
  geom_bar(aes(x=campaign, y=client_nbr/100000), stat='identity') +
  geom_point(aes(x=campaign,y=success_rate), stat='identity', color='blue', size=1.2) +
  geom_line(aes(x=campaign,y=trend_line),stat='identity',color='red', size=1.2)+
  geom_line(aes(x=campaign,y=trend_line_inv),stat='identity',color='green', size=1.2)+
  geom_line(aes(x=campaign,y=trend_line_inv_2degree),stat='identity',color='purple', size=1.2)+
  geom_line(aes(x=campaign,y=trend_line_exp),stat='identity',color='pink', size=1.2)+
  geom_line(aes(x=campaign,y=trend_line_inv_6degree),stat='identity',color='gold', size=1.2)+
  scale_y_continuous(sec.axis = sec_axis(~.*100000, name='Client Count'), name='Success Rate') 
#Similar to inverse proportional relationship
#Exp falls too quickly
#first degree inverse also falls a bit more quickly than desired
#regular 6 degree polynomial fit offers the best fit


