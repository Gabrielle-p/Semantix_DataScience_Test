################################################################################

#Date: 2019-07-11 
#Subject: Semantix Data Science Test
#Data: https://archive.ics.uci.edu/ml/datasets/bank+marketing
#Candidate: Gabrielle Provost

#Script 7
#Question 5
#Statistics on default rates

#Pre-requisite
#Having run script 1 named "02 Load data in R.R"

################################################################################


################################################################################
#Question 5: Qual o fator determinante para que o banco exija um seguro de crédito?
################################################################################


#install.packages('RColorBrewer')
library('RColorBrewer')
#install.packages('ggplot2')
library('ggplot2')



#We have no information on whether the bank did ask for credit insurance
#Instead we will look at clients identified with credit in default.

#Check if all clients who are identified with default are also identified with either loan or housing loan

bank_DT[,Loan_All:=mapply(function(x,y){if(x=='yes' | y=='yes') 1 else 0},loan,housing)]
bank$Loan_All<-mapply(function(x,y){if(x=='yes' | y=='yes') 1 else 0},bank$loan,bank$housing)

bank_DT[default=='yes',.N,loan]
bank_DT[default=='yes',.N,housing]
bank_DT[default=='yes',.N,Loan_All]
#1/4 of default have no loan in the data despite having credit in default
#Short of having better data, we will still draw conclusion using loan rates
#to account for propensity to have credit in first place


#Overall default rate
sum(bank$default=='yes')/nrow(bank) #1.8%


#How many defaults in the data?
bank_DT[default=='yes',.N,]
#only 800 defaults in data, small number to build a goog predictive model
#We will try to use classic probability notions instead


#Create a binomial version of default variable for computing purposes
bank_DT[,default_bin:=sapply(default,function(x){if(x=='yes') 1 else 0})]
bank$default_bin<-sapply(bank$default,function(x){if(x=='yes') 1 else 0})


#Link between default, negative balance and loans
bank_DT[balance<0,.N,Loan_All] #15% of people with negative balance have no loans pointing to other form of credit available

bank_DT[default=='yes',.N,Loan_All] #26% of clients with default have no loans pointing to other forms of credit

bank_DT[balance<0,.N,default] #88% of clients with negative balance are not flagged with default pointing to credit not yet due



##########################################################################
#Build one-way plots of Default_Rate in function of all other factors
######################################################################


#Create function if 04 Question 1B.R hasn't been run earlier

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


##################

#Draw plots on % of clients with credit in default


##################


#Age

factor<-'age'
one_way_plot_data<-f_one_way_plot_data(factor,'default_bin',bank)
plot(one_way_plot_data, type="h", xaxt="n", main=paste0('One-way plot ', factor), xlab=factor, ylab='Default Rate')
axis(1, at=one_way_plot_data$x_values, labels=one_way_plot_data$x_values)

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('% of clients with credit in default  ')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20))

#Job

factor<-'job'
one_way_plot_data<-f_one_way_plot_data(factor,'default_bin',bank)

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('% with credit in default')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))

#Entrepreneur are much more likely to default
#Check if more likely once we accounted for their higher likeliness to take a loan

#Make the job plot prettier

ggplot(data=cbind(one_way_plot_data,job=one_way_plot_data$x_values))+
  geom_bar(aes(x_values,y_values,fill=y_values), stat='identity',show.legend = FALSE)+
  xlab(factor)+
  ylab('% of clients with credit in default  ')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90),panel.background = element_rect(fill='white',colour='black'))+
  scale_fill_distiller(palette = "YlOrRd", direction=1)
#  scale_fill_distiller(palette = "Spectral")
#  scale_fill_gradient(low = "peachpuff", high = "firebrick3")

#Multiplier effect of entrepreneur on default rate
one_way_plot_data[one_way_plot_data$x_values=='entrepreneur',2]/(sum(bank$default=='yes')/nrow(bank))


#Marital

factor<-'marital'
one_way_plot_data<-f_one_way_plot_data(factor,'default_bin',bank)

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('% of clients with credit in default  ')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))
#Divorced more likely to default

#Multiplier effect of divorced
one_way_plot_data[one_way_plot_data$x_values=='divorced',2]/(sum(bank$default=='yes')/nrow(bank))
#1.4x

#Education

factor<-'education'
one_way_plot_data<-f_one_way_plot_data(factor,'default_bin',bank)

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('% of clients with credit in default  ')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))

#Balance

factor<-'balance'
one_way_plot_data<-f_one_way_plot_data(factor,'default_bin',bank)

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('% of clients with credit in default  ')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))
#Negative balance but at 60%, needs to be self-fulfilling
#0 similar

#Housing

factor<-'housing'
one_way_plot_data<-f_one_way_plot_data(factor,'default_bin',bank)

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('% of clients with credit in default  ')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))

#Loan

factor<-'loan'
one_way_plot_data<-f_one_way_plot_data(factor,'default_bin',bank)

ggplot(data=one_way_plot_data)+
  geom_bar(aes(x_values,y_values), stat='identity')+
  xlab(factor)+
  ylab('% of clients with credit in default  ')+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))

#Obviously lon is much more likely, but deemed too likely to be self-fulfilling

########################################################################
#Deep dive into entrepreneur
#Account for the propensity to have a loan and see if entrepreneur are 
#still more likely to default and by how much
########################################################################

#################################################
# First all loans are used are source fo credit
################################################


factor<-'job'
one_way_plot_data<-f_one_way_plot_data(factor,'default_bin',bank)

#Overall probability to have a loan (either housing or loan)
Overall_Loan_p<-sum(bank$Loan_All==1)/nrow(bank)
Entrepreneur_Loan_p<-sum(bank$Loan_All==1&bank$job=='entrepreneur')/sum(bank$job=='entrepreneur')

# Baye conditional propability concep
# Joint probability is what has been observed in the one-wat plot
#  P[Default & Loan] = P[Default | Loan] * P[Loan]
# Conditional probability is a better metric to look at the propensity to default.
# The formula below allows us the calculate (estimate in this case) the conditional probability to default given the client has a loan (e.g. has credit)
#  P[Default | Loan] = P[Default & Loan] / P[Loan]

#Joint probability rates
entrepreneur_joint_p_loan_default<-one_way_plot_data[one_way_plot_data$x_values=='entrepreneur',2]
overall_joint_p_loan_default<-(sum(bank$default=='yes')/nrow(bank))

#Estimated Conditional Probability (using all loans)
(entrepreneur_cond_p_all_loans<-entrepreneur_joint_p_loan_default/Entrepreneur_Loan_p)

(overall_cond_p_all_loans<-overall_joint_p_loan_default/Overall_Loan_p)

#Entrepreneur multiplier coefficient on condition probability of default knowing they have a loan (any loan)
(entrepreneur_cond_p_all_loans/overall_cond_p_all_loans) #1.9 x more likely to default

#Plot results
All_Loan_Cond_P<-data.frame(group=c('Entrepreneur', 'Overall'),
                            `Conditional Default Probability`=c(entrepreneur_cond_p_all_loans,overall_cond_p_all_loans))

ggplot(data=All_Loan_Cond_P)+
  geom_bar(aes(x=group, y=Conditional.Default.Probability, fill=Conditional.Default.Probability ), stat='identity', show.legend = FALSE) +
  coord_flip()+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),panel.background = element_rect(fill='white',colour='white'),
        axis.title.y = element_blank()) +
  scale_fill_distiller(palette = "YlOrRd", direction=1)



#######################################
#Second, personal loans only are used are source of credit 
#####################################

#Technically, housing is "always" guaranteed by the property and shouldn't "default"
#This multiplier effect should therefore be more relevant

(pers_loan_entrepreneur<-sum(bank$job=='entrepreneur'&bank$loan=='yes')/sum(bank$job=='entrepreneur'))
(pers_loan_overall<-sum(bank$loan=='yes')/nrow(bank))

#Calculate conditional probability to default "given" they have a personal loan
(entrepreneur_cond_p_pers_loan<-entrepreneur_joint_p_loan_default/pers_loan_entrepreneur)

(overall_cond_p_pers_loans<-overall_joint_p_loan_default/pers_loan_overall)


#Entrepreneur multiplier to conditional probability of default "given" personal loan
entrepreneur_cond_p_pers_loan/overall_cond_p_pers_loans #Entrepreneur are 1.4x more likely to default

#Plot results
Pers_Cond_P<-data.frame(group=c('Entrepreneur', 'Overall'),
                        `Conditional Default Probability`=c(entrepreneur_cond_p_pers_loan,Overall=overall_cond_p_pers_loans))

ggplot(data=Pers_Cond_P)+
  geom_bar(aes(x=group, y=Conditional.Default.Probability, fill=Conditional.Default.Probability ), stat='identity', show.legend = FALSE) +
  coord_flip()+
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),panel.background = element_rect(fill='white',colour='white'),
        axis.title.y = element_blank()) +
  scale_fill_distiller(palette = "YlOrRd", direction=1)