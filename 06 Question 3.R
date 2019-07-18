################################################################################

#Date: 2019-07-11 
#Subject: Semantix Data Science Test
#Data: https://archive.ics.uci.edu/ml/datasets/bank+marketing
#Candidate: Gabrielle Provost

#Script 5
#Question 3
#Dummy curves on survival models

#Pre-requisite
#Having run script 1 named "02 Load data in R.R"


################################################################################


####################################################
#Question 3
####################################################

##############################
#Theorical behaviour with 
#dummy assumptions
############################

#Dummy multiple follow-up calls

#Dummy decaying function

#Set a= to the overall subscription rate of data set

a<-sum(bank$y=='yes')/nrow(bank)

#Set b in function of reaching a probability of subcription of 0.01 at 30 days
b<-log(0.01/a)/30

#Control group decaying function
a<-0.06/(exp(b*10))
curve(a*exp(b*x),xlim=c(0,30), xlab='Days since first contact', ylab='Probability to subscribe', col='light coral',lwd=3, ylabels = scales::percent,yaxt="n")
axis(2, at=pretty(seq(0,0.12,by=0.02)), lab=paste0(pretty(seq(0,0.12,by=0.02)) * 100,' %'), las=TRUE)


#10 days follow-up call uplift
a.2<-0.06/(exp(b*10))
curve(a.2*exp(b*x),xlim=c(10,30), ylim=c(0,0.12),xlab='Days since first contact', ylab='Probability to subscribe', col='darkolivegreen3',lwd=3, ylabels = scales::percent,yaxt="n")
axis(2, at=pretty(seq(0,0.12,by=0.02)), lab=paste0(pretty(seq(0,0.12,by=0.02)) * 100,' %'), las=TRUE)

#15 day follow-up call uplift
a.3<-0.045/(exp(b*15*1.2))
curve(a.3*exp(b*x),xlim=c(15,30), ylim=c(0,0.12),xlab='Days since first contact', ylab='Probability to subscribe', col='gold',lwd=3, ylabels = scales::percent,yaxt="n")
axis(2, at=pretty(seq(0,0.12,by=0.02)), lab=paste0(pretty(seq(0,0.12,by=0.02)) * 100,' %'), las=TRUE)



################
#Plot all 3 curves together
#################

#original
curve(a*exp(b*x),xlim=c(0,30), xlab='Days since first contact', ylab='Probability to subscribe', col='light coral',lwd=3, ylabels = scales::percent,yaxt="n")
axis(2, at=pretty(seq(0,0.12,by=0.02)), lab=paste0(pretty(seq(0,0.12,by=0.02)) * 100,' %'), las=TRUE)

#10 day follow up
f_10d <- function(x){(x <10)*(a*exp(b*x))+(x>=10)*(a.2*exp(b*x))}
curve(f_10d(x),xlim=c(0,30), add=TRUE, xlab='Days since first contact', ylab='Probability to subscribe', col='darkolivegreen3',lwd=3, ylabels = scales::percent,yaxt="n", lty=3)

#15 day followup
f_15d <- function(x){(x <15)*(a*exp(b*x))+(x>=15)*(a.3*exp(b*x*1.2))}
curve(f_15d(x),xlim=c(0,30), add=TRUE,xlab='Days since first contact', ylab='Probability to subscribe', col='gold',lwd=3, ylabels = scales::percent,yaxt="n", lty=2)

legend(x=8, y=0.11,legend=c("Control", "One follow-up call at 10 days", "One follow-up call at 15 days"),
       col=c("light coral", "darkolivegreen3","gold"), lty=c(1,2,3), cex=0.8,box.lty=0,lwd=3)



#############################################
#Question 3 B / use data to make rough estimates 
#############################################
#Each campaign contact are unlikely to have started at the same time
#Day and month of last contact provided, but unless this was a "year long campaign" unlikely to be useful

unique(bank$month)
unique(bank_DT[campaign==1,month,])
unique(bank_DT[campaign==5,month,])
#Even if many contacts were made, all months are present.
#Defeats the hypothesis that perhaps all customers were started at once
#Could they have been monthly campaign?

hist(bank_DT[campaign==1,day,])
hist(bank_DT[campaign==2,day,])
hist(bank_DT[campaign==3,day,])
#No clear trend

#No relevant estimate can be thought of.
#Recommendation remains on lower number of contacts, experiment based on time since they 
#were originally contacted to understand if a step can be created be a follow-up call in the 
#original decay function


#######################################
#Target who is eligible to a follow-up call
########################################



bank_DT[,y_bin:=sapply(y,function(x){if (x=='yes') 1 else 0})]

bank_DT[,.(success_rate=sum(y_bin)/.N),.(dur_group=floor(duration/10)*10)][order(dur_group)]

success_by_dur<-
  bank_DT[,.(success_rate=sum(y_bin)/.N),.(dur_group=sapply(duration, function(x){
  if(x<30) floor(x/10)*10 else 
    if (x<2*60) floor(x/30)*30 else
      if (x<10*60) floor(x/60)*60 else
        if (x<30*60) floor(x/(60*5))*(60*5) else
          30*60}))][order(dur_group)]

ggplot(success_by_dur) + 
  geom_bar(aes(x=as.factor(dur_group),y=success_rate), stat='identity') +
  xlab('Call duration') +
  ylab('Success rate') +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 90))



