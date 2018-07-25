library()

dataset<- read.csv("Dataset.csv")

str(dataset)

#Create user defined function for descriptive analysis
var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

vars <- c("REVENUE",
          "MOU",
          "RECCHRGE",
          "DIRECTAS",
          "OVERAGE",
          "ROAM",
          "CHANGEM",
          "CHANGER",
          "DROPVCE",
          "BLCKVCE",
          "UNANSVCE",
          "CUSTCARE",
          "THREEWAY",
          "MOUREC",
          "OUTCALLS",
          "INCALLS",
          "PEAKVCE",
          "OPEAKVCE",
          "DROPBLK",
          "CALLFWDV",
          "CALLWAIT")

stats <- t(apply(dataset[vars],2,FUN= var_Summ))
View(stats)

dataset$REVENUE[dataset$REVENUE>225.53]<-225.53				
dataset$MOU[dataset$MOU>2450.5]<-2450.5				
dataset$RECCHRGE[dataset$RECCHRGE>119.99]<-119.99				
dataset$DIRECTAS[dataset$DIRECTAS>9.65]<-9.65				
dataset$OVERAGE[dataset$OVERAGE>427.75]<-427.75				
dataset$ROAM[dataset$ROAM>21.56]<-21.56				
dataset$CHANGEM[dataset$CHANGEM>740]<-740				
dataset$CHANGER[dataset$CHANGER>118.46]<-118.46				
dataset$UNANSVCE[dataset$UNANSVCE>179.33]<-179.33				
dataset$CUSTCARE[dataset$CUSTCARE>21]<-21				
dataset$THREEWAY[dataset$THREEWAY>4]<-4				
dataset$OUTCALLS[dataset$OUTCALLS>164.33]<-164.33				
dataset$INCALLS[dataset$INCALLS>77]<-77				
dataset$PEAKVCE[dataset$PEAKVCE>500]<-500				
dataset$OPEAKVCE[dataset$OPEAKVCE>437]<-437				
dataset$DROPBLK[dataset$DROPBLK>71.33]<-71.33				
dataset$CALLFWDV[dataset$CALLFWDV>0]<-0				
dataset$CALLWAIT[dataset$CALLWAIT>23.33]<-23.33				
dataset$MONTHS[dataset$MONTHS>49]<-49				
dataset$ACTVSUBS[dataset$ACTVSUBS>4]<-4				
dataset$PHONES[dataset$PHONES>7]<-7				
dataset$EQPDAYS[dataset$EQPDAYS>1150]<-1150				
dataset$AGE1[dataset$AGE1>74]<-74				
dataset$AGE2[dataset$AGE2>76]<-76				
dataset$RETCALLS[dataset$RETCALLS>1]<-1				
dataset$RETACCPT[dataset$RETACCPT>1]<-1				
dataset$REFER[dataset$REFER>1]<-1				
dataset$INCOME[dataset$INCOME>9]<-9				
dataset$CREDITAD[dataset$CREDITAD>1]<-1				
dataset$MONTHS[dataset$MONTHS<7]<-7	
dataset$ACTVSUBS[dataset$ACTVSUBS<1]<-1	
dataset$PHONES[dataset$PHONES<1]<-1	
dataset$EQPDAYS[dataset$EQPDAYS<42]<-42	
dataset$REVENUE[dataset$REVENUE<15.51]<-15.51
dataset$MOU[dataset$MOU<20.33]<-20.33
dataset$RECCHRGE[dataset$RECCHRGE<10]<-10

dataset$AGE1 <- ifelse(is.na(dataset$AGE1), mean(dataset$AGE1, na.rm=TRUE), dataset$AGE1)
dataset$AGE2 <- ifelse(is.na(dataset$AGE2), mean(dataset$AGE2, na.rm=TRUE), dataset$AGE2)
dataset$REVENUE <- ifelse(is.na(dataset$REVENUE), mean(dataset$REVENUE, na.rm=TRUE), dataset$REVENUE)
dataset$MOU <- ifelse(is.na(dataset$MOU), mean(dataset$MOU, na.rm=TRUE), dataset$MOU)
dataset$RECCHRGE <- ifelse(is.na(dataset$RECCHRGE), mean(dataset$RECCHRGE, na.rm=TRUE), dataset$RECCHRGE)
dataset$DIRECTAS <- ifelse(is.na(dataset$DIRECTAS), mean(dataset$DIRECTAS, na.rm=TRUE), dataset$DIRECTAS)
dataset$OVERAGE <- ifelse(is.na(dataset$OVERAGE), mean(dataset$OVERAGE, na.rm=TRUE), dataset$OVERAGE)
dataset$ROAM <- ifelse(is.na(dataset$ROAM), mean(dataset$ROAM, na.rm=TRUE), dataset$ROAM)

dataset$LNREVENUE <- log(dataset$REVENUE)
dataset$LNMOU <- log(dataset$MOU)
dataset$LNRECCHRGE <-log(dataset$RECCHRGE)
dataset$LNOVERAGE <- sqrt(dataset$OVERAGE)
dataset$LNEQPDAYS <-exp(log(log(dataset$EQPDAYS)))
dataset$LNMONTHS <- log(dataset$MONTHS)
dataset$LNUNANSVCE <-ifelse(dataset$UNANSVCE!=0,log(dataset$UNANSVCE), log(dataset$UNANSVCE+1)) 
dataset$LNCUSTCARE <- sqrt(dataset$CUSTCARE)
dataset$LNPEAKVCE <-ifelse(dataset$PEAKVCE!=0,log(dataset$PEAKVCE), log(dataset$PEAKVCE+1))

dataset$RETACCPT <- NULL
dataset$RETCALLS <- NULL
dataset$RETCALL  <- NULL

#use chi sqr test insted

chisq.test(table(dataset$CHURN,  dataset$CHILDREN))
chisq.test(table(dataset$CHURN,  dataset$CREDITA))
chisq.test(table(dataset$CHURN,  dataset$CREDITAA))
chisq.test(table(dataset$CHURN,  dataset$CREDITB))
chisq.test(table(dataset$CHURN,  dataset$CREDITC))
chisq.test(table(dataset$CHURN,  dataset$CREDITDE))
chisq.test(table(dataset$CHURN,  dataset$CREDITGY))
chisq.test(table(dataset$CHURN,  dataset$CREDITZ))
chisq.test(table(dataset$CHURN,  dataset$PRIZMRUR))
chisq.test(table(dataset$CHURN,  dataset$PRIZMUB))
chisq.test(table(dataset$CHURN,  dataset$PRIZMTWN))
chisq.test(table(dataset$CHURN,  dataset$REFURB))
chisq.test(table(dataset$CHURN,  dataset$WEBCAP))
chisq.test(table(dataset$CHURN,  dataset$TRUCK))
chisq.test(table(dataset$CHURN,  dataset$RV))
chisq.test(table(dataset$CHURN,  dataset$OCCPROF))
chisq.test(table(dataset$CHURN,  dataset$OCCCLER))
chisq.test(table(dataset$CHURN,  dataset$OCCCRFT))
chisq.test(table(dataset$CHURN,  dataset$OCCSTUD))
chisq.test(table(dataset$CHURN,  dataset$OCCHMKR))
chisq.test(table(dataset$CHURN,  dataset$OCCRET))
chisq.test(table(dataset$CHURN,  dataset$OCCSELF))
chisq.test(table(dataset$CHURN,  dataset$OWNRENT))
chisq.test(table(dataset$CHURN,  dataset$MARRYUN))
chisq.test(table(dataset$CHURN,  dataset$MARRYYES))
chisq.test(table(dataset$CHURN,  dataset$MARRYNO))
chisq.test(table(dataset$CHURN,  dataset$MAILORD))
chisq.test(table(dataset$CHURN,  dataset$MAILRES))
chisq.test(table(dataset$CHURN,  dataset$MAILFLAG))
chisq.test(table(dataset$CHURN,  dataset$TRAVEL))
chisq.test(table(dataset$CHURN,  dataset$PCOWN))
chisq.test(table(dataset$CHURN,  dataset$CREDITCD))
chisq.test(table(dataset$CHURN,  dataset$NEWCELLY))
chisq.test(table(dataset$CHURN,  dataset$NEWCELLN))
chisq.test(table(dataset$CHURN,  dataset$INCMISS))
chisq.test(table(dataset$CHURN,  dataset$MCYCLE))
chisq.test(table(dataset$CHURN,  dataset$SETPRCM))


fit <- aov(dataset$CHURN ~ CHILDREN+
             CREDITA+
             CREDITAA+
             CREDITB+
             CREDITC+
             CREDITDE+
             CREDITGY+
             CREDITZ+
             PRIZMRUR+
             PRIZMUB+
             PRIZMTWN+
             REFURB+
             WEBCAP+
             TRUCK+
             RV+
             OCCPROF+
             OCCCLER+
             OCCCRFT+
             OCCSTUD+
             OCCHMKR+
             OCCRET+
             OCCSELF+
             OWNRENT+
             MARRYUN+
             MARRYYES+
             MARRYNO+
             MAILORD+
             MAILRES+
             MAILFLAG+
             TRAVEL+
             PCOWN+
             CREDITCD+
             NEWCELLY+
             NEWCELLN+
             INCMISS+
             MCYCLE+
             SETPRCM, data = dataset)
ls(fit)
summary(fit)

#dont do factor on cat vars

fit_mod <- glm(dataset$CHURN ~ CREDITA+
               CREDITAA+
               CREDITB+
               CREDITC+
               CREDITDE+
               PRIZMRUR+
               PRIZMUB+
               PRIZMTWN+
               REFURB+
               WEBCAP+
               OCCRET+
               OWNRENT+
               INCMISS+
               SETPRCM+
             LNREVENUE+
             LNMOU+
             LNRECCHRGE+
             DIRECTAS+
             LNOVERAGE+
             ROAM+
             CHANGEM+
             CHANGER+
             LNUNANSVCE+
             LNCUSTCARE+
             THREEWAY+
             OUTCALLS+
             INCALLS+
             LNPEAKVCE+
             OPEAKVCE+
             DROPBLK+
             CALLFWDV+
             CALLWAIT+
             LNMONTHS+
             ACTVSUBS+
             PHONES+
             LNEQPDAYS+
             AGE1+
             AGE2+
             REFER+
             INCOME+
             CREDITAD, data = dataset, family = binomial(logit))

step <- step(fit_mod)
###################################### FINAL VARS FOR MODEL
#dataset$CHURN ~ LNREVENUE + LNMOU + LNRECCHRGE + DIRECTAS + LNOVERAGE + 
ROAM + LNUNANSVCE + LNCUSTCARE + THREEWAY + INCALLS + LNPEAKVCE + 
  DROPBLK + CALLWAIT + LNMONTHS + ACTVSUBS + PHONES + LNEQPDAYS + 
  AGE1 + CREDITAD


#SIGNIFICANT CATAGORICAL VARIABLES

dataset$CREDITAA <- as.factor(dataset$CREDITAA)
dataset$CREDITB <- as.factor(dataset$CREDITB)
dataset$CREDITC <- as.factor(dataset$CREDITC)
dataset$CREDITDE <- as.factor(dataset$CREDITDE)
dataset$REFURB <- as.factor(dataset$REFURB)
dataset$WEBCAP <- as.factor(dataset$WEBCAP)
dataset$SETPRCM <- as.factor(dataset$SETPRCM)
dataset$CHURN <- as.factor(dataset$CHURN)
dataset$OCCRET  <- as.factor(dataset$OCCRET)



#Splitting data into Training, Validaton and Testing Dataset
training <- dataset[which(dataset$CALIBRAT == 1),]
testing <- dataset[which(dataset$CALIBRAT== 0),]


#Building Models for training dataset

fit_train <- glm(training$CHURN ~ CREDITAA + CREDITB + CREDITC + CREDITDE + PRIZMRUR + 
  PRIZMUB + PRIZMTWN + REFURB + WEBCAP + OCCRET + SETPRCM + 
  LNREVENUE + LNMOU + LNRECCHRGE + DIRECTAS + LNOVERAGE + ROAM + 
  LNUNANSVCE + LNCUSTCARE + THREEWAY + INCALLS + LNPEAKVCE + 
  OPEAKVCE + DROPBLK + CALLWAIT + LNMONTHS + PHONES + LNEQPDAYS + 
  AGE1 + REFER + CREDITAD,data = training, family = binomial(logit))

#Stepwise regression  
step_train <- step(fit_train)

summary(fit_train)
ls$fit_train
fit_train$model
fit_train$

coeff <- fit_train$coefficients
write.csv(coeff, "coeff.csv")

#Checking for concordance 
source("H:/R/Linear Regression in R/Concordance.R")
Concordance(fit_train)  #NOTE: To run these command, first run concordance function in Concordance.R 

################################VALIDATION ##############################
#Decile Scoring for 
##Training dataset

train1<- cbind(training, Prob_churn=predict(fit_train, type="response")) 
View(train1)

##Creating Deciles
decLocations <- quantile(train1$Prob_churn, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob_churn,c(-Inf,decLocations, Inf))
View(train1)

#Decile Analysis Reports
require(sqldf)
fit_train_DA <- sqldf("select decile, min(Prob_churn) as Min_prob
                      , max(Prob_churn) as max_prob
                      , sum(CHURN) as default_Count
                      , (count(decile)-sum(CHURN)) as Non_default_Count 
                      from train1
                      group by decile
                      order by decile desc")

write.csv(fit_train_DA,"fit_train_DA.csv",row.names = F)


##Testing dataset
test1<- cbind(testing, Prob=predict(fit_train,testing, type="response")) 
View(test1)

##Creating Deciles
decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1), na.rm = TRUE)
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
names(test1)
#Decile Analysis Reports
require(sqldf)

fit_test_DA <- sqldf("select decile, min(Prob) as Min_prob
                      , max(Prob) as max_prob
                     , sum(CHURN) as default_Count
                     , (count(decile)-sum(CHURN)) as Non_default_Count 
                     from test1
                     group by decile
                     order by decile desc")

write.csv(fit_test_DA,"fit_test_DA1.csv",row.names = F)

# Confusion matrix
table(train1$Prob_churn>0.50, train1$CHURN)
table(test1$Prob>0.52, test1$CHURN)

