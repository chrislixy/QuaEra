library(stringr)
library(nnet)
library(glmnet)
library(fitdistrplus)
# read data
train <- read.csv("~/Documents/untitled folder/QuaEra/QuaEra/training_autoinsurance.csv")

# clean data

train$CustomerID=as.factor(train$CustomerID)
train$RecordType=as.factor(train$RecordType)

# Time
train$Day=as.ordered(train$Day)
train$Time=unlist(lapply(strsplit(as.character(train$Time),":"), function(x){return(round(as.numeric(x[1])+as.numeric(x[2])/60))}))
train$Time[train$Time==24]=0

# possible: location that is visited more often has faster process because they are more experienced
train$Location=as.factor(train$Location)

train$Homeowner=as.factor(train$Homeowner)

# treat missing entries as NA, predict NA later with other demographic description

# predict risk factor with existing data.
# only 1570 out of 19963 filled risk factor in the end, may induce bias
# only use RF at transaction to avoid weighting on #shopping points.
summary(train$RiskFactor)
length(unique(train$CustomerID[is.na(train$RiskFactor)]))
a=train[!is.na(train$RiskFactor) & train$RecordType==1,]
RFmodel=multinom(RiskFactor~State+GroupSize+Homeowner+CarAge+AgeOldest+Married,data=a)
# predict with RFmodel
train$RiskFactor[is.na(train$RiskFactor)]=predict(RFmodel,train[is.na(train$RiskFactor),])
train$RiskFactor=as.ordered(train$RiskFactor)

train$CarValue[train$CarValue==""]=NA
CVmodel=multinom(CarValue~State+GroupSize+Homeowner+CarAge+AgeOldest+Married,data=train)
train$CarValue[is.na(train$CarValue)]=predict(CVmodel,train[is.na(train$CarValue),])
train$CarValue=as.ordered(train$CarValue)

train$Married=as.factor(train$Married)

# 444 customers don't have information for prevC and years covered by previous issuers at the time of purchase

# PrevC is always missing when there is no PrevDuration info--3325 customers
length(unique(train$CustomerID[is.na(train$PrevDuration)]))
a=train[is.na(train$PrevDuration),]
b=train[train$CustomerID %in% a$CustomerID & train$RecordType==1,]
# max for all PrevDuration is 15, and there is a spike at PrevDuration=15,
# show it should be the case that PrevDuration >= 15
# Otherwise, follow geometric distribution
summary(b$PrevDuration)
hist(b$PrevDuration[b$PrevDuration!=15])
summary((b$PrevDuration[b$PrevDuration!=15]))
# get p=0.176, can generate missing values from geometric distribution
# assume: there is some reason that they don't want to review PrevDuration,
# so distribution for people with NAs is different from people with complete PrevDuration
train$PrevDuration[is.na(train$PrevDuration)]=rgeom(dim(a)[1],fitdist(b$PrevDuration[!is.na(b$PrevDuration)],"geom")$estimate)
train$PrevDuration[train$PrevDuration>15]=15

# assumption: randomly generate missing values with multinomial distribution,
# proportional to the distribution of people who didn't reveal their PrevC at first
# train$PrevC[is.na(train$PrevC)]=0
train$PrevC=as.ordered(train$PrevC)
d=b[!is.na(b$PrevDuration),]
prevCmodel=multinom(PrevC~State+GroupSize+Homeowner+CarAge+AgeOldest+Married,data=d)
# predict with model 
# predict(prevCmodel,d)
train$PrevC[is.na(train$PrevC)]=predict(prevCmodel,train[is.na(train$PrevC),])


# ordered options
train$A=as.ordered(train$A)
train$B=as.ordered(train$B)
train$C=as.ordered(train$C)
train$D=as.ordered(train$D)
train$E=as.ordered(train$E)
train$F=as.ordered(train$F)
train$G=as.ordered(train$G)


rm(list=c("a","b","d"))
