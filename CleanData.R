library(stringr)
# read data
train <- read.csv("~/Documents/untitled folder/QuaEra/QuaEra/training_autoinsurance.csv")

# clean data

train$CustomerID=as.factor(train$CustomerID)
train$RecordType=as.factor(train$RecordType)

# Time
train$Day=as.ordered(train$Day)
train$Time=unlist(lapply(strsplit(as.character(train$Time),":"), function(x){return(round(as.numeric(x[1])+as.numeric(x[2])/60))}))


# possible: location that is visited more often has faster process because they are more experienced
train$Location=as.factor(train$Location)

train$Homeowner=as.factor(train$Homeowner)

# treat missing entries as NA, predict NA later with other demographic description
train$CarValue[train$CarValue==""]=NA
train$CarValue=as.ordered(train$CarValue)

train$RiskFactor=as.ordered(train$RiskFactor)
train$Married=as.factor(train$Married)

# 444 customers don't have information for prevC and years covered by previous issuers at the time of purchase
train$PrevC=as.ordered(train$PrevC)

# ordered options
train$A=as.ordered(train$A)
train$B=as.ordered(train$B)
train$C=as.ordered(train$C)
train$D=as.ordered(train$D)
train$E=as.ordered(train$E)
train$F=as.ordered(train$F)
train$G=as.ordered(train$G)


# 2441 customers have different oldest and youngest age while groupsize = 1
# a=train[train$AgeOldest != train$AgeYoungest & train$GroupSize==1,]
# length(unique(a$CustomerID))

