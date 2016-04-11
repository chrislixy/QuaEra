purchase$change=1
purchase$change[purchase$A==lastview$A & purchase$B==lastview$B & 
                  purchase$C==lastview$C & purchase$D==lastview$D & 
                  purchase$E==lastview$E & purchase$F==lastview$F & 
                  purchase$G==lastview$G]=0

purchaseTest=purchase[list,]
purchaseTrain=purchase[-list,]

changeRF=tuneRF(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$change)
changeRF=randomForest(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$A,mtry=AtuneRF[order(AtuneRF[,2])[1],1])
changeRF$importance[order(-ARF$importance)[1:10],]
purchaseTest$changepredRF=predict(changeRF,purchaseTest[,c(2,4:6,8:17,25:45)])

sum(purchaseTest$changepredRF==purchaseTest$change)/dim(purchaseTest)[1]


library(glmnet)
predictors=data.matrix(purchaseTrain[,c(2,4:6,8:17,25:45)])
response=as.matrix(purchaseTrain$change)
changeLog=glmnet(predictors,response,family="binomial")

testpredictors=data.matrix(purchaseTest[,c(2,4:6,8:17,25:45)])
purchaseTest$changeprob=predict(changeLog,testpredictors,type="response", s=0.01,)
purchaseTest$changepred=sapply(1:5000, function(x){ return(rbinom(1,1,prob= purchaseTest$changeprob[x]))})
sum(purchaseTest$changepred==purchaseTest$change)/5000

list=which(purchaseTest)

# RF model for changed people

AtuneRF=tuneRF(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$A[which(purchaseTrain$change==1)])
AtuneRF[order(AtuneRF[,2])[1],1]
ARF=randomForest(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$A[which(purchaseTrain$change==1)],mtry=AtuneRF[order(AtuneRF[,2])[1],1])
ARF$importance[order(-ARF$importance)[1:10],]
purchaseTest$ApredRF=lastviewTest$A
purchaseTest$ApredRF[which(purchaseTest$changepred==1)]=predict(ARF,purchaseTest[which(purchaseTest$changepred==1),c(2,4:6,8:17,25:45)])
purchaseTest$ApredRF=as.ordered(purchaseTest$ApredRF)

BtuneRF=tuneRF(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$B[which(purchaseTrain$change==1)])
BtuneRF[order(BtuneRF[,2])[1],1]
BRF=randomForest(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$B[which(purchaseTrain$change==1)],mtry=BtuneRF[order(BtuneRF[,2])[1],1])
BRF$importance[order(-BRF$importance)[1:10],]
purchaseTest$BpredRF=lastviewTest$B
purchaseTest$BpredRF[which(purchaseTest$changepred==1)]=predict(BRF,purchaseTest[which(purchaseTest$changepred==1),c(2,4:6,8:17,25:45)])
purchaseTest$BpredRF=as.ordered(purchaseTest$BpredRF)

CtuneRF=tuneRF(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$C[which(purchaseTrain$change==1)])
CtuneRF[order(CtuneRF[,2])[1],1]
CRF=randomForest(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$C[which(purchaseTrain$change==1)],mtry=CtuneRF[order(CtuneRF[,2])[1],1])
CRF$importance[order(-CRF$importance)[1:10],]
purchaseTest$CpredRF=lastviewTest$C
purchaseTest$CpredRF[which(purchaseTest$changepred==1)]=predict(CRF,purchaseTest[which(purchaseTest$changepred==1),c(2,4:6,8:17,25:45)])
purchaseTest$CpredRF=as.ordered(purchaseTest$CpredRF)

DtuneRF=tuneRF(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$D[which(purchaseTrain$change==1)])
DtuneRF[order(DtuneRF[,2])[1],1]
DRF=randomForest(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$D[which(purchaseTrain$change==1)],mtry=DtuneRF[order(DtuneRF[,2])[1],1])
DRF$importance[order(-DRF$importance)[1:10],]
purchaseTest$DpredRF=lastviewTest$D
purchaseTest$DpredRF[which(purchaseTest$changepred==1)]=predict(DRF,purchaseTest[which(purchaseTest$changepred==1),c(2,4:6,8:17,25:45)])
purchaseTest$DpredRF=as.ordered(purchaseTest$DpredRF)

EtuneRF=tuneRF(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$E[which(purchaseTrain$change==1)])
EtuneRF[order(EtuneRF[,2])[1],1]
ERF=randomForest(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$E[which(purchaseTrain$change==1)],mtry=EtuneRF[order(EtuneRF[,2])[1],1])
ERF$importance[order(-ERF$importance)[1:10],]
purchaseTest$EpredRF=lastviewTest$E
purchaseTest$EpredRF[which(purchaseTest$changepred==1)]=predict(ERF,purchaseTest[which(purchaseTest$changepred==1),c(2,4:6,8:17,25:45)])
purchaseTest$EpredRF=as.ordered(purchaseTest$EpredRF)

FtuneRF=tuneRF(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$F[which(purchaseTrain$change==1)])
FtuneRF[order(FtuneRF[,2])[1],1]
FRF=randomForest(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$F[which(purchaseTrain$change==1)],mtry=FtuneRF[order(FtuneRF[,2])[1],1])
FRF$importance[order(-FRF$importance)[1:10],]
purchaseTest$FpredRF=lastviewTest$F
purchaseTest$FpredRF[which(purchaseTest$changepred==1)]=predict(FRF,purchaseTest[which(purchaseTest$changepred==1),c(2,4:6,8:17,25:45)])
purchaseTest$FpredRF=as.ordered(purchaseTest$FpredRF)

GtuneRF=tuneRF(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$G[which(purchaseTrain$change==1)])
GtuneRF[order(GtuneRF[,2])[1],1]
GRF=randomForest(purchaseTrain[which(purchaseTrain$change==1),c(2,4:6,8:17,25:45)],purchaseTrain$G[which(purchaseTrain$change==1)],mtry=GtuneRF[order(GtuneRF[,2])[1],1])
GRF$importance[order(-GRF$importance)[1:10],]
purchaseTest$GpredRF=lastviewTest$G
purchaseTest$GpredRF[which(purchaseTest$changepred==1)]=predict(GRF,purchaseTest[which(purchaseTest$changepred==1),c(2,4:6,8:17,25:45)])
purchaseTest$GpredRF=as.ordered(purchaseTest$GpredRF)


sum(purchaseTest$A==purchaseTest$ApredRF & purchaseTest$B==purchaseTest$BpredRF & 
      purchaseTest$C==purchaseTest$CpredRF & purchaseTest$D==purchaseTest$DpredRF & 
      purchaseTest$E==purchaseTest$EpredRF & purchaseTest$F==purchaseTest$FpredRF & 
      purchaseTest$G==purchaseTest$GpredRF )/dim(purchaseTest)[1]


