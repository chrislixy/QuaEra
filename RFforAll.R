AtuneRF=tuneRF(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$A)
AtuneRF[order(AtuneRF[,2])[1],1]
ARF=randomForest(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$A,mtry=10)
ARF$importance[order(-ARF$importance)[1:10],]
purchaseTest$ApredRF=predict(ARF,purchaseTest[,c(2,4:6,8:17,25:45)])
purchaseTest$ApredRF=as.ordered(purchaseTest$ApredRF)

BtuneRF=tuneRF(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$B)
BtuneRF[order(BtuneRF[,2])[1],1]
BRF=randomForest(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$B,mtry=BtuneRF[order(BtuneRF[,2])[1],1])
BRF$importance[order(-BRF$importance)[1:10],]
purchaseTest$BpredRF=predict(BRF,purchaseTest[,c(2,4:6,8:17,25:45)])
purchaseTest$BpredRF=as.ordered(purchaseTest$BpredRF)

CtuneRF=tuneRF(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$C)
CtuneRF[order(CtuneRF[,2])[1],1]
CRF=randomForest(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$C,mtry=CtuneRF[order(CtuneRF[,2])[1],1])
CRF$importance[order(-CRF$importance)[1:10],]
purchaseTest$CpredRF=predict(CRF,purchaseTest[,c(2,4:6,8:17,25:45)])
purchaseTest$CpredRF=as.ordered(purchaseTest$CpredRF)

DtuneRF=tuneRF(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$D)
DtuneRF[order(DtuneRF[,2])[1],1]
DRF=randomForest(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$D,mtry=DtuneRF[order(DtuneRF[,2])[1],1])
DRF$importance[order(-DRF$importance)[1:10],]
purchaseTest$DpredRF=predict(DRF,purchaseTest[,c(2,4:6,8:17,25:45)])
purchaseTest$DpredRF=as.ordered(purchaseTest$DpredRF)

EtuneRF=tuneRF(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$E)
EtuneRF[order(EtuneRF[,2])[1],1]
ERF=randomForest(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$E,mtry=EtuneRF[order(EtuneRF[,2])[1],1])
ERF$importance[order(-ERF$importance)[1:10],]
purchaseTest$EpredRF=predict(ERF,purchaseTest[,c(2,4:6,8:17,25:45)])
purchaseTest$EpredRF=as.ordered(purchaseTest$EpredRF)

FtuneRF=tuneRF(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$F)
FtuneRF[order(FtuneRF[,2])[1],1]
FRF=randomForest(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$F,mtry=FtuneRF[order(FtuneRF[,2])[1],1])
FRF$importance[order(-FRF$importance)[1:10],]
purchaseTest$FpredRF=predict(FRF,purchaseTest[,c(2,4:6,8:17,25:45)])
purchaseTest$FpredRF=as.ordered(purchaseTest$FpredRF)


sum(purchaseTest$A==purchaseTest$ApredRF & purchaseTest$B==purchaseTest$BpredRF & 
      purchaseTest$C==purchaseTest$CpredRF & purchaseTest$D==purchaseTest$DpredRF & 
      purchaseTest$E==purchaseTest$EpredRF & purchaseTest$F==purchaseTest$FpredRF & 
      purchaseTest$G==purchaseTest$GpredRF)/dim(purchaseTest)[1]