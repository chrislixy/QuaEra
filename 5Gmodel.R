library(randomForest)
library(tree)

Gmodel=multinom(purchaseTrain$G~State+CarAge+RiskFactor+
                A+B+C+D+E+F+G+Cost+agediff+individual,data=lastviewTrain)
purchaseTest$Gpred=predict(Gmodel,lastviewTest)
purchaseTest$Gpred=as.ordered(purchaseTest$Gpred)

purchaseTest$Gpred[purchaseTest$State=="ND" |purchaseTest$State=="SD"]=2
purchaseTest$Gpred[purchaseTest$State=="FL" & purchaseTest$Gpred<3]=3
purchaseTest$Gpred[purchaseTest$State=="OH" & purchaseTest$Gpred<2]=3



sum(purchaseTest$A==lastviewTest$A & purchaseTest$B==lastviewTest$B & 
      purchaseTest$C==lastviewTest$C & purchaseTest$D==lastviewTest$D & 
      purchaseTest$E==lastviewTest$E & purchaseTest$F==lastviewTest$F & 
      purchaseTest$G==lastviewTest$G )/dim(purchaseTest)[1]

sum(purchaseTest$A==lastviewTest$A & purchaseTest$B==lastviewTest$B & 
      purchaseTest$C==lastviewTest$C & purchaseTest$D==lastviewTest$D & 
      purchaseTest$E==lastviewTest$E & purchaseTest$F==lastviewTest$F & 
      purchaseTest$G==purchaseTest$Gpred)/dim(purchaseTest)[1]

sum(purchaseTest$Gpred != lastviewTest$G)
sum(purchaseTest$G != lastviewTest$G)



GtuneRF=tuneRF(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$G)
GtuneRF[order(GtuneRF[,2])[1],1]
GRF=randomForest(purchaseTrain[,c(2,4:6,8:17,25:45)],purchaseTrain$G,mtry=GtuneRF[order(GtuneRF[,2])[1],1])
GRF$importance[order(-GRF$importance)[1:10],]
purchaseTest$GpredRF=predict(GRF,purchaseTest[,c(2,4:6,8:17,25:45)])
purchaseTest$GpredRF=as.ordered(purchaseTest$GpredRF)
sum(purchaseTest$GpredRF != lastviewTest$G)

sum(purchaseTest$A==lastviewTest$A & purchaseTest$B==lastviewTest$B & 
      purchaseTest$C==lastviewTest$C & purchaseTest$D==lastviewTest$D & 
      purchaseTest$E==lastviewTest$E & purchaseTest$F==lastviewTest$F & 
      purchaseTest$G==purchaseTest$GpredRF)/dim(purchaseTest)[1]
