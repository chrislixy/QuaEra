set.seed(1)
list=sample(1:dim(purchase)[1],5000,replace=F)
purchaseTest=purchase[list,]
purchaseTrain=purchase[-list,]
firstviewTest=firstview[list,]
firstviewTrain=firstview[-list,]
lastviewTest=lastview[list,]
lastviewTrain=lastview[-list,]

sum(purchaseTest$A==lastviewTest$A & purchaseTest$B==lastviewTest$B & 
      purchaseTest$C==lastviewTest$C & purchaseTest$D==lastviewTest$D & 
      purchaseTest$E==lastviewTest$E & purchaseTest$F==lastviewTest$F & 
      purchaseTest$G==lastviewTest$G )/dim(purchaseTest)[1]

sum(purchaseTest$A==lastviewTest$A & purchaseTest$B==lastviewTest$B & 
      purchaseTest$C==lastviewTest$C & purchaseTest$D==lastviewTest$D & 
      purchaseTest$E==lastviewTest$E & purchaseTest$F==lastviewTest$F )/dim(purchaseTest)[1]

sum(purchaseTest$A==lastviewTest$A & purchaseTest$B==lastviewTest$B & 
      purchaseTest$C==lastviewTest$C & purchaseTest$D==lastviewTest$D & 
      purchaseTest$E==lastviewTest$E & purchaseTest$G==lastviewTest$G )/dim(purchaseTest)[1]
