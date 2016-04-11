set.seed(1)
list=sample(1:dim(purchase)[1],5000,replace=F)
purchase$SA=secondlastview$A
purchase$SB=secondlastview$B
purchase$SC=secondlastview$C
purchase$SD=secondlastview$D
purchase$SE=secondlastview$E
purchase$SF=secondlastview$F
purchase$SG=secondlastview$G

purchase$LA=lastview$A
purchase$LB=lastview$B
purchase$LC=lastview$C
purchase$LD=lastview$D
purchase$LE=lastview$E
purchase$LF=lastview$F
purchase$LG=lastview$G

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
