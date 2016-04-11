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
