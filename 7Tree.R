library(tree)
predictors=purchaseTrain[,c(8:17,25:45)]
Atree=tree(purchaseTrain$A~.,data=predictors)
purchaseTest$ApredTree=predict(Atree,purchaseTest[,c(8:17,25:45)],type="class")
plot(Atree)
text(Atree,pretty=0)
purchaseTest$ApredTree=as.ordered(purchaseTest$ApredTree)


Btree=tree(purchaseTrain$B~.,data=predictors)
purchaseTest$BpredTree=predict(Btree,purchaseTest[,c(8:17,25:45)],type="class")
plot(Btree)
text(Btree,pretty=0)
purchaseTest$BpredTree=as.ordered(purchaseTest$BpredTree)

Ctree=tree(purchaseTrain$C~.,data=predictors)
purchaseTest$CpredTree=predict(Ctree,purchaseTest[,c(8:17,25:45)],type="class")
plot(Ctree)
text(Ctree,pretty=0)
purchaseTest$CpredTree=as.ordered(purchaseTest$CpredTree)

Dtree=tree(purchaseTrain$D~.,data=predictors)
purchaseTest$DpredTree=predict(Dtree,purchaseTest[,c(8:17,25:45)],type="class")
plot(Dtree)
text(Dtree,pretty=0)
purchaseTest$DpredTree=as.ordered(purchaseTest$DpredTree)

Etree=tree(purchaseTrain$E~.,data=predictors)
purchaseTest$EpredTree=predict(Etree,purchaseTest[,c(8:17,25:45)],type="class")
plot(Etree)
text(Etree,pretty=0)
purchaseTest$EpredTree=as.ordered(purchaseTest$EpredTree)

Ftree=tree(purchaseTrain$F~.,data=predictors)
purchaseTest$FpredTree=predict(Ftree,purchaseTest[,c(8:17,25:45)],type="class")
plot(Ftree)
text(Ftree,pretty=0)
purchaseTest$FpredTree=as.ordered(purchaseTest$FpredTree)

Gtree=tree(purchaseTrain$G~.,data=predictors)
purchaseTest$GpredTree=predict(Gtree,purchaseTest[,c(8:17,25:45)],type="class")
plot(Gtree)
text(Gtree,pretty=0)
purchaseTest$GpredTree=as.ordered(purchaseTest$GpredTree)

sum(purchaseTest$A==purchaseTest$ApredTree & purchaseTest$B==purchaseTest$BpredTree & 
      purchaseTest$C==purchaseTest$CpredTree & purchaseTest$D==purchaseTest$DpredTree & 
      purchaseTest$E==purchaseTest$EpredTree & purchaseTest$F==purchaseTest$FpredTree & 
      purchaseTest$G==purchaseTest$GpredTree)/dim(purchaseTest)[1]
