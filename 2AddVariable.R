# Add some related variables

train$weekend=as.factor(ifelse(train$Day %in% 0:4, "Weekday", "Weekend"))
train$timeofday=as.factor(ifelse(train$Time %in% 6:11, "Morning",
                                 ifelse(train$Time %in% 12:18, "Afternoon",
                                 ifelse(train$Time %in% 19:23 , "Evening", "Night"))))
# by calculating the difference in oldest and youngest, and munually set agediff=0 for 1 customer,
# avoid the problem of different age for only 1 person (problem for 2193 customers)
train$agediff=train$AgeOldest-train$AgeYoungest
train$agediff[train$AgeOldest != train$AgeYoungest & train$GroupSize==1]=0
train$family=as.factor(ifelse(train$Married==1 & train$agediff>15 & train$GroupSize>=2, "Yes","No"))
train$couple=as.factor(ifelse(train$Married==1 & train$agediff<=15 & train$GroupSize==2, "Yes","No"))
train$individual=as.factor(ifelse(train$GroupSize==1, "Yes","No"))


purchase=train[train$RecordType==1,]
lastview=train[as.numeric(row.names(purchase))-1,]
secondlastview=train[as.numeric(row.names(purchase))-2,]
firstview=train[train$ShoppingPt==1,]
hist(purchase$ShoppingPt,breaks=11,main="Histogram of Number of Coverage Combinations Viewed \n at the Time of Puchase",xlab="Shopping Points")


# too slow
#a=sapply(1:dim(purchase)[1], function(x) {which(train$CustomerID %in% purchase$CustomerID[x] &
#                                    train$ShoppingPt==which.min(train$Cost[train$CustomerID %in% purchase$CustomerID[x] &
#                                                               train$RecordType==0]))})
#lowestcost=train[a,]

