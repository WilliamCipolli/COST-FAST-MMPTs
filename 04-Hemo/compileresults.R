library(tidyverse)
dat<-NULL

for(i in 1:10){
  datcurr<-read_csv(file = paste("pred/pred",i,".csv",sep = ""))
  dat<-rbind(dat,datcurr)
}

colnames(dat)

sqrt(mean((dat$CHHB-dat$pt2.mean)^2))
sqrt(mean((dat$CHHB-dat$pt2.median)^2))
sqrt(mean((dat$CHHB-dat$pt2.mode)^2))
sqrt(mean((dat$CHHB-dat$pt3.mean)^2))
sqrt(mean((dat$CHHB-dat$pt3.median)^2))
sqrt(mean((dat$CHHB-dat$pt3.mode)^2))
sqrt(mean((dat$CHHB-dat$ann)^2))
sqrt(mean((dat$CHHB-dat$bart)^2))
sqrt(mean((dat$CHHB-dat$cart)^2))
sqrt(mean((dat$CHHB-dat$gam)^2))
sqrt(mean((dat$CHHB-dat$gbm)^2))
sqrt(mean((dat$CHHB-dat$knn.pred)^2))
sqrt(mean((dat$CHHB-dat$ols)^2))
sqrt(mean((dat$CHHB-dat$svm.linpred)^2))
sqrt(mean((dat$CHHB-dat$svm.polypred)^2))
sqrt(mean((dat$CHHB-dat$svm.radpred)^2))
sqrt(mean((dat$CHHB-dat$svm.sigpred)^2))
