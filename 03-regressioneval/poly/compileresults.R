library(tidyverse)
dat<-NULL

for(i in 1:10){
  datcurr<-read_csv(file = paste("pred/pred",i,".csv",sep = ""))
  dat<-rbind(dat,datcurr)
}

colnames(dat)

sqrt(mean((dat$y-dat$pt2.mean)^2))
sqrt(mean((dat$y-dat$pt2.median)^2))
sqrt(mean((dat$y-dat$pt2.mode)^2))
sqrt(mean((dat$y-dat$pt3.mean)^2))
sqrt(mean((dat$y-dat$pt3.median)^2))
sqrt(mean((dat$y-dat$pt3.mode)^2))
sqrt(mean((dat$y-dat$ann)^2))
sqrt(mean((dat$y-dat$bart)^2))
sqrt(mean((dat$y-dat$cart)^2))
sqrt(mean((dat$y-dat$gam)^2))
sqrt(mean((dat$y-dat$gbm)^2))
sqrt(mean((dat$y-dat$knn.pred)^2))
sqrt(mean((dat$y-dat$ols)^2))
sqrt(mean((dat$y-dat$svm.linpred)^2))
sqrt(mean((dat$y-dat$svm.polypred)^2))
sqrt(mean((dat$y-dat$svm.radpred)^2))
sqrt(mean((dat$y-dat$svm.sigpred)^2))

