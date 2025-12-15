library(tidyverse)
dat<-NULL

dat<-read_csv(file ="pred/pred.csv")

sqrt(mean((dat$MEDV-dat$pt2.mean)^2))
sqrt(mean((dat$MEDV-dat$pt2.median)^2))
sqrt(mean((dat$MEDV-dat$pt2.mode)^2))
sqrt(mean((dat$MEDV-dat$pt3.mean)^2))
sqrt(mean((dat$MEDV-dat$pt3.median)^2))
sqrt(mean((dat$MEDV-dat$pt3.mode)^2))
sqrt(mean((dat$MEDV-dat$ann)^2))
sqrt(mean((dat$MEDV-dat$bart)^2))
sqrt(mean((dat$MEDV-dat$cart)^2))
sqrt(mean((dat$MEDV-dat$gam)^2))
sqrt(mean((dat$MEDV-dat$gbm)^2))
sqrt(mean((dat$MEDV-dat$knn.pred)^2))
sqrt(mean((dat$MEDV-dat$ols)^2))
sqrt(mean((dat$MEDV-dat$svm.linpred)^2))
sqrt(mean((dat$MEDV-dat$svm.polypred)^2))
sqrt(mean((dat$MEDV-dat$svm.radpred)^2))
sqrt(mean((dat$MEDV-dat$svm.sigpred)^2))

