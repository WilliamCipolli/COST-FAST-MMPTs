library(caret)

#simulate data
set.seed(7272)
n<-500
n<-500
x.poly<-runif(n, min=0,max=50)
y.poly<- (-1/50000)*x.poly^4 + (1/1000)*x.poly^3 +(1/1000)*x.poly^2 + (1/1000)*x.poly +rnorm(n,0,1)
Y.poly<- (-1/50000)*x.poly^4 + (1/1000)*x.poly^3 +(1/1000)*x.poly^2 + (1/1000)*x.poly

train<-data.frame(x=x.poly,y=y.poly)
mod.dat<-data.frame(x=x.poly,y=Y.poly)

set.seed(7272)
flds <- createFolds(train$y, k=10)

for(i in 1:10){
  dat.curr <- train 
  dat.curr$set <- rep(NA, nrow(dat.curr))
  dat.curr$set[flds[[i]]] = "test"
  dat.curr$set[-flds[[i]]] = "train"
  write.csv(x =dat.curr, file = paste("data/dat",i,".csv",sep = ""),row.names = F)
}
