library(caret)

#simulate data
set.seed(7272)
n<-500
x.ols<-runif(n, min=0,max=50)
y.ols<- (1/3)*x.ols +rnorm(n,0,1)
Y.ols<-(1/3)*x.ols

train<-data.frame(x=x.ols,y=y.ols)
mod.dat<-data.frame(x=x.ols,y=Y.ols)

set.seed(7272)
flds <- createFolds(train$y, k=10)

for(i in 1:10){
  dat.curr <- train 
  dat.curr$set <- rep(NA, nrow(dat.curr))
  dat.curr$set[flds[[i]]] = "test"
  dat.curr$set[-flds[[i]]] = "train"
  write.csv(x =dat.curr, file = paste("data/dat",i,".csv",sep = ""),row.names = F)
}
