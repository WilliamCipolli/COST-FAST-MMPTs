library(caret)

#simulate data
set.seed(7272)
n<-500
x.nl<-runif(n, min=0,max=50)
a<-15.58937  #runif(1,10,20)
b<-5.314167  #runif(1,0,10)
y.nl<-((a*x.nl)/(b+x.nl))+rnorm(n,0,1)
Y.nl<-((a*x.nl)/(b+x.nl))
train<-data.frame(x=x.nl,y=y.nl)
mod.dat<-data.frame(x=x.nl,y=Y.nl)

set.seed(7272)
flds <- createFolds(train$y, k=10)

for(i in 1:10){
  dat.curr <- train 
  dat.curr$set <- rep(NA, nrow(dat.curr))
  dat.curr$set[flds[[i]]] = "test"
  dat.curr$set[-flds[[i]]] = "train"
  write.csv(x =dat.curr, file = paste("data/dat",i,".csv",sep = ""),row.names = F)
}
