library(caret)

#simulate data
n<-500
a=0.2
d=1.4
h=3
s=0.05
w=0.02
set.seed(7272)
x.heart<-runif(n, min=-3,max=3)
y.heart<- a*(exp((-(x.heart + d)^2) / (2*w)) + exp((-(x.heart - d)^2) / (2*w))) + (h - abs(x.heart / s) - x.heart) * exp((-(7*x.heart)^2) / 2)*8 + rnorm(n,0,0.5)
Y.heart<- a*(exp((-(x.heart + d)^2) / (2*w)) + exp((-(x.heart - d)^2) / (2*w))) + (h - abs(x.heart / s) - x.heart) * exp((-(7*x.heart)^2) / 2)*8

train<-data.frame(x=x.heart,y=y.heart)
mod.dat<-data.frame(x=x.heart,y=Y.heart)
set.seed(7272)
flds <- createFolds(train$y, k=10)

for(i in 1:10){
  dat.curr <- train 
  dat.curr$set <- rep(NA, nrow(dat.curr))
  dat.curr$set[flds[[i]]] = "test"
  dat.curr$set[-flds[[i]]] = "train"
  write.csv(x =dat.curr, file = paste("data/dat",i,".csv",sep = ""),row.names = F)
}
