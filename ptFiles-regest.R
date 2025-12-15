#########################################################
## Run on cluster (I had to run this so R could find Java)
#########################################################
# R CMD javareconf -e

#########################################################
## Libraries and Helpers
#########################################################
library("MASS")           ###Multivariate Normal
library("mvtnorm")        ###Multivariate Normal
library("MCMCpack")       ###Wishart
library("ggplot2")        ###Plotting
library("patchwork")
library("Rcpp")           ###Rcpp
library("RcppArmadillo") 
library("RcppDist")
library("Bolstad2")       ###reimann summs
library("tidyverse")      ###Data

# Comparison Models
library("quantreg")
library("e1071") ##svm
library("nnet") ##Neural Net Package
library("caret") #knnreg
library("gbm")
library("mgcv")  #gam
library("bartMachine") #bart
library("rpart") #cart

#helper function passed to RCCP
getDeterminantMod<-function(A){
  determinant(A,log=T)$modulus[1]
}

sourceCpp(paste(strsplit(getwd(), "crand")[[1]][1], "crand/ptTest-thinning-efficient.cpp", sep="")) #omnibus function
source(paste(strsplit(getwd(), "crand")[[1]][1], "crand/ptTestOld.R", sep="")) #omnibus function
source(paste(strsplit(getwd(), "crand")[[1]][1], "crand/convertData.R", sep=""))   ###Convert data
options(scipen=2000000)

# Mean Regression
pt.mean<-function(row, dens){
  marg <- dens %>% 
    filter(x==row[1])
  # mean<- sum(marg$y * marg$z)/sum(marg$z)
  # mean
  rint <- sintegral(marg$y, marg$z, n.pts = length(marg))                  # f_y(y) 
  exint <- sintegral(marg$y, marg$y*marg$z/rint$int, n.pts = length(marg)) # E(V|s)
  exint$int #MEAN
}

# Quantile Regression
pt.quantile<-function(row, dens, tau){
  marg <- dens %>% 
    filter(x==row[1])
  rint<-sintegral(x = marg$y, fx = marg$z, n.pts = nrow(marg))
  if(rint$int==0){
    NA
  }else{
    marg$cumprb<-rint$y/rint$int
    quantile.index <- which(marg$cumprb >= tau)[1]
    if(marg$cumprb[quantile.index] == tau){
      quantile = marg$y[quantile.index]
    }else{
      quantile = (marg$z[quantile.index-1] * marg$y[quantile.index-1] + marg$z[quantile.index] * marg$y[quantile.index])/
        (marg$z[quantile.index-1] + marg$z[quantile.index])
    }
    quantile
  }
}

# Most Likely Regression
pt.mode<-function(row, dens){
  marg <- dens %>% 
    filter(x==row[1]) 
  ind <- which(marg$z == max(marg$z))
  if(length(ind)==1){
    mode <- marg$y[which(marg$z == max(marg$z))]
  }else{
    mode <- median(marg$y[which(marg$z == max(marg$z))])
  }
  mode
}

#########################################################
## Function to fit models
#########################################################
regest<-function(train, test){
  #########################################################
  ## FAST SAMPLER
  #########################################################
  burnin = 10000;
  save = 10000;
  thin = 0;
  Xseq=test[,1]
  Yseq=seq(min(c(train$y, test$y)),max(c(train$y,test$y)), length.out=500)
  grid<-expand.grid(Xseq,Yseq)
  
  pt2<-ptTest(train = as.matrix(train), test = as.matrix(grid),
              maxJ = 8,
              save=save,burnin=burnin,thin=thin,
              fast=T)
  p2<- pt2$pvec
  z2<-matrix(data=p2,nrow = length(Xseq), ncol = length(Yseq))
  # Dummy data
  res2<-cbind(grid, as.vector(z2))
  colnames(res2)<-c("x","y","z")
  pt2.mean<-sapply(X = test$x, FUN = pt.mean, dens=res2)
  pt2.median<-sapply(X = test$x, FUN = pt.quantile, dens=res2, tau=0.50)
  pt2.max<-sapply(X = test$x, FUN = pt.mode, dens=res2)
  
  #########################################################
  ## Full MCMC Sampler
  #########################################################
  burnin = 10000;
  thin = 0;
  save = 1000000;
  pt3<-ptTest(train = as.matrix(train), test = as.matrix(grid),
              maxJ = 8,
              save=save,burnin=burnin,thin=thin)
  p3<- pt3$pvec
  z3<-matrix(data=p3,nrow = length(Xseq), ncol = length(Yseq))
  # Dummy data
  res3<-cbind(grid, as.vector(z3))
  colnames(res3)<-c("x","y","z")
  
  pt3.mean<-sapply(X = test$x, FUN = pt.mean, dens=res3)
  pt3.median<-sapply(X = test$x, FUN = pt.quantile, dens=res3, tau=0.50)
  pt3.max<-sapply(X = test$x, FUN = pt.mode, dens=res3)
  
  #########################################################
  ## Fit Competitor Models
  #########################################################
  ## lm
  ols.mod <- lm(y~x, data = train)
  ols.pred <- predict(ols.mod, newdata = test)

  cat("ols")
    
  ## quantile
  quant.mod <- rq(y~x, data=train)
  quant.pred <- predict(quant.mod, newdata=test)

  cat("quant")
    
  # svm linear
  svm.linmod <- svm(y~x, data = train, kernel = "linear")
  svm.linpred <- predict(svm.linmod, newdata = test)
  
  cat("svm1")
    
  # svm polu
  svm.polymod <- svm(y~x, data = train, kernel = "polynomial")
  svm.polypred <- predict(svm.polymod, newdata = test)
    
  cat("svm2")
    
  # svm radial
  svm.radmod <- svm(y~x, data = train, kernel = "radial")
  svm.radpred <- predict(svm.radmod, newdata = test)
    
  cat("svm3")
    
  # svm sigmoid
  svm.sigmod <- svm(y~x, data = train, kernel = "sigmoid")
  svm.sigpred <- predict(svm.sigmod, newdata = test)
    
  cat("svm4")
    
  # ANN
  range01 <- function(x){2*(x-min(x))/(max(x)-min(x))-1}
  unrange <- function(x,train){0.50 * (min(train)*(-x) + min(train) + max(train)*x + max(train)) }
  #x *(max(train)-min(train))+min(train)}
  
  anntrain <- train
  anntrain$y <- range01(anntrain$y)
  d<-ncol(train)-1
  annSize<-ceiling(2*d/3)
  ann.mod <- nnet(y ~ x, anntrain, size=annSize, linout=T)
  ann.pred <- predict(ann.mod, newdata = test)
  ann.pred <- unrange(ann.pred, train$y)
 
  cat("ann")
    
  # KNN
  k<- sqrt(nrow(train))
  library(caret)
  knn.mod <- knnreg(y ~ x, data=train, k=ceiling(k))
  knn.pred<-predict(knn.mod, newdata = test)
  
  cat("knn")
    
  # GBoost
  gbm.mod<-gbm(y~x, data = train) # 100 trees
  gbm.pred<-predict(gbm.mod, newdata = test)
  cat("gbm")
    
  #GAM (spline)
  gam.mod <- gam(y ~ s(x), data = train)
  gam.pred <- predict(gam.mod, newdata = test)

  cat("gam")
    
  #BART
  test.x<-test %>% dplyr::select(x)
  bm<-bartMachine(data.frame(x=train$x),train$y,num_trees = 100)
  bart.pred<-predict(bm, test.x)
  cat("bard")
    
  #Cart
  cart.model<-rpart(y ~ x, data=train)
  cart.pred<-predict(cart.model, test)
  cat("cart")
    
  #########################################################
  ## Save and Return Results
  #########################################################
  res.dat<-data.frame(pt2.mean=pt2.mean,
                      pt2.median=pt2.median,
                      pt2.mode=pt2.max,
                      pt3.mean=pt3.mean,
                      pt3.median=pt3.median,
                      pt3.mode=pt3.max,
                      ols= ols.pred,
                      qr = quant.pred,
                      svm.linpred = svm.linpred,
                      svm.polypred = svm.polypred,
                      svm.radpred = svm.radpred, 
                      svm.sigpred = svm.sigpred,
                      ann = ann.pred,
                      knn.pred = knn.pred,
                      gbm = gbm.pred,
                      gam = gam.pred,
                      bart = bart.pred,
                      cart = cart.pred)
  res.dat
}
