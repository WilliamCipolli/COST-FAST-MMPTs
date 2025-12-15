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
source("/home/wcipolli/PTnew/crand/convertData.R")   ###Convert data
library("Rcpp")           ###Rcpp
library("RcppArmadillo") 
library("RcppDist")
library("Bolstad2")       ###reimann summs
library("tidyverse")      ###Data

#helper
getDeterminantMod<-function(A){
  determinant(A,log=T)$modulus[1]
}
sourceCpp("/home/wcipolli/PTnew/crand/ptTest-thinning-efficient.cpp") #omnibus function
source("/home/wcipolli/PTnew/crand/ptTestOld.R") #omnibus function
options(scipen=2000000)


# Mean Regression
pt.mean<-function(row, dens){
  marg <- dens %>% 
    filter(x==row[1])
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
reglines<-function(x,y){
  test <- data.frame(x=seq(min(x),max(x),length.out=100))
  
  #########################################################
  ## FAST Sampler
  #########################################################
  burnin = 10000;
  save = 10000;
  thin = 0;
  train = data.frame(x=x,y=y)
  Xseq=test[,1]
  Yseq=seq(min(y),max(y),length.out = 100)
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
  ## Return predictions and densities
  #########################################################
  res.dat<-data.frame(pt2.mean=pt2.mean,
                      pt2.median=pt2.median,
                      pt2.mode=pt2.max,
                      pt3.mean=pt3.mean,
                      pt3.median=pt3.median,
                      pt3.mode=pt3.max)
  
  list(results=res.dat,
       ptfast=res2,
       ptmcmc=res3)
}

