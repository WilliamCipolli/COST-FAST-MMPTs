# R CMD javareconf -e
#########################################################
#######################Helpers###########################
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
library("tidyverse")        ###Data

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

sourceCpp("../../ptTest-thinning-efficient.cpp") #omnibus function
source("../../ptTestOld.R") #omnibus function
source("../../convertData.R")   ###Convert data
options(scipen=2000000)

# Mean Regression
pt.mean<-function(row, dens){
  var.names <- colnames(dens)
  var.names<-var.names[-1]
  var.names<-var.names[-length(var.names)]
  
  marg <- dens
  for(i in 1:length(row)){
    marg<- marg %>% filter(!is.na(match(.data[[var.names[i]]],row[var.names[i]])))
  }
  
  # mean<- sum(marg$y * marg$z)/sum(marg$z)
  # mean
  rint <- sintegral(marg$y, marg$p, n.pts = length(marg))                  # f_y(y)   if(rint$int==0){
  if(rint$int==0){
    NA
  }else{
    exint <- sintegral(marg$y, marg$y*marg$p/rint$int, n.pts = length(marg)) # E(V|s)
    exint$int #MEAN
  }
}

# Quantile Regression
pt.quantile<-function(row, dens, tau){
  var.names <- colnames(dens)
  var.names<-var.names[-1]
  var.names<-var.names[-length(var.names)]
  
  marg <- dens
  for(i in 1:length(row)){
    marg<- marg %>% filter(!is.na(match(.data[[var.names[i]]],row[var.names[i]])))
  }
  
  rint<-sintegral(x = marg$y, fx = marg$p, n.pts = nrow(marg))
  if(rint$int==0){
    NA
  }else{
    marg$cumprb<-rint$y/rint$int
    quantile.index <- which(marg$cumprb >= tau)[1]
    if(marg$cumprb[quantile.index] == tau){
      quantile = marg$y[quantile.index]
    }else{
      # quantile = (marg$p[quantile.index-1] * marg$y[quantile.index-1] + marg$p[quantile.index] * marg$y[quantile.index])/
      #   (marg$p[quantile.index-1] + marg$p[quantile.index])
      quantile = (abs(tau - marg$cumprb[quantile.index-1])*marg$y[quantile.index-1] + (abs(tau - marg$cumprb[quantile.index])*marg$y[quantile.index])) /
        (abs(tau - marg$cumprb[quantile.index-1])+abs(tau - marg$cumprb[quantile.index]))
    }
    quantile
  }
}

# Most Likely Regression
pt.mode<-function(row, dens){
  var.names <- colnames(dens)
  var.names<-var.names[-1]
  var.names<-var.names[-length(var.names)]
  
  marg <- dens
  for(i in 1:length(row)){
    marg<- marg %>% filter(!is.na(match(.data[[var.names[i]]],row[var.names[i]])))
  }
  
  ind <- which(marg$p == max(marg$p))
  if(length(ind)==1){
    mode <- marg$y[which(marg$p == max(marg$p))]
  }else{
    mode <- median(marg$y[which(marg$p == max(marg$p))])
  }
  mode
}

###########################################################################
###########################################################################
# Load Data
###########################################################################
###########################################################################
train<-read.csv(file = "data/dat.csv",header = T)
train.cat<-read.csv(file = "data/datcat.csv",header = T)

curr.train<-train %>% filter(set=="train") %>% dplyr::select(!set)
curr.test<-train %>% filter(set=="test") %>% dplyr::select(!set)

curr.train.cat<-train.cat %>% filter(set=="train") %>% dplyr::select(!set)
curr.test.cat<-train.cat %>% filter(set=="test") %>% dplyr::select(!set)

curr.train.cat <- curr.train.cat  %>%
  mutate(sex = factor(sex),
         cp = factor(cp),
         fbs = factor(fbs),
         restecg = factor(restecg),
         exang = factor(exang),
         slope = factor(slope),
         thal = factor(thal))

curr.test.cat <- curr.test.cat  %>%
  mutate(sex = factor(sex),
         cp = factor(cp),
         fbs = factor(fbs),
         restecg = factor(restecg),
         exang = factor(exang),
         slope = factor(slope),
         thal = factor(thal))

train = curr.train
test = curr.test
traincat = curr.train.cat
testcat = curr.test.cat


###########################################################################
###########################################################################
# Fix Models
###########################################################################
###########################################################################
cnames<-colnames(train)

# train is a data frame 1st col y, rest x
train <- train %>%
  rename(y = 1) %>%
  rename_with(~ str_c("x", seq_along(.)), -1)

test <- test %>%
  rename(y = 1) %>%
  rename_with(~ str_c("x", seq_along(.)),-1)

trainx <- train %>%
  dplyr::select(starts_with("x"))

testx <- test %>%
  dplyr::select(starts_with("x"))

trainy <- train[,"y"] 

# train is a data frame 1st col y, rest x
traincat <- traincat %>%
  rename(y = 1) %>%
  rename_with(~ str_c("x", seq_along(.)), -1)

testcat <- testcat %>%
  rename(y = 1) %>%
  rename_with(~ str_c("x", seq_along(.)), -1)

trainxcat <- traincat %>%
  dplyr::select(starts_with("x"))

testxcat <- testcat %>%
  dplyr::select(starts_with("x"))

trainycat <- traincat[,"y"] 

frmla <- "y ~ x1"
gamfrmla <- "y ~ s(x1)"
if(length(cnames)>2){
  for(i in 2:(length(cnames)-1)){
    frmla <- paste(frmla, " + x" , i, sep="")
    if(i==11 | i==13){
      gamfrmla <- paste(gamfrmla, " + x" , i, sep="") # fix for observations that arent continuous enough
    }else{
      gamfrmla <- paste(gamfrmla, " + s(x" , i, ")", sep="") 
    }
  }    
}
frmla <- as.formula(frmla)
gamfrmla <- as.formula(gamfrmla)


## PT Fast
burnin = 10000;
save = 10000;
thin = 0;

Xseq=testx
#grid extends 10% beyond minimum and maximum
Yseq=seq(min(train$y)-.1*diff(range(train$y)),max(train$y)+.1*diff(range(train$y)),  length.out=500)
grid<-merge(Yseq,Xseq)
colnames(grid)[1]<-"y"

pt2<-ptTest(train = as.matrix(train), test = as.matrix(grid),
            maxJ = 8,
            save=save,burnin=burnin,thin=thin,
            fast=T)
p2<- pt2$pvec
z2<-matrix(data=p2, nrow=nrow(grid), ncol = 1)
# Dummy data
res2<-cbind(grid, as.vector(z2))

colnames(res2)[ncol(res2)]<-"p"

pt2.mean<-apply(X = testx, MARGIN = 1, FUN = pt.mean, dens=res2)
pt2.median<-apply(X = testx, MARGIN = 1, FUN = pt.quantile, dens=res2, tau=0.50)
pt2.max<-apply(X = testx, MARGIN=1, FUN = pt.mode, dens=res2)

## PT honest
burnin = 10000;
thin = 0;
save = 1000000;
pt3<-ptTest(train = as.matrix(train), test = as.matrix(grid),
            maxJ = 8,
            save=save,burnin=burnin,thin=thin)
p3<- pt3$pvec
z3<-matrix(data=p3, nrow=nrow(grid), ncol = 1)
# Dummy data
res3<-cbind(grid, as.vector(z3))

colnames(res3)[ncol(res3)]<-"p"

pt3.mean<-apply(X = testx, MARGIN = 1, FUN = pt.mean, dens=res3)
pt3.median<-apply(X = testx, MARGIN = 1, FUN = pt.quantile, dens=res3, tau=0.50)
pt3.max<-apply(X = testx, MARGIN=1, FUN = pt.mode, dens=res3)

## lm
ols.mod <- lm(frmla, data = traincat)
ols.pred <- predict(ols.mod, newdata = testcat)

## quantile
quant.mod <- rq(frmla, data=traincat)
quant.pred <- predict(quant.mod, newdata=testxcat)

# svm linear
svm.linmod <- svm(frmla, data = traincat, kernel = "linear")
svm.linpred <- predict(svm.linmod, newdata = testxcat)

# svm polu
svm.polymod <- svm(frmla, data = traincat, kernel = "polynomial")
svm.polypred <- predict(svm.polymod, newdata = testxcat)

# svm radial
svm.radmod <- svm(frmla, data = traincat, kernel = "radial")
svm.radpred <- predict(svm.radmod, newdata = testxcat)

# svm sigmoid
svm.sigmod <- svm(frmla, data = traincat, kernel = "sigmoid")
svm.sigpred <- predict(svm.sigmod, newdata = testxcat)

# ANN
range01 <- function(x){2*(x-min(x))/(max(x)-min(x))-1}
unrange <- function(x,train){0.50 * (min(train)*(-x) + min(train) + max(train)*x + max(train)) }

anntrain <- train
anntrain$y <- range01(anntrain$y)
d<-ncol(train)-1
annSize<-ceiling(2*d/3)
ann.mod <- nnet(frmla, anntrain, size=annSize, linout=T)
ann.pred <- predict(ann.mod, newdata = testx,type = "raw")
ann.pred <- unrange(ann.pred, train$y)

# KNN
k<- sqrt(nrow(train))
knn.mod <- knnreg(frmla, data=train, k=ceiling(k))
knn.pred<-predict(knn.mod, newdata = testx)

# GBoost
gbm.mod<-gbm(frmla, data = traincat) # 100 trees
gbm.pred<-predict(gbm.mod, newdata = testxcat)

#GAM (spline)
gam.mod <- gam(gamfrmla, data = train)
gam.pred <- predict(gam.mod, newdata = testx)

#Local regression
if(ncol(trainx) <=4){
  loess.mod <- loess(frmla, data=train) # 75% smoothing span (default)
  loess.pred <- predict(loess.mod, newdata = testx)
}else{
  loess.pred <- rep(NA, nrow(testx))
}

#BART
bm<-bartMachine(trainxcat,trainycat,num_trees = 100)
bart.pred<-predict(bm, testxcat)

#Cart
cart.model<-rpart(frmla, data=traincat)
cart.pred<-predict(cart.model, testxcat)

#return
curr.regest<-data.frame(pt2.mean=pt2.mean,
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
                        loess = loess.pred,
                        bart = bart.pred,
                        cart = cart.pred)


curr.output <- curr.test.cat %>% 
  mutate(id = rownames(curr.test)) %>%
  dplyr::select(c(id, colnames(curr.test))) %>%
  bind_cols(curr.regest) 

write.csv(x = curr.output, file = "pred/pred.csv",row.names = F)
