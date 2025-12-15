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
source("../../convertData.R") #Convert data
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
  if(is.na(rint$int)|rint$int==0){
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
  if(is.na(rint$int)|rint$int==0){
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

curr.train.cat <- curr.train.cat %>%
  mutate(school = factor(school),
         sex    = factor(sex),
         address= factor(address),
         famsize= factor(famsize),
         Pstatus= factor(Pstatus),
         Mjob   = factor(Mjob),
         Fjob   = factor(Fjob),
         reason = factor(reason),
         guardian=factor(guardian),
         schoolsup=factor(schoolsup),
         famsup=factor(famsup),
         paid=factor(paid),
         activities=factor(activities),
         nursery=factor(nursery),
         higher=factor(higher),
         internet=factor(internet),
         romantic=factor(romantic))

curr.test.cat <- curr.test.cat %>%
  mutate(school = factor(school),
         sex    = factor(sex),
         address= factor(address),
         famsize= factor(famsize),
         Pstatus= factor(Pstatus),
         Mjob   = factor(Mjob),
         Fjob   = factor(Fjob),
         reason = factor(reason),
         guardian=factor(guardian),
         schoolsup=factor(schoolsup),
         famsup=factor(famsup),
         paid=factor(paid),
         activities=factor(activities),
         nursery=factor(nursery),
         higher=factor(higher),
         internet=factor(internet),
         romantic=factor(romantic))

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

for(prop in c(0, 0.01, 0.025, seq(0.05, 1, 0.05))){

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
				cpar1 = 1000,
				cpar2 = 1,
				csamp = 1,
				fast=T, propaccept=prop)
	p2<- pt2$pvec
	z2<-matrix(data=p2, nrow=nrow(grid), ncol = 1)
	# Dummy data
	res2<-cbind(grid, as.vector(z2))

	colnames(res2)[ncol(res2)]<-"p"

	pt2.mean<-apply(X = testx, MARGIN = 1, FUN = pt.mean, dens=res2)
	pt2.median<-apply(X = testx, MARGIN = 1, FUN = pt.quantile, dens=res2, tau=0.50)
	pt2.max<-apply(X = testx, MARGIN=1, FUN = pt.mode, dens=res2)

	#return
	curr.regest<-data.frame(y= test$y,
	                        pt2.mean=pt2.mean,
	                        pt2.median=pt2.median,
	                        pt2.mode=pt2.max)

	write.csv(x = curr.regest, file = 
		paste("pred/pred-", gsub(x = sprintf("%.2f", prop), pattern = "[.]",  replacement = "_"), ".csv", sep=""),
		row.names = F)
}