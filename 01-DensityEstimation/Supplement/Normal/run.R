################################################################
# Load Libraries / Helpers
################################################################
library("ggplot2")         ###Plotting
library("patchwork")
library("Rcpp")            ###Rcpp
library("RcppArmadillo")
library("RcppDist")
library("MASS")            ###Gaussian
library("tidyverse")       ###Data Management

#helper
getDeterminantMod<-function(A){
  determinant(A,log=T)$modulus[1]
}
sourceCpp("../../../ptTest-thinning-efficient.cpp") #omnibus function
source("../../../ptTestOld.R") #omnibus function
options(scipen=2000000)

Xseq=seq(-13,13,length.out = 100)
Yseq=seq(-13,13,length.out = 100)
test<-expand.grid(Xseq,Yseq)
ns<-c(1000,500,250,50)

for(n in ns){
  ################################################################
  # Simulate Data
  ################################################################
  mu<-rep(0,2)
  c<-5
  sigma<- c*diag(2)
  
  samp2<-mvrnorm(n,mu,sigma)
  colnames(samp2)<-c("x","y")
  normaldata<-samp2[1:n,]
  
  data.for.plotting<-expand.grid(X=Xseq, Y=Yseq)
  data.for.plotting<-data.for.plotting %>%
    add_column("X1","X2","X3","X4","X5")
  
  colnames(data.for.plotting)[-c(1,2)]<-c("Average of Two Square Roots","Symmetric Square Root",
                                          "Rotated Square Root", "FAST Sampler", "MCMC Sampler")
  
  
  ################################################################
  # One/Two Rotations
  ################################################################
  pt.old1<-ptTestOLD(train=normaldata,test=test,
                     maxJ=8,
                     save=0, burnin=0,
                     test.equal.weight=F) 
  
  p.old1<- pt.old1$prb
  z1<-matrix(data=p.old1,nrow = length(Xseq),ncol = length(Yseq))
  # Dummy data
  dat1 <- expand.grid(X=Xseq, Y=Yseq)
  dat1$Z <- as.vector(z1)
  
  data.for.plotting[,3]<-dat1$Z
  
  pt.old2<-ptTestOLD(train=normaldata,test=test,
                     maxJ=8,
                     save=-1, burnin=0) 
  p.old2<- pt.old2$prb
  z2<-matrix(data=p.old2,nrow = length(Xseq),ncol = length(Yseq))
  # Dummy data
  dat2 <- expand.grid(X=Xseq, Y=Yseq)
  dat2$Z <- as.vector(z2)
  data.for.plotting[,4]<-dat2$Z
  
  pt.old3<-ptTestOLD(train=normaldata,test=test,
                     maxJ=8,
                     save=-2, burnin=0) 
  
  p.old3<- pt.old3$prb
  z3<-matrix(data=p.old3,nrow = length(Xseq),ncol = length(Yseq))
  # Dummy data
  dat3 <- expand.grid(X=Xseq, Y=Yseq)
  dat3$Z <- as.vector(z3)
  data.for.plotting[,5]<-dat3$Z
  
  ################################################################
  # FAST Sampler
  ################################################################
  #use mcmc
  burnin = 10000;
  save = 10000;
  thin = 0;
  pt.new<-ptTest(train = as.matrix(normaldata), test = as.matrix(test),
                 maxJ = 8,
                 save=save,burnin=burnin,thin=thin,
                 fast=T)
  i=20
  use= floor(save*(i*.05));
  p.new<- pt.new$pmat[,i]
  z.new<-matrix(data=p.new,nrow = length(Xseq),ncol = length(Yseq))
  # Dummy data
  dat.new <- expand.grid(X=Xseq, Y=Yseq)
  dat.new$Z <- as.vector(z.new)
  
  data.for.plotting[,6]<-dat.new$Z
  
  ################################################################
  # Full Sampler
  ################################################################
  #use mcmc
  burnin = 10000;
  thin = 0;
  save = 1000000;
  pt.new2<-ptTest(train = as.matrix(normaldata), test = as.matrix(test),
                  maxJ = 8,
                  save=save,burnin=burnin,thin=thin)
  
  i=20
  use= floor(save*(i*.05));
  p.new2<- pt.new2$pmat[,i]
  z.new2<-matrix(data=p.new2,nrow = length(Xseq),ncol = length(Yseq))
  # Dummy data
  dat.new2 <- expand.grid(X=Xseq, Y=Yseq)
  dat.new2$Z <- as.vector(z.new2)
  
  data.for.plotting[,7]<-dat.new2$Z
  
  
  data.for.plot <- data.for.plotting %>%
    dplyr::select(- "Average of Two Square Roots")%>%
    pivot_longer(cols = c("Symmetric Square Root","Rotated Square Root",
                          "FAST Sampler", "MCMC Sampler"), 
                 values_to = "Z",
                 names_to = "Method")%>%
    mutate(Method = factor(Method, levels = c("Symmetric Square Root","Rotated Square Root",
                                              "FAST Sampler", "MCMC Sampler")))
  
  ################################################################
  # Create and Save Plots
  ################################################################
  sizeaxis<- 8
  sizeaxislab<- 10
  sizetitle<- 11
  
  plot.n <- ggplot(data = data.for.plot, aes(X, Y, fill= Z)) + 
    geom_tile()+
    scale_fill_gradient("Density",low = "white", high = "black")+
    xlab(bquote(X[1]))+
    ylab(bquote(X[2]))+
    xlim(-13,13)+
    ylim(-13,13)+
    theme_bw()+
    ggtitle(paste("Gaussian Density Estimates (n=", n, ")",sep=""))+
    theme(axis.text=element_text(size=sizeaxis),
          axis.title=element_text(size=sizeaxislab),
          plot.title=element_text(size=sizetitle),
          legend.title=element_text(size=sizeaxislab),
          legend.text=element_text(size=sizeaxis),
          strip.text=element_text(size=sizeaxislab),
          legend.position = "bottom",
          legend.key.width = unit(0.75, "in"))+
    facet_wrap(.~Method, nrow = 2)
  
  pdf(paste("normal", n, ".pdf",sep=""),width=6,height=7)
  print(plot.n)
  dev.off()
}