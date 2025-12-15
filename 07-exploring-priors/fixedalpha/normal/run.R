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

set.seed(7272)

################################################################
# Simulate Data
################################################################
Xseq=seq(-13,13,length.out = 100)
Yseq=seq(-13,13,length.out = 100)
test<-expand.grid(Xseq,Yseq)

n<-1000 
mu<-rep(0,2)
c<-5
sigma<- c*diag(2)

samp2<-mvrnorm(n,mu,sigma)
colnames(samp2)<-c("x","y")
normaldata<-samp2[1:n,]

data.for.plotting<-expand.grid(X=Xseq, Y=Yseq)
data.for.plotting<-data.for.plotting %>%
  add_column("X1","X2","X3", "X4")

colnames(data.for.plotting)[-c(1,2)]<-c("FAST SAMPLER (shape=57.5487)", "FAST SAMPLER (shape=1)", "FAST SAMPLER (shape=10)", "FAST SAMPLER (shape=1000)")


################################################################
# FAST Sampler
################################################################
#use mcmc
burnin = 10000;
save = 10000;
thin = 0;
pt.new1<-ptTest(train = as.matrix(normaldata), test = as.matrix(test),
                maxJ = 8,
                save=save,burnin=burnin,thin=thin,
                fast=T)
i=20 # can print plots along the way (each 5% of saved mcmc iterates)

use= floor(save*(i*.05));
p.new1<- pt.new1$pmat[,i]
z.new1<-matrix(data=p.new1,nrow = length(Xseq),ncol = length(Yseq))
# Dummy data
dat.new1 <- expand.grid(X=Xseq, Y=Yseq)
dat.new1$Z <- as.vector(z.new1)
data.for.plotting[,2+1]<-dat.new1$Z



pt.new2<-ptTest(train = as.matrix(normaldata), test = as.matrix(test),
                maxJ = 8,
                save=save,burnin=burnin,thin=thin,
                cpar1 = 1, cpar2 = 1,
                fast=T)
i=20 # can print plots along the way (each 5% of saved mcmc iterates)

use= floor(save*(i*.05));
p.new2<- pt.new2$pmat[,i]
z.new2<-matrix(data=p.new2,nrow = length(Xseq),ncol = length(Yseq))
# Dummy data
dat.new2 <- expand.grid(X=Xseq, Y=Yseq)
dat.new2$Z <- as.vector(z.new2)
data.for.plotting[,2+2]<-dat.new2$Z

pt.new3<-ptTest(train = as.matrix(normaldata), test = as.matrix(test),
                maxJ = 8,
                save=save,burnin=burnin,thin=thin,
                cpar1 = 10, cpar2 = 1,
                fast=T)
i=20 # can print plots along the way (each 5% of saved mcmc iterates)

use= floor(save*(i*.05));
p.new3<- pt.new3$pmat[,i]
z.new3<-matrix(data=p.new3,nrow = length(Xseq),ncol = length(Yseq))
# Dummy data
dat.new3 <- expand.grid(X=Xseq, Y=Yseq)
dat.new3$Z <- as.vector(z.new3)
data.for.plotting[,2+3]<-dat.new3$Z

pt.new4<-ptTest(train = as.matrix(normaldata), test = as.matrix(test),
                maxJ = 8,
                save=save,burnin=burnin,thin=thin,
                cpar1 = 1000, cpar2 = 1,
                fast=T)
i=20 # can print plots along the way (each 5% of saved mcmc iterates)

use= floor(save*(i*.05));
p.new4<- pt.new4$pmat[,i]
z.new4<-matrix(data=p.new4,nrow = length(Xseq),ncol = length(Yseq))
# Dummy data
dat.new4 <- expand.grid(X=Xseq, Y=Yseq)
dat.new4$Z <- as.vector(z.new4)
data.for.plotting[,2+4]<-dat.new4$Z


data.for.plot <- data.for.plotting %>%
  pivot_longer(cols = c("FAST SAMPLER (shape=57.5487)", "FAST SAMPLER (shape=1)", "FAST SAMPLER (shape=10)", "FAST SAMPLER (shape=1000)"), 
               values_to = "Z",
               names_to = "Method")%>%
  mutate(Method = factor(Method, levels = c("FAST SAMPLER (shape=57.5487)", "FAST SAMPLER (shape=1)", "FAST SAMPLER (shape=10)", "FAST SAMPLER (shape=1000)")))

################################################################
#Create and Save Plot
################################################################
sizeaxis<- 8
sizeaxislab<- 10
sizetitle<- 11

plot.1000 <- ggplot(data = data.for.plot, aes(X, Y, fill= Z)) + 
  geom_tile()+
  scale_fill_gradient("Density",low = "white", high = "black")+
  xlab(bquote(X[1]))+
  ylab(bquote(X[2]))+
  xlim(-13,13)+
  ylim(-13,13)+
  theme_bw()+
  ggtitle("Gaussian Density Estimates (n=1000)")+
  theme(axis.text=element_text(size=sizeaxis),
        axis.title=element_text(size=sizeaxislab),
        plot.title=element_text(size=sizetitle),
        legend.title=element_text(size=sizeaxislab),
        legend.text=element_text(size=sizeaxis),
        strip.text=element_text(size=sizeaxislab),
        legend.position = "bottom",
        legend.key.width = unit(0.75, "in"))+
  facet_wrap(.~Method, nrow = 2)


pdf("sampling-over-alpha-normal1000.pdf",width=6,height=7)
print(plot.1000)
dev.off()


################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
data.for.plotting2<-expand.grid(X=Xseq, Y=Yseq)
data.for.plotting2<-data.for.plotting2 %>%
  add_column("X1","X2","X3", "X4")

colnames(data.for.plotting2)[-c(1,2)]<-c("FAST SAMPLER (alpha random)", "FAST SAMPLER (alpha=1)", "FAST SAMPLER (alpha=10)", "FAST SAMPLER (alpha=1000)")

################################################################
# FAST Sampler
################################################################
#use mcmc
#burnin = 10000;
#save = 10000;
#thin = 0;
#pt.new5<-ptTest(train = as.matrix(normaldata), test = as.matrix(test),
#                maxJ = 8,
#                save=save,burnin=burnin,thin=thin,
#                fast=T)
#i=20 # can print plots along the way (each 5% of saved mcmc iterates)
#
#use= floor(save*(i*.05));
#p.new5<- pt.new5$pmat[,i]
#z.new5<-matrix(data=p.new5,nrow = length(Xseq),ncol = length(Yseq))
# Dummy data
#dat.new5 <- expand.grid(X=Xseq, Y=Yseq)
#dat.new5$Z <- as.vector(z.new5)
#data.for.plotting2[,2+1]<-dat.new5$Z
data.for.plotting2[,2+1]<-dat.new1$Z


pt.new6<-ptTest(train = as.matrix(normaldata), test = as.matrix(test),
                maxJ = 8,
                save=save,burnin=burnin,thin=thin,
                csamp=3,
                cfix=1,
                fast=T)
i=20 # can print plots along the way (each 5% of saved mcmc iterates)

use= floor(save*(i*.05));
p.new6<- pt.new6$pmat[,i]
z.new6<-matrix(data=p.new6,nrow = length(Xseq),ncol = length(Yseq))
# Dummy data
dat.new6 <- expand.grid(X=Xseq, Y=Yseq)
dat.new6$Z <- as.vector(z.new6)
data.for.plotting2[,2+2]<-dat.new6$Z

pt.new7<-ptTest(train = as.matrix(normaldata), test = as.matrix(test),
                maxJ = 8,
                save=save,burnin=burnin,thin=thin,
                csamp=3,
                cfix=10,
                fast=T)
i=20 # can print plots along the way (each 5% of saved mcmc iterates)

use= floor(save*(i*.05));
p.new7<- pt.new7$pmat[,i]
z.new7<-matrix(data=p.new7,nrow = length(Xseq),ncol = length(Yseq))
# Dummy data
dat.new7 <- expand.grid(X=Xseq, Y=Yseq)
dat.new7$Z <- as.vector(z.new7)
data.for.plotting2[,2+3]<-dat.new7$Z

pt.new8<-ptTest(train = as.matrix(normaldata), test = as.matrix(test),
                maxJ = 8,
                save=save,burnin=burnin,thin=thin,
                csamp=3,
                cfix=1000,
                fast=T)
i=20 # can print plots along the way (each 5% of saved mcmc iterates)

use= floor(save*(i*.05));
p.new8<- pt.new8$pmat[,i]
z.new8<-matrix(data=p.new8,nrow = length(Xseq),ncol = length(Yseq))
# Dummy data
dat.new8 <- expand.grid(X=Xseq, Y=Yseq)
dat.new8$Z <- as.vector(z.new8)
data.for.plotting2[,2+4]<-dat.new8$Z


data.for.plot2 <- data.for.plotting2 %>%
  pivot_longer(cols = c("FAST SAMPLER (alpha random)", "FAST SAMPLER (alpha=1)", "FAST SAMPLER (alpha=10)", "FAST SAMPLER (alpha=1000)"), 
               values_to = "Z",
               names_to = "Method")%>%
  mutate(Method = factor(Method, levels = c("FAST SAMPLER (alpha random)", "FAST SAMPLER (alpha=1)", "FAST SAMPLER (alpha=10)", "FAST SAMPLER (alpha=1000)")))

################################################################
#Create and Save Plot
################################################################
sizeaxis<- 8
sizeaxislab<- 10
sizetitle<- 11

plot.1000.2 <- ggplot(data = data.for.plot2, aes(X, Y, fill= Z)) + 
  geom_tile()+
  scale_fill_gradient("Density",low = "white", high = "black")+
  xlab(bquote(X[1]))+
  ylab(bquote(X[2]))+
  xlim(-13,13)+
  ylim(-13,13)+
  theme_bw()+
  ggtitle("Gaussian Density Estimates (n=1000)")+
  theme(axis.text=element_text(size=sizeaxis),
        axis.title=element_text(size=sizeaxislab),
        plot.title=element_text(size=sizetitle),
        legend.title=element_text(size=sizeaxislab),
        legend.text=element_text(size=sizeaxis),
        strip.text=element_text(size=sizeaxislab),
        legend.position = "bottom",
        legend.key.width = unit(0.75, "in"))+
  facet_wrap(.~Method, nrow = 2)


pdf("sampling-over-fixedalpha-normal1000.pdf",width=6,height=7)
print(plot.1000.2)
dev.off()