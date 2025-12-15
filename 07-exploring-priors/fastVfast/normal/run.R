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
  add_column("X1","X2","X3","X4","X5")

colnames(data.for.plotting)[-c(1,2)]<-c("Average of Two Square Roots","Symmetric Square Root",
                                        "Rotated Square Root", "FAST Sampler 2", "FAST Sampler 1")

################################################################
#One/Two Rotations
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
# MCMC Sampler 2
################################################################
#use mcmc
burnin = 10000;
thin = 0;
save = 10000;
pt.new<-ptTest(train = as.matrix(normaldata), test = as.matrix(test),
               maxJ = 8,
               save=save,burnin=burnin,thin=thin,
               fast=T)
i=20 # can print plots along the way (each 5% of saved mcmc iterates)

use= floor(save*(i*.05));
p.new<- pt.new$pmat[,i]
z.new<-matrix(data=p.new,nrow = length(Xseq),ncol = length(Yseq))
# Dummy data
dat.new <- expand.grid(X=Xseq, Y=Yseq)
dat.new$Z <- as.vector(z.new)

data.for.plotting[,6]<-dat.new$Z



################################################################
# Full MCMC Sample
################################################################
#use mcmc
burnin = 10000;
thin = 0;
save = 10000;
pt.new2<-ptTest(train = as.matrix(normaldata), test = as.matrix(test),
                maxJ = 8,
                save=save,burnin=burnin,thin=thin,
                fast=T)

i=20 # can print plots along the way (each 5% of saved mcmc iterates)
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
                        "FAST Sampler 2", "FAST Sampler 1"), 
               values_to = "Z",
               names_to = "Method")%>%
  mutate(Method = factor(Method, levels = c("Symmetric Square Root","Rotated Square Root",
                                            "FAST Sampler 2", "FAST Sampler 1")))

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


pdf("normal1000.pdf",width=6,height=7)
print(plot.1000)
dev.off()


################################################################
#Compare Sampling
################################################################
df.fast <- tibble(Iterate = (1:10000),
                  mu1 = pt.new$muvals1[12001:22000],
                  mu2 = pt.new$muvals2[12001:22000],
                  sigma11 = pt.new$sigmavals11[12001:22000],
                  sigma12 = pt.new$sigmavals12[12001:22000],
                  sigma21 = pt.new$sigmavals21[12001:22000],
                  sigma22 = pt.new$sigmavals22[12001:22000],
                  cp = pt.new$cpvalues[12001:22000],
                  O11 = pt.new$Ovals11[12001:22000],
                  O12 = pt.new$Ovals12[12001:22000],
                  O21 = pt.new$Ovals21[12001:22000],
                  O22 = pt.new$Ovals22[12001:22000])

df.mcmc <- tibble(Iterate = (1:10000),
                  mu1 = pt.new2$muvals1[12001:22000],
                  mu2 = pt.new2$muvals2[12001:22000],
                  sigma11 = pt.new2$sigmavals11[12001:22000],
                  sigma12 = pt.new2$sigmavals12[12001:22000],
                  sigma21 = pt.new2$sigmavals21[12001:22000],
                  sigma22 = pt.new2$sigmavals22[12001:22000],
                  cp = pt.new2$cpvalues[12001:22000],
                  O11 = pt.new2$Ovals11[12001:22000],
                  O12 = pt.new2$Ovals12[12001:22000],
                  O21 = pt.new2$Ovals21[12001:22000],
                  O22 = pt.new2$Ovals22[12001:22000])
library(patchwork)


min.mean1  <- floor(min(min(df.mcmc$mu1)*10, min(df.fast$mu1)*10))/10
min.mean2  <- floor(min(min(df.mcmc$mu2)*10, min(df.fast$mu2)*10))/10
max.mean1  <- ceiling(max(max(df.mcmc$mu1)*10, max(df.fast$mu1)*10))/10
max.mean2  <- ceiling(max(max(df.mcmc$mu2)*10, max(df.fast$mu2)*10))/10

min.mean <- min(min.mean1, min.mean2, -max.mean1, -max.mean2)
max.mean <- max(-min.mean1, -min.mean2,max.mean1, max.mean2)

mu1 <- ggplot()+
  geom_point(data=df.mcmc, aes(x=Iterate, y=mu1, color="FAST 1", shape="FAST 1"))+
  geom_point(data=df.fast, aes(x=Iterate, y=mu1, color="FAST 2", shape="FAST 2"))+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  scale_shape_manual("Sampler", values=c(19,1))+
  xlab("FAST Iterate")+
  ylab(bquote(mu[x[1]]))+
  ylim(min.mean, max.mean)

mu2 <- ggplot()+
  geom_point(data=df.mcmc, aes(x=Iterate, y=mu2, color="FAST 1", shape="FAST 1"))+
  geom_point(data=df.fast, aes(x=Iterate, y=mu2, color="FAST 2", shape="FAST 2"))+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  scale_shape_manual("Sampler", values=c(19,1))+
  xlab("FAST Iterate")+
  ylab(bquote(mu[x[2]]))+
  ylim(min.mean, max.mean)

mu3 <- ggplot() +
  stat_density(data=df.mcmc, aes(x=mu1, color="FAST 1"), geom = "line")+
  stat_density(data=df.fast, aes(x=mu1, color="FAST 2"), geom = "line")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  xlab(bquote(mu[x[1]]))+
  xlim(min.mean, max.mean)+
  ylab("Density")


mu4 <- ggplot() +
  stat_density(data=df.mcmc, aes(x=mu2, color="FAST 1"), geom = "line")+
  stat_density(data=df.fast, aes(x=mu2, color="FAST 2"), geom = "line")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  xlab(bquote(mu[x[2]]))+
  xlim(min.mean, max.mean)+
  ylab("Density")


png("mutrace.png", width = 7, height = 3.5, units = "in", res = 400)
((mu1 + mu2)+ plot_layout(guides = "collect"))/
  ((mu3 + mu4)+ plot_layout(guides = "collect"))
dev.off()

min.sig.diag    <- floor(min(min(df.mcmc$sigma11)*10, min(df.mcmc$sigma22)*10,min(df.fast$sigma11)*10, min(df.fast$sigma22)*10))/10
max.sig.diag    <- ceiling(max(max(df.mcmc$sigma11)*10, max(df.mcmc$sigma22)*10, max(df.fast$sigma11)*10, max(df.fast$sigma22)*10))/10
min.sig.diag <- 5 - max(5-min.sig.diag, max.sig.diag-5)
max.sig.diag <- 5 + max(5-min.sig.diag, max.sig.diag-5)

min.sig.offdiag <- floor(min(min(df.mcmc$sigma12)*10, min(df.mcmc$sigma21)*10, min(df.fast$sigma12)*10, min(df.fast$sigma21)*10))/10
max.sig.offdiag <- ceiling(max(max(df.mcmc$sigma12)*10, max(df.mcmc$sigma21)*10, max(df.fast$sigma12)*10, max(df.fast$sigma21)*10))/10
min.sig.offdiag <- min(min.sig.offdiag, -max.sig.offdiag)
max.sig.offdiag <- max(-min.sig.offdiag, max.sig.offdiag)


sigma11 <- ggplot()+
  geom_point(data=df.mcmc, aes(x=Iterate, y=sigma11, color="FAST 1", shape="FAST 1"))+
  geom_point(data=df.fast, aes(x=Iterate, y=sigma11, color="FAST 2", shape="FAST 2"))+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  scale_shape_manual("Sampler", values=c(19,1))+
  xlab("FAST Iterate")+
  ylab(bquote(sigma[x[1]]^2))+
  ylim(min.sig.diag, max.sig.diag)

sigma12 <- ggplot()+
  geom_point(data=df.mcmc, aes(x=Iterate, y=sigma12, color="FAST 1", shape="FAST 1"))+
  geom_point(data=df.fast, aes(x=Iterate, y=sigma12, color="FAST 2", shape="FAST 2"))+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  scale_shape_manual("Sampler", values=c(19,1))+
  xlab("FAST Iterate")+
  ylab(bquote(sigma[x[1]*","*x[2]]))+
  ylim(min.sig.offdiag, max.sig.offdiag)

sigma21 <- ggplot()+
  geom_point(data=df.mcmc, aes(x=Iterate, y=sigma21, color="FAST 1", shape="FAST 1"))+
  geom_point(data=df.fast, aes(x=Iterate, y=sigma21, color="FAST 2", shape="FAST 2"))+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  scale_shape_manual("Sampler", values=c(19,1))+
  xlab("FAST Iterate")+
  ylab(bquote(sigma[x[2]*","*x[1]]))+
  ylim(min.sig.offdiag, max.sig.offdiag)

sigma22 <- ggplot()+
  geom_point(data=df.mcmc, aes(x=Iterate, y=sigma22, color="FAST 1", shape="FAST 1"))+
  geom_point(data=df.fast, aes(x=Iterate, y=sigma22, color="FAST 2", shape="FAST 2"))+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  scale_shape_manual("Sampler", values=c(19,1))+
  xlab("FAST Iterate")+
  ylab(bquote(sigma[x[2]]^2))+
  ylim(min.sig.diag, max.sig.diag)

png("sigmatrace.png", width = 7, height = 3.5, units = "in", res = 400)
(sigma11 + sigma12)/(sigma21 + sigma22) + plot_layout(guides = "collect")
dev.off()

sigma11 <- ggplot() +
  stat_density(data=df.mcmc, aes(x=sigma11, color="FAST 1"), geom = "line")+
  stat_density(data=df.fast, aes(x=sigma11, color="FAST 2"), geom = "line")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  xlab(bquote(sigma[x[1]]^2))+
  xlim(min.sig.diag, max.sig.diag)+
  ylab("Density")

sigma12 <- ggplot() +
  stat_density(data=df.mcmc, aes(x=sigma12, color="FAST 1"), geom = "line")+
  stat_density(data=df.fast, aes(x=sigma12, color="FAST 2"), geom = "line")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  xlab(bquote(sigma[x[1]*","*x[2]]))+
  xlim(min.sig.offdiag, max.sig.offdiag)+
  ylab("Density")

sigma21 <- ggplot() +
  stat_density(data=df.mcmc, aes(x=sigma21, color="FAST 1"), geom = "line")+
  stat_density(data=df.fast, aes(x=sigma21, color="FAST 2"), geom = "line")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  xlab(bquote(sigma[x[2]*","*x[1]]))+
  xlim(min.sig.offdiag, max.sig.offdiag)+
  ylab("Density")

sigma22 <- ggplot() +
  stat_density(data=df.mcmc, aes(x=sigma22, color="FAST 1"), geom = "line")+
  stat_density(data=df.fast, aes(x=sigma22, color="FAST 2"), geom = "line")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  xlab(bquote(sigma[x[2]]^2))+
  xlim(min.sig.diag, max.sig.diag)+
  ylab("Density")

png("sigmadens.png", width = 7, height = 3.5, units = "in", res = 400)
(sigma11 + sigma12)/(sigma21 + sigma22) + plot_layout(guides = "collect")
dev.off()

min.cp <- floor(min(0*10, min(df.mcmc$cp)*10, min(df.fast$cp)*10))/10
max.cp <- ceiling(max(0.2*10, max(df.mcmc$cp)*10, max(df.mcmc$cp)*10))/10

options(scipen=0, digits=5)
png("cptrace.png", width = 7, height = 2, units = "in", res = 400)
cp1 <- ggplot()+
  geom_point(data=df.mcmc, aes(x=Iterate, y=cp, color="FAST 1", shape="FAST 1"))+
  geom_point(data=df.fast, aes(x=Iterate, y=cp, color="FAST 2", shape="FAST 2"))+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  scale_shape_manual("Sampler", values=c(19,1))+
  xlab("FAST Iterate")+
  ylab(bquote(alpha))+
  ylim(min.cp, max.cp)+
  scale_y_continuous(breaks = c(0, 5*10^(305), 1*10^(306), 1.5*10^(306), 2*10^(306)),
                     labels = c("0", "5.0e305","1.0e306", "1.5e306", "2.0e306"))

cp2 <- ggplot() +
  stat_density(data=df.mcmc, aes(x=cp, color="FAST 1"), geom = "line")+
  stat_density(data=df.fast, aes(x=cp, color="FAST 2"), geom = "line")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  xlab(bquote(alpha))+
  xlim(min.cp, max.cp)+
  ylab("Density")+
  scale_x_continuous(breaks = c(0, 5*10^(305), 1*10^(306), 1.5*10^(306), 2*10^(306)),  
                     labels = c("0", "5.0e305","1.0e306", "1.5e306", "2.0e306"))

cp1 + cp2 + plot_layout(guides = "collect")
dev.off()

options(scipen=2000000)

O11 <- ggplot()+
  geom_point(data=df.mcmc, aes(x=Iterate, y=O11, color="FAST 1", shape="FAST 1"))+
  geom_point(data=df.fast, aes(x=Iterate, y=O11, color="FAST 2", shape="FAST 2"))+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  scale_shape_manual("Sampler", values=c(19,1))+
  xlab("FAST Iterate")+
  ylab(bquote(O[1*","*1]))+
  ylim(-1,1)

O12 <- ggplot()+
  geom_point(data=df.mcmc, aes(x=Iterate, y=O12, color="FAST 1", shape="FAST 1"))+
  geom_point(data=df.fast, aes(x=Iterate, y=O12, color="FAST 2", shape="FAST 2"))+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  scale_shape_manual("Sampler", values=c(19,1))+
  xlab("FAST Iterate")+
  ylab(bquote(O[1*","*2]))+
  ylim(-1,1)

O21 <- ggplot()+
  geom_point(data=df.mcmc, aes(x=Iterate, y=O21, color="FAST 1", shape="FAST 1"))+
  geom_point(data=df.fast, aes(x=Iterate, y=O21, color="FAST 2", shape="FAST 2"))+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  scale_shape_manual("Sampler", values=c(19,1))+
  xlab("FAST Iterate")+
  ylab(bquote(O[2*","*1]))+
  ylim(-1,1)

O22 <- ggplot()+
  geom_point(data=df.mcmc, aes(x=Iterate, y=O22, color="FAST 1", shape="FAST 1"))+
  geom_point(data=df.fast, aes(x=Iterate, y=O22, color="FAST 2", shape="FAST 2"))+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  scale_shape_manual("Sampler", values=c(19,1))+
  xlab("FAST Iterate")+
  ylab(bquote(O[2*","*2]))+
  ylim(-1,1)


png("Otrace.png", width = 7, height = 3.5, units = "in", res = 400)
(O11 + O12)/(O21 + O22) + plot_layout(guides = "collect")
dev.off()



O11 <- ggplot() +
  stat_density(data=df.mcmc, aes(x=O11, color="FAST 1"), geom = "line")+
  stat_density(data=df.fast, aes(x=O11, color="FAST 2"), geom = "line")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  xlab(bquote(O[1*","*1]))+
  xlim(-1,1)+
  ylab("Density")

O12 <- ggplot() +
  stat_density(data=df.mcmc, aes(x=O12, color="FAST 1"), geom = "line")+
  stat_density(data=df.fast, aes(x=O12, color="FAST 2"), geom = "line")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  xlab(bquote(O[1*","*2]))+
  xlim(-1,1)+
  ylab("Density")

O21 <- ggplot() +
  stat_density(data=df.mcmc, aes(x=O21, color="FAST 1"), geom = "line")+
  stat_density(data=df.fast, aes(x=O21, color="FAST 2"), geom = "line")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  xlab(bquote(O[2*","*1]))+
  xlim(-1,1)+
  ylab("Density")

O22 <- ggplot() +
  stat_density(data=df.mcmc, aes(x=O22, color="FAST 1"), geom = "line")+
  stat_density(data=df.fast, aes(x=O22, color="FAST 2"), geom = "line")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  scale_color_manual("Sampler", values=c("black","grey"))+
  xlab(bquote(O[2*","*2]))+
  xlim(-1,1)+
  ylab("Density")

png("Odens.png", width = 7, height = 3.5, units = "in", res = 400)
(O11 + O12)/(O21 + O22) + plot_layout(guides = "collect")
dev.off()

mindens <- 0
maxdens <- ceiling(max(max(as.numeric(pt.new$pvec))*1000, max(as.numeric(pt.new2$pvec))*1000))/1000
png("probdiff.png", width=7, height=2.5, units="in", res=400)
ggdat1=tibble(diff=(as.numeric(pt.new$pvec) - as.numeric(pt.new2$pvec)))
maxabsdiff=max(abs(ggdat1$diff))
p1 <- ggplot(data=ggdat1) +
  geom_histogram(aes(x=diff, y=after_stat(density)),
                 breaks = seq(-maxabsdiff, maxabsdiff, length.out=ceiling(log2(nrow(ggdat1))+1)),
                 color="lightgrey")+
  geom_hline(yintercept=0)+
  theme_bw()+
  xlab("Density Difference (FAST 1 - FAST 2)")+
  ylab("Density")

ggdat2=tibble(fast=as.numeric(pt.new$pvec),
              full=as.numeric(pt.new2$pvec))

p2 <- ggplot(data=ggdat2) +
  geom_point(aes(x=fast, y=full),
             shape=1)+
  geom_abline(color="red", linewidth=1.5, alpha=0.5)+
  theme_bw()+
  scale_x_continuous("Density Estimate (FAST 2)",
                     limits = c(mindens, maxdens),
                     breaks = seq(mindens, maxdens, length.out=5),
                     labels = format(round(seq(mindens, maxdens, length.out=5),3), nsmall=3))+
  scale_y_continuous("Density Estimate (FAST 1)",
                     limits = c(mindens, maxdens),
                     breaks = seq(mindens, maxdens, length.out=5),
                     labels = format(round(seq(mindens, maxdens, length.out=5),3), nsmall=3))

print(p1+p2)
dev.off()

options(scipen=10, digits=4)
full.sum <- df.mcmc |>
  pivot_longer(c(mu1, mu2, sigma11, sigma12, sigma21, sigma22, cp, O11, O12, O21, O22),
               names_to = "Quantity",
               values_to = "value") |>
  group_by(Quantity) |>
  summarize(
    min=min(value),
    Q1=quantile(value, 0.25),
    median=median(value),
    Q3=quantile(value, 0.75),
    max=max(value),
    mean=mean(value),
    sd=sd(value)
  )|>
  mutate(across(where(is.numeric), round, 3))|>
  mutate(Method = "FAST 1")|>
  select(Method, everything())

fast.sum <- df.fast |>
  pivot_longer(c(mu1, mu2, sigma11, sigma12, sigma21, sigma22, cp, O11, O12, O21, O22),
               names_to = "Quantity",
               values_to = "value") |>
  group_by(Quantity) |>
  summarize(
    min=min(value),
    Q1=quantile(value, 0.25),
    median=median(value),
    Q3=quantile(value, 0.75),
    max=max(value),
    mean=mean(value),
    sd=sd(value)
  )|>
  mutate(across(where(is.numeric), round, 3))|>
  mutate(Method = "FAST 2")|>
  select(Method, everything())



index <- c(6, 17,  7, 18,  8, 19, 9, 20, 10, 21, 11, 22,  1, 12,  2,  14, 3,  15, 4,  15, 5, 16)
rbind(full.sum, fast.sum)[index, ]  |>
  write_csv("summary.table.csv")

