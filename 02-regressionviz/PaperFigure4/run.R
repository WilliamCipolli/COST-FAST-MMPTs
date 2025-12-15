#########################################################
## Load libraries and helper functions
#########################################################
source("../../ptFiles.R")

#########################################################
## Linear Model
#########################################################
#simulate data
n<-500
x.ols<-runif(n, min=0,max=50)
y.ols<- (1/3)*x.ols +rnorm(n,0,1)
Y.ols<-(1/3)*x.ols

train<-data.frame(x=x.ols,y=y.ols)
mod.dat<-data.frame(x=x.ols,y=Y.ols)

#fit models
res.ols<-reglines(x=train$x, y=train$y)

#process estimates
test <- data.frame(x=seq(min(train$x),max(train$x),length.out=100))
mod.res<-cbind(test,res.ols$results)

mod.res<-mod.res %>% pivot_longer(!x, 
                                  names_to = "Model",
                                  values_to = "y")

#########################################################
## Save Density Data
#########################################################
datfast<-res.ols$ptfast
datfast<- datfast %>% mutate(Type=rep("FAST Sampler", nrow(datfast)))
datmcmc<-res.ols$ptmcmc
datmcmc<- datmcmc %>% mutate(Type=rep("MCMC Sampler", nrow(datmcmc)))

datdens<-rbind(datfast, datmcmc) 

#########################################################
## Save Predictions
#########################################################
dat2<- mod.res %>% filter(Model %in% c("pt2.mean","pt2.median","pt2.mode")) %>%
  mutate(Model = case_when(
    Model == "pt2.mean" ~ "Mean Estimate",
    Model == "pt2.median" ~ "Median Estimate",
    Model == "pt2.mode" ~ "Mode Estimate",
    TRUE ~ as.character(x))) 
dat2<- dat2 %>% mutate(Type=rep("FAST Sampler", nrow(dat2)))

dat3<- mod.res %>% filter(Model %in% c("pt3.mean","pt3.median","pt3.mode")) %>%
  mutate(Model = case_when(
    Model == "pt3.mean" ~ "Mean Estimate",
    Model == "pt3.median" ~ "Median Estimate",
    Model == "pt3.mode" ~ "Mode Estimate",
    TRUE ~ as.character(x)))
dat3<- dat3 %>% mutate(Type=rep("MCMC Sampler", nrow(dat3)))

datlines<-rbind(dat2, dat3)

#########################################################
## Create Plot
#########################################################
sizeaxis<- 8
sizeaxislab<- 10
sizetitle<- 11
linearplot<-ggplot() +
  geom_tile(data = datdens, aes(x, y, fill= z))+
  scale_fill_gradient("Density",low = "white", high = "grey30")+#,limits=c(0,0.20))+
  theme_classic()+
  xlab(bquote(X))+
  ylab(bquote(Y))+
  ylim(min(train$y),max(train$y))+
  ggtitle("Linear Relationship (n=500)")+
  geom_point(data=train,aes(x=x,y=y),shape=3, size=0.50,color="grey45")+
  geom_line(data=mod.dat,aes(x=x,y=y),col="black", size=2, alpha=0.4)+ #transparent real line
  geom_line(data=datlines,aes(x=x,y=y, linetype=Model), show.legend=F)+
  facet_wrap(.~Type, nrow=1)+
  theme(axis.text=element_text(size=sizeaxis),
        axis.title=element_text(size=sizeaxislab),
        plot.title=element_text(size=sizetitle),
        legend.title=element_text(size=sizeaxislab),
        legend.text=element_text(size=sizeaxis),
        strip.text=element_text(size=sizeaxislab))


#########################################################
## Nonlinear Model
#########################################################
#simulate data
n<-500
x.nl<-runif(n, min=0,max=50)
a<-15.58937  #runif(1,10,20)
b<-5.314167  #runif(1,0,10)
y.nl<-((a*x.nl)/(b+x.nl))+rnorm(n,0,1)
Y.nl<-((a*x.nl)/(b+x.nl))

train<-data.frame(x=x.nl,y=y.nl)
mod.dat<-data.frame(x=x.nl,y=Y.nl)
#fit models
res.nl<-reglines(x=train$x, y=train$y)

#process estimates
test <- data.frame(x=seq(min(train$x),max(train$x),length.out=100))
mod.res<-cbind(test,res.nl$results)

mod.res<-mod.res %>% pivot_longer(!x, 
                                  names_to = "Model",
                                  values_to = "y")

#########################################################
## Save Density Estimates
#########################################################
datfast<-res.nl$ptfast
datfast<- datfast %>% mutate(Type=rep("FAST Sampler", nrow(datfast)))
datmcmc<-res.nl$ptmcmc
datmcmc<- datmcmc %>% mutate(Type=rep("MCMC Sampler", nrow(datmcmc)))

datdens<-rbind(datfast, datmcmc) 

#########################################################
## Save Predictions
#########################################################
dat2<- mod.res %>% filter(Model %in% c("pt2.mean","pt2.median","pt2.mode")) %>%
  mutate(Model = case_when(
    Model == "pt2.mean" ~ "Mean Estimate",
    Model == "pt2.median" ~ "Median Estimate",
    Model == "pt2.mode" ~ "Mode Estimate",
    TRUE ~ as.character(x))) 
dat2<- dat2 %>% mutate(Type=rep("FAST Sampler", nrow(dat2)))

dat3<- mod.res %>% filter(Model %in% c("pt3.mean","pt3.median","pt3.mode")) %>%
  mutate(Model = case_when(
    Model == "pt3.mean" ~ "Mean Estimate",
    Model == "pt3.median" ~ "Median Estimate",
    Model == "pt3.mode" ~ "Mode Estimate",
    TRUE ~ as.character(x)))
dat3<- dat3 %>% mutate(Type=rep("MCMC Sampler", nrow(dat3)))

datlines<-rbind(dat2, dat3)

#########################################################
## Create Plot
#########################################################
sizeaxis<- 8
sizeaxislab<- 10
sizetitle<- 11
nonlinearplot<-ggplot() +
  geom_tile(data = datdens, aes(x, y, fill= z))+
  scale_fill_gradient("Density",low = "white", high = "grey30")+#,limits=c(0,0.20))+
  theme_classic()+
  xlab(bquote(S))+
  ylab(bquote(V))+
  ylim(min(train$y),max(train$y))+
  ggtitle("Nonlinear Relationship (n=500)")+
  geom_point(data=train,aes(x=x,y=y),shape=3, size=0.50,color="grey45")+
  geom_line(data=mod.dat,aes(x=x,y=y),col="black", size=2, alpha=0.4)+ #transparent real line
  geom_line(data=datlines,aes(x=x,y=y, linetype=Model), show.legend=F)+
  facet_wrap(.~Type, nrow=1)+
  theme(axis.text=element_text(size=sizeaxis),
        axis.title=element_text(size=sizeaxislab),
        plot.title=element_text(size=sizetitle),
        legend.title=element_text(size=sizeaxislab),
        legend.text=element_text(size=sizeaxis),
        strip.text=element_text(size=sizeaxislab))


#########################################################
## Polynomial Model
#########################################################
#simulate data
n<-500
x.poly<-runif(n, min=0,max=50)
y.poly<- (-1/50000)*x.poly^4 + (1/1000)*x.poly^3 +(1/1000)*x.poly^2 + (1/1000)*x.poly +rnorm(n,0,1)
Y.poly<- (-1/50000)*x.poly^4 + (1/1000)*x.poly^3 +(1/1000)*x.poly^2 + (1/1000)*x.poly

train<-data.frame(x=x.poly,y=y.poly)
mod.dat<-data.frame(x=x.poly,y=Y.poly)

#fit models
res.poly<-reglines(x=train$x, y=train$y)

#process estimates
test <- data.frame(x=seq(min(train$x),max(train$x),length.out=100))
mod.res<-cbind(test,res.poly$results)

mod.res<-mod.res %>% pivot_longer(!x, 
                                  names_to = "Model",
                                  values_to = "y")

#########################################################
## Save Density Estimates
#########################################################
datfast<-res.poly$ptfast
datfast<- datfast %>% mutate(Type=rep("FAST Sampler", nrow(datfast)))
datmcmc<-res.poly$ptmcmc
datmcmc<- datmcmc %>% mutate(Type=rep("MCMC Sampler", nrow(datmcmc)))

datdens<-rbind(datfast, datmcmc) 

#########################################################
## Save Predictions
#########################################################
dat2<- mod.res %>% filter(Model %in% c("pt2.mean","pt2.median","pt2.mode")) %>%
  mutate(Model = case_when(
    Model == "pt2.mean" ~ "Mean Estimate",
    Model == "pt2.median" ~ "Median Estimate",
    Model == "pt2.mode" ~ "Mode Estimate",
    TRUE ~ as.character(x))) 
dat2<- dat2 %>% mutate(Type=rep("FAST Sampler", nrow(dat2)))

dat3<- mod.res %>% filter(Model %in% c("pt3.mean","pt3.median","pt3.mode")) %>%
  mutate(Model = case_when(
    Model == "pt3.mean" ~ "Mean Estimate",
    Model == "pt3.median" ~ "Median Estimate",
    Model == "pt3.mode" ~ "Mode Estimate",
    TRUE ~ as.character(x)))
dat3<- dat3 %>% mutate(Type=rep("MCMC Sampler", nrow(dat3)))

datlines<-rbind(dat2, dat3)

#########################################################
## Create Plot
#########################################################
sizeaxis<- 8
sizeaxislab<- 10
sizetitle<- 11
polyplot<-ggplot() +
  geom_tile(data = datdens, aes(x, y, fill= z))+
  scale_fill_gradient("Density",low = "white", high = "grey30")+#,limits=c(0,0.20))+
  theme_classic()+
  xlab(bquote(X))+
  ylab(bquote(Y))+
  ylim(min(train$y),max(train$y))+
  ggtitle("Polynomial Relationship (n=500)")+
  geom_point(data=train,aes(x=x,y=y),shape=3, size=0.50,color="grey45")+
  geom_line(data=mod.dat,aes(x=x,y=y),col="black", size=2, alpha=0.4)+ #transparent real line
  geom_line(data=datlines,aes(x=x,y=y, linetype=Model), show.legend=F)+
  facet_wrap(.~Type, nrow=1)+
  theme(axis.text=element_text(size=sizeaxis),
        axis.title=element_text(size=sizeaxislab),
        plot.title=element_text(size=sizetitle),
        legend.title=element_text(size=sizeaxislab),
        legend.text=element_text(size=sizeaxis),
        strip.text=element_text(size=sizeaxislab))

#########################################################
## "Hard" Problem Model
#########################################################
#simulate data
n<-500
# x.heart<-runif(n, min=0,max=pi)
# y.heart<- sin(x.heart)^63 * sin(x.heart+1.5)*8 +rnorm(n,0,.1)
# Y.heart<-sin(x.heart)^63 * sin(x.heart+1.5)*8

a=0.2
d=1.4
h=3
s=0.05
w=0.02

x.heart<-runif(n, min=-3,max=3)
y.heart<- a*(exp((-(x.heart + d)^2) / (2*w)) + exp((-(x.heart - d)^2) / (2*w))) + (h - abs(x.heart / s) - x.heart) * exp((-(7*x.heart)^2) / 2)*8 + rnorm(n,0,0.5)
Y.heart<- a*(exp((-(x.heart + d)^2) / (2*w)) + exp((-(x.heart - d)^2) / (2*w))) + (h - abs(x.heart / s) - x.heart) * exp((-(7*x.heart)^2) / 2)*8

plot(x.heart, Y.heart)

train<-data.frame(x=x.heart,y=y.heart)
mod.dat<-data.frame(x=x.heart,y=Y.heart)

#fit models
res.heart<-reglines(x=train$x, y=train$y)

#process estimates
test <- data.frame(x=seq(min(train$x),max(train$x),length.out=100))
mod.res<-cbind(test,res.heart$results)

mod.res<-mod.res %>% pivot_longer(!x, 
                                  names_to = "Model",
                                  values_to = "y")

#########################################################
## Save Density Data
#########################################################
datfast<-res.heart$ptfast
datfast<- datfast %>% mutate(Type=rep("FAST Sampler", nrow(datfast)))
datmcmc<-res.heart$ptmcmc
datmcmc<- datmcmc %>% mutate(Type=rep("MCMC Sampler", nrow(datmcmc)))

datdens<-rbind(datfast, datmcmc) 

#########################################################
## Save Predictions
#########################################################
dat2<- mod.res %>% filter(Model %in% c("pt2.mean","pt2.median","pt2.mode")) %>%
  mutate(Model = case_when(
    Model == "pt2.mean" ~ "Mean Estimate",
    Model == "pt2.median" ~ "Median Estimate",
    Model == "pt2.mode" ~ "Mode Estimate",
    TRUE ~ as.character(x))) 
dat2<- dat2 %>% mutate(Type=rep("FAST Sampler", nrow(dat2)))

dat3<- mod.res %>% filter(Model %in% c("pt3.mean","pt3.median","pt3.mode")) %>%
  mutate(Model = case_when(
    Model == "pt3.mean" ~ "Mean Estimate",
    Model == "pt3.median" ~ "Median Estimate",
    Model == "pt3.mode" ~ "Mode Estimate",
    TRUE ~ as.character(x)))
dat3<- dat3 %>% mutate(Type=rep("MCMC Sampler", nrow(dat3)))

datlines<-rbind(dat2, dat3)

#########################################################
## Create Plot
#########################################################
sizeaxis<- 8
sizeaxislab<- 10
sizetitle<- 11
heartplot<-ggplot() +
  geom_tile(data = datdens, aes(x, y, fill= z))+
  scale_fill_gradient("Density",low = "white", high = "grey30")+#,limits=c(0,0.20))+
  theme_classic()+
  xlab(bquote(X))+
  ylab(bquote(Y))+
  ylim(min(train$y),max(train$y))+
  ggtitle("Heart Beat Relationship (n=500)")+
  geom_point(data=train,aes(x=x,y=y),shape=3, size=0.50,color="grey45")+
  geom_line(data=mod.dat,aes(x=x,y=y),col="black", size=2, alpha=0.4)+ #transparent real line
  geom_line(data=datlines,aes(x=x,y=y, linetype=Model), show.legend=F)+
  facet_wrap(.~Type, nrow=1)+
  theme(axis.text=element_text(size=sizeaxis),
        axis.title=element_text(size=sizeaxislab),
        plot.title=element_text(size=sizetitle),
        legend.title=element_text(size=sizeaxislab),
        legend.text=element_text(size=sizeaxis),
        strip.text=element_text(size=sizeaxislab))


#########################################################
## "Hard" Problem Model
#########################################################
#simulate data
n<-500

a=0.2
d=1.4
h=3
s=0.05
w=0.02
x.heart <- rnorm(n, mean=0, sd=1.5)
y.heart<- a*(exp((-(x.heart + d)^2) / (2*w)) + exp((-(x.heart - d)^2) / (2*w))) + (h - abs(x.heart / s) - x.heart) * exp((-(7*x.heart)^2) / 2)*8 + rnorm(n,0,0.5)
Y.heart<- a*(exp((-(x.heart + d)^2) / (2*w)) + exp((-(x.heart - d)^2) / (2*w))) + (h - abs(x.heart / s) - x.heart) * exp((-(7*x.heart)^2) / 2)*8

plot(x.heart, Y.heart)

train<-data.frame(x=x.heart,y=y.heart)
mod.dat<-data.frame(x=x.heart,y=Y.heart)

#fit models
res.heart<-reglines(x=train$x, y=train$y)

#process estimates
test <- data.frame(x=seq(min(train$x),max(train$x),length.out=100))
mod.res<-cbind(test,res.heart$results)

mod.res<-mod.res %>% pivot_longer(!x, 
                                  names_to = "Model",
                                  values_to = "y")

#########################################################
## Save Density Data
#########################################################
datfast<-res.heart$ptfast
datfast<- datfast %>% mutate(Type=rep("FAST Sampler", nrow(datfast)))
datmcmc<-res.heart$ptmcmc
datmcmc<- datmcmc %>% mutate(Type=rep("MCMC Sampler", nrow(datmcmc)))

datdens<-rbind(datfast, datmcmc) 

#########################################################
## Save Predictions
#########################################################
dat2<- mod.res %>% filter(Model %in% c("pt2.mean","pt2.median","pt2.mode")) %>%
  mutate(Model = case_when(
    Model == "pt2.mean" ~ "Mean Estimate",
    Model == "pt2.median" ~ "Median Estimate",
    Model == "pt2.mode" ~ "Mode Estimate",
    TRUE ~ as.character(x))) 
dat2<- dat2 %>% mutate(Type=rep("FAST Sampler", nrow(dat2)))

dat3<- mod.res %>% filter(Model %in% c("pt3.mean","pt3.median","pt3.mode")) %>%
  mutate(Model = case_when(
    Model == "pt3.mean" ~ "Mean Estimate",
    Model == "pt3.median" ~ "Median Estimate",
    Model == "pt3.mode" ~ "Mode Estimate",
    TRUE ~ as.character(x)))
dat3<- dat3 %>% mutate(Type=rep("MCMC Sampler", nrow(dat3)))

datlines<-rbind(dat2, dat3)

#########################################################
## Create Plot
#########################################################
sizeaxis<- 8
sizeaxislab<- 10
sizetitle<- 11
heartplot2<-ggplot() +
  geom_tile(data = datdens, aes(x, y, fill= z))+
  scale_fill_gradient("Density",low = "white", high = "grey30")+#,limits=c(0,0.20))+
  theme_classic()+
  xlab(bquote(X))+
  ylab(bquote(Y))+
  ylim(min(train$y),max(train$y))+
  ggtitle("Heart Beat Relationship (n=500)")+
  geom_point(data=train,aes(x=x,y=y),shape=3, size=0.50,color="grey45")+
  geom_line(data=mod.dat,aes(x=x,y=y),col="black", size=2, alpha=0.4)+ #transparent real line
  geom_line(data=datlines,aes(x=x,y=y, linetype=Model),show.legend = F)+
  facet_wrap(.~Type, nrow=1)+
  theme(axis.text=element_text(size=sizeaxis),
        axis.title=element_text(size=sizeaxislab),
        plot.title=element_text(size=sizetitle),
        legend.title=element_text(size=sizeaxislab),
        legend.text=element_text(size=sizeaxis),
        strip.text=element_text(size=sizeaxislab))




sizeaxis<- 8
sizeaxislab<- 10
sizetitle<- 11

# temp patch
linearplot <- linearplot + guides(linetype="none")
nonlinearplot <- nonlinearplot + guides(linetype="none")
polyplot <- polyplot + guides(linetype="none")
heartplot <- heartplot + guides(linetype="none")
heartplot2 <- heartplot2 + guides(linetype="none")

pdf("regviz.pdf",width=7,height=9)
linearplot/nonlinearplot/polyplot/heartplot
dev.off()

pdf("regviz2.pdf",width=7,height=9)
linearplot/nonlinearplot/polyplot/heartplot2
dev.off()

pdf("regvizReview.pdf", width=7, height=6)
(heartplot)/(heartplot2)
dev.off()
