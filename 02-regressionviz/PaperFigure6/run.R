#########################################################
## Load libraries and helper functions
#########################################################
source("../../ptFiles.R")

#########################################################
## Multimodal Model
#########################################################
#simulate data
n<-500
x.sin<-runif(n,-1.5*pi,2.5*pi)

library(caret)
flds<-createFolds(y=x.sin, k = 5)
y.sin <- rep(NA, n)
y.sin[flds[[1]]] <- 0.5*sin(x.sin[flds[[1]]]-0.5*pi)+3 + rnorm(length(flds[[1]]),0,0.2)
y.sin[c(flds[[2]],flds[[3]],flds[[4]])] <- 1.0*sin(1.5*x.sin[c(flds[[2]],flds[[3]],flds[[4]])]) + rnorm(length(c(flds[[2]],flds[[3]],flds[[4]])),0,0.2)
y.sin[flds[[5]]] <- 0.5*sin(x.sin[flds[[5]]])-3 + rnorm(length(flds[[5]]),0,0.2)

train<-data.frame(x=x.sin,y=y.sin)

#fit models
res.sin<-reglines(x=train$x, y=train$y)

#process estimates
test <- data.frame(x=seq(min(train$x),max(train$x),length.out=100))
mod.res<-cbind(test,res.sin$results)

mod.res<-mod.res %>% pivot_longer(!x, 
                                  names_to = "Model",
                                  values_to = "y")

#########################################################
## Save Density Estimates
#########################################################
datfast<-res.sin$ptfast
datfast<- datfast %>% mutate(Type=rep("FAST Sampler", nrow(datfast)))
datmcmc<-res.sin$ptmcmc
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
sin.plot<-ggplot() +
  geom_tile(data = datdens, aes(x, y, fill= z))+
  scale_fill_gradient("Density",low = "white", high = "grey30")+#,limits=c(0,0.20))+
  theme_classic()+
  xlab(bquote(X))+
  ylab(bquote(Y))+
  ylim(min(train$y),max(train$y))+
  ggtitle("Multimodal Regression (n=500)",
    subtitle="Simulated Data with Three Components")+
  geom_point(data=train,aes(x=x,y=y),shape=3, size=0.50,color="grey45")+
  #geom_line(data=mod.dat,aes(x=x,y=y),col="black", size=2, alpha=0.4)+ #transparent real line
  #geom_line(data=datlines,aes(x=x,y=y, linetype=Model))+
  facet_wrap(.~Type, nrow=1)+
  theme(axis.text=element_text(size=sizeaxis),
        axis.title=element_text(size=sizeaxislab),
        plot.title=element_text(size=sizetitle),
        legend.title=element_text(size=sizeaxislab),
        legend.text=element_text(size=sizeaxis),
        strip.text=element_text(size=sizeaxislab))

pdf("Multimodal.pdf",width=7,height=4.5)
sin.plot
dev.off()