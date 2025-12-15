source("../../ptFiles.R")
#########################################################
## Motorcycle
#########################################################
#simulate data
n<-133
x.mcy<-MASS::mcycle$times
y.mcy<- MASS::mcycle$accel
Y.mcy<-(1/3)*x.mcy

train<-data.frame(x=x.mcy,y=y.mcy)
mod.dat<-data.frame(x=x.mcy,y=Y.mcy)

#fit models
res.mcy<-reglines(x=train$x, y=train$y)

#process estimates
test <- data.frame(x=seq(min(train$x),max(train$x),length.out=100))
mod.res<-cbind(test,res.mcy$results)

mod.res<-mod.res %>% pivot_longer(!x, 
                                  names_to = "Model",
                                  values_to = "y")

#########################################################
## Save Density Data
#########################################################
datfast<-res.mcy$ptfast
datfast<- datfast %>% mutate(Type=rep("FAST Sampler", nrow(datfast)))
datmcmc<-res.mcy$ptmcmc
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
mcyplot<-ggplot() +
  geom_tile(data = datdens, aes(x, y, fill= z))+
  scale_fill_gradient("Density",low = "white", high = "grey30")+#,limits=c(0,0.20))+
  theme_classic()+
  xlab("Time After Impact (ms)")+
  ylab("Head Acceleration (g)")+
  ylim(min(train$y),max(train$y))+
  ggtitle("Motorcycle Data (n=133)")+
  geom_point(data=train,aes(x=x,y=y),shape=3, size=0.50,color="grey45")+
  geom_line(data=datlines,aes(x=x,y=y, linetype=Model))+
  facet_wrap(.~Type, nrow=1)+
  theme(axis.text=element_text(size=sizeaxis),
        axis.title=element_text(size=sizeaxislab),
        plot.title=element_text(size=sizetitle),
        legend.title=element_text(size=sizeaxislab),
        legend.text=element_text(size=sizeaxis),
        strip.text=element_text(size=sizeaxislab))

pdf("motorcycle.pdf",width=7,height=3.5)
mcyplot
dev.off()
