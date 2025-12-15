################################################################
#Libraries and Helpers
################################################################
library("ggplot2")
library("patchwork")
library("mvtnorm")

################################################################
#Generate plotting data
################################################################
dogbowl<-function(x,y){
  (2*pi)^(-3/2)*(x^2+y^2)^(-1/2)*exp(-.5*(sqrt(x^2+y^2)-10)^2)
}

Xseq=seq(-13,13,length.out = 100)
Yseq=seq(-13,13,length.out = 100)

grid=expand.grid(Xseq,Yseq)
z<-dogbowl(grid[,1],grid[,2])

# Dummy data
datdb <- expand.grid(X=Xseq, Y=Yseq)
datdb$Z<- z
datdb$datagroup = rep("Dog-Bowl", nrow(datdb))

mu<-rep(0,2)
c<-5
sigma<- c*diag(2)
datn <- expand.grid(X=Xseq, Y=Yseq)
datn$Z<-dmvnorm(grid,mean=mu,sigma)
datn$datagroup = rep("Gaussian", nrow(datn))

################################################################
#Create and Save Plot
################################################################
sizeaxis<- 8
sizeaxislab<- 10
sizetitle<- 11

dat<- rbind(datdb, datn)
pdf(file = "true.pdf",width=6,height=4.5)
ggplot(data = dat, aes(X, Y, fill= Z)) + 
  geom_tile()+
  scale_fill_gradient("Density",low = "white", high = "grey30")+#,limits=c(0,0.20))+
  theme_classic()+
  xlab(bquote(X[1]))+
  ylab(bquote(X[2]))+
  ggtitle("Dog-Bowl Density and Standard Bivariate Gaussian",
          subtitle = "True Densities")+
  xlim(-13,13)+
  ylim(-13,13)+
  theme(axis.text=element_text(size=sizeaxis),
      axis.title=element_text(size=sizeaxislab),
      plot.title=element_text(size=sizetitle),
      legend.title=element_text(size=sizeaxislab),
      legend.text=element_text(size=sizeaxis),
      strip.text=element_text(size=sizeaxislab),
      legend.position = "bottom",
      legend.key.width = unit(0.75, "in"))+
  facet_wrap(.~datagroup, nrow = 1,  )
dev.off()