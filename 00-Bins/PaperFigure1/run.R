################################################################
# Load Libraries
################################################################
library(tidyverse)
library(patchwork)

################################################################
# Text size for plots
################################################################
sizeannotate<- 9*0.36 #mm to point
sizeaxis<- 8
sizeaxislab<- 10
sizetitle<- 11

################################################################
# Plot level j=1
################################################################
ptplot1<-ggplot()+
  geom_vline(xintercept=qnorm(0.5), size=1.5)+
  geom_hline(yintercept=qnorm(0.5), size=1.5)+
  theme_minimal()+
  xlab(bquote(X[1]))+
  ylab(bquote(X[2]))+
  ggtitle("At level j=1")+
  xlim(-3,3)+
  ylim(-3,3)+
  theme(axis.text=element_text(size=sizeaxis),
        axis.title=element_text(size=sizeaxislab),
        plot.title=element_text(size=sizetitle))+
  annotate("text", color="grey40", x=1.5, y=1.5,
           label = deparse(bquote(B(1*",{"*2*","*2*"}"))),
           parse = T, size=sizeannotate)+
  annotate("text", color="grey40", x=1.5, y=1,
           label = deparse(bquote(r==3)),
           parse = T, size=sizeannotate)+
  annotate("text", color="grey40", x=1.5, y=-1.5,
           label = deparse(bquote(B(1*",{"*2*","*1*"}"))),
           parse = T, size=sizeannotate)+
  annotate("text", color="grey40", x=1.5, y=-2,
           label = deparse(bquote(r==1)),
           parse = T, size=sizeannotate)+
  annotate("text", color="grey40", x=-1.5, y=1.5,
           label = deparse(bquote(B(1*",{"*1*","*2*"}"))),
           parse = T, size=sizeannotate)+
  annotate("text", color="grey40", x=-1.5, y=1,
           label = deparse(bquote(r==2)),
           parse = T, size=sizeannotate)+
  annotate("text", color="grey40", x=-1.5, y=-1.5,
           label = deparse(bquote(B(1*",{"*1*","*1*"}"))),
           parse = T, size=sizeannotate)+
  annotate("text", color="grey40", x=-1.5, y=-2,
           label = deparse(bquote(r==0)),
           parse = T, size=sizeannotate)

################################################################
# Plot level j=2
################################################################
ptplot2<-ggplot()+
  geom_vline(xintercept=qnorm(0.5), size=1.5)+
  geom_hline(yintercept=qnorm(0.5), size=1.5)+
  theme_minimal()+
  xlab(bquote(X[1]))+
  ylab(bquote(X[2]))+
  ggtitle("At level j=2")+
  xlim(-3,3)+
  ylim(-3,3)+
  theme(axis.text=element_text(size=sizeaxis),
        axis.title=element_text(size=sizeaxislab),
        plot.title=element_text(size=sizetitle))

j=2
for(k in 1:(2^j-1)){
  ptplot2 <- ptplot2 + 
    geom_vline(xintercept=qnorm(k/(2^j)), size=0.75)+
    geom_hline(yintercept=qnorm(k/(2^j)), size=0.75)
}

ptplot2 <- ptplot2 +
  annotate("text", color="grey40", x=c(-2,-0.3,0.3,2), y=-2,
           label = 0:3, size=sizeannotate)+
  annotate("text", color="grey40", x=c(-2,-0.3,0.3,2), y=-0.3,
           label = 4:7, size=sizeannotate)+
  annotate("text", color="grey40", x=c(-2,-0.3,0.3,2), y=0.3,
           label = 8:11, size=sizeannotate)+
  annotate("text", color="grey40", x=c(-2,-0.3,0.3,2), y=2,
           label = 12:15, size=sizeannotate)

################################################################
# Plot level j=3
################################################################
ptplot3<-ggplot()+
  geom_vline(xintercept=qnorm(0.5), size=1.5)+
  geom_hline(yintercept=qnorm(0.5), size=1.5)+
  theme_minimal()+
  xlab(bquote(X[1]))+
  ylab(bquote(X[2]))+
  ggtitle("At level j=3")+
  xlim(-3,3)+
  ylim(-3,3)+
  theme(axis.text=element_text(size=sizeaxis),
        axis.title=element_text(size=sizeaxislab),
        plot.title=element_text(size=sizetitle))

j=2
for(k in 1:(2^j-1)){
  ptplot3 <- ptplot3 + 
    geom_vline(xintercept=qnorm(k/(2^j)), size=0.75)+
    geom_hline(yintercept=qnorm(k/(2^j)), size=0.75)
}

j=3
for(k in 1:(2^j-1)){
  ptplot3 <- ptplot3 + 
    geom_vline(xintercept=qnorm(k/(2^j)), size=0.25)+
    geom_hline(yintercept=qnorm(k/(2^j)), size=0.25)
}

ptplot3<-ptplot3+
  annotate("text", color="grey40", x=c(-2,-0.9,-0.5,-0.17,0.17,0.5,0.9,2), 
           y=rep(-2.5,8),
           label = 0:7, size=sizeannotate)+
  annotate("text", color="grey40", x=c(-2,-0.9,-0.5,-0.17,0.17,0.5,0.9,2), 
           y=rep(-0.9,5,8),
           label = 8:15, size=sizeannotate)+
  annotate("text", color="grey40", x=c(-2,-0.9,-0.5,-0.17,0.17,0.5,0.9,2), 
           y=rep(-0.5,8),
           label = 16:23, size=sizeannotate)+
  annotate("text", color="grey40", x=c(-2,-0.9,-0.5,-0.17,0.17,0.5,0.9,2), 
           y=rep(-0.17,8),
           label = 24:31, size=sizeannotate)+
  annotate("text", color="grey40", x=c(-2,-0.9,-0.5,-0.17,0.17,0.5,0.9,2), 
           y=rep(0.17,8),
           label = 32:39, size=sizeannotate)+
  annotate("text", color="grey40", x=c(-2,-0.9,-0.5,-0.17,0.17,0.5,0.9,2), 
           y=rep(0.5,8),
           label = 40:47, size=sizeannotate)+
  annotate("text", color="grey40", x=c(-2,-0.9,-0.5,-0.17,0.17,0.5,0.9,2), 
           y=rep(0.9,8),
           label = 48:55, size=sizeannotate)+
  annotate("text", color="grey40", x=c(-2,-0.9,-0.5,-0.17,0.17,0.5,0.9,2), 
           y=rep(2.5,8),
           label = 56:63, size=sizeannotate)

################################################################
# Save Plots
################################################################
pdf("ptbins.pdf",width=9, height=4)
ptplot1+ptplot2+ptplot3+
  plot_annotation(title = "Polya Tree Partitions") & 
  theme(legend.position = "top")
dev.off()

pdf("ptbinsvert.pdf",width=4, height=8.5)
ptplot1/ptplot2/ptplot3+
  plot_annotation(title = "Polya Tree Partitions") & 
  theme(legend.position = "top")
dev.off()

################################################################
# Checking position of points and counting of cells j=1,2
################################################################
# j=1 
2^(1*(1-1))*floor(2*pnorm(-1))+ 2^(1*(2-1))*floor(2*pnorm(-1))
2^(1*(1-1))*floor(2*pnorm(1)) + 2^(1*(2-1))*floor(2*pnorm(-1))
2^(1*(1-1))*floor(2*pnorm(-1))+ 2^(1*(2-1))*floor(2*pnorm(1))
2^(1*(1-1))*floor(2*pnorm(1)) + 2^(1*(2-1))*floor(2*pnorm(1))

# j=2
2^(2*(1-1))*floor(2^2*pnorm(-2))   + 2^(2*(2-1))*floor(2^2*pnorm(-2))
2^(2*(1-1))*floor(2^2*pnorm(-0.5)) + 2^(2*(2-1))*floor(2^2*pnorm(-2))
2^(2*(1-1))*floor(2^2*pnorm(0.5)) + 2^(2*(2-1))*floor(2^2*pnorm(-2))
2^(2*(1-1))*floor(2^2*pnorm(2))    + 2^(2*(2-1))*floor(2^2*pnorm(-2))
2^(2*(1-1))*floor(2^2*pnorm(-2))   + 2^(2*(2-1))*floor(2^2*pnorm(-0.5))
2^(2*(1-1))*floor(2^2*pnorm(-0.5)) + 2^(2*(2-1))*floor(2^2*pnorm(-0.5))
2^(2*(1-1))*floor(2^2*pnorm(0.5)) + 2^(2*(2-1))*floor(2^2*pnorm(-0.5))
2^(2*(1-1))*floor(2^2*pnorm(2))    + 2^(2*(2-1))*floor(2^2*pnorm(-0.5))
2^(2*(1-1))*floor(2^2*pnorm(-2))   + 2^(2*(2-1))*floor(2^2*pnorm(0.5))
2^(2*(1-1))*floor(2^2*pnorm(-0.5)) + 2^(2*(2-1))*floor(2^2*pnorm(0.5))
2^(2*(1-1))*floor(2^2*pnorm(0.5)) + 2^(2*(2-1))*floor(2^2*pnorm(0.5))
2^(2*(1-1))*floor(2^2*pnorm(2))    + 2^(2*(2-1))*floor(2^2*pnorm(0.5))
2^(2*(1-1))*floor(2^2*pnorm(-2))   + 2^(2*(2-1))*floor(2^2*pnorm(2))
2^(2*(1-1))*floor(2^2*pnorm(-0.5)) + 2^(2*(2-1))*floor(2^2*pnorm(2))
2^(2*(1-1))*floor(2^2*pnorm(0.5)) + 2^(2*(2-1))*floor(2^2*pnorm(2))
2^(2*(1-1))*floor(2^2*pnorm(2))    + 2^(2*(2-1))*floor(2^2*pnorm(2))
