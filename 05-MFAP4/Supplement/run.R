#########################################################
## Load Data
#########################################################
library("lubridate")
library("gplots")

dat <- read.csv(file="biomarker.csv", header=TRUE, sep=",")
###Calculate the age of each subject
###Create Date Variable for Date Sampled
dos<-mdy(dat$Date.of.sampling)
dos.year<-year(dos)
###Create age Variable
age <- dos.year - dat$Year.of.Birth
###Add age to original dataset
dat<-data.frame(dat,age)

dat$Fibrosis.Stage2<-rep(NA,nrow(dat))
dat$Fibrosis.Stage2[which(dat$Fibrosis.Stage==0 | dat$Fibrosis.Stage==1 | 
                            dat$Fibrosis.Stage==2)]<-"0-2"
dat$Fibrosis.Stage2[which(dat$Fibrosis.Stage==3 | dat$Fibrosis.Stage==4)]<-"3-4"
boxplotDat<-data.frame(MFAP4.U.mL=c(dat$MFAP4.U.mL,dat$MFAP4.U.mL), 
                       Fibrosis.Stage=c(dat$Fibrosis.Stage,dat$Fibrosis.Stage2))
boxplotDat$Fibrosis.Stage<-factor(boxplotDat$Fibrosis.Stage, levels = c("0","1","2","0-2","3-4","3","4"))

#########################################################
## Create Save Plot
#########################################################
pdf("mfap4.pdf", width = 8, height = 4)
boxplot2(MFAP4.U.mL~Fibrosis.Stage,data=boxplotDat,
         border=c("grey","grey","grey","black","black","grey","grey"),
         col=c("white","white","white","grey","grey","white","white"),
         xlab="Fibrosis Stage",ylab="MFAP4",top=T, ylim=c(0,100))


stripchart(MFAP4.U.mL ~ Fibrosis.Stage, data = boxplotDat,
           vertical = TRUE, method="jitter",
           pch = 1,add=TRUE,
           col=c("grey","grey","grey","black","black","grey","grey"),
           subset=which(boxplotDat$Fibrosis.Stage!="0-2"&boxplotDat$Fibrosis.Stage!="3-4")
)
dev.off()
