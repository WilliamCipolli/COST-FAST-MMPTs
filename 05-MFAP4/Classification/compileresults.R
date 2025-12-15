#########################################################
## Load libraries and helper functions
#########################################################
library(tidyverse)
dat<-NULL

for(i in 1:10){
  datcurr<-read_csv(file = paste("pred/pred",i,".csv",sep = ""))
  dat<-rbind(dat,datcurr)
}

colnames(dat)

mean(dat$Fibrosis.Stage == dat$pt1class)
mean(dat$Fibrosis.Stage == dat$pt2class)
mean(dat$Fibrosis.Stage == dat$pt3class)
mean(dat$Fibrosis.Stage == dat$log.class)
mean(dat$Fibrosis.Stage == dat$knn.class)
mean(dat$Fibrosis.Stage == dat$svmlin.class)
mean(dat$Fibrosis.Stage == dat$svmpol.class)
mean(dat$Fibrosis.Stage == dat$svmrad.class)
mean(dat$Fibrosis.Stage == dat$svmsig.class)
mean(dat$Fibrosis.Stage == dat$ann.class)
mean(dat$Fibrosis.Stage == dat$lda.class)
mean(dat$Fibrosis.Stage == dat$qda.class)
mean(dat$Fibrosis.Stage == dat$rf.class)
mean(dat$Fibrosis.Stage == dat$nb.class)
mean(dat$Fibrosis.Stage == dat$mclust.class)
mean(dat$Fibrosis.Stage == dat$edda.class)
mean(dat$Fibrosis.Stage == dat$hdda.class)
mean(dat$Fibrosis.Stage == dat$gbm.class)
mean(dat$Fibrosis.Stage == dat$cart.class)
mean(dat$Fibrosis.Stage == dat$bart.class)
