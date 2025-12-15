###########################################################################################
# Diabetes Data
# https://www.openml.org/d/41517
#
# Bradley Efron, Trevor Hastie, Iain Johnstone and Robert Tibshirani (2004) "Least Angle Regression," Annals of Statistics (with discussion), 407-499.
# (http://web.stanford.edu/~hastie/Papers/LARS/LeastAngle_2002.pdf)
###########################################################################################
###########################################################################################
#data.files<-list.files()
library(tidyverse)
source("convertData.R")

dat<-read.csv("dataset.csv", na.strings = "?")

dat <- dat %>%
  mutate(sex = if_else(sex<0, "Female", "Male")) %>%
  mutate(sex = factor(sex)) %>%
  select(class  , !class )

dat<-dat[complete.cases(dat),]
datcat<-dat

dat<-convertData(dat)


library(GGally)
pdf(file = "diabetescat.pdf", width = 12, height = 8)
ggpairs(datcat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()

pdf(file = "diabetescont.pdf", width = 12, height = 8)
ggpairs(dat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()


train <- dat
traincat <- datcat

library(caret)
flds <- createFolds(train$class, k=3)

dat.curr <- train 
dat.curr$set <- rep(NA, nrow(dat.curr))
dat.curr$set[flds[[1]]] = "test"
dat.curr$set[-flds[[1]]] = "train"
write.csv(x =dat.curr, file = paste("data/dat.csv",sep = ""),row.names = F)

dat.curr <- traincat
dat.curr$set <- rep(NA, nrow(dat.curr))
dat.curr$set[flds[[1]]] = "test"
dat.curr$set[-flds[[1]]] = "train"
write.csv(x =dat.curr, file = paste("data/datcat.csv",sep = ""),row.names = F)
                     