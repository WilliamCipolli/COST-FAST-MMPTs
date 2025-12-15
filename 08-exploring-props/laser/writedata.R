###########################################################################################
# Laser Data
# https://www.openml.org/d/42364
#
# KEEL - [original](https://sci2s.ugr.es/keel/dataset.php?cod=47)
###########################################################################################
###########################################################################################
#data.files<-list.files()
library(tidyverse)
source("convertData.R")

dat<-read.csv("dataset.csv", na.strings = "?")

dat <- dat %>%
  select(Output , !Output )

dat<-dat[complete.cases(dat),]
datcat<-dat

library(GGally)
pdf(file = "laser.pdf", width = 12, height=8)
#png(file = "laser.png", width = 12, height=8, units = "in", res = 300)
ggpairs(dat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()


train <- dat
traincat <- datcat

library(caret)
flds <- createFolds(train$Output , k=3)

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
                     