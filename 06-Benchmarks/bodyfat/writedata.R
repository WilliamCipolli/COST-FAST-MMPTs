###########################################################################################
###########################################################################################
# Bodyfat Data
# https://www.openml.org/d/560
#
# Author: Roger W. Johnson
# Source: [UCI (not available anymore)](https://archive.ics.uci.edu/ml/index.php), [TunedIT](http://tunedit.org/repo/UCI/numeric/bodyfat.arff)
# Please cite: None.
###########################################################################################
###########################################################################################
#data.files<-list.files()
library(tidyverse)
source("../../convertData.R")

dat<-read.csv("dataset.csv", na.strings = "?")

dat <- dat %>%
  select(class, !class)

dat<-dat[complete.cases(dat),]
datcat<-dat

library(GGally)
#pdf(file = "bodyfat.pdf", width = 12, height = 8)
png(file = "bodyfat.png", width = 12, height = 8, units="in", res = 300)
ggpairs(dat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()


train <- dat
traincat <- datcat

set.seed(7272)
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
                     