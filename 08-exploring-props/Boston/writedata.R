###########################################################################################
###########################################################################################
# Boston Data
# https://www.openml.org/d/531
#
# Harrison, D. and Rubinfeld, D.L. 'Hedonic
# prices and the demand for clean air', J. Environ. Economics & Management,
# vol.5, 81-102, 1978. 
###########################################################################################
###########################################################################################
#data.files<-list.files()
library(tidyverse)
source("convertData.R")

dat<-read.csv("dataset.csv", na.strings = "?")

dat <- dat %>%
  mutate(CHAS = factor(CHAS))

dat <- dat %>%
  select(MEDV , !MEDV)

dat<-dat[complete.cases(dat),]
datcat<-dat

dat<-convertData(dat)

library(GGally)
#pdf(file = "Bostoncat.pdf", width = 12, height = 8)
png(file = "Bostoncat.png", width = 12, height = 8, units="in", res=300)
ggpairs(datcat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()

#pdf(file = "Bostoncont.pdf", width = 12, height = 8)
png(file = "Bostoncont.png", width = 12, height = 8, units="in", res=300)
ggpairs(dat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()


train <- dat
traincat <- datcat

library(caret)
flds <- createFolds(train$MEDV, k=3)

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
                     