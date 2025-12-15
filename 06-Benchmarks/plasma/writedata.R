###########################################################################################
###########################################################################################
# Plasma Data
# https://www.openml.org/d/511
#
# Nierenberg DW, Stukel TA, Baron JA, Dain BJ, Greenberg ER. Determinants of plasma levels of beta-carotene and retinol. American Journal of Epidemiology 1989;130:511-521.
###########################################################################################
###########################################################################################
#data.files<-list.files()
library(tidyverse)
source("../../convertData.R")

dat<-read.csv("dataset.csv", na.strings = "?")

dat <- dat %>%
  mutate(SEX = factor(SEX),
         SMOKSTAT = factor(SMOKSTAT),
         VITUSE = factor(VITUSE))

dat <- dat %>%
  select(RETPLASMA , !RETPLASMA )

dat<-dat[complete.cases(dat),]
datcat<-dat

set.seed(7272)
dat<-convertData(dat)

library(GGally)
# pdf(file = "plasmacat.pdf", width = 12, height = 8)
png(file = "plasmacat.png", width = 12, height = 8, units="in", res=300)
ggpairs(datcat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()

library(GGally)
#pdf(file = "plasmacont.pdf", width = 12, height = 8)
png(file = "plasmacont.png", width = 12, height = 8, units="in", res=300)
ggpairs(dat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()


train <- dat
traincat <- datcat

library(caret)
flds <- createFolds(train$RETPLASMA , k=3)

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
                     