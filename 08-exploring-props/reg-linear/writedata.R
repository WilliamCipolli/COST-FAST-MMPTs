###########################################################################################
###########################################################################################
# Motorcycle Data (MASS)
###########################################################################################
###########################################################################################
#data.files<-list.files()
library(tidyverse)
source("convertData.R")

dat<-MASS::mcycle |> select(times, accel)

dat<-dat[complete.cases(dat),]

library(GGally)
pdf(file = "motorcycle.pdf", width = 12, height = 8)
ggpairs(dat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()

png(file = "motorcycle.png", width = 12, height = 8, units="in", res=300)
ggpairs(dat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()



train <- dat


library(caret)
flds <- createFolds(train$times, k=3)

dat.curr <- train 
dat.curr$set <- rep(NA, nrow(dat.curr))
dat.curr$set[flds[[1]]] = "test"
dat.curr$set[-flds[[1]]] = "train"
write.csv(x =dat.curr, file = paste("data/dat.csv",sep = ""),row.names = F)
