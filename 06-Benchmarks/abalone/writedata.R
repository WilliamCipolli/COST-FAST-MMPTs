###########################################################################################
###########################################################################################
# Abalone Data
# https://www.openml.org/d/42726
#
# Marine Resources Division
# Marine Research Laboratories - Taroona
# Department of Primary Industry and Fisheries, Tasmania
# GPO Box 619F, Hobart, Tasmania 7001, Australia
# (contact: Warwick Nash +61 02 277277, wnash@dpi.tas.gov.au)
###########################################################################################
###########################################################################################
#data.files<-list.files()
library(tidyverse)
source("../../convertData.R")

dat<-read.csv("dataset.csv", na.strings = "?")

dat <- dat %>%
  mutate(Sex = factor(Sex))

dat <- dat %>%
  select(Class_number_of_rings, !Class_number_of_rings)

dat<-dat[complete.cases(dat),]
datcat<-dat

set.seed(7272)
dat<-convertData(dat)

library(GGally)
#pdf(file = "abalonecat.pdf", width = 12, height = 8)
png(file = "abalonecat.png", width = 12, height = 8, units="in", res=300)
ggpairs(datcat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()
#pdf(file = "abalonecont.pdf", width = 12, height = 8)
png(file = "abalonecont.png", width = 12, height = 8, units="in", res=300)
ggpairs(dat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()



train <- dat
traincat <- datcat

library(caret)
flds <- createFolds(train$Class_number_of_rings, k=3)

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
                     