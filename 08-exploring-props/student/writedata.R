###########################################################################################
###########################################################################################
# Student Performance Data
# https://www.openml.org/d/42351
#
# P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student Performance. In A. Brito and J. Teixeira Eds., Proceedings of 5th FUture BUsiness TEChnology Conference (FUBUTEC 2008) pp. 5-12, Porto, Portugal, April, 2008, EUROSIS, ISBN 978-9077381-39-7.
###########################################################################################
###########################################################################################
#data.files<-list.files()
library(tidyverse)
source("convertData.R")

dat<-read.csv("dataset.csv", na.strings = "?")

dat <- dat %>%
  mutate(school = factor(school),
         sex    = factor(sex),
         address= factor(address),
         famsize= factor(famsize),
         Pstatus= factor(Pstatus),
         Mjob   = factor(Mjob),
         Fjob   = factor(Fjob),
         reason = factor(reason),
         guardian=factor(guardian),
         schoolsup=factor(schoolsup),
         famsup=factor(famsup),
         paid=factor(paid),
         activities=factor(activities),
         nursery=factor(nursery),
         higher=factor(higher),
         internet=factor(internet),
         romantic=factor(romantic))

dat <- dat %>%
  select(G3, !G3)

dat<-dat[complete.cases(dat),]
datcat<-dat

dat<-convertData(dat)

library(GGally)
pdf(file = "studentcat.pdf", width = 12, height = 8)
ggpairs(datcat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()

pdf(file = "studentcont.pdf", width = 12, height = 8)
ggpairs(dat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()


train <- dat
traincat <- datcat

library(caret)
flds <- createFolds(train$G3, k=3)

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
                     