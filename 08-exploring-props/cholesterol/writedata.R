###########################################################################################
###########################################################################################
# Cholesterol Data
# https://www.openml.org/d/204
#
# Kilpatrick, D. & Cameron-Jones, M. (1998). Numeric prediction
# using instance-based learning with encoding length selection. In Progress
# in Connectionist-Based Information Systems. Singapore: Springer-Verlag.
###########################################################################################
###########################################################################################
#data.files<-list.files()
library(tidyverse)
source("convertData.R")

dat<-read.csv("dataset.csv", na.strings = "?")

dat <- dat %>%
  mutate(sex = factor(sex),
         cp = factor(cp),
         fbs = factor(fbs),
         restecg = factor(restecg),
         exang = factor(exang),
         slope = factor(slope),
         thal = factor(thal))

dat <- dat %>%
  select(chol, !chol)

dat<-dat[complete.cases(dat),]
datcat<-dat

dat<-convertData(dat)

library(GGally)
#pdf(file = "cholesterolcont.pdf", width = 12, height = 8)
png(file = "cholesterolcont.png", width = 12, height = 8, units = "in", res=300)
ggpairs(dat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()


# pdf(file = "cholesterolcat.pdf", width = 12, height = 8)
png(file = "cholesterolcat.png", width = 12, height = 8, units = "in", res=300)
ggpairs(datcat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()


train <- dat
traincat <- datcat

library(caret)
flds <- createFolds(train$chol, k=3)

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
                     