#########################################################
## Load libraries and helper functions
#########################################################
library(tidyverse)
library(lubridate)
source("../../convertData.R")   ###Convert data

#########################################################
## Load data
#########################################################
dat<-read_csv("biomarker.csv")

###Create Date Variable for Date Sampled
dos<-mdy(dat$`Date of sampling`)
dos.year<-year(dos)

###Create age Variable
age <- dos.year - dat$`Year of Birth`

###Add age to original dataset
dat <- dat %>% 
  add_column(Age=age) %>%
  mutate(Gender=factor(Gender)) %>%
  mutate(`Fibrosis Stage` = ifelse(`Fibrosis Stage`<2,0,1)) %>%
  mutate(`Fibrosis Stage` = factor(`Fibrosis Stage`)) %>%
  dplyr::select(Gender, Age, `MFAP4 U/mL`, `Fibrosis Stage`) 

dat <- dat[complete.cases(dat),]

datcat<-data.frame(dat)
dat<-data.frame(convertData(data.frame(dat)[,-4]),Fibrosis.Stage=dat$`Fibrosis Stage`)


library(GGally)
png(file = "mfap4cont.png", width = 12, height = 8, units = "in", res = 300)
#pdf(file = "mfap4cont.pdf", width = 12, height = 8)
ggpairs(dat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()

#pdf(file = "mfap4cat.pdf", width = 12, height = 8)
png(file = "mfap4at.png", width = 12, height = 8, units = "in", res = 300)
ggpairs(datcat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()

#########################################################
## Create Folds and Save Data
#########################################################
train <- dat
traincat <- datcat

library(caret)
flds <- createFolds(train$MFAP4.U.mL, k=10)

for(i in 1:10){
  dat.curr <- train 
  dat.curr$set <- rep(NA, nrow(dat.curr))
  dat.curr$set[flds[[i]]] = "test"
  dat.curr$set[-flds[[i]]] = "train"
  write.csv(x =dat.curr, file = paste("data/dat",i,".csv",sep = ""),row.names = F)

  dat.curr <- traincat
  dat.curr$set <- rep(NA, nrow(dat.curr))
  dat.curr$set[flds[[i]]] = "test"
  dat.curr$set[-flds[[i]]] = "train"
  write.csv(x =dat.curr, file = paste("data/datcat",i,".csv",sep = ""),row.names = F)
}
