library(haven)
library(tidyverse)
source("../convertData.R")   ###Convert data

fulldata<-read_sas(data_file="hemodat/a_eendfu.sas7bdat") #Efficacy Endpoints Analysis dataset (Entire Period up to the 496th Death)

dat<-as.data.frame(fulldata[,c("CHHB",                            #Response
                               "TX", "B_ECOGN","B_LDHN",          #Section 10.5.1.1 Change in Hemoglobin Concentration
                               "SEX","AGE","B_HGB","B_WGTN"       
)]) 

dat$TX<-factor(dat$TX,levels=c("PLACEBOQ3WK","NESP300Q3WK"))    #Treatment: NESP = darbpoetin alfa
dat$SEX<-factor(dat$SEX,levels=c("Female","Male"))              #Sex
dat$B_ECOGN<-factor(dat$B_ECOGN,levels=c(1,2))                  #ECOG: Performance Status at randomization: <2 or 2
dat$B_WGTN<-factor(dat$B_WGTN,levels=c(1,2))                    #Weight: baseline <65kg, >=65kg
dat$B_LDHN<-factor(dat$B_LDHN,levels=c(1,2))                    #LDH: baseline <65kg, >=65kg

dat<-dat %>% dplyr::select(CHHB, everything())
dat<-dat[complete.cases(dat),]
datcat<-dat
dat<-convertData(dat)

library(GGally)
#pdf(file = "amgencontpairs.pdf", width = 12, height = 8)
png(file = "amgencontpairs.png", width = 12, height = 8, units="in", res=300)
ggpairs(dat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()

#pdf(file = "amgencatpairs.pdf", width = 12, height = 8)
png(file = "amgencatpairs.png", width = 12, height = 8, units="in", res=300)
ggpairs(datcat, diag=list(continuous = "barDiag")) + theme_bw()
dev.off()

train <- dat
traincat <- datcat

library(caret)
flds <- createFolds(train$CHHB, k=10)

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
