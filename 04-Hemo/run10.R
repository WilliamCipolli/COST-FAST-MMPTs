#########################################################
## Load libraries and helper functions
#########################################################
source("../ptFiles-pred.R")
train<-read.csv(file = "data/dat10.csv",header = T)
train.cat<-read.csv(file = "data/datcat10.csv",header = T)

curr.train<-train %>% filter(set=="train") %>% dplyr::select(!set)
curr.test<-train %>% filter(set=="test") %>% dplyr::select(!set)

curr.train.cat<-train.cat %>% filter(set=="train") %>% dplyr::select(!set)
curr.test.cat<-train.cat %>% filter(set=="test") %>% dplyr::select(!set)

curr.train.cat$TX<-factor(curr.train.cat$TX,levels=c("PLACEBOQ3WK","NESP300Q3WK"))    #Treatment: NESP = darbpoetin alfa
curr.train.cat$SEX<-factor(curr.train.cat$SEX,levels=c("Female","Male"))              #Sex
curr.train.cat$B_ECOGN<-factor(curr.train.cat$B_ECOGN,levels=c(1,2))                  #ECOG: Performance Status at randomization: <2 or 2
curr.train.cat$B_WGTN<-factor(curr.train.cat$B_WGTN,levels=c(1,2))                    #Weight: baseline <65kg, >=65kg
curr.train.cat$B_LDHN<-factor(curr.train.cat$B_LDHN,levels=c(1,2))                    #LDH: baseline <65kg, >=65kg

curr.test.cat$TX<-factor(curr.test.cat$TX,levels=c("PLACEBOQ3WK","NESP300Q3WK"))    #Treatment: NESP = darbpoetin alfa
curr.test.cat$SEX<-factor(curr.test.cat$SEX,levels=c("Female","Male"))              #Sex
curr.test.cat$B_ECOGN<-factor(curr.test.cat$B_ECOGN,levels=c(1,2))                  #ECOG: Performance Status at randomization: <2 or 2
curr.test.cat$B_WGTN<-factor(curr.test.cat$B_WGTN,levels=c(1,2))                    #Weight: baseline <65kg, >=65kg
curr.test.cat$B_LDHN<-factor(curr.test.cat$B_LDHN,levels=c(1,2))                    #LDH: baseline <65kg, >=65kg


curr.regest <- regest(train = curr.train, test = curr.test, 
                      traincat = curr.train.cat, testcat = curr.test.cat)

curr.output <- curr.test.cat %>% 
  mutate(id = rownames(curr.test)) %>%
  dplyr::select(c(id, colnames(curr.test))) %>%
  bind_cols(curr.regest) 

write.csv(x = curr.output, file = "pred/pred10.csv",row.names = F)
