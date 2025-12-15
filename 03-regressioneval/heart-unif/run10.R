#########################################################
## Load libraries and helper functions
#########################################################
source("../../ptFiles-regest.R")
train<-read.csv(file = "data/dat10.csv",header = T)

curr.train<-train %>% filter(set=="train") %>% dplyr::select(!set)
curr.test<-train %>% filter(set=="test") %>% dplyr::select(!set)

curr.regest <- regest(curr.train, curr.test)

curr.output <- curr.test %>% 
  mutate(id = rownames(curr.test)) %>%
  dplyr::select(c(id, colnames(curr.test))) %>%
  bind_cols(curr.regest) 

write.csv(x = curr.output, file = "pred/pred10.csv",row.names = F)
