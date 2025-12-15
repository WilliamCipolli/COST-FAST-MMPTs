#########################################################
## Load libraries and helper functions
#########################################################
library("MASS")         ###Multivariate Normal ###LDA ###QDA
library("FNN")          ###KNN package
library("e1071")        ###SVM Package ##Naive Bayes
library("nnet")         ###Neural Net Package
library("randomForest") ###Random Forests
library("gbm")          ###Gradient Boosting
library("mclust")       ###mclustda
library("HDclassif")    ###EDDA #Mclustda
library("rpart")        ###cart
library("bartMachine")  ###bart
library("tidyverse")    ###handling data
library("lubridate")    ###Making age variable
library("ggplot2")      ###Plotting
library("patchwork")
library("Rcpp")         ###Rcpp
library("RcppArmadillo")
library("RcppDist")

#helper
getDeterminantMod<-function(A){
  determinant(A,log=T)$modulus[1]
}
sourceCpp("../../ptTest-thinning-efficient.cpp") #omnibus function
source("../../ptTestOld.R")                       #omnibus function
source("../../convertData.R") ###Converting Categorical variables
options(scipen=2000000)

#########################################################
## Load data
#########################################################
###Add age to original dataset
datcat<-read_csv("data/datcat3.csv")
g0cat  <- datcat  %>% filter(Fibrosis.Stage==0)
g0traincat  <- g0cat %>% filter(set=="train") %>% dplyr::select(!set)
g0testcat   <- g0cat %>% filter(set=="test") %>% dplyr::select(!set)
g1cat  <- datcat  %>% filter(Fibrosis.Stage==1)
g1traincat <-g1cat %>% filter(set=="train") %>% dplyr::select(!set)
g1testcat  <-g1cat %>% filter(set=="test") %>% dplyr::select(!set)

traincat <-rbind(g0traincat,g1traincat) %>% 
  mutate(Gender=factor(Gender)) %>%
  mutate(Fibrosis.Stage = factor(Fibrosis.Stage)) %>%
  dplyr::select(Gender, Age, MFAP4.U.mL, Fibrosis.Stage)
testcat <-rbind(g0testcat,g1testcat)%>% 
  mutate(Gender=factor(Gender)) %>%
  mutate(Fibrosis.Stage = factor(Fibrosis.Stage)) %>%
  dplyr::select(Gender, Age, MFAP4.U.mL, Fibrosis.Stage)

g0traincat  <- g0traincat %>% dplyr::select(Gender, Age, MFAP4.U.mL)
g0testcat   <- g0testcat  %>% dplyr::select(Gender, Age, MFAP4.U.mL)
g1traincat <-g1traincat   %>% dplyr::select(Gender, Age, MFAP4.U.mL)
g1testcat  <-g1testcat    %>% dplyr::select(Gender, Age, MFAP4.U.mL)
  
###Add age to original dataset
datcont <- read_csv("data/dat3.csv")

g0cont <- datcont %>% filter(Fibrosis.Stage==0)
g0traincont <- g0cont %>% filter(set=="train") %>% dplyr::select(!set)
g0testcont  <- g0cont %>% filter(set=="test") %>% dplyr::select(!set)
g1cont <- datcont %>% filter(Fibrosis.Stage==1)
g1traincont<-g1cont %>% filter(set=="train") %>% dplyr::select(!set)
g1testcont <-g1cont %>% filter(set=="test") %>% dplyr::select(!set)

traincont<-rbind(g0traincont,g1traincont)  %>% 
  mutate(Fibrosis.Stage = factor(Fibrosis.Stage)) %>%
  dplyr::select(Gender, Age, MFAP4.U.mL, Fibrosis.Stage)

testcont<-rbind(g0testcont,g1testcont) %>% 
  mutate(Fibrosis.Stage = factor(Fibrosis.Stage)) %>%
  dplyr::select(Gender, Age, MFAP4.U.mL, Fibrosis.Stage)

g0traincont <- g0traincont %>%  dplyr::select(Gender, Age, MFAP4.U.mL)
g0testcont  <- g0testcont  %>%  dplyr::select(Gender, Age, MFAP4.U.mL)
g1traincont <-g1traincont  %>%  dplyr::select(Gender, Age, MFAP4.U.mL)
g1testcont  <-g1testcont   %>%  dplyr::select(Gender, Age, MFAP4.U.mL)

traincontcombined<-traincont%>%dplyr::select(-Fibrosis.Stage)
testcontcombined<-testcont%>%dplyr::select(-Fibrosis.Stage)


###KNN - how many neighbors
knnNeighbors<-round(sqrt(nrow(traincont)))
###ANN - how many hidden nodes
d=ncol(traincont)-1
annSize<-ceiling(2*d/3)

#########################################################
## One/Two Rotations
#########################################################
condPRB<-matrix(0, nrow=nrow(testcontcombined), ncol=2+1, byrow = TRUE);
condPRB[,1]<-ptTestOLD(train=g0traincont,
                       test=testcontcombined,
                       save=0, burnin=0,test.equal.weight=F)$prb *
  (nrow(g0traincont)/(nrow(g0traincont)+nrow(g1traincont)));
condPRB[,2]<-ptTestOLD(train=g1traincont,
                       test=testcontcombined,
                       save=0, burnin=0,test.equal.weight=F)$prb*
  (nrow(g1traincont)/(nrow(g0traincont)+nrow(g1traincont)));

for(j in 1:nrow(condPRB)){
  condPRB[j,3]<-which.max(c(condPRB[j,1],condPRB[j,2]))-1
}
pt1class<-condPRB[,3]

#########################################################
## FAST Sampler
#########################################################
burnin = 1000;
save = 10000;
thin = 0;
pt.new0<-ptTest(train = as.matrix(g0traincont), 
                test = as.matrix(testcontcombined),
                maxJ = 8, save=save,burnin=burnin,thin=thin, fast=T)
pt.new1<-ptTest(train = as.matrix(g1traincont),
                test = as.matrix(testcontcombined),
                maxJ = 8, save=save,burnin=burnin,thin=thin,fast=T)

condPRB<-matrix(0, nrow=nrow(testcontcombined), ncol=2+1, byrow = TRUE);
condPRB[,1]<-pt.new0$pvec*(nrow(g0traincont)/(nrow(g0traincont)+nrow(g1traincont)));
condPRB[,2]<-pt.new1$pvec*(nrow(g1traincont)/(nrow(g0traincont)+nrow(g1traincont)));
for(j in 1:nrow(condPRB)){
  condPRB[j,3]<-which.max(c(condPRB[j,1],condPRB[j,2]))-1
}
pt2class<-condPRB[,3]

#########################################################
## Full Sampler
#########################################################
burnin = 100000;
save = 1000000;
thin = 0;
pt.new0<-ptTest(train = as.matrix(g0traincont), 
                test = as.matrix(testcontcombined),
                maxJ = 8, save=save,burnin=burnin,thin=thin)

pt.new1<-ptTest(train = as.matrix(g1traincont), test = as.matrix(testcontcombined),
                maxJ = 8, save=save,burnin=burnin,thin=thin)

condPRB<-matrix(0, nrow=nrow(testcontcombined), ncol=2+1, byrow = TRUE);
condPRB[,1]<-pt.new0$pvec*(nrow(g0traincont)/(nrow(g0traincont)+nrow(g1traincont)));
condPRB[,2]<-pt.new1$pvec*(nrow(g1traincont)/(nrow(g0traincont)+nrow(g1traincont)));
for(j in 1:nrow(condPRB)){
  condPRB[j,3]<-which.max(c(condPRB[j,1],condPRB[j,2]))-1
}
pt3class<-condPRB[,3]

#########################################################
## Competitor Models
#########################################################
##################Log ###################
log.model<-glm(Fibrosis.Stage ~ Gender + Age + MFAP4.U.mL, data = traincat, family = "binomial")
log.class<-ifelse(predict(log.model, newdata = testcat, type = "response")>0.50,1,0)

##################K Nearest Neighbors ###################
knndat<-traincont %>% dplyr::select(-Fibrosis.Stage)
knntest <- testcont %>% dplyr::select(-Fibrosis.Stage)
cl<-traincont$Fibrosis.Stage

knn.model<-knn(train=knndat,test=knntest,cl=cl,k=knnNeighbors)
knn.class<-as.character(knn.model)

##################Support Vector Machines#################
svmlin.model  <-svm(Fibrosis.Stage ~ Gender + Age + MFAP4.U.mL, data=traincat,kernel="linear")
svmpoly.model <-svm(Fibrosis.Stage ~ Gender + Age + MFAP4.U.mL, data=traincat,kernel="poly")
svmrad.model  <-svm(Fibrosis.Stage ~ Gender + Age + MFAP4.U.mL, data=traincat,kernel="radial",degree=3)
svmsig.model  <-svm(Fibrosis.Stage ~ Gender + Age + MFAP4.U.mL, data=traincat,kernel="sigmoid")
svmlin.class<-predict(svmlin.model, testcat)
svmpol.class<-predict(svmpoly.model, testcat)
svmrad.class<-predict(svmrad.model, testcat)
svmsig.class<-predict(svmsig.model, testcat)

########################### ANN ##########################
clann<-class.ind(cl) 
dataset<-data.frame(traincat,clann)
colnames(dataset)<-c(colnames(testcat),"class1","class2")
nnet.model<-nnet(clann~Gender + Age + MFAP4.U.mL,data = dataset,size=annSize,softmax=TRUE,trace=FALSE)
ann.class<-predict(nnet.model, testcat,type="class")

########################## LDA ##########################
dataset<-data.frame(traincat,cl)
colnames(dataset)<-c(colnames(testcat),"class")
lda.model<-lda(class ~ Gender + Age + MFAP4.U.mL,data=dataset)
lda.class<-predict(lda.model,testcat)$class

########################## QDA ##########################
dataset<-data.frame(traincat,cl)
colnames(dataset)<-c(colnames(testcat),"class")
qda.model<-qda(class ~ Gender + Age + MFAP4.U.mL,data=dataset)
qda.class<-predict(qda.model,testcat)$class

##################### Random Forest #####################
dataset<-data.frame(traincat,cl)
colnames(dataset)<-c(colnames(testcat),"class")
rf.model<-randomForest(class ~ Gender + Age + MFAP4.U.mL,data=dataset)
rf.class <-predict(rf.model,testcat)

###################### Naive Bayes ######################
dataset<-data.frame(traincat,cl)
colnames(dataset)<-c(colnames(testcat),"class")
nb.model<-naiveBayes(class ~ Gender + Age + MFAP4.U.mL,data=dataset)
nb.class <-predict(nb.model,testcat)

###################### mclustda #########################
mclust.model <- MclustDA(traincont, cl,verbose=FALSE)
mclust.class<-predict(mclust.model, testcont)$classification

###################### EDDA #############################
EDDA.model <- MclustDA(traincont, cl,verbose=FALSE,modelType="EDDA")
edda.class <-predict(EDDA.model, testcont)$classification

###################### HDDA #############################
hddadat<-traincont%>%dplyr::select(-Fibrosis.Stage)
hddadattest<-testcont%>%dplyr::select(-Fibrosis.Stage)
HDDA.model <- hdda(hddadat, cl, scaling=TRUE, d_select="bic")
hdda.class<-predict(HDDA.model, hddadattest)$class

###################### gbm #############################
dataset<-data.frame(traincont,cl)
colnames(dataset)<-c(colnames(testcont),"class")
dataset$class<-as.numeric(dataset$class)-1
gbm.model<-gbm(class ~ Gender + Age + MFAP4.U.mL, data=dataset, dist="adaboost",n.tree = 100)
classification<-predict(gbm.model,testcont,n.trees=100)
gbm.class<- ifelse(classification<0,0,1)

####################### CART ######################
dataset<-data.frame(traincontcombined,cl)
colnames(dataset)<-c(colnames(testcontcombined),"class")
cart.model<-rpart(class ~ Gender + Age + MFAP4.U.mL, data=dataset)
cart.class <- predict(cart.model, testcontcombined,type = "class")

###################### MPBART ######################
bm<-bartMachine(data.frame(traincontcombined),cl,num_trees = 100)
bart.class<-predict(bm, testcontcombined, type="class")


#########################################################
## Save and Return Results
#########################################################
res<- cbind(testcat, pt1class, pt2class, pt3class, 
            log.class, 
            knn.class, svmlin.class, svmpol.class, svmrad.class, 
            svmsig.class, ann.class, lda.class, qda.class, rf.class, 
            nb.class, mclust.class, edda.class, hdda.class, gbm.class, 
            cart.class, bart.class)
write_csv(data.frame(res), "pred/pred3.csv")
