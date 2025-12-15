library("mvtnorm")
###################################################
###################################################
## This function 
###################################################
###################################################
###################################################
###################################################
## This function calculates the number of 
## observations in each piece of the partition
###################################################
###################################################
ptTrainOLD<-function(n,dim,z,J=5){
  ###n x dim matrix
  r=matrix(0,n,J)                           ### A place to keep track of observation bins
  ### Each element rij gives the numbered partition for level j of the Polya tree
  
  for(i in 1:n){                            ### For each observation i
    for(j in J:1){                          ### For each level of the Polya Tree j
      rfl=floor(2^j*pnorm(z[,i]))           ### Placement of observation i (rfl is multidimensional)
      rcurr<-0                              ### rcurr to 0
      for(t in 1:dim){                      ### Placement of observation i (rcurr is univariate)
        rcurr<-rcurr+2^(j*(t-1))*rfl[t]     
      }
      r[i,j]=rcurr                          ### Update the bin for observation i on level j
    }
  }
  r
}
###################################################
###################################################
## This function evaluates closeness parameters
## based on how well the data are fit via the 
## density estimation -- choose cp with best fit
###################################################
###################################################
jointPRBOLD<-function(rij,z,J,cp,ldet,ll=F){
  n<-ncol(z)
  dim<-nrow(z)
  
  ###################################################
  ## This part calculates the probability of the observed
  ## data given the rotation passed in (on the log scale)
  ##################################################
  sum<-dmvnorm(z[,1],mean=rep(0,dim),sigma=diag(dim),log=TRUE)          # Handle observation i=1
  
  for(i in 2:n){                                                        # Handle observations i=2, ..., n
    #j=1
    numer<- 2^dim * cp       + 2^dim * sum(rij[1:(i-1),1  ]==rij[i,1  ])
    denom<- 2^dim * cp       +         (i-1)
    sum<-sum+log(numer/denom)
    
    #j=2 to j=J
    for(j in 2:J){
      numer<- 2^dim * cp * j^2 + 2^dim * sum(rij[1:(i-1),j  ]==rij[i,j  ])
      denom<- 2^dim * cp * j^2 +         sum(rij[1:(i-1),j-1]==rij[i,j-1])
      sum<-sum+log(numer/denom)
    }
    
    #The normal part
    norm <- dmvnorm(z[,i],mean=rep(0,dim),sigma=diag(dim),log=TRUE)
    sum<-sum+norm
  }
  
  
  # Return these valuse
  if(ll==TRUE){
    ldet+sum
  }else{
    exp(ldet+sum)
  }
  
}
###################################################
###################################################
## This function calculates the probability of
## one observation (yc) using the polya tree method
###################################################
###################################################

pt1TestOLD<-function(yc,mu,sigmahalfinv,n,dim,J,cp=1,r,ldet){
  yc<-rbind(yc)
  ###################################################
  ## This part standardizes the observed yc
  ###################################################
  z=t(sigmahalfinv%*%t(yc-mu));
  
  ###################################################
  ## This part calculates the bins for this observation
  ## for each level j 
  ###################################################
  rnew=rep(0,J)
  for(j in J:1){
    rfl<-floor(2^j*pnorm(z))
    for(t in 1:dim){
      rnew[j]=rnew[j]+2^(j*(t-1))*rfl[t]
    }
  }
  
  ###################################################
  ## This part calculates the probability of the observed
  ## data given the rotation passed in (on the log scale)
  ##################################################
  ##line 1
  numer<-2^dim * cp + 2^dim*sum(r[,1]==rnew[1])
  denom<-2^dim * cp + n
  sum=log(numer/denom)
  
  ##line 2
  for(j in 2:J){
    numer<- 2^dim * cp * j^2 + 2^dim * sum(r[,j  ]==rnew[j])
    denom<- 2^dim * cp * j^2 +         sum(r[,j-1]==rnew[j-1])
    sum<-sum+log(numer/denom)
  }
  
  ##line 3
  norm <- dmvnorm(z,mean=rep(0,dim),sigma=diag(dim),log=TRUE)
  sum<-sum+norm
  
  exp(ldet+sum)
}

###################################################
###################################################
## This function 
###################################################
###################################################
ptTestOLD<-function(train,test,maxJ=8,                       #basic input
                 save=500,burnin=0,                       #mcmc
                 test.equal.weight=F,test.accept.all=F){   #for testing 
  mcmc<-save+burnin
  ###################################################
  ## Collect information about the testing and training
  ###################################################
  dim<-ncol(train)                                       #dimensions of observations
  ntrain<-nrow(train)                                    #n_training
  n<-ntrain
  ntest<-nrow(test)                                      #n_testing
  J=max(2,min(ceiling(log(ntrain,2^dim)),maxJ))          #Choose J via Hanson's metric
  mu=colMeans(train)                                     #column means for each feature based on training data
  sigma=cov(train)                                       #covariance matrix based on training data
  ldet=-0.5*determinant(sigma,log=T)$modulus[1]
  
  ###################################################
  ## Save Squareroots -- extending Cipolli, Hanson 2018
  ###################################################
  sigmahalfinv_list=list()
  
  ###################################################
  ###################################################
  ## Recreate Cipolli, Hanson 2018 (average of a handful)
  ## This averages the symmetric and 45-degree spin square roots
  ###################################################
  ###################################################
  if(mcmc<1){
    e=eigen(sigma)                                                           #e-values & e-vectors
    z_vectors<-list()                                                        #save standardized observation (mu, sigma)
    rij_vectors<-list()                                                      #save r[i,j] matrices (mu, sigma)
    prb_vectors<-list()                                                      #save calculated probabilities (mu, sigma)
    rotWeight_values<-c()                                                    #save rotation weights (mu, sigma)
    cp_output<-c()                                                           #save closeness parameters (mu, sigma)
    
    # Set up lists of inverses requested
    if(mcmc==0){  #Average of symmetric and 45
      sigmahalfinv_list[[1]]=e$vectors%*%diag(1/sqrt(e$values))%*%t(e$vectors) #Symmetric Square-root of the inverse
      sigmahalfinv_list[[2]]=e$vectors%*%diag(1/sqrt(e$values))                #45-degree spin 
    }else if(mcmc==-1){ #Symmetric
      sigmahalfinv_list[[1]]=e$vectors%*%diag(1/sqrt(e$values))%*%t(e$vectors) #Symmetric Square-root of the inverse
    }else if(mcmc==-2){ #45
      sigmahalfinv_list[[1]]=e$vectors%*%diag(1/sqrt(e$values))
    }else{
      stop("Please enter a valid MCMC value: >=1, (-2,-1,0 will recreated Cipolli Hanson 2018)")
    }
    ###################################################
    ## START calculations for the inverse matrices 
    ###################################################
    for(i in 1:length(sigmahalfinv_list)){
      ###################################################
      ## This part standardizes the training data
      ###################################################
      z=sigmahalfinv_list[[i]]%*%(t(train)-mu)
      z_vectors[[i]]<-z                                   #save standardized training data for this sigmahalfinv

      ###################################################
      ## This part trains the Polya tree
      ###################################################
      r_ij<-ptTrainOLD(n,dim,z,J)                            #Calculate r_ij
      rij_vectors[[i]]<-r_ij                              #save r_ij for this sigmahalfinv
            
      ###################################################
      ## This part evaluates a handful of closeness parameters
      ###################################################
      cseq<-c(0.01, 0.1, 1, 10, 50, 100)
      cprb<-jointPRBOLD(r_ij,z,J,cseq,ldet,ll=T)              #Calculate the joint probability for various cp?
      cp<-cseq[min(which(cprb==max(cprb)))]               #which cp maximizes the joint probability? 
      cp_output<-c(cp_output,cp)                          #save closeness parameter for this sigmahalfinv

      ###################################################
      ## This part saves the weight for this rotation
      ###################################################
      rot<-max(cprb)
      rotWeight_values<-c(rotWeight_values,rot)           #save joint probability for post-hoc weighting for this sigmahalfinv
      
      ###################################################
      ## Evaluate probabilities for testing data
      ###################################################
      test_prbs<-c()
      for(j in 1:nrow(test)){
        test_prbs<-c(test_prbs,pt1TestOLD(test[j,],mu,sigmahalfinv_list[[i]],n,dim,J,cp,r_ij,ldet))
      }
      prb_vectors[[i]]<-test_prbs                         #save probability for each observation (row) of the testing data for this sigmahalfinv
    }
    ###################################################
    ## END calculations for the  inverse matrices 
    ###################################################
    
    ###################################################
    ## Calculate rotation weights                                           
    ###################################################
    denom<-0                                              #denominator is the sum of all joint probabilities
    for(i in 1:length(sigmahalfinv_list)){
      denom<-denom + rotWeight_values[i]
    }
    
    for(i in 1:length(sigmahalfinv_list)){                #weights are the joint probability over the sum of all joint probabilities
      rotWeight_values[i]<-rotWeight_values[i]/denom
    }
    
    ###################################################
    ## Calculate probabilities using weighted average across rotations
    ###################################################
    prb<-rep(NA,nrow(test))
    for(i in 1:nrow(test)){
      prb_i<-0
      for(j in 1:length(sigmahalfinv_list)){
        if(test.equal.weight){ #testing
          prb_i<- prb_i + prb_vectors[[j]][i]*(1/length(sigmahalfinv_list)) #(equal weight for debugging)
        }else{
          prb_i<- prb_i + prb_vectors[[j]][i]*rotWeight_values[j] 
        }
      }
      prb[i]<-prb_i
    }
    
    ###################################################
    ## Return values of interest:
    ###################################################
    ret<-list(prb=prb,rot=rotWeight_values,cps=cp_output)
    return(ret)
  }
}