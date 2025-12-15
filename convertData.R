convertData<-function(x){
  number.cols<-colnames(x)[(sapply(x, is.numeric))]
  factor.cols<-colnames(x)[(sapply(x, is.factor))]
  
  if(length(number.cols)+length(factor.cols) != ncol(x)){
    stop("Each column of data should be either numeric or factor type.")
  }
  
  n.to.convert<-length(factor.cols)
  
  #where we save new data
  dat.new<-x
  
  for(col in 1:n.to.convert){
    #subset data
    curr.data<-x[factor.cols[col]]
    
    #Sort the categories from most occurring to least
    (sorted.tab<-sort(table(curr.data),decreasing=TRUE))
    
    #Calculate Cumulative probability
    (t.tab<-transform(sorted.tab, cumFreq = cumsum(Freq), relative = prop.table(Freq)))
    sorted.tab<-t.tab[,c(1,3)]
    sorted.tab[,2]<-sorted.tab[,2]/sum(t.tab[,2])
    sorted.tab
    
    #Simulate truncated normal
    library("truncnorm")
    #number of factors to handle
    (factors.to.handle<-as.vector(t(unique(sorted.tab[,1]))))
    (n.factors<-length(factors.to.handle))
    
    boundaries<-c(0,sorted.tab$cumFreq)
    dat.new[,factor.cols[col]]<-rep(NA,nrow(dat.new))
    for(i in 1:n.factors){
      simdata<-rtruncnorm(t.tab$Freq[i], a=boundaries[i], b=boundaries[i+1], mean=((boundaries[i]+boundaries[i+1])/2), sd=((boundaries[i+1]-boundaries[i])/6))
      dat.new[which(x[,factor.cols[col]]==factors.to.handle[i]),factor.cols[col]]<-simdata
    }
  }
  return(dat.new)
}

