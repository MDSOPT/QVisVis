GenAgreeDist <- function(D1,D2,Startk,Endk,ProxMode='dis',SymMode='sym',Description=NULL,DataDescription=NULL){
  #GenAgree
  #Calculates a generalized agreement metric as per France and Carroll (2007)
  #This variant takes raw proxmity matrices, which can be symmetric or asymmetric
  #
  #D1
  #Config 2 - Configuration 2 to compare
  #Startk - The starting value of k
  #Endk - The end value of k
  #ProxMode - 'dis' Utilizes input dissimilarites
  #         - 'sym' Utilizes input simmilarities
  #SymMode- 'sym' Symmetric (counts agreement in both directions)
  #       - 'row' Checks if it is symmetric by row
  #       - 'col' Checks if symmetric by column
  #OutValue - The value of the generalized agreement
  #RBuild - k*4 table giving following values for each k
  # 1 - The value of k
  # 2 - The value of AR(k)
  # 3 - ER(k) drawn from hypergeometric
  # 4 - Numerator of agreement
  # 5 - Denominator of agreement
  #Calculates a generalized agreement metric as per France and Carroll (2007)
  #Written Stephen France 2011: france@uwm.edu
  #Please cite
  #France, S.L., Carroll, J.D. (2007).  Development of an Agreement Metric 
  #based upon the RAND Index for the Evaluation of Dimensionality Reduction 
  #Techniques, with Applications to Mapping Customer Data. Machine Learning 
  #and Data Mining in Pattern Recognition, Petra Perner (Ed.), LNAI, 
  #Vol. 4571, Proceedings Conference MLDM 2007, Leipzig/Germany, Springer: Heidelberg
  
  
  nItems<-nrow(D1)    #Size for the configuration
  
  switch(SymMode, 
         row={
           #Assume asymetric, do by row
           SortDim<-1
         },
         col={
           #Assume asymetric, do by column
           SortDim<-2   
         },
         {
           #Symmetricize both matrices
           D1<-(D1+t(D1))/2
           D2<-(D2+t(D2))/2
           SortDim<-1
         }
  ) 
  #Check whether or not decreasing
  IsDecreasing <- if(ProxMode=='dis') FALSE else TRUE
  
  #Calculate nearest neighbors
  Temp<-apply (D1,SortDim,sort,index.return=TRUE,decreasing = IsDecreasing)
  IDX1<-t(matrix(unlist(Temp),nItems*2,nItems))
  IDX1 <- IDX1[,(nItems+2):(2*nItems)]
  
  Temp<-apply(D2,SortDim,sort,index.return=TRUE,decreasing = IsDecreasing)
  IDX2<-t(matrix(unlist(Temp),nItems*2,nItems))
  IDX2 <- IDX2[,(nItems+2):(2*nItems)]
  
  #In a dissimilarity matrix the column names and row names should be the same.  Try
  #Column names first and then row names.
  if (is.null(colnames(D1))==FALSE) 
  {
    ItemNames<-colnames(D1)
  } 
  else
  {
    if (is.null(rownames(D1))==FALSE)
    {
      ItemNames<-rownames(D1)
    }
    else
    {
      #Neither rows or columns have names, so make up these names
      ItemNames<-as.character(1:nItems)
    }
  }
  rm (D1,D2)
  
  Nok<-Endk-Startk+1
  RBuild<-as.data.frame(matrix(0,Nok,4))
  RowTotalAgree<-matrix(0,nItems,Nok+1)
  RowAgree<-matrix(0,nItems,Nok+1)
  TotalAgree<-0
  
  #Update total agreement if not starting at 1
  if (Startk>1)
    {
    for (nCounter in 1:nItems)
      {
      Temp<-intersect(IDX1[nCounter,1:(Startk-1)],IDX2[nCounter,1:(Startk-1)])
      RowTotalAgree[nCounter,1]<-length(Temp)
      RowAgree[nCounter,1]<-RowTotalAgree[nCounter,1]/(Startk-1)
      TotalAgree<-TotalAgree+RowAgree[nCounter,1]
      }
    rm(Temp)
    }
  
  for (kCounter in Startk:Endk)
  {
    for (nCounter in 1:nItems)
    {
      #Check for agreement with added item
      Increment<-0
      Increment<-Increment+as.numeric(IDX1[nCounter,kCounter]==IDX2[nCounter,kCounter])
      Increment<-Increment+(length(which(IDX1[nCounter,0:(kCounter-1)]==IDX2[nCounter,kCounter]))>0)
      Increment<-Increment+(length(which(IDX2[nCounter,0:(kCounter-1)]==IDX1[nCounter,kCounter]))>0)
      TotalAgree<-TotalAgree+Increment
      RowTotalAgree[nCounter,kCounter-Startk+2]<-RowTotalAgree[nCounter,kCounter-Startk+1]+Increment
      RowAgree[nCounter,kCounter-Startk+2]<-RowTotalAgree[nCounter,kCounter-Startk+2]/kCounter
    }
    RBuild[kCounter-Startk+1,1]<-TotalAgree/(nItems*kCounter)
    RBuild[kCounter-Startk+1,2]<-(kCounter)/(nItems-1)
  }     
      
  #Numerator of aggregate agreement rate
  RBuild[,3]=RBuild[,1]-RBuild[,2]
  #Denominator of aggregate agreement rate 
  RBuild[,4]=1-RBuild[,2]
  RBuild<-cbind(Startk:Endk,RBuild)
  names(RBuild)<-c('k','Agr','ER(Agr)','AdjNum','AdjDenom')
      
  #Get the final Agreement rate
  OutStruct<-NULL
  OutStruct$n<-nItems
  OutStruct$Description<-Description
  OutStruct$DataDescription<-DataDescription
  OutStruct$Startk<-Startk
  OutStruct$Endk<-Endk
  OutStruct$Nok<-Nok
  OutStruct$ItemNames<-ItemNames
  OutStruct$RBuild <-RBuild
  OutStruct$RowTotalAgree<-RowTotalAgree
  OutStruct$RowAgree<-RowAgree
  OutStruct$TotalAgree<-TotalAgree
  OutStruct$Agree<-sum(RBuild[,2])/Nok
  OutStruct$AdjAgree <- sum(RBuild[,4])/sum(RBuild[,5])

  return(OutStruct)
}

SubAgreeDist <- function(AgreeStruct,Startk,Endk)
{
  RBuild<-AgreeStruct$RBuild
  Nok<-Endk-Startk+1
  OutStruct<-AgreeStruct
  OutStruct$SubAgree<-sum(RBuild[Startk:Endk,2])/Nok
  OutStruct$SubAdjAgree <- sum(RBuild[Startk:Endk,4])/sum(RBuild[Startk:Endk,5])
  return(OutStruct)
}
