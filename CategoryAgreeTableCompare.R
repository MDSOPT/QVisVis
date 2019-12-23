CategoryAgreeTableCompare <- function(YConfig1,YConfig2,BaseConfig,Categories,Startk=1,Endk=1,Description1=NULL,Description2=NULL,DataDescription=NULL){
  #CategoryAgreeTableCompare
  #For a data with classes, calculates the difference in agreement across categories for any set of items for
  #a dataset.  For each combination of categories (i,j), differences in agreements (Config1-Config2) are calculated across
  #all items. Cell (i,j) gives the average agreement for the items with i category (across i and j)
  #and cell (j,i) gives the converse. 
  #YConfig1 - The output configuration for the first technique
  #YConfig2 - The output configuration for the second technique 
  #BaseConfig - The configuration being compared against
  #Categories - The categories of the items in the data
  #Startk,Endk - The start and end k for the agreement
  #Description1 - Description of the first technique
  #Description2 - Description of the second technique
  #DataDescription - Description of the dataset
  #Outputs
  # Output - Output object containing the following
  # Startk,Endk, Description1, Description2, DataDescription - As input
  # CatDiffAgreeMatrix - A Category * Category matrix of differences in agreement across categories
  # CatAsjDiffAgreeMatrix - A Category * Category matrix of adjusted differences in agreement across categories
  #Written Stephen France 2016-2019: sfrance@business.msstat.edu
  #Please cite
  #France, S.L., Carroll, J.D. (2007).  Development of an Agreement Metric 
  #based upon the RAND Index for the Evaluation of Dimensionality Reduction 
  #Techniques, with Applications to Mapping Customer Data. Machine Learning 
  if (!require("ggplot2")) install.packages("ggplot2")
  library(ggplot2)
  
  if (!require("reshape2")) install.packages("reshape2")
  library(reshape2)
  
  #Get the number of items
  NoItems<-nrow(YConfig1)
  CatLevels<-levels(Categories)
  CatDiffAgreement<-matrix(0,length(CatLevels),length(CatLevels))
  CatAdjDiffAgreement<-matrix(0,length(CatLevels),length(CatLevels))
  #Go through each combination of neighborhoods
  for (i in 1:(length(CatLevels)-1))
  {
    for (j in (i+1):length(CatLevels))
    {
      #Choose the items
      CatItems<-which(Categories==CatLevels[i]|Categories==CatLevels[j])
      #Subset the categories
      SubCategories<-Categories[CatItems]
      SubYConfig1<-YConfig1[CatItems,]
      SubYConfig2<-YConfig2[CatItems,]
      SubBaseConfig<-BaseConfig[CatItems,]
      #Now create agreement for each item
      DBase<-as.matrix(dist(SubBaseConfig, method = 'euclidean', diag = TRUE, upper = TRUE))
      DConfig<-as.matrix(dist(SubYConfig1, method = 'euclidean', diag = TRUE, upper = TRUE))
      #Note, Endk needs to be less than the distribution for smallest category (error checking in future versions)
      AgreeStruct1<-GenAgreeDist(DBase,DConfig,Startk,Endk,ProxMode='dis',SymMode='sym',Description=Description1,DataDescription=DataDescription)
      #Repeat for the second configuration
      DConfig<-as.matrix(dist(SubYConfig2, method = 'euclidean', diag = TRUE, upper = TRUE))
      AgreeStruct2<-GenAgreeDist(DBase,DConfig,Startk,Endk,ProxMode='dis',SymMode='sym',Description=Description1,DataDescription=DataDescription)
      
      #Now calculate the differences in agreement
      #The overall difference between the plot values
      TempValRowCol<-AgreeStruct1$RowAgree[SubCategories==CatLevels[i],(Startk+1):(Endk+1),drop=FALSE]-AgreeStruct2$RowAgree[SubCategories==CatLevels[i],(Startk+1):(Endk+1),drop=FALSE]
      TempValColRow<-AgreeStruct1$RowAgree[SubCategories==CatLevels[j],(Startk+1):(Endk+1),drop=FALSE]-AgreeStruct2$RowAgree[SubCategories==CatLevels[j],(Startk+1):(Endk+1),drop=FALSE]
      
      #Now calculate this relative to expected value
      ExpValRowCol<-outer(rep(1,nrow(TempValRowCol)),((Startk:Endk)/(nrow(TempValRowCol)-1)))
      ExpValColRow<-outer(rep(1,nrow(TempValColRow)),((Startk:Endk)/(nrow(TempValColRow)-1)))
      AdjTempValRowCol<-(TempValRowCol)/(1-ExpValRowCol)
      AdjTempValColRow<-(TempValColRow)/(1-ExpValColRow)
      
      CatDiffAgreement[i,j]<-mean(TempValRowCol)
      CatDiffAgreement[j,i]<-mean(TempValColRow)
      CatAdjDiffAgreement[i,j]<-mean(AdjTempValRowCol)
      CatAdjDiffAgreement[j,i]<-mean(AdjTempValColRow)
    }
  }
  
  #Get the category levels from the list
  rownames(CatDiffAgreement)<-CatLevels
  colnames(CatDiffAgreement)<-CatLevels
  rownames(CatAdjDiffAgreement)<-CatLevels
  colnames(CatAdjDiffAgreement)<-CatLevels
  
  #Create an output object
  Output<-NULL
  Output$Startk<-Startk
  Output$Endk<-Endk
  Output$Description1<-Description1
  Output$Description2<-Description2
  Output$DataDescription<-DataDescription
  Output$CatDiffAgreeMatrix<-CatDiffAgreement
  Output$CatAdjDiffAgreeMatrix<-CatAdjDiffAgreement
  return(Output)
}