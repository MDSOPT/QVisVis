CategoryAgreeTableSingle <- function(YConfig,BaseConfig,Categories,Startk=1,Endk=1,Description=NULL,DataDescription=NULL){
  #CategoryAgreeTableSingle
  #For a data with classes, calculates the agreement and adjusted agreement across categories for any set of items for
  #a dataset.  For each combination of categories (i,j), differences in agreements (Config1-Config2) are calculated across
  #all items. Cell (i,j) gives the average agreement for the items with i category (across i and j)
  #and cell (j,i) gives the converse. 
  #YConfig - The output configuration for the comparison technique
  #BaseConfig - The configuration being compared against
  #Categories - The categories of the items in the data
  #Startk,Endk - The start and end k for the agreement
  #Description - Description of the technique
  #DataDescription - Description of the dataset
  #Outputs
  # Output - Output object containing the following
  # Startk,Endk, Description1, Description2, DataDescription - As input
  # CatAgreeMatrix - A Category * Category matrix of agreement across categories
  # CatCompAgreeMatrix - A Category * Category matrix of agreement - expected agreement across categories
  # CatAdjAgreeMatrix - A Category * Category matrix of adjusted agreements across categories
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
  NoItems<-nrow(YConfig)
  CatLevels<-levels(Categories)
  CatAgreement<-matrix(0,length(CatLevels),length(CatLevels))
  CatCompAgreement<-matrix(0,length(CatLevels),length(CatLevels))
  CatAdjAgreement<-matrix(0,length(CatLevels),length(CatLevels))
  for (i in 1:(length(CatLevels)-1))
  {
    for (j in (i+1):length(CatLevels))
    {
      #Choose the items
      CatItems<-which(Categories==CatLevels[i]|Categories==CatLevels[j])
      #Subset the categories
      SubCategories<-Categories[CatItems]
      SubYConfig<-YConfig[CatItems,]
      SubBaseConfig<-BaseConfig[CatItems,]
      #Now create agreement for each item
      DBase<-as.matrix(dist(SubBaseConfig, method = 'euclidean', diag = TRUE, upper = TRUE))
      DConfig<-as.matrix(dist(SubYConfig, method = 'euclidean', diag = TRUE, upper = TRUE))
      #Note, Endk needs to be less than the distribution for smallest category (error checking in future versions)
      AgreeStruct<-GenAgreeDist(DBase,DConfig,Startk,Endk,ProxMode='dis',SymMode='sym',Description=Description,DataDescription=DataDescription)
      #Repeat for the second configuration

      #Now calculate the differences in agreement
      #The overall difference between the plot values
      TempValRowCol<-AgreeStruct$RowAgree[SubCategories==CatLevels[i],(Startk+1):(Endk+1),drop=FALSE]
      TempValColRow<-AgreeStruct$RowAgree[SubCategories==CatLevels[j],(Startk+1):(Endk+1),drop=FALSE]
      
      #Now calculate this relative to expected value
      ExpValRowCol<-outer(rep(1,nrow(TempValRowCol)),((Startk:Endk)/(nrow(TempValRowCol)-1)))
      ExpValColRow<-outer(rep(1,nrow(TempValColRow)),((Startk:Endk)/(nrow(TempValColRow)-1)))
      CompTempValRowCol<-(TempValRowCol-ExpValRowCol)
      CompTempValColRow<-(TempValColRow-ExpValColRow)
      AdjTempValRowCol<-CompTempValRowCol/(1-ExpValRowCol)
      AdjTempValColRow<-CompTempValColRow/(1-ExpValColRow)
      
      CatAgreement[i,j]<-mean(TempValRowCol)
      CatAgreement[j,i]<-mean(TempValColRow)
      CatCompAgreement[i,j]<-mean(AdjTempValRowCol)
      CatCompAgreement[j,i]<-mean(AdjTempValColRow)
      CatAdjAgreement[i,j]<-mean(AdjTempValRowCol)
      CatAdjAgreement[j,i]<-mean(AdjTempValColRow)
    }
  }
  
  #Get the category levels from the list
  rownames(CatAgreement)<-CatLevels
  colnames(CatAgreement)<-CatLevels
  rownames(CatCompAgreement)<-CatLevels
  colnames(CatCompAgreement)<-CatLevels
  rownames(CatAdjAgreement)<-CatLevels
  colnames(CatAdjAgreement)<-CatLevels
  
  #Create an output object
  Output<-NULL
  Output$Startk<-Startk
  Output$Endk<-Endk
  Output$Description<-Description
  Output$DataDescription<-DataDescription
  Output$CatAgreeMatrix<-CatAgreement
  Output$CatCompAgreeMatrix<-CatCompAgreement
  Output$CatAdjAgreeMatrix<-CatAdjAgreement
  return(Output)
}