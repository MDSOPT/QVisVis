HMapCategoryTableSinglegg <- function(CatStruct,PlotMode="simple",LowCol="darkred",MidCol="white",HighCol="darkblue",
                         ShowMode="show",TitleElements=c(TRUE,FALSE,FALSE),TextSize=10){
  #HMapSinglegg
  #Plots a category match table, giving agreement across categories
  #AgreeStruct - The category structure, created with CategoryAgreeSingle
  #PlotMode - The mode for the heatmap
  # "simple" - A simple plot, showing agreement from 0 to 1 in shades of a single color
  # "compare" - Subtract the expected value of agreement for each value of k
  # "adjusted" - The adjusted agreement, (Agr-E(Agr))/(1-E(Agr)). Is infinity for k-1
  # LowCol,MidCol,HighCol - The plot colors. Default to red(-ve), white(0), and blue(+ve)
  # ShowMode - TRUE if show graph and FALSE if return graph as function output
  # TitleElements - Array of 3 items. 1st technique name, 2nd dataset name, 3rd k-values
  # TextSize - The text size in standard ggplot units
  #Written Stephen France 2016-2019: sfrance@business.msstat.edu
  #Please cite
  #France, S.L., Carroll, J.D. (2007).  Development of an Agreement Metric 
  #based upon the RAND Index for the Evaluation of Dimensionality Reduction 
  #Techniques, with Applications to Mapping Customer Data. Machine Learning 
  #and Data Mining in Pattern Recognition, Petra Perner (Ed.), LNAI, 
  #Vol. 4571, Proceedings Conference MLDM 2007, Leipzig/Germany, Springer: Heidelberg
  require(ggplot2)
  require(reshape2)
  
  #The k values are in the category structure
  Startk<-CatStruct$Startk
  Endk<-CatStruct$Endk
  
  #Get the correct value of the matrix
  if (PlotMode=="simple") 
  {
    kText="Agreement"
    PlotMatrix<-CatStruct$CatAgreeMatrix
    LegendText<-"Agr"
  }
  else if (PlotMode=="compare")
  {
    kText="Relative Agreement"
    PlotMatrix<-CatStruct$CatCompAgreeMatrix  
    LegendText<-"CompAgr"
  }
  else
  {
    kText="Adjusted Agreement"
    PlotMatrix<-CatStruct$CatAdjAgreeMatrix
    LegendText<-"AdjAgr"
  }
  kText<-paste("Cross-Category ",kText)
  

  if (TitleElements[1]==TRUE)
  {
    kText=paste(kText,"for",CatStruct$Description,sep=" ")
  }
  else
  {
    kText=""
  }
  if (TitleElements[2]==TRUE)
  {
    kText=paste(kText,"on",kTextAgreeStruct$DataDescription)
  }
  
  if (TitleElements[3]==TRUE)
  {
    if (Startk==Endk)
    {kAdd=paste("k=",Startk," ",sep="")}
    else
    {kAdd=paste("k=",Startk,":",Endk," ",sep="")}
    kText=paste(kText,kAdd,sep=" ")
  }
  PlotTitle<-kText

  #Convert the matrix into structure using melt
  PlotMatrix2 <- melt(PlotMatrix)
  colnames(PlotMatrix2)<-c("Cat1","Cat2",LegendText)
  

  sp<-ggplot(data =  PlotMatrix2, aes(x = Cat1, y = Cat2,fill=get(LegendText)))
  sp<-sp+geom_tile()
  sp<-sp+theme(legend.title=element_text(face="bold"))
  sp<-sp+scale_fill_gradient2(low = LowCol,mid=MidCol, high=HighCol, guide="colorbar",name="Agreement\n",midpoint=0)
  sp<-sp+ggtitle(PlotTitle)
  sp<-sp+theme_bw()
  sp<-sp+theme(legend.title=element_text(face="bold"))
  sp<-sp+theme(text=element_text(size = TextSize),plot.title = element_text(hjust = 0.5,size = rel(1),face="bold"))
  
  if (ShowMode=="show")
  {
    plot(sp)
  }
  else
  {
    return(sp)
  }
}