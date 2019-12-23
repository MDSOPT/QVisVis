PlotAgree3D2Dgg <- function(Source,Dest,AgreeStruct,PlotMode="simple"){
  #PlotAgree3D2Dgg
  #For a dimensionality reduction from 3 to 2 dimensions, plots the agreement results
  #on both the source 3D map and the destination 2D map.
  #for each value of i and k
  #AgreeStruct - An agreement structure created with GenAgree or GenAgreeDist
  #PlotMode - The mode for the plot
  # "simple" - A simple plot, showing agreement from 0 to 1 in shades of a single color
  # "compare" - Subtract the expected value of agreement for each value of k
  # "adjusted" - The adjusted agreement, (Agr-E(Agr))/(1-E(Agr)). Is infinity for k-1
  #Written Stephen France 2016: sfrance@business.msstat.edu
  #Please cite
  #France, S.L., Carroll, J.D. (2007).  Development of an Agreement Metric 
  #based upon the RAND Index for the Evaluation of Dimensionality Reduction 
  #Techniques, with Applications to Mapping Customer Data. Machine Learning 
  #and Data Mining in Pattern Recognition, Petra Perner (Ed.), LNAI, 
  #Vol. 4571, Proceedings Conference MLDM 2007, Leipzig/Germany, Springer: Heidelberg
  require(ggplot2)
  require(reshape2)
  require(grid)
  require(plot3D)
  
  if (is.null(AgreeStruct1$Description)) Desc1<-"Config" else Desc1<-AgreeStruct$Description
  PlotTitle<-paste("Agree:",round(AgreeStruct$Agree,4)," AdjAgree:",round(AgreeStruct$AdjAgree,4))
  
  #We don't know names of dimensions, so we'll redo
  Source<-as.data.frame(Source)
  names(Source)<-c("D1","D2","D3")
  Dest<-as.data.frame(Dest)
  names(Dest)<-c("D1","D2")
  n<-AgreeStruct$n
  
  PlotTitle<-paste("Agree:",round(AgreeStruct$Agree,4)," AdjAgree:",round(AgreeStruct$AdjAgree,4))
  
  PlotVal<-AgreeStruct$RowAgree[,2:(Endk-Startk+2)]
  if ((PlotMode=="compare")||(PlotMode=="adjusted"))
  {
    ExpVal<-outer(rep(1,n),((Startk:Endk)/(n-1)))
    if (PlotMode=="compare")
    {
      PlotVal<-(PlotVal-ExpVal)
    }
    else
    {
      PlotVal<-(PlotVal-ExpVal)/(1-ExpVal)
      #This will give infinity values, which will mess up the plot
      if (Endk==(n-1))
      {
        PlotVal=PlotVal[,1:AgreeStruct$Nok-1]
      }
    
    }
  }
  #In this case, we need an aggregate row per item
  PlotVal<-apply(PlotVal,1,mean,drop=FALSE)
  Source<-cbind(Source,PlotVal)
  names(Source)[4]<-"Agree"
  Dest<-cbind(Dest,PlotVal)
  names(Dest)[3]<-"Agree"
  #Create two plots side by side with single title
  par(mfrow=c(1,2),oma=c(0,0,2,0))
  
  scatter3D(Source$D1, Source$D2, Source$D3, phi = 0, pch = 20,cex=0.75,colvar=Source$Agree,col = ramp.col(c("red","white", "blue")))
  scatter2D(Source$D1, Source$D2, pch = 20,cex=0.75, colvar=Source$Agree,col = ramp.col(c("red","white", "blue")))
  title(PlotTitle, side = 3, line = -2,outer=TRUE)
  
  
}