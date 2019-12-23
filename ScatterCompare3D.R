ScatterCompare3D <- function(AgreeStruct1,AgreeStruct2,Source3D,PlotMode="increase",LowCol="darkred",MidCol="white",HighCol="darkblue",Startk=NULL,Endk=NULL,
PointSize=0.5,IsBorder=TRUE,ColMode="standard",cex.lab=1.25,cex.main=1.5,cex.axis=1,IsLegend=TRUE){
  #ScatterComparegg
  #Plots an aggrement comparison between two techniques at a disaggregate level.  Plots results in a 3D scatterplot
  #for each value of i and k
  #AgreeStruct1 - An agreement structure created with GenAgree or GenAgreeDist
  #AgreeStruct2 - A destination structure created with GenAgree or GenAgreeDist
  #Source3D - The 3D source for the visualization
  #PlotMode - The mode for the scatterplot
  # "increase" - Shows Agr(Config1)-Agr(Config2)
  # "adjusted" - The adjusted agreement, (Agr(Config1)-Agr(Config2))/(1-E(Agr)). Is infinity for k-1
  # LowCol,MidCol,HighCol - The plot colors. Default to red(-ve), white(0), and blue(+ve)
  # Startk,Endk - Optional k values to allow the plot to be restricted
  # PointSize - The size of the scatter points
  # IsBorder - True if the points are bordered, false otherwise
  # ColMode - "standard" - The color map goes from low to medium to high linearly
  #           "enhanced" - Uses a square root transformation to bias the color map to low/high values
  #           "binary" - Only plots binary comparisons of the transformations
  # cex.lab,cex.main, cex.axis - The size for the text components
  # IsLegend - If TRUE and the ColMode = binary
  #Written Stephen France 2016: sfrance@business.msstat.edu
  #Please cite
  #France, S.L., Carroll, J.D. (2007).  Development of an Agreement Metric 
  #based upon the RAND Index for the Evaluation of Dimensionality Reduction 
  #Techniques, with Applications to Mapping Customer Data. Machine Learning 
  #and Data Mining in Pattern Recognition, Petra Perner (Ed.), LNAI, 
  #Vol. 4571, Proceedings Conference MLDM 2007, Leipzig/Germany, Springer: Heidelberg
  if (!require("ggplot2")) install.packages("ggplot2")
  library(ggplot2)

  if (!require("reshape2")) install.packages("reshape2")
  library(reshape2)
  
  if (!require("gridExtra")) install.packages("gridExtra")
  library(gridExtra)
  
  if (!require("scatterplot3d")) install.packages("scatterplot3d")
  library(scatterplot3d)
  
  if (is.null(Startk)&&is.null(Endk))
  {
    Fixk=FALSE
    kText=''
  }
  else {
    Fixk=TRUE}
  
  n<-AgreeStruct1$n
  if (is.null(Startk))
  {Startk<-AgreeStruct1$Startk}
  if (is.null(Endk)) 
  {Endk<-AgreeStruct1$Endk}
  
  if (Fixk==TRUE)
  {
    if (Startk==Endk)
    {kText=paste("k=",Startk," ",sep="")}
    else
    {kText=paste("k=",Startk,":",Endk," ",sep="")}
    AgreeStruct1<-SubAgreeDist(AgreeStruct1,Startk,Endk)
    PlAgree1<-AgreeStruct1$SubAgree
    AgreeStruct2<-SubAgreeDist(AgreeStruct2,Startk,Endk)
    PlAgree2<-AgreeStruct2$SubAgree
  }
  else
  {
    PlAgree1<-AgreeStruct1$Agree
    PlAgree2<-AgreeStruct2$Agree
  }

  if (is.null(AgreeStruct1$Description)) Desc1<-"Config1" else Desc1<-AgreeStruct1$Description 
  if (is.null(AgreeStruct2$Description)) Desc2<-"Config2" else Desc2<-AgreeStruct2$Description
  PlotTitle<-paste(kText,"Agr1(",Desc1,"):",round(PlAgree1,4)," Agr2(",Desc2,"):",round(PlAgree2,4),sep="")
  
  #The overall difference between the plot values
  PlotVal<-AgreeStruct1$RowAgree[,(Startk+1):(Endk+1),drop=FALSE]-AgreeStruct2$RowAgree[,(Startk+1):(Endk+1),drop=FALSE]
  if (PlotMode=="adjusted")
  {
    ExpVal<-outer(rep(1,n),((Startk:Endk)/(n-1)))
    PlotVal<-(PlotVal-ExpVal)/(1-ExpVal)
    if (Endk==(n-1))
    {
      PlotVal=PlotVal[,1:AgreeStruct$Nok-1]
    }
    PlotVal<-apply(PlotVal,1,mean,drop=FALSE)
    TempNeg<-PlotVal[PlotVal<0]
    TempPos<-PlotVal[PlotVal<0]
    
    #Scale the color values into [0,1]
    ColorIn<-PlotVal
    if (length(TempNeg)>0)
    {
      ColorIn[ColorIn<0]<-(ColorIn[ColorIn<0]/(min(TempNeg))+1)/2
    }
    if (length(TempPos)>0)
    {
      ColorIn[ColorIn>0]<-(ColorIn[ColorIn>0]/(max(TempPos))+1)/2
    } 
  }
  else
  {
    #Use the simple increment mode
    PlotVal<-apply(PlotVal,1,mean,drop=FALSE)
    ColorIn<-(PlotVal+1)/2
  }
  if (ColMode=="enhanced")
  {
    ColorIn[ColorIn>0.5]=sqrt(ColorIn[ColorIn>0.5]-0.5)+0.5
    MaxVal=max(ColorIn[ColorIn>0.5])
    if (MaxVal!=-Inf)  #Instance when none are above 0.5 gives MaxVal=-inf!
      {ColorIn[ColorIn>0.5]=(ColorIn[ColorIn>0.5]-0.5)/(2*(MaxVal-0.5))+0.5}
    ColorIn[ColorIn<0.5]=ColorIn[ColorIn<0.5]^2
  }
  else if (ColMode=="binary")
  {
    ColorIn[ColorIn>0.5]=1
    ColorIn[ColorIn<0.5]=0
  }
  
  CFFunction<-colorRamp(c(LowCol,MidCol,HighCol))
  RGBColors<-CFFunction(ColorIn)
  FinalColors<-rgb(RGBColors[,1],RGBColors[,2],RGBColors[,3],maxColorValue=255)
  #ColRampIn<-ramp.col (col = c("darkred","white", "darkblue"), n = 100, alpha = 1)
  #colvar=ColorIn,col=ColRampIn
  #scatter3D (x=Source3D[,1], y=Source3D[,2], z=Source3D[,3],colvar=PlotVal,col = ramp.col(c("blue", "yellow", "green", "red")),
  #    main = PlotTitle)

  if (IsBorder==TRUE)
  {
    sp3d<-scatterplot3d(x=Source3D[,1], y=Source3D[,2], z=Source3D[,3],bg=FinalColors,pch=21,
                  color="black",xlab="x",cex.lab=cex.lab,cex.main=cex.main,cex.axis=cex.axis,ylab="y", zlab="z",main=PlotTitle,cex.symbols=PointSize)
  }
  else
  {  
    sp3d<-scatterplot3d(x=Source3D[,1], y=Source3D[,2], z=Source3D[,3],pch=16,
                  color=FinalColors,xlab="x",cex.lab=cex.lab,cex.main=cex.main,cex.axis=cex.axis,ylab="y", zlab="z",main=PlotTitle,cex.symbols=PointSize)
  }
  
  if ((IsLegend==TRUE)&(ColMode=="binary"))
  {
    legend(sp3d$xyz.convert(max(Source3D[,1])-(max(Source3D[,1])-min(Source3D[,1]))*0.10, 
                            max(Source3D[,2])-(max(Source3D[,2])-min(Source3D[,2]))*0.10,max(Source3D[,3])- (max(Source3D[,3])-min(Source3D[,3]))*0.08), legend = c("WinT1","WinT2"),
           col =  c("darkblue","darkred"),cex=1.5, pch = 16)
  }
  
}