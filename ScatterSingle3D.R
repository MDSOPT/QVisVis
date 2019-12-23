ScatterSingle3D <- function(AgreeStruct,Source3D,PlotMode="simple",LowCol="darkred",MidCol="white",HighCol="darkblue",Startk=NULL,Endk=NULL,
                            PointSize=0.5,IsBorder=TRUE){
  #ScatterSingle3D
  #Plots the agreement results at a disaggregate level.  Plots results in a heatmap
  #for each value of i and k
  #AgreeStruct - An agreement structure created with GenAgree or GenAgreeDist
  #Source3D - The 3D source mapping of the original objects
  #PlotMode - The mode for the heatmap
  # "simple" - A simple plot, showing agreement from 0 to 1 in shades of a single color
  # "compare" - Subtract the expected value of agreement for each value of k
  # "adjusted" - The adjusted agreement, (Agr-E(Agr))/(1-E(Agr)). Is infinity for k-1
  # LowCol,MidCol,HighCol - The plot colors. Default to red(-ve), white(0), and blue(+ve)
  # Startk,Endk - Optional k values to allow the plot to be restricted
  # cex.lab,cex.main, cex.axis - The size for the text components
  # IsLegend - If TRUE and the ColMode = binary
  #Written Stephen France 2016-2019: sfrance@business.msstat.edu
  #Please cite
  #France, S.L., Carroll, J.D. (2007).  Development of an Agreement Metric 
  #based upon the RAND Index for the Evaluation of Dimensionality Reduction 
  #Techniques, with Applications to Mapping Customer Data. Machine Learning 
  #and Data Mining in Pattern Recognition, Petra Perner (Ed.), LNAI, 
  #Vol. 4571, Proceedings Conference MLDM 2007, Leipzig/Germany, Springer: Heidelberg
   require(colorRamps)
   require(plot3D)
  library(scatterplot3d)
  if (is.null(Startk)&&is.null(Endk))
  {
    Fixk=FALSE
    kText=''
  }
  else {
    Fixk=TRUE}
  
  n<-AgreeStruct$n
  if (is.null(Startk))
  {Startk<-AgreeStruct$Startk}
  if (is.null(Endk)) 
  {Endk<-AgreeStruct$Endk}
  if (Fixk==TRUE)
  {
    if (Startk==Endk)
    {kText=paste("k=",Startk," ",sep="")}
    else
    {kText=paste("k=",Startk,":",Endk," ",sep="")}
    AgreeStruct<-SubAgreeDist(AgreeStruct,Startk,Endk)
    PlAgree<-AgreeStruct$SubAgree
    PlAdjAgree<-AgreeStruct$SubAdjAgree
  }
  else
  {
    PlAgree<-AgreeStruct$Agree
    PlAdjAgree<-AgreeStruct$AdjAgree
  }
  
  PlotTitle<-paste(kText,"Agree:",round(PlAgree,4)," AdjAgree:",round(PlAdjAgree,4))
  
  Source<-as.data.frame(Source3D)
  names(Source)<-c("D1","D2","D3")
  
  CFFunction<-colorRampPalette(c("darkred","white","darkblue"))
  PlotVal<-AgreeStruct$RowAgree[,(Startk+1):(Endk+1),drop=FALSE]
  if (PlotMode=="simple")
  {
    #Positive should be blue only
    PlotVal<-apply(PlotVal,1,mean,drop=FALSE)
    ColorIn<-(PlotVal+1)*0.5
  }
  else
  {
    ExpVal<-outer(rep(1,n),((Startk:Endk)/(n-1)))
    if (PlotMode=="compare")
    {
      PlotVal<-(PlotVal-ExpVal)
      PlotVal<-apply(PlotVal,1,mean,drop=FALSE)
      #Red for negative (up to 0.5) and blue for positive (greater than 0.5)
      ColorIn<-(PlotVal+1)/2
    }
    else
    {
      #This will give infinity values, which will mess up the plot
      if (Endk==(n-1))
      {
        PlotVal=PlotVal[,1:AgreeStruct$Nok-1]
      }
      PlotVal<-apply(PlotVal,1,mean,drop=FALSE)
      #Scale each side separately
      PlotVal<-(PlotVal-ExpVal)/(1-ExpVal)
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
    scatterplot3d(x=Source3D[,1], y=Source3D[,2], z=Source3D[,3],bg=FinalColors,pch=21,
                color="black",xlab="x", ylab="y", zlab="z",main=PlotTitle,cex.symbols=PointSize,cex.lab=cex.lab,cex.main=cex.main,cex.axis=cex.axis)
  }
  else
  {
    scatterplot3d(x=Source3D[,1], y=Source3D[,2], z=Source3D[,3],pch=16,
                  color=FinalColors,xlab="x", ylab="y", zlab="z",main=PlotTitle,cex.symbols=PointSize,cex.lab=cex.lab,cex.main=cex.main,cex.axis=cex.axis)
  }
  
  
  
}