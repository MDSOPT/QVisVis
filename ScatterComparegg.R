ScatterComparegg <- function(AgreeStruct1,AgreeStruct2,Dest,PlotMode="increase",LowCol="darkred",MidCol="white",HighCol="darkblue",
                             Startk=NULL,Endk=NULL,GradientMode="none",ShowMode="show",TextSize=10){
  #ScatterComparegg
  #Plots an aggrement comparison between two techniques at a disaggregate level.  Plots results in a heatmap
  #for each value of i and k
  #AgreeStruct1 - An agreement structure created with GenAgree or GenAgreeDist
  #AgreeStruct2 - A destination structure created with GenAgree or GenAgreeDist
  #PlotMode - The mode for the scatterplot
  
  # "increase" - Shows Agr(Config1)-Agr(Config2)
  # "adjusted" - The adjusted agreement, (Agr(Config1)-Agr(Config2))/(1-E(Agr)). Is infinity for k-1
  # LowCol,MidCol,HighCol - The plot colors. Default to red(-ve), white(0), and blue(+ve)
  # Startk,Endk - Optional k values to allow the plot to be restricted
  # GradientMode - "none" - Just show the points
  #              - "loess" - Show a loess transformation on the points
  # ShowMode - "show" if wish to show plot in graphics environment
  #          - "return" if wish to return the plot 
  # TextSize - The text size in standard ggplot units
  #Written Stephen France 2016: sfrance@business.msstat.edu
  #Please cite
  #France, S.L., Carroll, J.D. (2007).  Development of an Agreement Metric 
  #based upon the RAND Index for the Evaluation of Dimensionality Reduction 
  #Techniques, with Applications to Mapping Customer Data. Machine Learning 
  #and Data Mining in Pattern Recognition, Petra Perner (Ed.), LNAI, 
  #Vol. 4571, Proceedings Conference MLDM 2007, Leipzig/Germany, Springer: Heidelberg
  require(ggplot2)
  require(reshape2)
  require(gridExtra)
  
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
  
  Dest<-as.data.frame(Dest)
  names(Dest)<-c("D1","D2")

  if (is.null(AgreeStruct1$Description)) Desc1<-"Config1" else Desc1<-AgreeStruct1$Description 
  if (is.null(AgreeStruct2$Description)) Desc2<-"Config2" else Desc2<-AgreeStruct2$Description
  PlotTitle<-paste(kText,"Agr1(",Desc1,"):",round(PlAgree1,4)," Agr2(",Desc2,"):",round(PlAgree2,4),sep="")
  
  #The overall difference between the plot values
  PlotVal<-AgreeStruct1$RowAgree[,(Startk+1):(Endk+1),drop=FALSE]-AgreeStruct2$RowAgree[,(Startk+1):(Endk+1),drop=FALSE]
  if (PlotMode=="adjusted")
  {
    ExpVal<-outer(rep(1,n),((Startk:Endk)/(n-1)))
    PlotVal<-(PlotVal-ExpVal)/(1-ExpVal)
  }
  
  #In this case, we need an aggregate row per item
  PlotVal<-apply(PlotVal,1,mean,drop=FALSE)
  Dest<-cbind(Dest,PlotVal)
  names(Dest)[3]<-"Agree"
  
  if (GradientMode=="none")
  {
    sp<-ggplot(data =  Dest, aes(x = D1, y = D2,color=Agree))+geom_point()
    sp<-sp+scale_color_gradient2(low = LowCol,mid=MidCol, high=HighCol,guide="colorbar",name="Agree(1-2)\n",midpoint=0)
    sp<-sp+ggtitle(PlotTitle)
    sp<-sp+theme_bw()
    sp<-sp+theme(legend.title=element_text(face="bold"))
    sp<-sp+theme(text=element_text(size = TextSize),plot.title = element_text(hjust = 0.5,size = rel(1),face="bold"))
  }
  else
  {
    #create the loess scaling output
    loess.out <- loess(Agree ~ D1 * D2, data = Dest)
    RangeD1<-(max(Dest$D1)-min(Dest$D1))
    RangeD2<-(max(Dest$D2)-min(Dest$D2))
    xgrid <-  seq(min(Dest$D1), max(Dest$D1),RangeD1/50)
    ygrid <-  seq(min(Dest$D2), max(Dest$D2),RangeD2/50)
    
    NewData <-  expand.grid(D1 = xgrid, D2 = ygrid)
    ShadeAgree<-  predict(loess.out, newdata = NewData)
    
    #Create an interpolation for the shaded data
    ShadeAgree2 <- melt(ShadeAgree, id.vars = c("D1", "D2"), measure.vars = Agree)
    ShadeAgree2$D1<-as.numeric(substr(ShadeAgree2$D1, 4,20))
    ShadeAgree2$D2<-as.numeric(substr(ShadeAgree2$D2, 4,20))
    colnames(ShadeAgree2)[3]<-"Agree"
    
    #Create the ggplot output
    sp <- ggplot(ShadeAgree2, aes(x = D1, y = D2, z = Agree))
    sp <-sp+stat_contour(geom = "polygon", aes(fill = ..level..))
    sp <-sp+geom_tile(aes(fill = Agree)) 
    sp <-sp+stat_contour(bins = 15)
    sp <-sp+scale_fill_gradient2(low = LowCol,mid=MidCol, high=HighCol,guide="colorbar",name="Agreement\n",midpoint=0)
    sp <-sp+guides(fill = guide_colorbar(title = "Agree"))
    sp <-sp+geom_point(data = Dest, aes(x = D1, y = D2,fill=Agree), shape = 21, size = 2,color="black")
    sp <-sp+scale_color_gradient2(low = LowCol,mid=MidCol, high=HighCol,midpoint=0)
    sp<-sp+ggtitle(PlotTitle)
    sp<-sp+theme_bw()
    sp<-sp+theme(legend.title=element_text(face="bold"))
    sp<-sp+theme(text=element_text(size = TextSize),plot.title = element_text(hjust = 0.5,size = rel(1),face="bold"))
  }
  
  
  if (ShowMode=="show")
  {
    plot(sp)
  }
  else
  {
    return(sp)
  }
  
}