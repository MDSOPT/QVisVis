HMapComparegg <- function(AgreeStruct1,AgreeStruct2,PlotMode="increase",LowCol="darkred",MidCol="white",HighCol="darkblue",
                          Startk=NULL,Endk=NULL,ShowMode="show",NoxLabels=10,NoyLabels=10,ColMode="standard",Ordering=NULL,TextSize=10){
  #HMapComparegg
  #Plots an aggrement comparison between two techniques at a disaggregate level.  Plots results in a heatmap
  #for each value of i and k
  #Inputs
  # AgreeStruct1 - An agreement structure created with GenAgree or GenAgreeDist
  # AgreeStruct2 - An agreement structure created with GenAgree or GenAgreeDist  
  # PlotMode - The mode for the heatmap
  # "increase" - Shows Agr(Config1)-Agr(Config2)
  # "adjusted" - The adjusted agreement, (Agr(Config1)-Agr(Config2))/(1-E(Agr)). Is infinity for k-1
  # ColMode - "standard" - The color map goes from low to medium to high linearly
  #           "enhanced" - Uses a square root transformation to bias the color map to low/high values
  #           "binary" - Only plots binary comparisons of the transformations
  # Ordering - A double or integer ordering, the same length as the number of items. Defaults to NULL.
  # TextSize - The text size in standard ggplot units
  #Outputs
  # sp - The ggplot object
  #Written Stephen France 2016-2019: sfrance@business.msstat.edu
  #Please cite
  #France, S.L., Carroll, J.D. (2007).  Development of an Agreement Metric 
  #based upon the RAND Index for the Evaluation of Dimensionality Reduction 
  #Techniques, with Applications to Mapping Customer Data. Machine Learning 
  if (!require("ggplot2")) install.packages("ggplot2")
  library(ggplot2)
  
  if (!require("reshape2")) install.packages("reshape2")
  library(reshape2)
  
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
  }
  
  #If ordered then reorder plotval
  if (is.null(Ordering)==FALSE)
  {
    PlotVal<-PlotVal[Ordering,]
  }
  
  PlotVal<-as.data.frame(cbind(1:n,PlotVal))
  
  names(PlotVal)<-c("Item",as.character(Startk:Endk))
  PlotVal$Item<-as.factor(PlotVal$Item)
  
  #Create a structure with colummn for item,  column for k, and a column for the row agreement
  PlotVal <- melt(PlotVal, id.vars=c("Item"), variable.name="k", value.name="RowAgree")
  PlotVal$k<-as.factor(PlotVal$k)
  
  #Boost colors based on the color mode
  if (ColMode=="enhanced")
  {
    PlotVal$RowAgree[PlotVal$RowAgree>0]=sqrt(PlotVal$RowAgree[PlotVal$RowAgree>0])
    PlotVal$RowAgree[PlotVal$RowAgree<0]=-sqrt(abs(PlotVal$RowAgree[PlotVal$RowAgree<0]))
    AgreeName="EhAgree(1-2)\n"
  }
  else if (ColMode=="binary")
  {
    PlotVal$RowAgree[PlotVal$RowAgree>0]=1
    PlotVal$RowAgree[PlotVal$RowAgree<0]=-1
    AgreeName="WinAgree(1-2)\n"
  }
  else
  {
    AgreeName="Agree(1-2)\n"
  }
  
  sp<-ggplot(data =  PlotVal, aes(x = k, y = Item))
  sp<-sp+geom_tile(aes(fill = RowAgree))
  sp<-sp+scale_fill_gradient2(low = LowCol,mid=MidCol, high=HighCol, guide="colorbar",name=AgreeName,midpoint=0)
  sp<-sp+ggtitle(PlotTitle)
  sp<-sp+theme_bw()
  sp<-sp+theme(legend.title=element_text(face="bold"))
  sp<-sp+theme(text=element_text(size = TextSize),plot.title = element_text(hjust = 0.5,size = rel(1),face="bold"))
  if (n>NoyLabels)
  {
    Jump<-round(n/NoyLabels)
    sp<-sp+scale_y_discrete(breaks=seq(0,n,by=Jump))
  }
  if (AgreeStruct1$Nok>NoxLabels)
  {
    Jump<-round(AgreeStruct1$Nok/NoxLabels)
    sp<-sp+scale_x_discrete(breaks=seq(Startk,Endk,by=Jump))
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