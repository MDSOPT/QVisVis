PLiftComparegg <- function(AgreeStruct1,AgreeStruct2,IsFill=TRUE,ShowMode="show",TextSize=10){
  #ScatterComparegg
  #Creates a lift diagram which shows the agreement of two techniques over k 
  #(the k defined by the agreement structures) in comparison to expected agreement
  #AgreeStruct1 - An agreement structure created with GenAgree or GenAgreeDist
  #AgreeStruct2 - A destination structure created with GenAgree or GenAgreeDist
  #IsFill - TRUE if the areas between the agreement curves and 
  # the expected agreement are colored (with transparent colors for overlapping areas)
  #         FALSE otherwise
  #ShowMode - "show" if wish to show plot in graphics environment
  #          - "return" if wish to return the plot 
  #TextSize - The text size in standard ggplot units
  #Written Stephen France 2016-2019: sfrance@business.msstat.edu
  #Please cite
  #France, S.L., Carroll, J.D. (2007).  Development of an Agreement Metric 
  #based upon the RAND Index for the Evaluation of Dimensionality Reduction 
  #Techniques, with Applications to Mapping Customer Data. Machine Learning 
  #and Data Mining in Pattern Recognition, Petra Perner (Ed.), LNAI, 
  #Vol. 4571, Proceedings Conference MLDM 2007, Leipzig/Germany, Springer: Heidelberg  
  
  require(ggplot2)
  require(reshape2)
  
  if (is.null(AgreeStruct1$Description)) Desc1<-"Config1" else Desc1<-AgreeStruct1$Description 
  if (is.null(AgreeStruct2$Description)) Desc2<-"Config2" else Desc2<-AgreeStruct2$Description
    
  #Build a data frame, with agreement for both configurations and expected agreement
  PlotVal<-cbind(AgreeStruct1$RBuild[1:3],AgreeStruct2$RBuild[2])  
  names(PlotVal)[2]<-"Agr1"
  names(PlotVal)[4]<-"Agr2"
  PlotTitle<-paste("Agr1(",Desc1,"):",round(AgreeStruct1$Agree,4)," Agr2(",Desc2,"):",round(AgreeStruct2$Agree,4),sep="")
  PlotVal <- cbind(PlotVal,min_line=pmin(PlotVal[,2],PlotVal[,3],PlotVal[,4])) 
  PlotVal <- melt(PlotVal, id.vars=c("k","min_line"), variable.name="Config", value.name="Agree")
  
  if (IsFill==TRUE)
  {
    #Create filled plot
    sp <- ggplot(data=PlotVal, aes(x=k, fill=Config,color=Config,linetype=Config))
    sp <- sp + geom_ribbon(alpha = 0.2,aes(ymax=Agree, ymin=min_line))
    sp <- sp + geom_line(aes(y = Agree),size=1)
    sp <- sp + scale_fill_manual(values=c(Agr1="blue",Agr2="green", `ER(Agr)`="red"))
    sp <- sp + scale_color_manual(values=c(Agr1="black",Agr2="black", `ER(Agr)`="darkred"))
    sp <- sp + scale_linetype_manual(values=c(Agr1="solid",Agr2="solid", `ER(Agr)`="twodash"))
  }
  else
  {
    #Create a standard line plot
    sp <- ggplot(data=PlotVal, aes(x=k,color=Config,linetype=Config))
    sp <- sp + geom_line(aes(y = Agree),size=1)
    sp <- sp + scale_color_manual(values=c(Agr1="blue",Agr2="green", `ER(Agr)`="darkred"))
    sp <- sp + scale_linetype_manual(values=c(Agr1="solid",Agr2="solid", `ER(Agr)`="twodash"))
  }
  
  sp <- sp + ggtitle(PlotTitle)
  sp <-sp + theme_bw()
  sp <-sp + theme(legend.title=element_text(face="bold"))
  sp <-sp + theme(text=element_text(size = TextSize),plot.title = element_text(hjust = 0.5,size = rel(1),face="bold"))
  
  if (ShowMode=="show")
  {
    plot(sp)
  }
  else
  {
    return(sp)
  }
  
}