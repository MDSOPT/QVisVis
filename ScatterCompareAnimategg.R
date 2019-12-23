ScatterCompareAnimategg <- function(ParamStruct,AgreeStruct2,Dest,PlotMode="increase",LowCol="darkred",MidCol="white",HighCol="darkblue",
                             Startk=NULL,Endk=NULL,ShowMode="show",MatchMode="none",ParamName="k",FileName=NULL,TitleElements=c(TRUE,FALSE,FALSE,TRUE,TRUE),TextSize=10){
  #ScatterComparegg
  #Plots a aggrement comparison between a parameterized method with a basline method.
  #Animates results in 2D scatterplots for each item for selected k values, comparing with the baseline
  #ParamStruct - A list of length m. Each item in the list contains
  # InParam - The value of the trained parameter
  # AgreeStruct - An agreement structure created with GenAgree or GenAgreeDist
  # Dest - The destination mapping
  #AgreeStruct2 - A destination structure created with GenAgree or GenAgreeDist for the basline comparison
  #PlotMode - The mode for the scatterplot
  # "increase" - Shows Agr(Config1)-Agr(Config2)
  # "adjusted" - The adjusted agreement, (Agr(Config1)-Agr(Config2))/(1-E(Agr)). Is infinity for k-1
  #LowCol,MidCol,HighCol - The plot colors. Default to red(-ve), white(0), and blue(+ve)
  #Startk,Endk - Optional k values to allow the plot to be restricted
  #ShowMode - "show" if wish to show plot in graphics environment
  #          - "return" if wish to return the plot 
  #MatchMode - "none" - Configurations are plotted as outputted by the algorithm (can result in flipped solutions)
  #           - "proc" - Procrusetes analysis without scaling
  #           - "procscale" - Procrustes analysis with scaling
  #ParamName - The name of the tuned parameter. Defaults to k
  #FileName - The filename for the animated graphic if save mode
  #TitleElements - Array of 5 items. 1st technique name, 2nd dataset name, 3rd k-values, 4th agreement, 5th adjusted agreement
  #TextSize - The text size in standard ggplot units
  #Written Stephen France 2019: sfrance@business.msstat.edu
  #Please cite
  #France, S.L., Carroll, J.D. (2007).  Development of an Agreement Metric 
  #based upon the RAND Index for the Evaluation of Dimensionality Reduction 
  #Techniques, with Applications to Mapping Customer Data. Machine Learning 
  #and Data Mining in Pattern Recognition, Petra Perner (Ed.), LNAI, 
  #Vol. 4571, Proceedings Conference MLDM 2007, Leipzig/Germany, Springer: Heidelberg
  if (!require("ggplot2")) install.packages("ggplot2")
  library(ggplot2)
  if (!require("gganimate")) install.packages("gganimate")
  library(gganimate)
  if (!require("gifski")) install.packages("gifski")
  library(gifski)
  if (!require("png")) install.packages("png")
  library(png)
  if (!require("reshape2")) install.packages("reshape2")
  library(reshape2)
  if (!require("vegan")) install.packages("vegan")
  library(vegan)
  
  #Get the length of the parameter structure
  NoParam<-length(ParamStruct)
  
  #In the case of Procrustes transformations, set base config to be in the middle of the parameter sequence
  if (MatchMode!="simple")
  {
    #Set the base configuration to be something in the middle of parameter sequence
    BaseConfig<-ParamStruct[[round(NoParam/2)]]$Dest
  }
  #Get basic information. Use first structure for basic information
  AgreeStruct<-ParamStruct[[1]]$AgreeStruct
  if (is.null(Startk)&&is.null(Endk))
  {
    Fixk<-FALSE
  }
  else 
  {
    Fixk=TRUE
  }
  
  #Determine the start and end k
  nItems<-AgreeStruct$n
  if (is.null(Startk))
  {Startk<-AgreeStruct$Startk}
  if (is.null(Endk)) 
  {Endk<-AgreeStruct$Endk}
  
  #Build up the title
  #Put the dataset first
  if (TitleElements[2]==TRUE)
  {
    kText<-paste(AgreeStruct$DataDescription,":",sep="")
  }
  else
  {
    kText<-""
  }
  
  #Add the k-values included
  if (TitleElements[3]==TRUE)
  {
    if (Startk==Endk)
    {kAdd=paste("k=",Startk,sep="")}
    else
    {kAdd=paste("k=",Startk,":",Endk,sep="")}
    kText=paste(kText,kAdd,sep=" ")
  }
  
  #Then add the technique
  if (TitleElements[1]==TRUE)
  {
    kText<-paste(kText,AgreeStruct$Description,sep=" ")
    kText2<-paste("vs.",AgreeStruct2$Description,sep=" ")
  }
  
  #Now k is decided, setup the agreement structure for the comparison
  AgreeStruct2<-SubAgreeDist(AgreeStruct2,Startk,Endk)
  PlAgree2<-AgreeStruct2$SubAgree
  PlAdjAgree2<-AgreeStruct2$SubAdjAgree
  
  OverallConfig<-NULL
  PlAgree<-0;PlAdjAgree<-0
  for (i in 1:NoParam)
  {
    AgreeStruct<-SubAgreeDist(ParamStruct[[i]]$AgreeStruct,Startk,Endk)
    
    PlAgree<-PlAgree+AgreeStruct$SubAgree
    PlAdjAgree<-PlAdjAgree+AgreeStruct$SubAdjAgree
    
    PlotVal<-AgreeStruct$RowAgree[,(Startk+1):(Endk+1),drop=FALSE]-AgreeStruct2$RowAgree[,(Startk+1):(Endk+1),drop=FALSE]
    if (PlotMode=="adjusted")
    {
      ExpVal<-outer(rep(1,nItems),((Startk:Endk)/(nItems-1)))
      PlotVal<-(PlotVal-ExpVal)/(1-ExpVal)
      AgreeName<-"AdjAgree"
      #This will give infinity values, which will mess up the plot
      if (Endk==(nItems-1))
      {
        PlotVal=PlotVal[,1:AgreeStruct$Nok-1]
      }
    }
    #In this case, we need an aggregate row per item
    PlotVal<-apply(PlotVal,1,mean,drop=FALSE)
    ParamVal<-rep(ParamStruct[[i]]$InParam,nItems)
    #Get the configuration to be added
    AddConfig<-ParamStruct[[i]]$Dest
    if (MatchMode=="proc")
    {
      #Transform the new configuration onto the base configuration
      AddConfig<-procrustes(BaseConfig, AddConfig, scale = FALSE)$Yrot
    }
    else if (MatchMode=="procscale")
    {
      AddConfig<-procrustes(BaseConfig, AddConfig, scale = TRUE)$Yrot
    }
    #Create a data frame with parameter values and the parameter name
    ParamConfig<-cbind(AddConfig,PlotVal,ParamVal)
    colnames(ParamConfig)<-c("D1","D2","Agree","ParamVal")
    OverallConfig<-rbind(OverallConfig,ParamConfig)
  }
  #Put into data frame for ggplot
  OverallConfig<-as.data.frame(OverallConfig)
  #Get the aggregate agreement across parameters
  PlAgree<-PlAgree/NoParam
  PlAdjAgree<-PlAdjAgree/NoParam
  
  #Add the agreement rates
  if (TitleElements[4]==TRUE)
  {
    kText<-paste(kText,"Agree=",round(PlAgree,4),sep=" ")
    kText2<-paste(kText2,"Agree=",round(PlAgree2,4),sep=" ")
  }
  if (TitleElements[5]==TRUE)
  {
    kText<-paste(kText,"AdjAgree=",round(PlAdjAgree,4),sep=" ")
    kText2<-paste(kText2,"AdjAgree=",round(PlAdjAgree2,4),sep=" ")
  }

  #Trim any leading spaces from the plot title
  PlotTitle<-trimws(paste(kText,kText2,sep=" "))

  sp<-ggplot(data = OverallConfig, aes(x = D1, y = D2,color=Agree))+geom_point()
  sp<-sp+scale_color_gradient2(low = LowCol,mid=MidCol, high=HighCol,guide="colorbar",name="Agree(1-2)\n",midpoint=0)
  sp<-sp+ggtitle(PlotTitle)
  sp<-sp+theme_bw()
  sp<-sp+theme(legend.title=element_text(face="bold"))
  sp<-sp+theme(text=element_text(size = TextSize),plot.title = element_text(hjust = 0.5,size = rel(1),face="bold"))
  
  sp <- sp + transition_states(ParamVal,
                               transition_length = 2,
                               state_length = 1)
  sp<-sp+ggtitle(PlotTitle,subtitle=paste("Parameter ",ParamName," = ","{closest_state}",sep="")) 
  
  if (ShowMode=="save")
  {
    #Save as a file in the current directory
    animate(sp, renderer = gifski_renderer(FileName))
  }
  else
  {
    #Return the object to show (or save)
    return(sp)
  }
  
}