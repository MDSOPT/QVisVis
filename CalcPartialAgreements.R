CalcPartialAgreements <- function(AllConfigs,BaseConfig,Dim){
#AllConfigs - Config Matrix.  Config i starts at the 1+*
#Calculate agreements and partial agreements between a series of
#configurations
#INPUTS
#AllConfigs - The configurations
#Dim - The dimensionality of each configuration
#OUTPUTS
#OutFull -
#
#Written Stephen France 2011: france@uwm.edu
#Please cite
#France, S.L., Carroll, J.D. (2007).  Development of an Agreement Metric
#based upon the RAND Index for the Evaluation of Dimensionality Reduction
#Techniques, with Applications to Mapping Customer Data. Machine Learning
#and Data Mining in Pattern Recognition, Petra Perner (Ed.), LNAI,
#Vol. 4571, Proceedings Conference MLDM 2007, Leipzig/Germany, Springer: Heidelberg
#France, S.L. (2011). Properties of a General Measure of Configuration
#Agreement, Working Paper, (under review at IFCS 2011).

Startk <- 1
Endk <- size(AllConfigs,1)-(Dim-1)
#Set end k to be the number of items -1

OutFull <- ones(Endk,Endk)
OutPart <- ones(Endk,Endk)

#Calculate the general and partial agreement between configurations
ConfigR <- AllConfigs(:,1:Dim)
for (YCounter1 in 1:Endk-1){
  for (YCounter2 in 2:Endk){
    Config1 <- AllConfigs(:,((YCounter1-1)*Dim+1):(YCounter1*Dim))
    Config2 <- AllConfigs(:,((YCounter2-1)*Dim+1):(YCounter2*Dim))
    [OutValue] <- GenAgree(Config1,Config2,Startk,Endk)
    OutFull(YCounter1,YCounter2) <- OutValue
    OutFull(YCounter2,YCounter1) <- OutValue
    [OutValue] <- PartialAgree(Config1,Config2,ConfigR,Startk,Endk)
    OutPart(YCounter1,YCounter2) <- OutValue
    OutPart(YCounter2,YCounter1) <- OutValue
  }
	return(list(OutFull,OutPart))
}

#Calculate the partial agreements between configurations relative to the
#1960 agreement.
