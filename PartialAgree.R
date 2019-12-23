PartialAgree <- function(Config1,Config2,ConfigR,Startk,Endk){
#UNTITLED Summary of this function goes here
#   Detailed explanation goes here
  #Calculate the individual agreements

  Agree12 <- GenAgree(Config1,Config2,Startk,Endk)
  Agree1R <- GenAgree(Config1,ConfigR,Startk,Endk)
  Agree2R <- GenAgree(Config2,ConfigR,Startk,Endk)

  Num <- Agree12-(Agree1R*Agree2R)
  Denom <- sqrt((1-(Agree1R^2)))*(sqrt(1-(Agree2R^2)))

  PartAgree <- Num./Denom

	return(PartAgree)
}

