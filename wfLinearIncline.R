wfLinearIncline <- function(n,Startk,Endk){
#wfLinear
#Weighting function for a linear decline in weights
#If k=1 Then weight = 1/n-1 if k = n-1 then weight = 1
#INPUTTS
# n - The number of items in a configuration
#Startk - The starting value of k
#Endk - The end value of k
#OUTPUT
#Weights - A (Endk-Stark+1)*1 column vector of weights

Weights <- zeros(Endk-Startk+1,1)

for (Counter in Startk:Endk){
  Weights(Counter-Startk+1) <- (Counter)/(n-1)
	return(Weights)
}

