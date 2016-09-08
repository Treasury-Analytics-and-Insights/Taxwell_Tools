weightedMean <- function(values, weights){
  #   This function takes the weights and values as inputs and determines the weighted means.
  #   The size of the values and weights variables must be the same and both must be vectors.
  #
  # inputs:
  #   Values: Refers to the data that we are interested in
  #   weights: The prevalence of the demographic in the entire population
  # output:
  #   meanValue: weighted mean
  
  if(!(all(length(values) == length(weights)))) 
    stop('Size of values and weights variables are not equal') 
  
  if (NCOL(values)>1)
    stop('Values is not a vector.  Values must be 1 by n or n by 1.')
  
  if (NCOL(weights)>1)
    stop('Weights is not a vector.  Weights must be 1 by n or n by 1, spelled variables not presently supported.')
  
  meanValue = sum(values*weights)/sum(weights)
  
  return (meanValue)
}