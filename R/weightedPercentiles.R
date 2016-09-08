weightedPercentiles <- function(values, weights, varargin){
  # This function takes an array of values and weights and returns the percentile values and 
  # weights. The percentiles can be chosen using the percentile input argument. For example,
  # if the median is required then percentile = .5. Alternatively, if quartiles are required 
  # then percentiles = [.25 .5 .75] is an optional argument and if percentile is not specified,
  # the default percentilesWeighted returns the values for deciles. Output results are lowest 
  # to highest precentiles.
  #
  # inputs:
  #   Values: Refers to the data that we are interested in
  #   weights: The prevalence of the demographic in the entire population
  #   varargin: This is an optional input. If it is not entered, the default percentiles are the 
  #             deciles. Otherwise, the values in varargin determine which percentiles are required
  # output:
  #   percentilesValues: The first value in each percentile
  #   percentilesIndexes: The index of the first value in each percentile
  #   indxsInPercentile: The index of each element in each percentile

  
  # Check the number of inputs. One input argument is not allowed, two input arguments are acceptable.
  # In the case of two input arguments, the default is deciles. If three input arguments are entered,
  # the value of varargin is the desired value for percentiles.
  percentiles <- switch(nargs(), stop('Too few inputs specified, minimum of two (values, weights)'), c(.1,.2,.3,.4,.5,.6,.7,.8,.9), varargin)
  
  # Checking for input error
  if (length(weights) != length(values))
    stop('Size of weights array does not equal size of values array')
  
  if (min(percentiles)<0 || max(percentiles)>1)
    stop('Percentiles 0 or less or 1 or greater. It must be 0<Percentile<1')
  
  if (NCOL(values)>1)
    stop('values is not a vector.  values must be 1 by n or n by 1.') 

  if (NCOL(weights)>1)
    stop('weights not a vector.  weights must be 1 by n or n by 1, spelled variables not presently supported.') 
  
  # Prepopulating the indxsInPercentile matrix
  indxsInPercentile <- matrix(list(),length(percentiles)+1,1)

  # Sorts the values in ascending order. Also keeps the index of each value.
  # There are two outputs. Firstly, y$x outputs all the sorted values. 
  # Secondly, y$ix returns the original indices of the sorted values. weightsSorted 
  # changes the ordering of the weights so that the weights match  with the sorted values
  
  y <- sort.int(values, partial = NULL,decreasing = FALSE, index.return = T)
  weightsSorted = weights[y$ix]
  
  weightsSortedCumSumNrmlzd = cumsum(weightsSorted)/sum(weightsSorted)
  
  # Pre-populate sortedPercentileIndx
  sortedPercentileIndx <- matrix(, nrow = 1, ncol = length(percentiles))
  
  # Needed for one percentile situation 
  sortedPercentileIndx[1,1] = min(which((weightsSortedCumSumNrmlzd > percentiles[1]) == T))
  indxsInPercentile[[1,1]]= y$ix[1:sortedPercentileIndx[1]]
 
  # Dealing with second percentiles to the second to last precentile
  if (length(percentiles) > 1) {
      for (i in 2:length(percentiles)){
          sortedPercentileIndx[1,i] = min(which((weightsSortedCumSumNrmlzd > percentiles[i]) == T)) 
          indxsInPercentile[[i,1]] = y$ix[(sortedPercentileIndx[i-1]+1):(sortedPercentileIndx[i])]
      }
  } else {
      i = 1
  }
  # Dealing with the  last percentile
  indxsInPercentile[[i+1,1]] = y$ix[(sortedPercentileIndx[i]+1):(length(values))]
 
  percentilesValues = y$x[sortedPercentileIndx]
  percentilesIndexes = y$ix[sortedPercentileIndx]
   
  return (list(percentilesValues, percentilesIndexes, indxsInPercentile))
}


