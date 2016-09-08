slctHouseFam <- function(Values, ID){
  #  slctHouseFam can be used to change Taxwell units. It selects the 
  #  values of an array for each unique ID. So for instance if you have 
  #  weights on a person level and want them on a family level, you run 
  #  slctHouseFam(Weights, F_ID)and you will end up getting FF_Weights. 
  #  Where each FF_Weight is just the first weight within each unique family.
  #
  # inputs:
  #   Values: Refers to the attribute that we are interested in
  #   ID: Refers to the corresponding ID for each value in the desired unit of output
  # output:
  #   Correct_values: Refers to the desired attribute (Values) in 
  #                   the desired unit (ID)
  
  # Checking inputs for errors: Checking to see if the arrays are of
  # the same length, if the ID values are sorted, if the ID is numeric,
  # and if ID is a vector (single column or single row)
  
  if(length(Values)!= length(ID)) 
    stop("Length of array not equal to length of ID array") 
  
  if (is.unsorted(ID))
    stop('ID values not sorted, see help for further details.')
  
  if (!is.numeric(ID))
    stop('ID not numeric.  ID must be a vector of floats or integers') 
  
  if (NCOL(ID)>1)
    stop('ID not a vector.  ID must be 1 by n or n by 1.')
  
  
  # In the following two lines of code, the first position for each ID was identified.
  # unik is a logical vector of unique values. If it finds the first instance of an ID,
  # it shows TRUE. UniqueIndex refers to the indices where unik shows true
  unik <- !duplicated(ID)   
  UniqueIndex <- seq_along(ID)[unik]
  
  # If Spelled
  if (NCOL(Values) > 1) {     
    
    numOfRecords = length(UniqueIndex)
    numOfPeriods = length(Values)
    
    # Pre-populate output matrix
    Correct_Values = matrix(, nrow = numOfRecords, ncol = numOfPeriods)
    
    # Fill in output matrix one period at a time
    for (i in 1:numOfPeriods)
      Correct_Values[,i] = Values[UniqueIndex,i]
  
  # Not-Spelled
  } else{
    Correct_Values = Values[UniqueIndex]
  }
  
  return (Correct_Values)
}