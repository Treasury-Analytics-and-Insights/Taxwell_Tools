sumApplyToAllHouseFam <- function(Values, ID){
  # sumApplyToAllHouseFam can be used to convert personal level data into family or 
  # houshold level data (depending on which ID is used). It can also convert 
  # family level data into household level data. The function takes the values 
  # and the corresponding ID array for values and it sums all the values 
  # for each unique ID in the array. Importantly, the output does repeat 
  # itself - it outputs the family/household level data for each individual. 
  #
  # inputs:
  #   Values: Refers to the data that we are interested in
  #   ID: Refers to the corresponding ID for each value
  # output:
  #   Correct_values: The sum of all entries with the same ID.
  
  # The first block of code checks to see if the input has been entered correctly.
  # ID must be in sorted order, numeric and a vector  
  if (is.unsorted(ID))
    stop('ID values not sorted, see help for further details.')
  
  if (!is.numeric(ID))
    stop('ID not numeric.  ID must be a vector of floats or integers') 
  
  if (NCOL(ID)>1)
    stop('ID not a vector.  ID must be 1 by n or n by 1.')
  
  # Create array which lists the ID in ascending order. Each ID is entered once.
  uniqID = unique(ID);   
  
  # Pre-populate the output matrix based on the length of the ID array (this must 
  # also be equal to the length of the values vector). 
  colSize = as.numeric(ncol(Values))
  if(length(colSize) == 0)
    colSize = 1
  Correct_Values = matrix(, nrow = length(ID), ncol = colSize)
  
  # Non-spelled variables
  if(colSize == 1){
    
    if(length(Values)!= length(ID)) 
      stop("Length of array not equal to length of ID array") 
  
    # Add all the values with the same ID and fill the output matrix using the 
    # for loop.
    for (i in 1:length(uniqID))
      Correct_Values[ID==uniqID[i]] = sum( Values[ID==uniqID[i]] );
    
  # Spelled variables  
  } else{
    
    if(nrow(Values)!= length(ID)) 
      stop("Length of array not equal to length of ID array") 
        
    # Add all the values with the same ID and fill the output matrix using the for loop.
    for (i in 1:length(ID)){
      for (j in 1:colSize){
        blnArray = ID==uniqID[i]
        Correct_Values[blnArray,j] = sum(Values[blnArray,j])
      }
    }
  }
  
  return (Correct_Values)
}