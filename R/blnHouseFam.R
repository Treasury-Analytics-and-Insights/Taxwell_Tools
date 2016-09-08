blnHouseFam <- function(Values, ID){ 
  # blnHouseFam takes the values and the corresponding ID array for values 
  # and it "Or's" all the values for each unique ID in the array. This allows 
  # the user to take values on a person level and make them on a family level if 
  # the family ID is used and on a household level if the household ID is used.  
  # 
  # For example, if there is a family with two members recieving a benefit 
  # and a third is not the result will be a family recieving benefits.  
  #
  # inputs:
  #   Values: Refers to the data that we are interested in
  #   ID: Refers to the corresponding ID for each value
  # output:
  #   Correct_values: The "Or" of all entries with the same ID
  #
  # ID's must be in sorted order and values must correspond to each ID, i.e.
  # the fourth value is for the fourth ID. 
  
  # The following lines check the input
  # variables to see if they have been entered correctly.
  if (is.unsorted(ID))
    stop('ID values not sorted, see help for further details.')
  
  if (!is.numeric(ID))
    stop('ID not numeric.  ID must be a vector of floats or integers') 
  
  if (NCOL(ID)>1)
    stop('ID not a vector.  ID must be 1 by n or n by 1.')
  
  # Create an array which lists the ID in ascending order. Each ID is entered once.
  uniqID = unique(ID);
  
  # Pre-populate output matrix
  colSize = as.numeric(ncol(Values))
  if(length(colSize) == 0)
    colSize = 1
  Correct_Values = matrix(, nrow = length(uniqID), ncol = colSize)
    
  # Not spelled variable
  if(colSize == 1){
    
    if(length(Values)!= length(ID)) 
      stop("Length of array not equal to length of ID array") 

    for (i in 1:length(uniqID))
      Correct_Values[i, 1] = any( Values[ID==uniqID[i]] )
  
    # Spelled variable
    } else{
      
      if(nrow(Values)!= length(ID)) 
        stop("Length of array not equal to length of ID array") 
      
      for (i in 1:length(ID)){
        # Loop through each period   
        for (j in 1:colSize){
          
          blnArray = ID==uniqID[i]
          Correct_Values[blnArray, j] = any( Values[blnArray,j] )
        }
      }
    }
  
  return (Correct_Values)
}
