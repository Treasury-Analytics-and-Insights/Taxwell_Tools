sumHouseFam <- function(Values, ID){ 
# sumHouseFam can be used to convert personal level data into family or 
# houshold level data (depending on which ID is used). It can also convert 
# family level data into household level data. The function takes the values 
# and the corresponding ID array for values and it sums all the values 
# for each unique ID in the array. Importantly, the output does not repeat 
# itself - it creates a single row of data for each unique ID 
#  
# inputs:
#   Values: Refers to the data that we are interested in
#   ID: Refers to the corresponding ID for each value
# output:
#   Correct_values: The sum of all entries with the same ID
#
# The first block of code checks to see if the input has been entered correctly.
# ID's must be in sorted order and values must correspond to each ID, i.e.
# the fourth value is for the fourth ID.  
  if (is.unsorted(ID))
    stop('ID values not sorted, see help for further details.')
  
  if (!is.numeric(ID))
    stop('ID not numeric.  ID must be a vector of floats or integers') 
  
  if (NCOL(ID)>1)
    stop('ID not a vector.  ID must be 1 by n or n by 1.')
  
# Create an array which lists the ID in ascending order. Each ID is entered once.
# Also determine the number of columns in the Value array. If it is greater than 1, 
# value is spelled
  uniqID = unique(ID);

# Pre-populate output matrix
  colSize = as.numeric(ncol(Values))
  if(length(colSize) == 0)
    colSize = 1
  Correct_Values <- matrix(, nrow = length(uniqID), ncol = colSize)
 
  
# Non-spelled variables
  if(colSize == 1){
    
    if(length(Values)!= length(ID)) 
      stop("Length of array not equal to length of ID array") 
  
    for (i in 1:length(uniqID))
      Correct_Values[i, 1] = sum( Values[ID==uniqID[i]] )
  
# Spelled variables
    } else{
      if(nrow(Values)!= length(ID)) 
        stop("Length of array not equal to length of ID array") 
            
# Add all the values with the same ID and fill the output matrix using the for loop.
      for (i in 1:length(uniqID)){
          for (j in 1:colSize){
            blnArray = ID==uniqID[i]
            Correct_Values[i,j] = sum(Values[blnArray,j])
          }
      }
    }
  return (Correct_Values)
}
