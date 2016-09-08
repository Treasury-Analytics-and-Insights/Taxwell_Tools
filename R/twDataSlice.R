twDataSlice <- function( sliceVariableValues, ranges, values ){
  # This function returns a list of data sliced by certain criteria 
  # (useful for Winners and Losers)
  #  Inputs:
  #       sliceVariableValues: The variable that the ranges will be compared to.
  #       ranges: This variable is a n by 1 or n by 2 array for the ranges.  If n
  #               by 1 then the sliceVariableValues must equal the range of that 
  #               row to go into that slice.
  #               If n by 2 then the sliceVariableValues must be >= to the first column
  #               value and < than the second column value, for a given row.
  #       values: Is a vector with the actual data being sliced.
  #  Outputs: 
  #       slicedValues: A list of the form slice1, slice2, etc where each slice is the data 
  #                     for a given range, if no data meets range that the a empty array is 
  #                     returned for that slice. The final elemnt of the list is named 
  #                     startOfRanges. It is an array containing the starting element of each range.
  
  # Error Checking the inputs
  if (NCOL(values)>1)
    stop('values is not a vector.  values must be 1 by n or n by 1.') 
  
  if (NCOL(sliceVariableValues)>1)
    stop('sliceVariableValues not a vector.  sliceVariableValues must be 1 by n or n by 1, spelled variables not presently supported.') 
  
  # Colzise determines the nature of the range (exact match or range) while rowsize determines the
  # length of startOfRanges
  rowSize = nrow(ranges)
  colSize = ncol(ranges)
  
  # Need to pre-allocate in R
  startOfRanges = matrix(,rowSize,1)
  slicedValues = list()

  # When range is a single column matrix - we want exact matches.
  if (colSize == 1){
   
  # If one column then looks for variable being equal to ranges, use for things like booleans, 
  # enumerations.
      for (i in 1:rowSize){
        booleanSlice = (ranges[i,1] == sliceVariableValues)
        slicedValues[[i]] = values[booleanSlice]
        startOfRanges[i] = ranges[i,1]
      }
      # When range has two columns - we are actually dealing with a range
      }else if (colSize == 2){
        # If two column then looks for variable value being in a range, used for
        # things like income or age.      
        for (i in 1:rowSize){
          booleanSlice = (ranges[i,1] <= sliceVariableValues) & (sliceVariableValues < ranges[i,2])
          slicedValues[[i]] = values[booleanSlice]
          startOfRanges[i] = ranges[i,1]  
        }
  }
  # Naming the individual slices 'slice1', 'slice2' etc.
  names(slicedValues) = paste0("slice", seq_along(slicedValues))
  
  # Adding startOfRanges to the output variable and naming it appropriately
  slicedValues[[(length(slicedValues)+1)]] = startOfRanges
  names(slicedValues)[[length(slicedValues)]] = "startOfRanges"

  return(slicedValues)
}