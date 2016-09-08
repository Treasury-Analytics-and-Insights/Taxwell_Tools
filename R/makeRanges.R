makeRanges <- function(range.array){ 
# A simple function to create a matrix of ranges for the winners and 
  # losers function or the twDataSlice function.  It expects a 1-D array 
  # that starts at the minimum valuem if this can be any value than a large
  # negative such as -10E10 should be used, and ends at the maximum value,
  # if any value is posible than a large positive value such as 10E10 should
  # be used.  The array must be monotonically increasing.
  #
  # Example you want a range starting at the min and going to 10000
  # 50000 and 70000 to the maximum then 
  # range.array = c(-10E10, 10000, 50000, 70000)
  # This will return a 2-D matrix of the form
  # -10E10  10000
  # 10000   50000
  # 50000   70000
  # 70000   10E10
  
  if (any(diff(range.array) <= 0))
    stop('Values must be monotonically increasing')
  
  col1 =c(range.array[seq(1,length(range.array)-1)])
  col2 =c(range.array[seq(2,length(range.array))])

  range.matrix = matrix(c(col1, col2), ncol = 2)  
  
  return(range.matrix)
}