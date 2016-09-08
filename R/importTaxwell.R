importTaxwell = function(path.file4Import, exportVars = TRUE, exportDataTable = T) {
#' This function imports a Taxwell output file. It is presently designed to only accept
#' numeric outputs for spelled variables  It also only accepts csv files at this time.
#' @param filename4Import is the only input and should consist of a csv filename.  If
#' the input does not have a .csv suffix it will give a warning, but try to import the
#' file.
#' @param exportsVars is an optional input variable with a default of TRUE.  When TRUE
#' all Taxwell variables are saved individually, i.e. P.ID, P.Income.DisposableIncome,
#' etc.
#' @param exportDataTable is an optional input witha  default of FALSE.  When TRUE
#' a data.table of the Taxwell import is saved called p.dt.  It is called p.dt since 
#' it is a data.table (dt) and there is one entry for every person, regardless of the
#' variable type.
#' @return filename4Export is returned.  This is the name of the rData file the data
#' is saved to.
#' 
#' 
# A possible alternative would be to pass in or make lists of variables by variable types.
# With a list of variables and there data types the P level to F or H level aggregation
# could be done automatically into  P.dt, F.dt, and H.dt data.tables.

  library(data.table)

  # Returns the string after the last '/' this should be the file name if a path was specified
  filename4Import = strsplit(path.file4Import,'/')[[1]][length(strsplit(path.file4Import,'/')[[1]])]

  # Check filename has csv suffix included
  if (substr(filename4Import,nchar(filename4Import)-3,nchar(filename4Import))!='.csv') {
    warning('Import file name does not have .csv extension')
  }

  if (!(exportVars) & !(exportDataTable)) {
    stop('Both exportVars and exportDataTable were set false, something must be exported')
  }
    
  # Import csv into data frame, use read.csv instead of fread as this 
  # automatically removes '/' and replaces them with '.'
  p.dt = read.csv(path.file4Import)
  
  # Convert file into data.table
  p.dt = data.table(p.dt)
  
  # Logic for when exporting variables
  if (exportVars) {
    spelledVar = FALSE
    
    # Create a variable from every column of the data.table
    for (colName in names(p.dt)) {
      
      # If the variable name ends _01 consider it spelled and the first
      # spelled variable for that variable. i.e. P/Income/DisposableIncome_01
      # will be the first disposable income in the data.table.
      if (substr(colName,nchar(colName)-2,nchar(colName)) == '_01') {
        spelledVar = TRUE
        spelledVarName = sub('_01','_Spl',colName)
        spelledColNames = c(colName)
      # If a spelled variable, then added the column name to the list of variables 
      } else if (spelledVar == TRUE) {
        spelledColNames = c(spelledColNames, colName)
        
        # Double check that the variable is the same of for spelled variable added
        if (substring(spelledVarName,1,nchar(spelledVarName)-4) != substring(colName,1,nchar(colName)-3)) {
          stop(paste('Error the next variable is not same as the previous spelled variable',
                     spelledVarName,1,colName))
          
        # If it is the same then on the last spell write the variable and reset
        # spelled variable.
        } else if (substr(colName,nchar(colName)-2,nchar(colName)) == '_24') {
          spelledVar = FALSE
          # There's got to be a clever way of doing this with an apply family function
          spelledData = c()
          for (spelledColName in spelledColNames) {
            spelledData = c(spelledData, p.dt[[spelledColName]])
          }
          
          assign(spelledVarName,matrix(spelledData,nrow=length(p.dt[[spelledColName]])))
        }
      }
      
      # Just assign normally if not spelled
      else {
        assign(colName,p.dt[[colName]])
      }
    }
    # Make a list of all the variables in the 
    varNames = ls()
    
    vars4Save =  varNames[grepl('P.*|F.*|H.*',varNames)]
  } else {
      # If not exporting vars than save as blank list
      vars4Save = c()
  }

  if (exportDataTable) {
    dt4Save = 'p.dt'
  } else {
    # If not exporting data.table than save as blank list
    dt4Save = c()  
  }
  
  # Combine variable name list in case exporting as variables and as data.table
  vars4Save = c(vars4Save,dt4Save)

  # Remove .csv extension 
  filename4Export = gsub('.csv','',filename4Import)

  # Add .Rdata extension in seperate line incase no .csv extension
  filename4Export = paste0(filename4Export, '.Rdata')

    
  save(list=vars4Save,file = filename4Export)

  return(filename4Export)
}