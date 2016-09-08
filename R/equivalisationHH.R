equivalisationHH <- function(countAdults, countKids, equivType){
  #   Determines the equivillization factor based on household makeup.  Choices are 'sqrt',
  #   'jensen', 'OECD', and 'none'. 
  #
  # inputs:
  #   countAdults: number of adults in the household
  #   countKids: number of dependent children in the household
  #   equivType: The type of equivalisation we want to do. Choices are 'sqrt', 'jensen', 
  #              'OECD', and 'none'
  # output:
  #   eqFactor: The equivalisation factor
  
  
  # Checking to see if the entered equivType is one of the possible options. The input is 
  # case insensitive.
  equivType <- tolower(equivType)
  if ((equivType != "sqrt") & (equivType != "jensen") & (equivType != "oecd") & (equivType != "none"))
      stop("Equivilization type given not defined")
 
  jensen_beta <- 0.730348;
  jensen_alpha <- 0.621448;
  OECD_adult <- .5;
  OECD_kids <- .3;
  eqFactor <- 0
  
  if (equivType =='sqrt'){
    eqFactor = sqrt(countAdults + countKids)}  
  else if (equivType =='jensen'){
    eqFactor = (countAdults + jensen_beta * countKids)^jensen_alpha} 
  else if (equivType == 'oecd'){
    eqFactor = 1 + (countAdults-1)*OECD_adult + countKids*OECD_kids}  
  else if (equivType =='none'){
    eqFactor = 1}    

  return (eqFactor)
}
