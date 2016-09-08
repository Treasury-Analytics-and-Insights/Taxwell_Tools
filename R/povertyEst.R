povertyEst <- function(P.income, P.weight, H.ID, P.dependent, percentageOfMedian, equiv.type, varargin ) {
  # povertyEst is used to estimate the number of people in households in poverty and the number of children 
  #   in households in poverty.  This is estimated as those households with an equivalised income below
  #   a given percentage of the median.  The median is determine as the median equivalised income of households
  #   where every person in that household is given the household equivalised income.  Equivalisation is done
  #   within this function and is specified via a parameter as described below.
  #
  # NOTE: all input variables must be on a person unit.  So  one record (row) for 
  # every person each row only represents the individual person.
  # 
  # INPUTS
  #     P.income: Non equivalised incomes on a person level
  #     P.weight: Person level weights
  #     H.ID:     Household IDs, one for every person
  #     P.dependent:  If 1 or T then person is dependent if not than 0 or False
  #     percentageOfMedian: Percentage of median to be used for poverty often .5 or .6
  #     equiv.type: The type of equivalisation we want to do. Choices are 'sqrt', 'jensen', 
  #              'OECD', and 'none'
  #     varargin: Optional argument, if set then this is used as the median instead 
  #               of using the calculated median, this allows fixed poverty calculations.
  #               If no option is set then relative poverty is used.
  # OUTPUTS
  #     poverty.df: Data frame with out put variables, see list below
  #       Pct.people.poverty: The percent of people living in households in poverty
  #       Pct.kids.Poverty:   The percent of dependents living in households in poverty
  #       cnt.poverty.people: The number of people living in households in poverty
  #       cnt.poverty.children:   The number of dependents living in households in poverty
  #       median.used:  The median used in the calculation of the poverty line
  #       sample.poverty.people: The number of samples of people in households in poverty  
  #       sample.poverty.children:  The number of samples of children in hosueholds in poverty.
  #
  # Last modified: 03/08/2016 by Brett Stawinski

  # Checking for input error
  if (length(P.income) != length(P.weight) | length(P.weight) != length(H.ID)  | length(P.weight) != length(P.dependent))
    stop('Size of of P.income, P.weights, H.ID, or P.dependent is not equal, all variables should be same size')
  
  if ( percentageOfMedian<0 || percentageOfMedian>1 )
    stop('percentageOfMedian must be 0<Percentile<1')
  
  P.H.cnt.adults = sumApplyToAllHouseFam(P.dependent==0, H.ID)
  P.H.cnt.kids = sumApplyToAllHouseFam(P.dependent==1, H.ID)
  P.H.equiv = equivalisationHH(P.H.cnt.adults, P.H.cnt.kids, equiv.type)

  P.H.income = sumApplyToAllHouseFam(P.income, H.ID)
  P.H.income.equiv = P.H.income/P.H.equiv
  
  # If optional argument use this for median, this allows you to do fixed poverty levels
  if (nargs() == 7) {
    income.median.HH.equiv = varargin[1]
    
    # If no optional input than calculate the median, this is for relative poverty levels
  } else if (nargs() == 6) {
    income.median.HH.equiv = weightedPercentiles(P.H.income.equiv, P.weight, .5)[[1]]
    
  } else { 
    stop('Error number of inputs must be 6 or 7') }
  
  P.povertyBln = P.H.income.equiv < percentageOfMedian*income.median.HH.equiv
  cnt.poverty.people = sum(P.weight[P.povertyBln])
  cnt.poverty.children = sum(P.weight[P.povertyBln & P.dependent == 1])
  
  sample.poverty.people = sum(P.povertyBln)
  sample.poverty.children = sum(P.dependent*P.povertyBln)
  
  pov.people.pct = cnt.poverty.people / sum(P.weight)
  pov.kids.pct = cnt.poverty.children / sum(P.dependent*P.weight)
  
  poverty.df = data.frame(pov.people.pct, pov.kids.pct, cnt.poverty.people, cnt.poverty.children, income.median.HH.equiv, sample.poverty.people, sample.poverty.children)
  colnames(poverty.df) = c('Pct.people.poverty', 'Pct.kids.Poverty',
                           'cnt.poverty.people','cnt.poverty.children', 'median.used',
                           'sample.poverty.people','sample.poverty.children')

  return(poverty.df)
}