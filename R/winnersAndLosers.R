winnersAndLosers <- function(receiveVar, sliceVar, rangesOrBands, weights, incomeOrEMTROrig, incomeOrEMTRNew, makeWeekly = T){
  # WinnersAndLosers is used to determine the winners and losers of a Taxwell policy evaluation.
  # It requires the twDataSlice function as well.  Any winners and losers differences must be 
  # atleast .001 in magnitude so ($.001 or .1% for EMTRs). Any differences smaller than that 
  # will be considered no change.
  #
  # NOTE: all input variables must be of the same Taxwell unit.  So either one record (row) for 
  # every person, one record for every family, or one record for every household.
  # 
  # INPUTS
  #     receiveVar: In the output, for each slice, there is a count of the population for whom 
  #                 this variable is positive. Not strictly necessary in a winners and losers 
  #                 analysis but often helpful for identifying the proportion of a given 
  #                 population being affected or not effected by a change.
  #     sliceVar: The variable that will be used to slice the data into
  #     subgroups.  This will be compared with the rangesOrBands variable.
  #     weights: This variable is simply the weights
  #     incomeOrEMTROrig: The disposable income from the original Taxwell run (unchanged 
  #                     legislation).  This does not have to be disposable income but for nearly
  #                     all income cases should be.  It could also be an EMTR when doing an EMTR comparison.
  #                     NOTE: EMTRs should all be base 100 so an EMTR of 5% is 5, this is for numerical precision.
  #     incomeOrEMTRNew: The disposable income from the Taxwell run with the new policy.  This does
  #                     not have to be disposable income but for nearly income cases should be.
  #                     It could also be an EMTR when doing an EMTR comparison. NOTE: EMTRs 
  #                     should all be base 100 so an EMTR of 5% is 5, this is for numerical precision.
  #     rangesOrBands: This is either a single column vector for slicing, e.g. [1;2] slices are 
  #                    those equaling 1 or 2, or 2 column ranges, e.g. [0 1; 1 3]. Note that 
  #                    ranges are greater than or equal to first value but strictly less than 
  #                    second value.
  #     makeWeekly: This optional variable has a default of TRUE.  When TRUE the average gains
  #                 and loses are divided by 52.2.  When it is false the averages are just annualy.
  #
  # OUTPUTS
  #     data: Table of those who receiving receiveVar, those who win, and those who lose and the 
  #           number of samples behind each calculation. Note the winners and losers are out of 
  #           all records not just those receiving the receiveVar. Note the last row is the total of the column.
  #     NOTE average gains and losses are per week unless makeWeekly is set false.
  #       Format of the following variables are shown below: CountsOfThoseRecieving 
  #       PercentageOfThoseRecieving CountsOfWinners PercentageOfWinners AverageGainPerWeek  
  #       CountsOfLosers PercentageOfLosers AverageLossPerWeek 
  #   Range1
  #   Range2
  #   Range3
  #   ...etc
  #   Total
  #
  #    The samples behind calculations include the followiing variables and follow the format below.
  #    variables: sampleOfThoseRecieving SampleOfWinners SampleOfLosers
  #   Range1
  #   Range2
  #   Range3
  #   ...etc
  #   Total
  
  # Initializing the variables
  rcv.cnts = 0
  rcv.samples = 0
  rcv.pctCnts = 0
  win.cnts = 0
  win.samples = 0
  win.pctCnts = 0
  win.avgPerWeek = 0
  lose.cnts = 0
  lose.samples = 0
  lose.pctCnts = 0
  lose.avgPerWeek = 0
  
  if (makeWeekly == T) {
    weeksPerYear = 52.2
    print('Note: Averages are per week')
  } else {weeksPerYear = 1}
  

  # Calculating the income difference and setting the minnimum difference that will be considered
  variableDiff  = incomeOrEMTRNew - incomeOrEMTROrig;
  minDiffMagnitude = .001;
  
  # Recieve - Calculate who recieves the variable being measured
  recieveBln = receiveVar > 0;
  rcv.weights = weights[recieveBln];
  rcv.sliceVar = sliceVar[recieveBln];
  rcv.totalCnts = sum(rcv.weights);
  slicedWeights = twDataSlice(rcv.sliceVar,rangesOrBands,rcv.weights);
  
  # Loop though structure of slices
  for (i in 1:(length(slicedWeights)-1)){
    weightTmp = slicedWeights[[i]];
    rcv.cnts[i] = sum(weightTmp);
    rcv.samples[i] = length(weightTmp);
    rcv.pctCnts[i] = sum(weightTmp)/rcv.totalCnts;
  }
  
  rcv.totalPct = sum(rcv.cnts)/rcv.totalCnts;
  
  # Winners - Calculate who gains from policy change
  winnersBln = variableDiff >= minDiffMagnitude; # Find Winners
  win.variableDiff = variableDiff[winnersBln];
  win.weights = weights[winnersBln];
  win.sliceVar = sliceVar[winnersBln];
  win.totalCnts = sum(win.weights);
  win.totalAvgPerWeek = weightedMean(win.variableDiff,win.weights)/weeksPerYear;
  
  slicedvariableDiff.win = twDataSlice(win.sliceVar,rangesOrBands,win.variableDiff);
  slicedWeights.win = twDataSlice(win.sliceVar,rangesOrBands,win.weights);
  
  # Loop though structure of slices
  for (i in 1:(length(slicedWeights.win)-1)){
    weightTmp = slicedWeights.win[[i]];
    variableDiffTmp = slicedvariableDiff.win[[i]];
    win.cnts[i] = sum(weightTmp);
    win.samples[i] = length(weightTmp);
    win.avgPerWeek[i] = weightedMean(variableDiffTmp,weightTmp)/weeksPerYear;
    win.pctCnts[i] = sum(weightTmp)/win.totalCnts;
  }
  
  win.totalPct = sum(win.cnts)/win.totalCnts;
  
  # Losers - Calculate who loses from policy change
  losersBln = variableDiff<=-minDiffMagnitude; # Find losers
  lose.variableDiff = variableDiff[losersBln];
  lose.weights = weights[losersBln];
  lose.sliceVar = sliceVar[losersBln];
  lose.totalCnts = sum(lose.weights);
  lose.totalAvgPerWeek = weightedMean(lose.variableDiff,lose.weights)/weeksPerYear;
  
  slicedvariableDiff.lose = twDataSlice(lose.sliceVar,rangesOrBands,lose.variableDiff);
  slicedWeights.lose = twDataSlice(lose.sliceVar,rangesOrBands,lose.weights);
  
  # Loop though structure of slices
  for (i in 1:(length(slicedWeights.lose)-1)){
    weightTmp = slicedWeights.lose[[i]];
    variableDiffTmp = slicedvariableDiff.lose[[i]];
    lose.cnts[i] = sum(weightTmp);
    lose.samples[i] = length(weightTmp);
    lose.avgPerWeek[i] = weightedMean(variableDiffTmp,weightTmp)/weeksPerYear;
    lose.pctCnts[i] = sum(weightTmp)/lose.totalCnts;
  }
  
  lose.totalPct = sum(lose.cnts)/lose.totalCnts;
  
  # Adding the totals at the end of each variable to make them appear on the same column
  rcv.cnts[length(slicedWeights)] = rcv.totalCnts
  rcv.pctCnts[length(slicedWeights)] = rcv.totalPct
  win.cnts[length(slicedWeights.win)] = win.totalCnts
  win.pctCnts[length(slicedWeights.win)] = win.totalPct 
  win.avgPerWeek[length(slicedWeights.win)] = win.totalAvgPerWeek
  lose.cnts[length(slicedWeights.lose)] = lose.totalCnts
  lose.pctCnts[length(slicedWeights.lose)] = lose.totalPct
  lose.avgPerWeek[length(slicedWeights.lose)] = lose.totalAvgPerWeek
  rcv.samples[length(slicedWeights)] = sum(rcv.samples)
  win.samples[length(slicedWeights.win)] = sum(win.samples)
  lose.samples[length(slicedWeights.lose)] = sum(lose.samples)
  
  # Creating the output dataframe with the required variables. The final row of each column 
  # contains totals
  data <- data.frame(
    Received.Counts = rcv.cnts,
    Received.Percent.Counts = rcv.pctCnts,
    Winners.Counts = win.cnts,
    Winners.Percent.Counts = win.pctCnts,
    Winners.Average.PerWeek = win.avgPerWeek,
    Losers.Counts = lose.cnts,
    Losers.Percent.Counts = lose.pctCnts,
    Losers.Average.PerWeek = lose.avgPerWeek,
    Received.Samples = rcv.samples,
    Winners.Samples = win.samples,
    Losers.Samples = lose.samples
  )
  return(data)
}


# setwd("Q:/Taxwell User Group/Brett Stawinski/MatlabTrainingCourse/17_June_2015_Tools")
# FF_IncomeOrig = slctHouseFam(Run1Data$`F/Income/Disposable`,Run1Data$`F/ID`)
# FF_IncomeNew = slctHouseFam(Run2Data$`F/Income/Disposable`,Run1Data$`F/ID`);
# FF_weights = slctHouseFam(Run2Data$`F/Weight/FamilyWeight`,Run1Data$`F/ID`);
# FF_WfFIncomeOrig = sumHouseFam(Run1Data$`P/Income/WfFIncome`,Run1Data$`F/ID`);
# FF_FamAssistTotOrig = sumHouseFam(Run1Data$`P/Cost/Amount/WFFTotal`,Run1Data$`F/ID`);
# 
# winnersAndLosers()