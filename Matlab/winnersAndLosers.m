function [outputExcelTable,outputSampleTable] = winnersAndLosers(receiveVar,sliceVar,rangesOrBands,weights,dispIncomeOrig,dispIncomeNew)
% [outputExcelTable,outputSampleTable] = winnersAndLosers(receiveVar,sliceVar,rangesOrBands,weights,dispIncomeOrig,dispIncomeNew)
%
% winnersAndLosers is used to determine the winners and losers of a Taxwell
% policy evaluation.  It requires the twDataSlice function as well.  Any
% winners and losers differences must beat least $.01 in magnitude.
% Any differences smaller than that will be considered no change.
%
% NOTE: all input variables must be of the same Taxwell unit.  So either one
% record (row) for every person, one record for every family, or one record
% for every household.
% 
% INPUTS
% receiveVar: In the output, for each slice, there is a count of the 
%   population for whom this variable is positive. Not strictly 
%   necessary in a winners and losers analysis but often helpful for 
%   identifying the proportion of a given population being affected or 
%   not effected by a change.
% sliceVar: The variable that will be used to slice the data into
%   subgroups.  This will be compared with the rangesOrBands variable.
% weights: This variable is simply the weights
% dispIncomeOrig: The disposable income from the original Taxwell run
%   (unchanged legislation).  This does not have to be disposable income 
%   but for nearly all cases should be.  
% dispIncomeNew: The disposable income from the Taxwell run with the new
%   policy.  This does not have to be disposable income but
%   for nearly all cases should be.  
% rangesOrBands: This is either a single column vector for slicing, e.g. 
%   [1;2] slices are those equaling 1 or 2, or 2 column ranges, 
%   e.g. [0 1; 1 3]. Note that ranges are greater than or equal to first
%   value but strictly less than second value.
%
% OUTPUTS
% outputExcelTable: Table of those who receiving receiveVar, those who win,
%   and those who lose, following the format below. Note the winners
%   and losers are out of all records not just those receiving the
%   receiveVar. Note the last row is the total of the column.
%   NOTE average gains and loses are per week.
%       CountsOfThoseRecieving PercentageOfThoseRecieving CountsOfWinners PercentageOfWinners AverageGainPerWeek  CountsOfLosers PercentageOfLosers AverageLossPerWeek 
%   Range1
%   Range2
%   Range3
%   ...etc
%   Total
%
% outputSampleTable: The number of sampels behind calculations. This
%   follows the format below.
%       SampleOfThoseRecieving SampleOfWinners SampleOfLosers
%   Range1
%   Range2
%   Range3
%   ...etc
%   Total
%
% Last updated 2015-10-23 by Brett Stawinski  QA Status: QA by Emily Irwin
% Unchecked
%
% Updated: 2015-10-23 by Brett Stawinski 
% Added $.01 tolerance for winners and losers, meaning changes must be 
% at least $.01 in magnitude for someone to be counted as a winner or loser.
%
%% Example 1
% filename1 = 'CS_HES13_TY17_HYEFU14x_Run1Data.mat';
% filename2 = 'CS_HES13_TY17_HYEFU14x_Run2Data.mat';
% 
% 
% dataOrig = load(filename1);
% dataNew = load(filename2);
% dispIncomeOrig = slctHouseFam(dataOrig.F_Income_Disposable,dataOrig.F_ID);
% dispIncomeNew = slctHouseFam(dataNew.F_Income_Disposable,dataNew.F_ID);
% sliceVar = sumHouseFam(dataOrig.P_Income_WfFIncome, dataOrig.F_ID);
% receiveVar = sumHouseFam(dataOrig.P_FamilyAssistance_Total,dataOrig.F_ID);
% weights =  slctHouseFam(dataOrig.F_Weight_FamilyWeight, dataOrig.F_ID);
% 
% rangesOrBands = [ 0 35900
%                 35900 50000
%                 50000 75000
%                 75000 100000
%                 100000 10^10];
%             
% [outputExcelTable,outputSampleTable] =...
%     winnersAndLosers(receiveVar,sliceVar,rangesOrBands,weights,dispIncomeOrig,dispIncomeNew);
%% Example 2
% filename1 = 'CS_HES13_TY17_HYEFU14x_Run1Data.mat';
% filename2 = 'CS_HES13_TY17_HYEFU14x_Run2Data.mat';
% 
% dataOrig = load(filename1);
% dataNew = load(filename2);
% dispIncomeOrig = sumHouseFam(dataOrig.P_Income_Disposable,dataOrig.H_ID);
% dispIncomeNew = sumHouseFam(dataNew.F_Income_Disposable,dataNew.H_ID);
% sliceVar = blnHouseFam(dataOrig.P_Relationships_IsCouple, dataOrig.H_ID);
% receiveVar = sumHouseFam(dataOrig.F_CoreBenefits_Abated,dataOrig.H_ID);
% weights =  slctHouseFam(dataOrig.P_Weight_2017TaxYearIntegrated, dataOrig.H_ID);
% 
% rangesOrBands = [0; 1];
%             
% [outputExcelTable,outputSampleTable] =...
%     winnersAndLosers(receiveVar,sliceVar,rangesOrBands,weights,dispIncomeOrig,dispIncomeNew);
%% Calculate Difference
incomeDiff = dispIncomeNew-dispIncomeOrig;
minDiffMagnitude = .01;
%% Recieve - Calculate who recieves the thing being measures
recieveBln = receiveVar>0;
rcv.weights = weights(recieveBln);
rcv.sliceVar = sliceVar(recieveBln);
rcv.totalCnts = sum(rcv.weights);

slicedWeights = twDataSlice(rcv.sliceVar,rangesOrBands,rcv.weights);
sliceFieldnames = fieldnames(slicedWeights);
% Loop though structure of slices
for i=1:length(sliceFieldnames)
    weightTmp = slicedWeights.(sliceFieldnames{i});
    rcv.cnts(i,1) = sum(weightTmp);
    rcv.samples(i,1) = length(weightTmp);
    rcv.pctCnts(i,1) = sum(weightTmp)/rcv.totalCnts;
end
rcv.totalPct = sum(rcv.cnts)/rcv.totalCnts;

%% Winners - Calculate who gains from policy change
winnersBln = incomeDiff>=minDiffMagnitude; % Find Winners
win.incomeDiff = incomeDiff(winnersBln);
win.weights = weights(winnersBln);
win.sliceVar = sliceVar(winnersBln);
win.totalCnts = sum(win.weights);
win.totalAvgPerWeek = weightedMean(win.incomeDiff,win.weights)/52;

slicedIncomeDiff = twDataSlice(win.sliceVar,rangesOrBands,win.incomeDiff);
slicedWeights = twDataSlice(win.sliceVar,rangesOrBands,win.weights);
sliceFieldnames = fieldnames(slicedWeights);
% Loop though structure of slices
for i=1:length(sliceFieldnames)
    weightTmp = slicedWeights.(sliceFieldnames{i});
    incomeDiffTmp = slicedIncomeDiff.(sliceFieldnames{i});
    win.cnts(i,1) = sum(weightTmp);
    win.samples(i,1) = length(weightTmp);
    win.avgPerWeek(i,1) = weightedMean(incomeDiffTmp,weightTmp)/52;
    win.pctCnts(i,1) = sum(weightTmp)/win.totalCnts;
end
win.totalPct = sum(win.cnts)/win.totalCnts;

%% Losers - Calculate who loses from policy change
losersBln = incomeDiff<=-minDiffMagnitude; % Find losers
lose.incomeDiff = incomeDiff(losersBln);
lose.weights = weights(losersBln);
lose.sliceVar = sliceVar(losersBln);
lose.totalCnts = sum(lose.weights);
lose.totalAvgPerWeek = weightedMean(lose.incomeDiff,lose.weights)/52;

slicedIncomeDiff = twDataSlice(lose.sliceVar,rangesOrBands,lose.incomeDiff);
slicedWeights = twDataSlice(lose.sliceVar,rangesOrBands,lose.weights);
sliceFieldnames = fieldnames(slicedWeights);
% Loop though structure of slices
for i=1:length(sliceFieldnames)
    weightTmp = slicedWeights.(sliceFieldnames{i});
    incomeDiffTmp = slicedIncomeDiff.(sliceFieldnames{i});
    lose.cnts(i,1) = sum(weightTmp);
    lose.samples(i,1) = length(weightTmp);
    lose.avgPerWeek(i,1) = weightedMean(incomeDiffTmp,weightTmp)/52;
    lose.pctCnts(i,1) = sum(weightTmp)/lose.totalCnts;
end
lose.totalPct = sum(lose.cnts)/lose.totalCnts;

%% Output
% Output table formatted to enable copying into excel
outputExcelTable = [    rcv.cnts        rcv.pctCnts     win.cnts        win.pctCnts     win.avgPerWeek      lose.cnts       lose.pctCnts    lose.avgPerWeek
                        rcv.totalCnts   rcv.totalPct    win.totalCnts   win.totalPct    win.totalAvgPerWeek lose.totalCnts  lose.totalPct   lose.totalAvgPerWeek];

% Table of the samples for each group.  Needed to check if sample large
% enough for confidentiality issues.
outputSampleTable = [ rcv.samples      win.samples      lose.samples
                sum(rcv.samples) sum(win.samples) sum(lose.samples)];