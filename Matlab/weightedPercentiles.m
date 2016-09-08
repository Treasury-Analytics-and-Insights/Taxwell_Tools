function [percentilesValues, percentilesIndexes, percentiles, indxsInPercentile] = weightedPercentiles(values, weights, varargin)
% [percentilesValue, percentilesIndex, percentiles, indxsInPercentile] = weightedPercentiles(values, weights, percentiles) takes an 
% array of values and weights and returns the percentile values and 
% weights. The percentiles can be chosen by the percentile.  So
% if the median is required then percentile = .5, if quartiles are
% required than percentiles = [.25 .5 .75] percentiles is an optional argument and if not specified
% is specified then percentilesWeighted returns the
% values for .1, .25, .5, .75, .9.  Output results are lowest to highest
% precentiles.
% 
% If only the percentileValues are desired then only require one output 
% when function is called, i.e. 
% percentilesValue = weightedPercentiles(values,percentiles, weights),
% if  the percentilesValue and percentiles are required 
% [percentilesValue, ~, percentiles] = weightedPercentiles(values,percentiles, weights).
% if the percentilesIndex is required
% [percentilesValue, percentilesIndex, percentiles] = weightedPercentiles(values,percentiles, weights).
% additionally if just the percentilesIndex is needed
% [~, percentilesIndex, ~ ] = weightedPercentiles(values,percentiles, weights).
% indxsInPercentile: This output is a cell array where is cell is an array
% of indexes where all the indexes are inside the percentile.  For example.
% if the user is finding the median than there would be two cells in the
% array, one for indexes above 50% and one below 50%.
%
% The median value is determined by:
% - Sorting the weights. Then 
% - Cumulative summing the weights, i.e. if weights  = [1 2 3] then
%       cumulative sums = [1 3 6]
% - Finally determine the cumulative summed weight above to 50% of the
%       total sum of the weights.
%
% Created by: Brett Stawinski
% Modification: 24 July 2015 - Brett Stawinski now outputs indexes for each
% subgroup (percentile, decile, etc)
%
% Last updated 2015-07-27 by Brett Stawinski  QA Status: Checked 7/10/2015
% by Emily Irwin

%% Determine Percentiles
switch nargin
    
    % Default case since percentiles not specified.
    case 1
        error('Too few inputs specified, minimum of two (values, weights)')
    case 2
        percentiles = [.1 .25 .5 .75 .9];
        disp(['No percentile size specified so default precentiles of ' num2str(percentiles)]);
    case 3
        percentiles = varargin{1};
    otherwise
        error('Too many inputs specified, three variables max, minimum of two')
end

%% Error Checking
if any(size(weights) ~= size(values))
    error('Size of weights array does not equal size of values array')
end

if min(percentiles)<0 || max(percentiles)>1
    error('Percentiles 0 or less or 1 or greater. It must be 0<Percentile<1')
end

if not(isvector(values))
   error('values is not a vector.  values must be 1 by n or n by 1.') 
end

if not(isvector(weights))
   error('weights not a vector.  weights must be 1 by n or n by 1, spelled variables not presently supported.') 
end

%% Determine Nominalized Weights
% Sort weights and their values
[valuesSorted,sortedIndxs] = sort(values);
weightsSorted =  weights(sortedIndxs);

% In order to track which index percentile is at.
valueIndexes = (1:length(values))';
valueIndexesSorted = valueIndexes(sortedIndxs);

% Create normalized cumulative sum of sorted weights 
weightsSortedCumSumNrmlzd = cumsum(weightsSorted)/sum(weightsSorted);

%% Determine Indexes of Values Closest to Percentiles

% Pre-allocate array
sortedPercentileIndx = nan(1,length(percentiles));

sortedPercentileIndx(1,1) = find(weightsSortedCumSumNrmlzd > percentiles(1),1,'first'); 
indxsInPercentile{1,1}= sortedIndxs(1:sortedPercentileIndx);
for i = 2:length(percentiles)
    sortedPercentileIndx(1,i) = find(weightsSortedCumSumNrmlzd > percentiles(i),1,'first');  
    indxsInPercentile{i,1} = sortedIndxs( sortedPercentileIndx(i-1)+1:sortedPercentileIndx(i) );
end

indxsInPercentile{i+1,1} = sortedIndxs( sortedPercentileIndx(i)+1:end );
%% Set function outputs
percentilesValues = valuesSorted(sortedPercentileIndx);
percentilesIndexes = valueIndexesSorted(sortedPercentileIndx);
end

%% Old Code for Case 3
%         sizeOfPercentile = varargin{1};
%         % Create array of percentiles based on user specified sizeOfPercentile
%         numOfPercentiles = floor(1/sizeOfPercentile);
%         for i = 1:numOfPercentiles-1
%             percentiles(1,i) =  i*sizeOfPercentile;
%         end