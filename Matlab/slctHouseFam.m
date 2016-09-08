function Select_Vals = slctHouseFam(Values, ID)
% Select_Vals = slctHouseFam(Values, ID)
%
% slctHouseFam selects the values of an array for each unique ID.  
% This function supports spelled variables. 

% So for instance if you have 
% weights on a person level and want them on a family level, you run 
% slctHouseFam(Weights, F_ID)and you will end up getting FF_Weights.  
% Where each FF_Weight is just the first weight within each unique family. 
% 

%% Error Check
if length(Values)~= length(ID)
    error('Length of array not equal to length of ID array')
end

if not(issorted(ID))
    error('ID values not sorted, see help for further details.')
end

if not(isnumeric(ID))
   error('ID not numeric.  ID must be a vector of floats or integers') 
end

if not(isvector(ID))
   error('ID not a vector.  ID must be 1 by n or n by 1.') 
end

%% Logic
[~,uniqIndx] = unique(ID);

% Spelled Variables Determine If Spelled Variables
if not(isvector(Values))      
    % Find size to pre-allocate array
    [numOfRecords,~] = size(uniqIndx);
    [~,numOfPeriods] = size(Values);
    
    Select_Vals = NaN(numOfRecords,numOfPeriods);
    
    % Iterate through for each period (column)
    for i=1:numOfPeriods
        Select_Vals(:,i) = Values(uniqIndx,i);
    end
else
% If non-spelled data this is faster
   Select_Vals = Values(uniqIndx);
end
