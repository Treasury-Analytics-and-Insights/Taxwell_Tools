function Sumd_Vals = sumHouseFam(Values, ID)
% Sumd_Vals = sumHouseFam(Values, ID)
%
% sumHouseFam takes the values of an 1-D array and the corresponding ID 
% array for that array and it sums all the values for each unique ID in the
% array. This function does NOT presently support spells.
% 
% ID's must be in sorted order and values must correspond to each ID, i.e.
% the fourth value is for the fourth ID.  

% This allows you to take values on
% a person level and make them on a family level if the family ID is used
% and on a household level if the household ID is used.  
%
% You can also 
% determine the household values from the family values if using the
% household ID.

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

if not(isvector(Values))
   error('Values not a vector.  Values must be 1 by n or n by 1, spelled variables not presently supported.') 
end

%% Logic
[uniqID,uniqIndx] = unique(ID);

for i=1:length(uniqID)
   Sumd_Vals(i,1) = sum(Values(ID==uniqID(i)));
    
end

