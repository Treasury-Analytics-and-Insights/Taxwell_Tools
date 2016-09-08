function Bln_Vals = blnHouseFam(Values, ID)
% Bln_Vals = blnHouseFam(Values, ID)
%
% blnHouseFam takes the values of an 1-D array and the corresponding ID 
% array for that array and it "Or's" all the values for each unique ID in the
% array. This function does NOT presently support spells.
% 
% ID's must be in sorted order and values must correspond to each ID, i.e.
% the fourth value is for the fourth ID.  

% This allows you to take values on
% a person level and make them on a family level if the family ID is used
% and on a household level if the household ID is used.  So for example if
% there is a family with two members recieving a benefit and a third is not
% the result will be a family recieving benefits.  The booleans are or'd so
% P1 or P2 or P3 = Family
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

if not(isvector(Values))
   error('Values not a vector.  Values must be 1 by n or n by 1, spelled variables not presently supported.') 
end

%% Logic

uniqID = unique(ID);
Bln_Vals = NaN(length(uniqID),1);

for i=1:length(uniqID)
   Bln_Vals(i,1) = any(Values(ID==uniqID(i)));
end

