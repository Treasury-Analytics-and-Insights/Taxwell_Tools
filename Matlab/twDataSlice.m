function [slicedValues, startOfRanges] = twDataSlice( sliceVariableValues, ranges, values )
%twDataSlice( sliceVariableValues, ranges, values )
% This function returns a structure of data sliced by certain 
% criteria (useful for Winners and Losers)
%   Inputs:
%       sliceVariableValues: The variable that you will be comparing the ranges
%           to.
%       ranges: This variable is a n by 1 or n by 2 array for the ranges.  If n
%           by 1 then the sliceVariableValues must equal the range of that row to go into that slice.
%           If n by 2 then the sliceVariableValues must be >= to the first column
%           value and < than the second column value, for a given row.
%       values: Is an vector with the actual data being sliced.
%   Outputs: 
%       slicedValues: A structure of the form slicedValues.slice1, 
%           slicedValues.slice2, etc where each slice is the data for a given
%           range, if no data meets range that the a empty array is returned
%           for that slice
%       startOfRanges: An array of the beginning of each range
    
%% Error Checking 
if not(isvector(values))
   error('values is not a vector.  values must be 1 by n or n by 1.') 
end

if not(isvector(sliceVariableValues))
   error('sliceVariableValues not a vector.  sliceVariableValues must be 1 by n or n by 1, spelled variables not presently supported.') 
end

%% Logic
[rowSize, colSize] = size(ranges);

% Pre-allocate for speed
startOfRanges = NaN(rowSize,1);

if strcmpi(sliceVariableValues,'none')
    slicedValues.slice1 = values;
    startOfRanges = [];
else
    if colSize==1
    % If one column then looks for variable being equal to ranges, use for things like booleans, enumerations. 
        for i = 1:rowSize
            booleanSlice = ranges(i,1) == sliceVariableValues;

    %       Build a structure of the form
    %       slicedValues.slice1,slicedValues.slice2, etc
            newFieldname = ['slice' num2str(i)];
            slicedValues.(newFieldname) = values(booleanSlice);
            startOfRanges(i,1) = ranges(i,1);
        end
    elseif colSize==2
    % If two column then looks for variable value being in a range, used for
    % things like income or age.
        for i = 1:rowSize
            booleanSlice = ranges(i,1) <= sliceVariableValues & sliceVariableValues < ranges(i,2);
    %       Build a structure of the form
    %       slicedValues.slice1,slicedValues.slice2, etc
            newFieldname = ['slice' num2str(i)];
            slicedValues.(newFieldname) = values(booleanSlice);
            startOfRanges(i,1) = ranges(i,1);
        end
    else
        error('Only one or two column arrays can be used for ranges input')
    end
end

% End of Fucntion
end





