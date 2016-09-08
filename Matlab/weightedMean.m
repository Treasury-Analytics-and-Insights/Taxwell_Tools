function  meanValue  = weightedMean(values, weights)
%weightedMean(values, weights) This function takes the weights and values as
% inputs and determines the weighted means.
%   The size of the values and weights variables must be the same.
%   meanValue = sum(values.*weights)./sum(weights);

if not(all(size(values)==size(weights)))
    error('Size of values and weights variables are not equal')
end

if not(isvector(values))
   error('values is not a vector.  values must be 1 by n or n by 1.') 
end

if not(isvector(weights))
   error('weights not a vector.  weights must be 1 by n or n by 1, spelled variables not presently supported.') 
end

meanValue = sum(values.*weights)/sum(weights);
end

