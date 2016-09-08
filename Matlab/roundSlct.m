function roundedValues  = roundSlct( values, roundPlace )
% roundedValues  = roundSlct( values, roundPlace )
%   Rounds value to the specified place, so to round to the thousands 
%   place roundSlct( 122111, 1000 ) = 122000 to round the the tenths place
%   roundSlct( 10.151, .1 )= 10.2
%
%   roundedValues = round(values/roundPlace)*roundPlace;

roundedValues = round(values/roundPlace)*roundPlace;

end

