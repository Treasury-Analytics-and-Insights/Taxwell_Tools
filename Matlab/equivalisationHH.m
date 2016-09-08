function eqFactor = equivalisationHH( countAdults,countKids, equivType )
%eqFactor = equivalisationHH( countAdults,countKids, equivType ) Determines
%the equivillization factor based on household makeup.  Choices are 'sqrt',
% 'jensen', 'OECD', and 'none'

if not(strcmpi(equivType,'sqrt')) && not(strcmpi(equivType,'Jensen'))...
        && not(strcmpi(equivType,'OECD')) && not(strcmpi(equivType,'none'))
    error('Equivilization type given not defined')
end

jensen_beta = 0.730348;
jensen_alpha = 0.621448;

% In Case Other Jesen data used.
% jensen_Age0To1 = .67;
% jensen_Age12To16 = 1.19;
% jensen_Age1To7 = 1.33;
% jensen_Age2To6 = .81;
% jensen_Age7To11 = 1;

OECD_adult = .5;
OECD_kids = .3;

if strcmpi(equivType,'sqrt') == 1
    eqFactor = sqrt(countAdults + countKids);
elseif strcmpi(equivType,'jensen') == 1
    eqFactor = (countAdults + jensen_beta * countKids).^jensen_alpha;
elseif strcmpi(equivType, 'OECD')
    eqFactor = 1 + (countAdults-1)*OECD_adult + countKids*OECD_kids;
elseif strcmpi(equivType,'none') == 1
    eqFactor = 1;     
end

end

