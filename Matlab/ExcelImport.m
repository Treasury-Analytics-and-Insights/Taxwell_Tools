function outputFilename = ExcelImport(inputFilename, varargin)
% ExcelImport(inputFilename (optional), ExcelSheetName(optional) )
% This function takes the name of an excel file (xls, xlsx, csv) and sheet
% name and imports the sheet.  All columns must be named.  The excel sheet
% name is optional and Run1Data, Run2Data, Run1Output, or Run2Output will 
% be used as the default sheet if not specified.
% 
% If no filename is specified, i.e. ExcelImport or ExcelImport() then the
% user selects the file from a GUI and default sheet names are searched
% for.
%
% If the excel sheet is specified, then the given name is used, otherwise 
% it uses a default name. The order that default names are looked for are 
% Run1Data, Run2Data, Run1Output, or Run2Output
%
% Last updated 2015-09-18 by Brett Stawinski  QA Status: Unchecked
%
% Modification: 2015-09-18 Brett Stawinski
% Description: 
% - Moved Error check to after nargin switch section.  Was
%   needed to prevent error when no inputs given.
% - Fixed error where first column of spelled data was wrong

%% Based on how many inputs were given determine Filename and ExcelSheetName
switch nargin
% No file name or sheet name specified
    case 0
        [inputFilename,PathName] = uigetfile('*.*', 'Please select a .xls, .xlsx, or .csv file to import');
        [~,sheets] = xlsfinfo([PathName inputFilename]); 
        
        % Check to see if default named sheet exists.        
        ExcelSheetName = determineSheetName(sheets);
        disp(['No sheet specified so ' ExcelSheetName ' used as default sheet.'])
               
% inputFilename given but no sheet name specified
    case 1
        [~,sheets] = xlsfinfo(inputFilename); 
        
        % Check to see if default named sheet exists.        
        ExcelSheetName = determineSheetName(sheets);
        disp(['No sheet specified so ' ExcelSheetName ' used as default sheet.'])
        
% Sheet name specified        
    case 2
        ExcelSheetName = varargin{1};
    otherwise
        error('Unexpected number of inputs, only inputFilename and optionally sheet name are allowed, 1 input minumum, 2 inputs maximum.')
end

%% Error check on extension
if not(strcmpi(inputFilename(end-3:end),'.csv') || strcmpi(inputFilename(end-3:end),'.xls') || strcmpi(inputFilename(end-4:end),'.xlsx')) 
    error('File does not have csv, xls, or xlsx extension')
end

%% Import File Into MATLAB
if exist('PathName', 'var')
    [Z_Data,Z_Names,Z_RawSheet] =  xlsread([PathName inputFilename], ExcelSheetName);
else
    [Z_Data,Z_Names,Z_RawSheet] =  xlsread(inputFilename, ExcelSheetName);
end

%% Create variables from imported data.
[rowSzNames,colSzNames] = size(Z_Names);

% Check to see if there is a column of non numbers. Matlab will
% import the rows so if there are more than one row in the Z_Names variable
% than there is non numeric data.
if rowSzNames > 1
    numericCols = nan(1,colSzNames);
% Determine which columns are numeric.
    for i = 1:colSzNames
        if isnumeric(Z_RawSheet{2,i})
            numericCols(i) = 1;
        else
            numericCols(i) = 0;
        end
    end
else
% If all are numeric just set true.
    numericCols(1,1:colSzNames)= true;
end

% Need for searching for spelled variales
SpelledTextArray = {'_01' '_02' '_03' '_04' '_05' '_06' '_07' '_08' '_09'...
    '_10' '_11' '_12','_13' '_14' '_15' '_16' '_17' '_18' '_19' '_20'...
    '_21' '_22' '_23' '_24'};

i=1;
while i <=colSzNames
%     If a column is un-named present an error
    if  strcmpi( Z_Names{1,i},'')
        if not(any(isnan(Z_Data(2:end,i))))
            error('Un-named column with data!')
        end
    else
        % Remove spaces and replace backslashes with underscores to make
        % variable names MATLAB friendly.
        Z_Names{1,i} = strrep(Z_Names{1,i},'/','_');
        Z_Names{1,i} = strrep(Z_Names{1,i},' ','');
        
        % If a column is numeric create a array variable.
        if numericCols(i) == 1
        
            % Look to determine if this is a spelled variable by checking
            % suffix looking for _## format. >4 is to check there are more
            % characters than the 3 required for the suffix            
            if length(Z_Names{1,i})>4 && strcmpi(Z_Names{1,i}(end-2:end),'_01')
                VarNameTmp = strrep(Z_Names{1,i},'_01','_Spelled');
                eval([VarNameTmp ' = nan(length(Z_Data(:,i)),1);']);  % Pre-allocate array with nans
                
                eval([VarNameTmp '(:,1) = Z_Data(:,i);']);
                for k=2:24
                    i = i+1;
                   if not(strcmpi(Z_Names{1,i}(end-2:end),SpelledTextArray{k}))
                       error('Spelled variable does not contain 24 consectutive columns in Excel/csv')
                   end
                   eval([VarNameTmp '(:,k) = Z_Data(:,i);']); 
                end

            % Non spelled numbers
            else
                eval([Z_Names{1,i}, ' = Z_Data(:,i);']);
%                 eval([Z_Names{1,i}, ' = Z_RawSheet(2:end,i);']);
                
            end
            
        % If a column is NOT numeric create a cell variable.
        else        
            eval([Z_Names{1,i}, ' = Z_RawSheet(2:end,i);']);
        end
    end
    i = i + 1;
end

%% Save data to .mat file


% Remove file extension
FileNameNoExtension = strrep(inputFilename,'.csv','');
FileNameNoExtension = strrep(FileNameNoExtension,'.xlsx','');
FileNameNoExtension = strrep(FileNameNoExtension,'.xls','');
        
outputFilename = [FileNameNoExtension '_' ExcelSheetName '.mat'];

% Save data excluding un-needed variables
save(outputFilename, '-regexp', '^P_|^F_|^H_');
% Kept running out of memory when running batches
clear 
end

%% Functions
function SheetName = determineSheetName(sheets)
    % This function takes the sheets variable from xlsfinfo and determine
    % what sheetname used in MATLAB import of a Taxwell output Excel or csv
    % file.
if length(sheets)==1
    SheetName = sheets{1};
elseif any(strcmp(sheets,'Run1Data'))
    SheetName = 'Run1Data';
elseif any(strcmp(sheets,'Run2Data'))
    SheetName = 'Run2Data'; 
elseif any(strcmp(sheets,'Run1Output'))
    SheetName = 'Run1Output';
elseif any(strcmp(sheets,'Run2Output'))
    SheetName = 'Run2Output';      
else
    error('More than one sheet, and missing sheet name of Run1Data, Run2Data, Run1Output, or Run2Output not found. Please specify sheet name.') 
end
end