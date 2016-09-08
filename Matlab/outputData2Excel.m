function nextBlankCell = outputData2Excel(data, outputFilename, sheetName, startCell, varargin)
%outputData2Excel This function prints data with specified titles to Excel,
%and outputs the next blank cell by row.
%   nextBlankCell = outputData2Excel(data, outputFilename, sheetName, startCell, dataNames(optional), sectionNames(optional))
% data is the data values
% data names is a cell array of strings of the titles of the columns
% outputFilename is a string of the name of the file to be saved to
% sheetName is a string of the name of the Excel sheet to be saved too
% startCell is a string of the first cell to write to, e.g. ''A1''
% dataNames is a cell array of strings that label the columns of data
% sectionNames is string of the name of the section of data this is
% optional, e.g.    Section Name
%                   Title 1      Title 2
%                     1             2

    % Parse start cell into letter and number component
    if isnumeric(str2double(startCell(2)))
        % One letter followed by number(s), e.g. A12
        cellLetter = startCell(1);
        cellNumber = str2double(startCell(2:end));
    elseif isnumeric(str2double(startCell(3)))
        % Two letters followed by number(s), e.g. AB12
        cellLetter = startCell(1:2);
        cellNumber = str2double(startCell(3:end));
    else
        error('More than two letters in startCell, e.g. AAA12, one 1 to 2 letters valid')
    end
    
%     Only column titles specified
    if nargin == 5
        dataNames = varargin{1};
    elseif nargin== 6

        dataNames = varargin{1};
        sectionNames = varargin{2};
        xlswrite(outputFilename,{sectionNames}, sheetName,[cellLetter num2str(cellNumber)]) 
        [row,~] = size(sectionNames);
        cellNumber =  cellNumber + row;
    elseif nargin<4 || nargin>6
        error('Incorrect number of inputs 7 or 8 inputs only')
    end

    if nargin > 4
        xlswrite(outputFilename,dataNames, sheetName,[cellLetter num2str(cellNumber)])
        [row,~] = size(dataNames);
        cellNumber =  cellNumber + row;
    end


    xlswrite(outputFilename,data, sheetName,[cellLetter num2str(cellNumber)])
    % The next blank cell (by row, not column)
    [row,~] = size(data);
    cellNumber =  cellNumber + row;
    nextBlankCell = [cellLetter num2str(cellNumber)];
end