% Setting the working directory and add path:
% The matlab codes need to be run from the same folder that contains the 
% "Calibration_files" subfolder in order to use the code below to add the
% subfolders to the path


scriptDirectory = fileparts(mfilename('fullpath'));
addpath(genpath(fullfile(scriptDirectory, 'Calibration_files')));

% save current directoy into var for alter use
whichfolder=cd()

%load raw data for calibration from mat file
load('Calibration_files/Calibration_3x1.mat')
set(0,'defaultAxesFontName', 'Arial');
set(0,'defaultTextFontName', 'Arial');
%change directory to the subfolder containing the pixelselection dataset
cd("Calibration_files/pixelselection_dataset")
format long;

%%
%gather all filenames for importing the SELECTION dataset
SELECT_files = dir('*.csv');

%Import Temperature array
SelTemp = [];

opts = delimitedTextImportOptions("NumVariables", 3);

% Specify range and delimiter
opts.DataLines = [4, 4];
opts.Delimiter = ",";

opts.VariableNames = ["Var1", "Temp", "Var3"];
opts.SelectedVariableNames = "Temp";
opts.VariableTypes = ["string", "double", "string"];

% Specify file level properties
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";

% Specify variable properties
opts = setvaropts(opts, ["Var1", "Var3"], "WhitespaceRule", "preserve");
opts = setvaropts(opts, ["Var1", "Var3"], "EmptyFieldRule", "auto");


for i = 1:length(SELECT_files);
    
  SelTemp(i,1) = table2array(readtable(SELECT_files(i).name, opts));
end

%%
%Import Integrationtime array
%a shortcut would be creating a 1x67 array just containing value 5000

%preallocate var
SelIT = [];

opts = delimitedTextImportOptions("NumVariables", 3);

% Specify range and delimiter
opts.DataLines = [3, 3];
opts.Delimiter = ",";

opts.VariableNames = ["Var1", "IT", "Var3"];
opts.SelectedVariableNames = "IT";
opts.VariableTypes = ["string", "double", "string"];

% Specify file level properties
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";

% Specify variable properties
opts = setvaropts(opts, ["Var1", "Var3"], "WhitespaceRule", "preserve");
opts = setvaropts(opts, ["Var1", "Var3"], "EmptyFieldRule", "auto");

%import the Integration time data iterating all 67 files 
for i = 1:length(SELECT_files)
    
  SelIT(i,1) = table2array(readtable(SELECT_files(i).name, opts));
end

%convert Integrationtime given in microseconds to ms 
SelIT=SelIT/1000;

%%
%preallocate var
SelCounts= [];
% Intensity Counts Import
opts = delimitedTextImportOptions("NumVariables", 3);

% Specify range and delimiter
opts.DataLines = [6, Inf];
opts.Delimiter = ",";

% Specify column names and types
opts.VariableNames = ["Var1", "Var2", "Intensitycounts"];
opts.SelectedVariableNames = "Intensitycounts";
opts.VariableTypes = ["string", "string", "double"];

% Specify file level properties
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";

% Specify variable properties
opts = setvaropts(opts, ["Var1", "Var2"], "WhitespaceRule", "preserve");
opts = setvaropts(opts, ["Var1", "Var2"], "EmptyFieldRule", "auto");

% Import the data

for i = 1:length(SELECT_files);
    
  SelCounts(1:1024,i) = table2array(readtable(SELECT_files(i).name, opts));
end;

%%
%preallocate var
DarkNoise=[];

%compute Darknoise data predicted from the 3x1 model
%using double for loop to iterate across all pixels and 
% in each pixel iterate across all 67 samples of the SELECTION dataset

for i= 1:length(SelCounts);
    for f=1:length(SelIT);
DarkNoise(i,f)=fitresults31{i,1}(SelTemp(f),SelIT(f));
    end
end

%%
%preallocate var for the RMSE values of the Select dataset
SELECT_RMSE=[];

%compute the Squared errors (Y-Y')²
SQe=(SelCounts-DarkNoise).^2;

%compute RMSE values for each wavelength pixel.
for i=1:length(SQe);
   SELECT_RMSE(i,1)=sqrt(sum(SQe(i,:)/size(SQe,2)));
end

%set Pixels out of visible range ="NA"
SELECT_RMSE(1:92,1)="NA";
SELECT_RMSE(940:1024,1)="NA";
%%
% extract the RMSE from the 3x1 fit as matrix
FIT_RMSE = zeros(1024, 1);

% Loop through each row and extract the rmse value from the second column
for i = 1:1024;
    % Assuming 'rmse' is a property of the 'sfit' object
    FIT_RMSE(i) = fitresults31{i, 2}.rmse;
end
%%
%extract the coefficients from the 3x1 fit and merge them into a table


% Initialize a cell array to store the fit coefficients
FIT_COEFFS = cell(1024, 1);

% Loop through each row and extract the fit coefficients from the first column
for i = 1:1024;
    % Assuming the fit coefficients are stored in the first column of the cell array
    FIT_COEFFS{i} = coeffvalues(fitresults31{i, 1});
end

%helper var with number of rows
numRows = size(FIT_COEFFS, 1);

colnames={'wvlngth','FIT_RMSE', 'SELECT_RMSE'}

%merge the relevant fitting data into 1 table (missing the coeffs)
 Calibration=table(wvlngth',FIT_RMSE, SELECT_RMSE,'VariableNames', ...
    colnames);

%then add the names for coeff vars
  coef_names={'p00', 'p10', 'p01', 'p20', 'p11', 'p30', 'p21'}
% Initialize new variables with zeros
Calibration.p00 = zeros(numRows, 1);
Calibration.p10 = zeros(numRows, 1);
Calibration.p01 = zeros(numRows, 1);
Calibration.p20 = zeros(numRows, 1);
Calibration.p11 = zeros(numRows, 1);
Calibration.p30 = zeros(numRows, 1);
Calibration.p21 = zeros(numRows, 1);


% Assign the values from coefficients_cell to the corresponding columns
for i = 1:7
    Calibration.(coef_names{i}) = cellfun(@(x) x(i), FIT_COEFFS, 'UniformOutput', false);
end

  
%%
%Excluded pixel Wavelengths [nm]
%training dataset: 397.197, 436.928, 444.347, 521.350, 547.219, 576.028, 577.922,
%624.000, 643.092, 736.930, 741.776, 754.877). 
%validation dataset: 576.028000(excluded in step before), 587.871000, 452.239000, 477.371000)

%save all relevant calibration data to a file
cd(whichfolder)
save('Calibration_files/Calibration.mat',"Calibration")

