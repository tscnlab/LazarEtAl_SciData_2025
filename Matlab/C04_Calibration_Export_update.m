% Setting the working directory and add path:
% The matlab codes need to be run from the same folder that contains the 
% "Calibration_files" subfolder in order to use the code below to add the
% subfolders to the path


scriptDirectory = fileparts(mfilename('fullpath'));
addpath(genpath(fullfile(scriptDirectory, 'Calibration_files')));

%load raw data for calibration from mat file
%load('Calibration_files/Calibration_3x1.mat')
load('Calibration_files/Calibration.mat')


set(0,'defaultAxesFontName', 'Arial');
set(0,'defaultTextFontName', 'Arial');
%%

% Open a text file for writing
fileID = fopen('your_file.txt', 'w');

% Check if the file is successfully opened
if fileID == -1
    error('Unable to open the file for writing');
end

% Define the multiline string using double quotes and square brackets
text = ["x=sensor temperature [°C], y= integrationtime [ms], z=counts" ...
    "NOTE: in the raw data (.csv) files integrationtime is given in microseconds instead of milliseconds" ...
    "thus the integrationtime has to be divided by 1000 before using it as 'y' with this calibration file. " ...
    "Pixel selection: pixels with FIT_RMSE > 3.25 (RMSE in training dataset) &  dead pixels with SELECT_RMSE > 25" ...
    "(RMSE in selection dataset) should be excluded from further analysis." ...
    "z(x,y)= p00 + p10*x + p01*y + p20*x^2 + p11*x*y + p30*x^3 + p21*x^2*y"];

% Write each line with a newline character
for i = 1:numel(text)
    fprintf(fileID, '%s\n', text(i));
end


writetable(Calibration, 'your_file.txt', 'Delimiter', '\t', 'WriteMode', ...
    'append', 'WriteVariableNames', true)

% Close the file
fclose(fileID);

% How can I have the coefficient numbers in scientific notation instead?


