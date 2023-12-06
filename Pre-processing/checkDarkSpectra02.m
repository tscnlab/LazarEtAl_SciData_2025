 whichFolder = {'/home/spitschan/Desktop/SpectroSens_data/dark01/spectra','/home/spitschan/Desktop/SpectroSens_data/dark02/spectra','/home/spitschan/Desktop/SpectroSens_data/dark03/spectra' }
 
C = {'k','b','r'} % Cell array of colours.

 
 
  for ii=1:length(whichFolder)
      cd(whichFolder{ii})
      
    allFiles = dir('*.csv');
for ff = 1:length(allFiles)
    tmp =  csvread(allFiles(ff).name, 5, 0);
    T_model(:, ff) = tmp(:, 2);
    T_empirical(:, ff) = tmp(:, 3);
    
    delimiter = ',';
    startRow = 4;
    endRow = 4;
    
    %% Format for each line of text:
    %   column2: double (%f)
    % For more information, see the TEXTSCAN documentation.
    formatSpec = '%*s%f%*s%[^\n\r]';
    
    %% Open the text file.
    fileID = fopen(allFiles(ff).name,'r');
    
    %% Read columns of data according to the format.
    % This call is based on the structure of the file used to generate this
    % code. If an error occurs for a different file, try regenerating the code
    % from the Import Tool.
    dataArray = textscan(fileID, formatSpec, endRow-startRow+1, 'Delimiter', delimiter, 'TextType', 'string', 'HeaderLines', startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
    
    %% Close the text file.
    fclose(fileID);
    
    %% Post processing for unimportable data.
    % No unimportable data rules were applied during the import, so no post
    % processing code is included. To generate code which works for
    % unimportable data, select unimportable cells in a file and regenerate the
    % script.
    
    %% Create output variable
    Untitled = table(dataArray{1:end-1}, 'VariableNames', {'Darkcounts'});
    
    %% Clear temporary variables
    clearvars filename delimiter startRow endRow formatSpec fileID dataArray ans;
    
    temperature(ff) = Untitled.Variables;
end

plot(temperature, sum(T_model),'color',C{ii}, 'marker','o','LineStyle','none'); hold on
plot(temperature, sum(T_empirical),'color',C{ii}, 'marker','x','LineStyle','none'); hold on
xlabel('Temperature [deg C]');
ylabel('Total sum');
legend('Tmodel1 [temp vs darkcounts]','Tempirical1 [temp vs dark measurements]','Tmodel2 [temp vs darkcounts]','Tempirical2 [temp vs dark measurements]','Tmodel3 [temp vs darkcounts]','Tempirical3 [temp vs dark measurements]'); hold on

%clearvars -except ii ff whichFolder C;


  end