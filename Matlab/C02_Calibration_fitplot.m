% Setting the working directory and add path:
% The matlab codes need to be run from the same folder that contains the 
% "Calibration_files" subfolder in order to use the code below to add the
% subfolders to the path

%This code requires the Curve Fitting Toolbox & Statistics and Machine Learning Toolbox

scriptDirectory = fileparts(mfilename('fullpath'));
addpath(genpath(fullfile(scriptDirectory, 'Calibration_files')));


%load raw data for calibration from mat file
load('Calibration_files/Calibration_rawdata.mat')
set(0,'defaultAxesFontName', 'Arial');
set(0,'defaultTextFontName', 'Arial');
format long;
%% Fit: 1x1 Polymodel X= Sensor Temperature, Y= Integration Time in ms , Z = Intensity Counts
fitresults11={};

for i=1:size(Counts, 2)
[xData, yData, zData] = prepareSurfaceData( Temp, IT, Counts(:,i));

% Set up fittype and options.
ft = fittype( 'poly11' );

% Fit model to data.
[fitresult, gof] = fit( [xData, yData], zData, ft);

%save the results of each wvlngth pixel into a fitresult struct.
fitresults11{i,1}=fitresult;
fitresults11{i,2}=gof;
fitresults11{i,3}=ft;
end
%clear the temporary vars
clear gof ft fitresult xData yData zData i ;
%% Fit: 2x1 Polymodel X= Sensor Temperature, Y= Integration Time in ms , Z = Intensity Counts
fitresults21={};

for i=1:size(Counts, 2)
[xData, yData, zData] = prepareSurfaceData( Temp, IT, Counts(:,i));

% Set up fittype and options.
ft = fittype( 'poly21' );

% Fit model to data.
[fitresult, gof] = fit( [xData, yData], zData, ft );

fitresults21{i,1}=fitresult;
fitresults21{i,2}=gof;
fitresults21{i,3}=ft;
end
%clear the temporary vars
clear gof ft fitresult xData yData zData i ;
%% Fit: 2x2 Polymodel X= Sensor Temperature, Y= Integration Time in ms , Z = Intensity Counts
%preallocate structure for fitting results
fitresults22={};

for i=1:size(Counts, 2)
[xData, yData, zData] = prepareSurfaceData( Temp, IT, Counts(:,i));

% Set up fittype and options.
ft = fittype( 'poly22' );

% Fit model to data.
[fitresult, gof] = fit( [xData, yData], zData, ft );

%save the data in the fitresult structure
fitresults22{i,1}=fitresult;
fitresults22{i,2}=gof;
fitresults22{i,3}=ft;
end
%clear the temporary vars
clear gof ft fitresult xData yData zData i ;
%% Fit: 3x1 Polymodel X= Sensor Temperature, Y= Integration Time in ms , Z = Intensity Counts

%preallocate structure for fitting results
fitresults31={};

%Iterate the fitting procedure across all Wavelength Pixels.
for i=1:size(Counts, 2)
[xData, yData, zData] = prepareSurfaceData( Temp, IT, Counts(:,i));

% Set up fittype and options.
ft = fittype( 'poly31' );

% Fit model to data.
[fitresult, gof] = fit( [xData, yData], zData, ft );

%save the data in the fitresult structure
fitresults31{i,1}=fitresult;
fitresults31{i,2}=gof;
fitresults31{i,3}=ft;
end
%clear the temporary vars
clear gof ft fitresult xData yData zData i ;
%% Fit: 3x2 Polymodel X= Sensor Temperature, Y= Integration Time in ms , Z = Intensity Counts
fitresults32={};

for i=1:size(Counts, 2)
[xData, yData, zData] = prepareSurfaceData( Temp, IT, Counts(:,i));

% Set up fittype and options.
ft = fittype( 'poly32' );

% Fit model to data.
[fitresult, gof] = fit( [xData, yData], zData, ft );

fitresults32{i,1}=fitresult;
fitresults32{i,2}=gof;
fitresults32{i,3}=ft;
end
%clear the temporary vars
clear gof ft fitresult xData yData zData i ;

%% Fit: 4x1 Polymodel X= Sensor Temperature, Y= Integration Time in ms , Z = Intensity Counts
fitresults41={};

for i=1:size(Counts, 2)
[xData, yData, zData] = prepareSurfaceData( Temp, IT, Counts(:,i));

% Set up fittype and options.
ft = fittype( 'poly41' );

% Fit model to data.
[fitresult, gof] = fit( [xData, yData], zData, ft );

fitresults41{i,1}=fitresult;
fitresults41{i,2}=gof;
fitresults41{i,3}=ft;
end
%clear the temporary vars
clear gof ft fitresult xData yData zData i ;
%%
%create the following items from the fitdata:

%1) table comparing models: 1x1, 2x1, 2x2, 3x1,3x2,4x1: 
%columns:Model degree, n coeff, min med max R²adj, n R²<.9, min med max RMSE 

%2) CDF Plot Model 1x1 vs 2x1 vs 3x1

%3) plot R² across wavelengths model 2x1 vs 3x1

%4) Pixel selection: RMSE plot threshold "elbow" at 3.25

%(5) dead pixel elimination: everything > than SELECT_RMSE 25 with
%"Selection" dark samples.

%%
%reshape Rsquared adjusted values of pixels in vis range into Matrix

%model 1x1
for i=1:1024
adjRsq11(i,1)=fitresults11{i,2}.adjrsquare;
end
adjRsq11=adjRsq11(93:939);

for i=1:1024
rmse11(i,1)=fitresults11{i,2}.rmse;
end
rmse11=rmse11(93:939);

%model 2x1
for i=1:1024
adjRsq21(i,1)=fitresults21{i,2}.adjrsquare;
end
adjRsq21=adjRsq21(93:939);

for i=1:1024
rmse21(i,1)=fitresults21{i,2}.rmse;
end
rmse21=rmse21(93:939);

%Model 2x2

for i=1:1024
adjRsq22(i,1)=fitresults22{i,2}.adjrsquare;
end
adjRsq22=adjRsq22(93:939);

for i=1:1024
rmse22(i,1)=fitresults22{i,2}.rmse;
end
rmse22=rmse22(93:939);

%Model 3x1

for i=1:1024
adjRsq31(i,1)=fitresults31{i,2}.adjrsquare;
end
adjRsq31=adjRsq31(93:939);

for i=1:1024
rmse31(i,1)=fitresults31{i,2}.rmse;
end
rmse31=rmse31(93:939);

%Model 4x1

for i=1:1024
adjRsq41(i,1)=fitresults41{i,2}.adjrsquare;
end
adjRsq41=adjRsq41(93:939);

for i=1:1024
rmse41(i,1)=fitresults41{i,2}.rmse;
end
rmse41=rmse41(93:939);

%Model 3x2

for i=1:1024
adjRsq32(i,1)=fitresults32{i,2}.adjrsquare;
end
adjRsq32=adjRsq32(93:939);

for i=1:1024
rmse32(i,1)=fitresults32{i,2}.rmse;
end
rmse32=rmse32(93:939);
%%
%create summary table!
%table comparing models: 1x1, 2x1, 2x2, 3x1,4x1,3x2: 
%columns:Model degree, n coeff, min med max R²adj, n R²<.9, min med max RMSE 

% compute descriptive stats of the different polymodels
fitnames=["1x1";"2x1";"2x2";"3x1";"4x1";"3x2"];
n_coeff=[3;5;6;7;9;9];
rmse_min=[min(rmse11);min(rmse21);min(rmse22);min(rmse31);min(rmse41); min(rmse32)];
rmse_med=[median(rmse11);median(rmse21);median(rmse22);median(rmse31);median(rmse41);median(rmse32) ];
rmse_max=[max(rmse11);max(rmse21);max(rmse22);max(rmse31);max(rmse41);max(rmse32)];
adjrsquare_min=[min(adjRsq11);min(adjRsq21);min(adjRsq22);min(adjRsq31);min(adjRsq41);min(adjRsq32) ];
adjrsquare_med=[median(adjRsq11);median(adjRsq21);median(adjRsq22);median(adjRsq31);median(adjRsq41);median(adjRsq32)];
adjrsquare_max=[max(adjRsq11);max(adjRsq21);max(adjRsq22);max(adjRsq31);max(adjRsq41);median(adjRsq32)];
n_adjrsquare9=[sum(adjRsq11 < .9);sum(adjRsq21 < .9); sum(adjRsq22 < .9); sum(adjRsq31 < .9); sum(adjRsq41 < .9); sum(adjRsq32 < .9)];

columnNames={'Model degree','Number of Coefficients', 'Min R²adj',...
    'Med R²adj', 'Max R²adj','n pixels, R²adj <0.9'}

%merge the computed descritive stats into 1 summary table
summary_fit=table(fitnames,n_coeff,rmse_min, rmse_med, rmse_max, adjrsquare_min, adjrsquare_med,...
    adjrsquare_max, n_adjrsquare9)

%merge the a reduced version 1 table for publication
sum_fit_pub=table(fitnames, n_coeff,adjrsquare_min, adjrsquare_med,...
    adjrsquare_max, n_adjrsquare9,'VariableNames', columnNames)

% Create a cell array with variable names to round
variablesToRound = {'Min R²adj',  'Med R²adj', 'Max R²adj','n pixels, R²adj <0.9'};  % Add the names of the variables you want to round

% Round selected variables to the third number after the decimal point
roundedTable = array2table(round(sum_fit_pub{:, variablesToRound}, 3), 'VariableNames', variablesToRound);
disp('Rounded Table:');
disp(roundedTable);

% Add the non-rounded tables back to the mix
nonRoundedVariables = setdiff(sum_fit_pub.Properties.VariableNames, variablesToRound);
finalTable = [sum_fit_pub(:, nonRoundedVariables), roundedTable];
disp('Final Table with both rounded and non-rounded variables:');
disp(finalTable);

% % Create a figure with a UI table
% fig = uifigure('Name', 'My Table Figure', 'Position', [100, 100, 400, 300]);
% uitable('Parent', fig, 'Data', sum_fit_pub{:,:}, 'ColumnName', sum_fit_pub.Properties.VariableNames, ...
%     'Position', [10, 10, 380, 280]);
% 
% close all

%Modify the table for publication in R after exporting as .csv

%%
%Cumulative distributution Function (CDF) plot MODEL SELECTION poly 1x1 vs. 2x1 vs 3x1
%using R²adjusted values to show progress across models
cdf1=cdfplot(adjRsq11);
    cdf1.Color="black";
    cdf1.LineStyle="--" ;
    cdf1.LineWidth=1.5;
    hold on;
cdf2=cdfplot(adjRsq21);
cdf2.Color=[0.667, 0.475, 0.224];% same as in example spectra
    cdf2.LineStyle="--"; 
    cdf2.LineWidth=1.5;
    hold on;
cdf3=cdfplot(adjRsq31);
    cdf3.Color=[0.439, 0.557, 0.643]; % same as in example spectra
    cdf3.LineStyle="-" ;
    cdf3.LineWidth=1.5;
    hold on;
xlim([0.75,1.01]);
yticks([0:0.2:1])
xticks([0.8:0.05:1])
grid, box off;
title('');
xlabel('');
ylabel('');

% Set the paper size to match the figure size
set(gcf, 'PaperUnits', 'inches');
set(gcf, 'PaperSize', [6, 4]); % Adjust the paper size as needed

% Print the figure to a PDF file with vector format
print('Calibration_files/output/Polymodels_CDF.pdf', '-bestfit', '-dpdf', '-painters', '-r300'); % '-painters' ensures vector output

% Close the figure if needed
close(gcf);

%%
%plot R²adj across wavelengths model 2x1 vs 3x1 to show differences in
%models

p1=plot(vis_wvlngth, adjRsq31);
    p1.Color=[0.439, 0.557, 0.643];
    p1.LineStyle="-" ;
    p1.LineWidth=1.5;
hold on
p2=plot(vis_wvlngth, adjRsq21);
    p2.Color=[0.667, 0.475, 0.224];
    p2.LineStyle=":" ;
    p2.LineWidth=0.5;
    hold on
p1a=plot(vis_wvlngth(adjRsq31<.9), adjRsq31(adjRsq31<.9), "o");
p1a.Color=[0.439, 0.557, 0.643]
hold on
p2a=plot(vis_wvlngth(adjRsq21<.9), adjRsq21(adjRsq21<.9), "x");
p2a.Color=[0.667, 0.475, 0.224];
ylim([0.65,1]);
xlim([375, 780]);
xticks(380:100:780);
yticks(0.7:0.1:1);
title('');
xlabel('');
ylabel('');
box, hold off;

% Set the paper size to match the figure size
set(gcf, 'PaperUnits', 'inches');
set(gcf, 'PaperSize', [6, 4]); % Adjust the paper size as needed

% Print the figure to a PDF file with vector format
print('Calibration_files/output/Polymodels_wvlngth.pdf', '-bestfit', '-dpdf', '-painters', '-r300'); % '-painters' ensures vector output

% Close the figure if needed
close(gcf);

%%
%4) Pixel exlusion in TRAINING dataset: plot the RMSE value threshold
%vs number of pixels to exclude - show "elbow" at 3.25

thresholdLevels = 10:-0.25:3;
for ii = 1:length(thresholdLevels)
  countBelowThreshold(ii) = sum(rmse31> thresholdLevels(ii));
end


p3=plot(thresholdLevels,countBelowThreshold);
p3.LineStyle="-" ;
p3.LineWidth=1.5;
p3.Color=[0.439, 0.557, 0.643];
hold on
p3a=plot(thresholdLevels(28), countBelowThreshold(28), "or");
p3a.Color="red";
p3a.LineWidth=1.5;
xlim([3,5.4])
xticks(3:0.5:7)
set(gca, 'XDir','reverse')

box off

%Set the paper size to match the figure size
set(gcf, 'PaperUnits', 'inches');
set(gcf, 'PaperSize', [6, 4]); % Adjust the paper size as needed

%Print the figure to a PDF file with vector format
print('Calibration_files/output/PolyPixel_RMSEthreshold.pdf', '-bestfit', '-dpdf', '-painters', '-r300'); % '-painters' ensures vector output

%Close the figure if needed
close(gcf);

%%
%Variation plot Counts vs Temperature in all pixels
subplot(1,2,1);
plot(Temp(1441:1800,:),Counts(1441:1800,:));
xlim([20, 34]);
ylim([1450, 1600]);
box off;
subplot(1,2,2);
plot(Temp(1081:1440,:),Counts(1081:1440,:));
xlim([20, 34]);
ylim([1450, 1600]);
box off;

% Set the paper size to match the figure size
set(gcf, 'PaperUnits', 'inches');
set(gcf, 'PaperSize', [6, 4]); % Adjust the paper size as needed

% Print the figure to a PDF file with vector format
print('Calibration_files/output/Variation1000msvs100ms.pdf', '-bestfit', '-dpdf', '-painters', '-r300'); % '-painters' ensures vector output

% Close the figure if needed
close(gcf);


%%
%save 3x1 calibration data
save('Calibration_files/Calibration_3x1.mat',"fitresults31",'wvlngth','vis_wvlngth');

%save summary table
writetable(summary_fit,'Calibration_files/summary_fit_table.csv');

%save table for calibration model selection
writetable(finalTable,'Calibration_files/Cali_model_tab.csv');
