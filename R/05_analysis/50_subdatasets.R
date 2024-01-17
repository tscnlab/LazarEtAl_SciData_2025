###preparing environment: unloaded prior packages, loading new ones----

#unload packages that were loaded before (run function twice to "catch" all pkgs)
#this is a workaround to avoid masking problems when running the scripts successively
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

# Libraries
library(tidyverse)
library(gtable)
library(gtExtras)

set.seed(20230703) 


### [1] Load merged data ------------------------------------------------------
#loading merged data with included participants 
load(file="./R/03_datamerge/merged_data_conf.rda")
load(file="./R/03_datamerge/mergeddata_all.rda")

### [2] General Subdatasets ------------------------------------------------------------
#create subdatasets for calculations, hypothesis testing and plotting

#remove data with missing pupil and save into cf_data sub dataset
cf_data <- merged_data_conf[!is.na(merged_data_conf$diameter_3d),] 


#create subdatasets of the cfdata set

#field data, which includes only complete observations (light intensity & pupil size)

Fielddata <- cf_data[!is.na(cf_data$exp_phase)  & cf_data$exp_phase == "Field" &
                       !is.na(cf_data$Mel_EDI),]

#number of observations of field data
obs_Fielddata <- nrow(Fielddata)


#Dark data, which includes only complete valid pupil size data (like cfdata)
#light data is not used in the "dark adaptation", 
#since spectroradiometric measures usually contain some noise in very dim conditions
Darkdata <- cf_data[!is.na(cf_data$exp_phase)  & cf_data$exp_phase == "Dark",]

#number of observations of field data
obs_Darkdata <- nrow(Darkdata)


#Labdata,  which includes only complete observations (light intensity & pupil size)
Labdata <- cf_data[!is.na(cf_data$exp_phase) & cf_data$exp_phase == "Lab" &
                     !is.na(cf_data$Mel_EDI),
]

#number of observations of field data
obs_Labdata <- nrow(Labdata)


#data in the transitional phase between lab and field conditions that was not tagged
#and does not belong to either condition
#includes only complete observations (light intensity & pupil size)
Transitiondata <- cf_data[is.na(cf_data$exp_phase)&
                            !is.na(cf_data$Mel_EDI),]

#number of observations of field data
obs_Transitiondata <- nrow(Transitiondata)

# adding up the different subdatasets to all data observations
obs_allincl <- obs_Transitiondata+obs_Labdata+obs_Darkdata+obs_Fielddata


# field  & dark data combined (used for plotting only)
Darkfield <- cf_data [!is.na(cf_data$exp_phase)  & ((cf_data$exp_phase == "Field" |
                       cf_data$exp_phase == "Dark")),]

# field  & dark data combined (used for "postive control" tables only)
Darklab <-  cf_data [!is.na(cf_data$exp_phase)  & ((cf_data$exp_phase == "Lab" |
                                                  cf_data$exp_phase == "Dark")),]

### [4] Subdataset for weather data -------------------------------------------


#creating subdata set with weather data 
# The dataset contains more observations, as missing pupil data are still included)

weatherdata <- merged_data_conf[merged_data_conf$exp_phase == "Field" &
                                  !is.na(merged_data_conf$exp_phase) &
                                  !is.na(merged_data_conf$phot_lux)
                                  ,] %>%
  select(id, sample_nr, date, begin, sample_nr, 
         phot_lux, MelIrrad, Mel_EDI,
         weather, exp_phase, season)

### [5] Subdatasets for case data -------------------------------------------

#create "agecomp data" for Figure 6 (plotting only)
# comparing 3 typical subjects at 20 and 80 and 51 years old

agecomp <- cf_data[!cf_data$exp_phase == "Lab" & !is.na(cf_data$exp_phase) & 
                     (cf_data$id == "SP064" | cf_data$id == "SP048" |
                       cf_data$id == "SP059"),]

#set the dark phase light data to 0 for the data in Fig. 6,
#because they cannot be plotted when m-EDI is set to "NA".

for (i in 1: nrow(agecomp)){
  if ((agecomp$exp_phase[i] == "Dark")
  )
  { agecomp$`CIE 1931 x`[i] <- 0
  agecomp$`CIE 1931 y`[i] <- 0
  agecomp$phot_lux[i] <- 0
  agecomp$SConeIrrad[i] <- 0
  agecomp$MConeIrrad[i] <- 0
  agecomp$LConeIrrad[i] <- 0
  agecomp$RodIrrad[i] <- 0
  agecomp$MelIrrad[i] <- 0
  agecomp$Mel_EDI[i] <- 0
  
  }
}


### [6] Subdatasets for autocorrelation  ---------------------------------------
#autocorrelation approach (create subdata set) for Suppl. Fig. 4

#create a subdataset with all NAs still included (no missing rows) 
# but with all Field data
#then make sure it is ordered by id and sample_nr

autocor_data <- merged_data_conf[merged_data_conf$exp_phase == "Field" &
                                   !is.na (merged_data_conf$exp_phase) ,] %>% 
  arrange(id, sample_nr) #%>% group_by(id)

#create a "filler" variable that fills a number of NA rows between ids
#the number of rows in the filler corresponds to the autocor lag +1
# This way, the acf algorithm does NOT falsely take into account the
#autocorrelation of samples from DIFFERENT ids

#if we want to adjust the max lag in the autocorrelation to more than 18 samples (3 min)
# we also need to adjust the number of rows in the NA filler var

filler = data.frame(matrix( nrow = 19, ncol=length(autocor_data)))

#the id length var is used for the for loop
# because we will fill NA values after every id in the included dataset (n=83)
# we need to subtract idlegnth -1 because after the last id we do NOT need to
# add any NA values 

idlength<- length(unique(autocor_data$id))

#the for loop includes the "autocor_data" + adding the length of the added
#NA values which corresponds to (n-1)*(max.lag+1)
# in this case this is nrow(autocor_data) + (83-1) *(18+1)
# the for loop goes through the full dataframe row by row and checks whether the 
# sample_nr is ascending by 1. If this is NOT the case (and it's not a missing value)
# this is due to the data of the next id is starting
# if this is the case (= if condition) the loop adds 19 NA values (= "filler")
#between where the 2 ids "meet" (i:i+18). This is done via the "insertRows" function
#of the "berryFunctions" package

# for (i in 2:(nrow(autocor_data)+((idlength-1)*nrow(filler))))
# {if (autocor_data$sample_nr[i] != (autocor_data$sample_nr[i-1]+1) & 
#      !is.na(autocor_data$sample_nr[i-1]) & 
#      !is.na(autocor_data$sample_nr[i])
# )
# {autocor_data <- berryFunctions::insertRows(autocor_data, r = (i:(i+18)), new = filler 
# )}
# }

#after the filler NAs are added to the autocor_data, 

#computing the autocorrelation for the Melanopic EDI variable in the field data

# mel_acf <- data.frame(cor = acf(autocor_data$Mel_EDI,
#                                 lag.max = 18, na.action = na.pass, plot = F)$acf,
#                       lag = acf(autocor_data$Mel_EDI,
#                                 lag.max = 18, na.action = na.pass, plot = F)$lag)
# 
# #computing the autocorrelation for the Melanopic EDI variable in the field data
# pupil_acf <- data.frame(cor = acf(autocor_data$diameter_3d,
#                                   lag.max = 18, na.action = na.pass, plot = F)$acf,
#                         lag = acf(autocor_data$diameter_3d,
#                                   lag.max = 18, na.action = na.pass, plot = F)$lag)

#the autocorrelation slightly increases compared to 
#NOT using the "filler" work-around autocorellation data approach
#This shows that the work-around was succesful:
# the autocorrelation now does not take into account the samples of a the previous subject


#create subdataset for Sci Dat. table generation

#by mistake there is a missing row in SP093 (Dark). The observation was not valid though.
#correct number of rows and high quality data
nrow(merged_data_all)

plus <- merged_data_all %>%
add_row(sample_nr = 20, `Original file`="20211116011218.jpg", diameter_3d=NA,
        confidence=0, id="SP093", excl="results", excl_time="Included",
        data_loss=0.4504021, log_MelIrrad=NA, log_phot_lux=NA, Mel_EDI=NA, 
        log_Mel_EDI=NA, exp_phase="Dark",
        .before = 27533)

plus <- plus %>% mutate(
  sample_nr = 
  case_when(id=="SP093" & sample_nr>20 ~ sample_nr+1,
  .default=sample_nr)
)

# #checking wether sample nr assignement was correct.
# a <- plus[plus$sample_nr != plus$sample_nr2 & !is.na(plus$sample_nr) & !is.na(plus$sample_nr2),]
# 


nrow(plus)

plus$data_loss[plus$id=="SP093"]<-168/374

plus <- plus[which(plus$excl=="results"),]


plus %>% 
  group_by(id)%>%
 summarise(`ID`=sample(`id`,1),
           `Age [years]`=mean(age,na.rm=T),
           `Sex`=sample(`sex`,1),
           `Date`=sample(`date`,1),
           `Weather`=sample(`weather`,1),
           `n total observations` = sum(`excl`== "results"), #max(`sample_nr`)+1, #sample_nr starts with 0
           `n dark observ.` = sum(`exp_phase`== "Dark", na.rm = T),
           `n laboratory observ.` = sum(`exp_phase`== "Lab", na.rm = T),#
           `n field observ.` = sum(`exp_phase`== "Field", na.rm = T),
           `n mixed observ.` = sum(is.na(`exp_phase`)),
           #`sum_all` = (lab_data + field_data + dark_data + transi_data),
           `High-quality data pairs [%]` = (round(mean(1-(`data_loss`)),2))*100,
           `Lab. light sequence`=sample(`Lablight_seq`,1)

         #.keep="none"
            ) %>% select(-`id`) -> sum_scidat 

Sum_tab <- sum_scidat %>% 
  select(`ID`, `Sex`,#`Date`,
         `Age [years]`, `n total observations`, `n dark observ.`,
         `n laboratory observ.`, `n field observ.`, 
         `n mixed observ.`,`High-quality data pairs [%]`, `Lab. light sequence`
         )

#create table spanners two differentiate between field and positive control data
Sum_tab <- Sum_tab %>% gt() %>% 
  cols_align("center")
  opt_table_font(data= Sum_tab, font = "Arial")  %>%
  #adjust the fontsize of the cell bodies and footnotes 
  tab_style(style=cell_text(size=px(7)), locations=list(cells_body(),
                                                         cells_footnotes()
  )
  ) %>%
  #adjust the cell title font sizes and padding
  tab_style(style=cell_text(size=px(9), weight="bold"),
            locations=list(cells_column_labels())
  ) %>% tab_options(data_row.padding = px(4)) -> Sum_tab


 # change "dark", "laboratory", "field" and "mixed" to italic? 
  
#   
# gtsave(Sum_tab,        # save table as pdf
#        filename = "R/06_output/Sum_tab.pdf")





#create supplementary table with data on the experimental phases


  Sample_phase <- plus %>%
    group_by(id) %>%
    summarise(first_dark = (min(sample_nr[exp_phase == "Dark" & !is.na(exp_phase)]))+1,
              last_dark = (max(sample_nr[exp_phase == "Dark"& !is.na(exp_phase)]))+1,
              first_lab = (min(sample_nr[exp_phase == "Lab" & !is.na(exp_phase)]))+1,
              last_lab = (max(sample_nr[exp_phase == "Lab"& !is.na(exp_phase)]))+1,
              first_field = (min(sample_nr[exp_phase == "Field" & !is.na(exp_phase)]))+1,
              last_field = (max(sample_nr[exp_phase == "Field"& !is.na(exp_phase)]))+1
              )
  

#list all raw csv files 

  # Specify the path to the main folder containing subject folders
  main_folder <- "D:/SpectroSense/Data_pub/xraw_renamed2"
  
  # Get a list of subject folders
  subject_folders <- list.dirs(main_folder, recursive = FALSE)
  
  # Initialize an empty list to store CSV files for each subject
  csv_files_list <- list()
  
  # Loop through each subject folder
  for (subject_folder in subject_folders) {
    # Construct the path to the "spectra" subfolder for the current subject
    spectra_folder <- file.path(subject_folder, "spectra")
    
    # Get a list of CSV files in the "spectra" subfolder
    csv_files <- list.files(spectra_folder, pattern = "\\.csv$", full.names = TRUE)
    
    # Store the list of CSV files in the result list
    csv_files_list[[basename(subject_folder)]] <- csv_files
  }
  
  # Print the list of CSV files for each subject
  print(csv_files_list)
  

  # Convert the list of CSV files into a dataframe
  df_files <- data.frame(
    id = rep(names(csv_files_list), sapply(csv_files_list, length)),
    file_path = unlist(csv_files_list),
    stringsAsFactors = FALSE
  )
  
  # Extract only the file name from the full path
  df_files$filename <- basename(df_files$file_path)
  
  # Add a new variable with ascending file numbers
  df_files$file_number <- ave(df_files$file_path, df_files$id, FUN = seq_along)
  

  # Print the resulting dataframe
head(df_files)
  

# now extract the respective filename from the Sample_phase dataframe

  
# Initialize columns to store filenames
file_columns <- c("first_dark", "last_dark", "first_lab", "last_lab", "first_field", "last_field")


# Iterate over each row in Sample_phase
for (i in 1:nrow(Sample_phase)) {
  id <- Sample_phase$id[i]
  
  # Iterate over each file_column and extract the corresponding filename
  for (column in file_columns) {
    file_number <- Sample_phase[[column]][i]
    file_path <- df_files$file_path[df_files$id == id & df_files$file_number == file_number]
    
    # Assign the extracted filename (basename) to the corresponding column in Sample_phase
    Sample_phase[[paste0("file_name_", column)]][i] <- ifelse(length(file_path) > 0, basename(file_path), NA)
  }
}


Sample_phase






Sample_phase <- Sample_phase %>%
  select(id, 
         first_dark, file_name_first_dark, last_dark, file_name_last_dark,
         first_lab, file_name_first_lab, last_lab, file_name_last_lab,
         first_field, file_name_first_field,last_field, file_name_last_field
         )

#create new dataframe as basis for gttable
Sample_phase_tab <- Sample_phase  %>%
  rename(
    ID = id,
    `First file no.` = first_dark,
    `First file name` = file_name_first_dark,
    `Last file no.` = last_dark, 
    `Last file name` = file_name_last_dark,
    `First file no. ` = first_lab, 
    `First file name ` = file_name_first_lab, 
    `Last file no. ` = last_lab,
    `Last file name ` = file_name_last_lab,
    ` First file no.` = first_field, 
    ` First file name` = file_name_first_field, 
    ` Last file no.` = last_field,
    ` Last file name` = file_name_last_field
  )



#create table spanners two differentiate between field and positive control data
Sample_phase_tab <- Sample_phase_tab %>% gt()  %>%  cols_align("center")%>%
  tab_spanner(
    label = 'Dark observations',
    columns = c(2, 3, 4, 5)) %>%  tab_spanner(
    label = 'Laboratory observations',
    columns = c( 6, 7, 8,9)) %>%  tab_spanner(
    label = 'Field observations',
    columns = c(10, 11, 12, 13)
    )

 
#Adjust gt tab
Sample_phase_tab <- Sample_phase_tab %>% 
  cols_align("center")
opt_table_font(data= Sample_phase_tab, font = "Arial")  %>%
  #adjust the fontsize of the cell bodies and footnotes 
  tab_style(style=cell_text(size=px(7)), locations=list(cells_body(),
                                                         cells_footnotes()
  )
  ) %>%
  #adjust the cell title font sizes and padding
  tab_style(style=cell_text(size=px(9), weight="bold"),
            locations=list(cells_column_labels(), cells_column_spanners())
  ) %>% tab_options(data_row.padding = px(4)) -> Sample_phase_tab



gtsave(Sample_phase_tab,        # save table as pdf
       filename = "R/06_output/Sample_phase_tab.pdf")


### modify calibration summary table------------------------------------------------------


Cali_model_tab<- read.csv(file="Matlab/Calibration_files/Cali_model_tab.csv",
                          check.names = FALSE)

Cali_model_tab <- Cali_model_tab %>% gt() %>% 
  cols_align("center")
opt_table_font(data= Cali_model_tab, font = "Arial")  %>%
  #adjust the fontsize of the cell bodies and footnotes 
  tab_style(style=cell_text(size=px(7)), locations=list(cells_body(),
                                                        cells_footnotes()
  )
  ) %>%
  #adjust the cell title font sizes and padding
  tab_style(style=cell_text(size=px(9), weight="bold"),
            locations=list(cells_column_labels())
  ) %>% tab_options(data_row.padding = px(4)) -> Cali_model_tab


gtsave(Cali_model_tab,        # save table as pdf
       filename = "R/06_output/Cali_model_tab.pdf")


### [8] Saving subdatasets----------------------------------------------------------------------------------------------------------

#save all created subdataset in the environment via save.image
save.image(file="./R/05_analysis/subdata/conf_subdata.rda")


### NOTES---------------------------------
