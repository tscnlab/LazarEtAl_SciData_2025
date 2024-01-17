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
library(psych)
library(lubridate)
library(gtsummary)
library(gt)
library(webshot2)

### [1] Demographic data preparation -------------------------------------------
#Prepare data for demographic tables & figure

#load complete merged dataset
load(file="./R/03_datamerge/mergeddata_all.rda")

#create dataset for demographic analysis
#select only 1 line per participant and omit unneeded variables
dem_data <- merged_data_all[!duplicated(merged_data_all[c('id')]),]
dem_data <- select(dem_data, id, excl, excl_time, date, begin, age, age_group,
                   sex, handedness, visual_aid, contact_lenses, 
                   visual_acuity_snellen, BMI, MSFsc, time_awake, sleeping_hours,
                   SDweek, acute_sum, acute_sum_rel, habitual_sum_rel, 
                   habitual_sum, iris_colour, weather, kss_pre, kss_post,
                   time_awake, season
                   )

#rename the factors for the time of exclusion variable
dem_data %>%
  mutate(excl_time = factor(excl_time, labels = c("Included", 
                                                  "Excluded after trial",
                                                  "Excluded before trial"))) -> dem_data

# dem_data$excl[dem_data$id=="SP040" | dem_data$id=="SP058" | dem_data$id=="SP061" | dem_data$id=="SP106"] <- "results"
# 
# dem_data$excl_time[dem_data$id=="SP040" | dem_data$id=="SP058" | dem_data$id=="SP061" | dem_data$id=="SP106"] <- "Included"

dem_data  <- dem_data[which(dem_data$excl=="results"),]

#drop factor levels of dem_Data because they interfere with creating the 
#demographic table
dem_data <- droplevels(dem_data)



#[2] Demographic table ---------------------------------------------------------
#create demographic table with gt summary:

#first categorize the data into data types.
#then include only varaibles needed for the table
#define decimal digits for continouus data
#relabel the variabes for the demographic table (Table 1)
#modify the header format




#rename factor levels for iris into Hazel/green
  levels(dem_data$iris_colour)<- c("Blue", "Hazel/green", "Brown")

#rename factor levels for visual acuity correction
levels(dem_data$visual_aid) <- c("No","Yes - correction for myopia",
                                 "Yes - correction for hyperopia")

#and make contact lenses yes/no table rows

levels(dem_data$contact_lenses) <- c("No","Yes")
#and 

dem_data %>%
  tbl_summary(
    type = list(everything() ~ "continuous",
                     "sex" ~ "categorical",
                     "visual_aid" ~ "categorical",
                     "contact_lenses" ~ "categorical",
                     "iris_colour" ~ "categorical",
                     "weather" ~ "categorical",
                    "season"~ "categorical",
                    "age_group" ~ "categorical",
                    "handedness"~ "categorical"),
  
    statistic = list(
       all_continuous() ~ "{mean} ({sd})",
       all_categorical() ~ "{n} ({p}%)"),
    by = excl_time,
        include = c(-id, -begin, -excl, -excl_time, -acute_sum_rel, #take out superfluos vars
                    -habitual_sum_rel, -date, -visual_acuity_snellen, -time_awake
                    ),
    digits = all_continuous() ~ 2,
    label = list(
                 season ~ "Season",
                 age ~ "Age [years]",
                 age_group ~ "Age group [years]",
                 sex ~ "Sex (assigned at birth)",
                 BMI ~ "Body Mass Index (BMI) [kg/m²]",
                 handedness ~ "Handedness (self-report)",
                 visual_aid ~ "Uses visual aid",
                 contact_lenses ~ "Wearing contact lenses during trial",
                 MSFsc ~  "Midsleep on free days corrected for oversleep [h]",
                 sleeping_hours ~ "Hours of sleep before trial [h]",
                 SDweek ~ "Average sleep duration [h]",
                 acute_sum ~ "Estimated acute caffeine consumption [mg]",
                 habitual_sum ~ "Estimated habitual caffeine consumption [mg]",
                 iris_colour ~ "Iris colour (experimenter-rated)",
                 weather ~ "Weather during trial (experimenter-rated)",
                 kss_pre ~ "KSS-assessed sleepiness (during trial)",
                 kss_post ~ "KSS-assessed sleepiness (after trial)"
                 ),
    missing = "no",
    missing_text = "(Missing)"
  ) %>% bold_labels()  %>%
  modify_header(label ~ "**Variable**") -> desc_table 

desc_table 

#format the table as gt object
desc_table  %>%   as_gt() -> desc_table_gt 

desc_table_gt[["_boxhead"]][["column_label"]][[6]] <- "Valid data, n=83"

#set the font to Arial
desc_table_gt <- opt_table_font(data= desc_table_gt, font = "Arial")  %>%
  #adjust the cell bodies and footnotes to be fontsize 9
  tab_style(style=cell_text(size=px(11)), locations=list(cells_body(),
                                                            cells_footnotes()
                                                            )
  ) %>%
  #adjust the cell title to be fontsize 9.75
  tab_style(style=cell_text(size=px(13)),
                            locations=cells_column_labels()
            ) %>% tab_options(data_row.padding = px(4))

  gtsave(desc_table_gt,        # save table as pdf
    filename = "R/06_output/dem_tab.pdf")
  gtsave(desc_table_gt,        # save table as pdf
         filename = "R/06_output/dem_tab.png")

# [4] Demographic figure ------------------------------------------------------
#create demographic figure (cf. Figure 2)

#load additional libraries
library(reshape2)
library(ggplot2)
library(plyr)

#use only the included data
dem_data_incl <- dem_data[dem_data$excl_time == "Included",]

#check levels of variables "sex
levels(dem_data_incl$sex)
#drop factor levels of  because they interfere with creating the 
#demographic Figure
dem_data_incl <- droplevels(dem_data_incl)

#create pyraymid plot stratified by sex
agepyr_plot<- apyramid::age_pyramid(data = dem_data_incl,
                                    age_group = "age_group",
                                    split_by = "sex",
                                    show_midpoint = FALSE)+
  
  # additional ggplot commands
  theme_classic()+                               # simplify background
  labs(y = "Included participants [count]",     # note x and y labs are switched
       x = "Age categories [years]",                          
       fill = "Sex", 
       #caption = "My data source and caption here",
       #title = "Sample distribution across age groups and sex ",
       subtitle = " ")

agepyr_plot <- agepyr_plot+
  theme(
    legend.position = "bottom",          #legend to bottom
    axis.text = element_text(size = 9),  # fonts/sizes
    axis.title = element_text(size = 11),
    legend.text = element_text(size=9),
    plot.title = element_text(size = 12, face="bold"))
  
#save the plot as pdf 
#in case you encounter problems during the pdf saving process, this could be
#related to the cairo_pdf device (especially the case for MACOS users)
#In that case try deleting "device=cairo_pdf" in following code bit below
ggsave("06_output/agepyr_plot.pdf", plot = agepyr_plot ,
       width = 246.2/3.2, height = 73.4, units = "mm", 
       bg = "white", device=cairo_pdf)

agepyr_plot

### Notes-------------------------------------------------------------------

#the experiment of SP060 was aborted mid-trial, so that the MCTQ was not filled in
#hence resulting in missing values MCTQ vars, caffeine questionnaires,
#KSS tests
dem_data$id[is.na(dem_data$MSFsc) & dem_data$excl_time == "POST"]
 