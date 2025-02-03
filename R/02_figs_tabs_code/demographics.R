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
load(file="./R/01_processed_data/mergeddata_dem.rda")


#create dataset for demographic analysis
#select only 1 line per participant and omit unneeded variables
dem_data <- merged_data_dem[!duplicated(merged_data_dem[c('id')]),]


dem_data <- select(dem_data, id, excl, excl_time, date, begin, age, age_group,
                   sex, handedness, visual_aid, contact_lenses, 
                   visual_acuity_snellen, BMI, MSFsc, time_awake, sleeping_hours,
                   SDweek, acute_sum, acute_sum_rel, habitual_sum_rel, 
                   habitual_sum, iris_colour, weather, kss_pre, kss_post,
                   time_awake, season, overthrs75
                   )

#rename the factors for the time of exclusion variable
dem_data %>%
  mutate(excl_time = factor(excl_time, labels = c("Included", 
                                                  "Excluded after trial",
                                                  "Excluded before trial"))) -> dem_data

# dem_data$excl[dem_data$id=="SP040" | dem_data$id=="SP058" | dem_data$id=="SP061" | dem_data$id=="SP106"] <- "results"
# 
# dem_data$excl_time[dem_data$id=="SP040" | dem_data$id=="SP058" | dem_data$id=="SP061" | dem_data$id=="SP106"] <- "Included"

dem_data  <- dem_data[which(dem_data$excl_time=="Included" & dem_data$overthrs75==FALSE),]

#drop factor levels of dem_Data because they interfere with creating the 
#demographic table
dem_data <- droplevels(dem_data) 

dem_data <- dem_data %>% select (-overthrs75)

#save main demgraphic data into csv metadata file
dem_data_csv <- dem_data %>% select(-excl, -excl_time, - visual_acuity_snellen)
write.csv(dem_data_csv, file="./R/03_output/Tables/demographic_data.csv")

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
  tab_style(style=cell_text(size=px(8)), locations=list(cells_body(),
                                                            cells_footnotes()
                                                            )
  ) %>%
  #adjust the cell title to be fontsize 9.75
  tab_style(style=cell_text(size=px(10)),
                            locations=cells_column_labels()
            ) %>% tab_options(data_row.padding = px(4))

  gtsave(desc_table_gt,        # save table as pdf
    filename = "./R/03_output/Tables/dem_tab.pdf")
  gtsave(desc_table_gt,        # save table as pdf
         filename = "./R/03_output/Tables/dem_tab.png")
  
