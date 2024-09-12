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
library(ggthemes)
library(cowplot)
library(scales);
library(ggpmisc)
library(ggpubr)
library(gridExtra)
library(grid);
library(gridtext)
library(gt)

set.seed(20230703) 


### [1] Loading subdatasets---------------------------------------------------

load(file="./R/01_processed_data/subdata/conf_subdata.rda")

### [2] Source ggplot functions-------------------------------------------------------

#use ggplot functions defined in the functions-script
source("./R/02_figs_tabs_code/ggplot_functions.R")

### [3] Case data age comparison-------------------------------------------------------------

#Figure 6: comparing age case data, dose response
#use the agecomp dataset for comparing a typical 18-year old & 87-year-old subject
#x-axis: mEDI; y-axis: pupil size (mm) for a single participant
# horizontal lines give maximum pupil range



#create top labels for the subplots
wraplabs <- c(`SP048`="20-year-old participant",
              `SP059`="44-year-old participant",
              `SP064`="80-year-old participant"
              
              )

#create horizontal lines with maximum - minimum pupil range
hline_dat = data.frame(id=c(id="SP048","SP059","SP064"),
                       Minpup = c(
                         min(agecomp$diameter_3d[agecomp$id == "SP048"]),
                         min(agecomp$diameter_3d[agecomp$id == "SP059"]),
                         min(agecomp$diameter_3d[agecomp$id == "SP064"])
                         
                                  ),
                       Maxpup =c(
                         max(agecomp$diameter_3d[agecomp$id == "SP048"]),
                         max(agecomp$diameter_3d[agecomp$id == "SP059"]),
                         max(agecomp$diameter_3d[agecomp$id == "SP064"])
                         
                                 )
)
#create the plot via the ggdr_mel function and add the horizontal lines
#agecomp <- agecomp[order(agecomp$age),]

agecomp_plot <- ggdr_mel(agecomp)+
  scale_fill_manual(values=c("#D4A66A", "#486E88", "#999999"))+
  scale_color_manual(values=c("#D4A66A", "#486E88", "#999999"))+
  geom_hline(aes(yintercept=Maxpup, colour = id
                 ),
             data=hline_dat)+
  geom_hline(aes(yintercept=Minpup, colour = id
                 ),
             data=hline_dat)+
  labs()
agecomp_plot

ggsave("./R/03_output/Figures/agecomp_plot.pdf", plot = agecomp_plot ,
       width = 180, height = 100, units = "mm", 
       bg = "white")

ggsave("./R/03_output/Figures/agecomp_plot.png", plot = agecomp_plot ,
       width = 180, height = 100, units = "mm", 
       bg = "white")


### [4] Plot example spectra FIg 1 Sci Dat.-----------------------------------------

exalight<-read_csv("./R/01_processed_data/subdata/example_spectra.csv")

exa<-ggplot(exalight, aes(x = wvlngth, y = flux, colour=group)) +
  geom_line(size=1.0)+
  scale_x_continuous(limits=c(380,780), 
                     breaks =seq(380,780,100))+
  scale_y_log10(limits=c(0.1,30))+
  labs(x=" ", y=" ")+
  theme_classic()+
  theme(legend.position = "none",  aspect.ratio = 1/1)+
  #theme(legend.title=element_blank(),legend.justification = c(1,0),
  #legend.position=c(1,0.53), legend.text.align = 1)+
  scale_color_manual(breaks=c("1300lux","150lux","dark"),
                     labels=c("~1300 lux","~  150 lux","Dark"),
                     values=c(rgb(0.667, 0.475, 0.224),rgb(0.439, 0.557, 0.643), "black"
                              )
                     )

exa#print


ggsave("./R/03_output/Figures/example_spectra.pdf", plot = exa ,
       width = 180, height = 100, units = "mm", 
       bg = "white")

ggsave("./R/03_output/Figures/example_spectra.png", plot = exa ,
       width = 180, height = 100, units = "mm", 
       bg = "white")


### [5] Autocorrelation ------------------

#using the ggacf helper function to create AUtocor Fig A
#get rid of axis labels for subplot for larger Figure
cf="#D4A66A"
mel_acf_plot<- ggacf(mel_acf)+
  scale_color_manual(values = c("#D4A66A"))+
  labs(x = "", 
       y = "",
       title = "A",
       #subtitle = "Autocorrelation melanopic EDI"
       )
mel_acf_plot

#using the ggacf helper function to create AUtocor Fig B
#get rid of axis labels for subplot for larger Figure
cf="#486E88"
pupil_acf_plot <- ggacf(pupil_acf)+ 
  scale_color_manual(values = c("#486E88"))+
  labs(x = "", 
       y = "",
       title = "B"#,
       #subtitle = "Autocorrelation pupil size"
       )

pupil_acf_plot

#create layout
lay3 <- rbind( 
  c(1, 2)
  
)    

#align the axes of the subplots 
autocor_plots <- align_plots(mel_acf_plot, pupil_acf_plot,
                             align = "hv", axis = 'b')


#Y-Axis Label for Suppl. Figure 4 
yleft3 <- textGrob("Autocorrelation [r]", 
                   rot = 90, 
                   gp = gpar(fontsize = 11),
                   vjust= 1.5)

#X-Axis Label for Suppl. Figure 4
bottom3 <- textGrob("Sample lag, t0 - [minute]",
                    gp = gpar(fontsize = 11),
                    vjust= -1.5
)



#Combine the subplots to one Figure (Suppl Figure 4)

autocor_panel <- grid.arrange(autocor_plots[[1]], autocor_plots[[2]],
                              layout_matrix = lay3, 
                              left = yleft3, bottom = bottom3)


#41.5 mm x 41.5 mm subplot axes
ggsave("./R/03_output/Figures/autocor_panels.pdf", plot = autocor_panel ,
       width = 180, height = 100, units = "mm", 
       bg = "white")

#41.5 mm x 41.5 mm subplot axes
ggsave("./R/03_output/Figures/autocor_panels.png", plot = autocor_panel ,
       width = 180, height = 100, units = "mm", 
       bg = "white")




## [6] Plotting  pupil & lido cross correlation ---------------------------------


#### Plotting the timeline -------------------------------------------------------
plot_labs <-  c("Time of Experiment [min]", "Melanopic Irradiance [mW/m²]", "Pupil Size [mm]")  

#setting up vars that allow empty ticks (empty label slots)
g<-seq(10, time_max/60, 10)
k = rbind(g, rep(" ", n=length(g)))
k<-k[-12]


gg_binned <- ggplot(binned_data)+
  scale_x_continuous(limits=c(600,
                              time_max),
                     breaks =seq(600,time_max , 300),
                     labels = c(k))+
  scale_y_log10(#limits=c(threshold, 100000),
    breaks = 10^(0:5),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x)))+
  theme(legend.position = "none", axis.title = element_text(face = "plain",
                                                            size = 11))+
  labs(x = " ", y = " ")+
  #annotation_logticks(sides="l")+
  theme_cowplot()


oo_timeline <- gg_binned +
  geom_col(aes(x=timestamp_oo, 
               y=melirrad_oo), 
           color ="#999999", fill= "#999999", width = 10)+
  annotate("text", x=2100, y=100000,
           label= paste(paste("OceanInsight STS-VIS")))+
  ggtitle("A")+# Timeline of Light Exposure
  theme(plot.title =element_text(size = 12, face="bold"),
        legend.position = "none", axis.title = element_text(face = "plain",
                                                            size = 11))


A1 <- ggplotGrob(oo_timeline)

lido_timeline <- gg_binned +
  geom_col(aes(x=timestamp_lido,
               y=melirrad_lido), 
           color="#999999", fill="#999999", width=10)+
  annotate("text", x=2100, y=100000,
           label= paste(paste("Lido")))+
  theme(legend.position = "none", axis.title = element_text(face = "plain",
                                                            size = 11))+
  labs(x=paste(plot_labs[[1]],"\n(Dataset SP033)"), y = " ")

A2 <- ggplotGrob(lido_timeline)

#axis labels are father away from axis than in the other plots
A <- grid.arrange(A1, A2, nrow = 2,
                  left =textGrob(paste(plot_labs[[2]]),
                                 gp=gpar(fontsize = 11,
                                         fontface = "plain"),
                                 rot=90, hjust = 1/3, vjust = 1))

#, bottom = plot_labs[[1]])

#### Plotting the Correlation data -------------------------------------------------------

rr <- cor.test(binned_data_all_lido$melirrad_oo, binned_data_all_lido$melirrad_lido,
               alternative = c("two.sided"),
               method = c("pearson"),
               exact = NULL, conf.level = 0.95, continuity = FALSE)

r<-(cor(binned_data_all_lido$melirrad_oo, binned_data_all_lido$melirrad_lido, 
        method='pearson',
        use = "complete.obs"))



gg_binned_all<-ggplot(binned_data_all_lido, 
                      aes(shape = Dataset,
                          fill = Dataset))+
  scale_fill_manual(values=c("#D4A66A", "#486E88", "#999999"))+
  scale_x_log10(breaks = 10^(0:5), 
                labels = scales::trans_format("log10",
                                              scales::math_format(10^.x)))+
  scale_y_log10(breaks = 10^(0:5),
                labels = scales::trans_format("log10",
                                              scales::math_format(10^.x)))+
  scale_shape_manual(values=c(21:23))+
  labs(x=paste("STS-VIS-derived\n", plot_labs[[2]]), 
       y=paste("Lido-derived\n", plot_labs[[2]]))+
  annotation_logticks()+
  coord_fixed(ratio = 1)+
  coord_cartesian(xlim =c(threshold, 10^5), 
                  ylim = c(threshold, 10^5))+
  theme_classic()
 #Device Comparison
  


# Corr plot
Corr_plot <- gg_binned_all+
  geom_point(aes(x=melirrad_oo, y = melirrad_lido),
             size = 3
  )+
  
  annotate("text", x=60, y=8000, fontface = "italic",
           label= paste("r "), hjust = 1)+
  annotate("text", x=60, y=8000, fontface = "plain", hjust = 0,
           label= paste("=", round(r,2)))+
  annotate("text", x=60, y=4000, hjust = 0.25,
           label= paste("~italic(p )< .001"), parse = T)+
  theme(legend.position = c(.75, .23), axis.title = element_text(size = 11))+
theme_classic()+
  ggtitle("B")+
  theme(plot.title = element_text(size = 12, face="bold"),  aspect.ratio = 1/1)

B  <- ggplotGrob(Corr_plot)
plot(B)



#### arranging all plots into subplots-------------------------------------


allplotslist <- align_plots(A, B, align = "hv", axis = 'b')


# create layout for all three figures plot panel
lay <- rbind(c(1, 2))

# create one plot panel for all three figures
plot_panel <- grid.arrange(allplotslist[[1]], allplotslist[[2]],
                           layout_matrix = lay)


# create layout for a & b figures plot panel


lay2 <- rbind(c(3,3,3,3,3),
              c(1,1,2,2,2),
              c(1,1,2,2,2))



# create one plot panel for a & b figures
plot_panelAB <- grid.arrange(allplotslist[[1]], allplotslist[[2]],
                             layout_matrix = lay2)




ggsave("./R/03_output/Figures/Lido_comp_plot.pdf", plot = plot_panelAB ,
       width = 200, height = 150, units = "mm", 
       bg = "white")



ggsave("./R/03_output/Figures/Lido_comp_plot.png", plot = plot_panelAB ,
       width = 200, height = 150, units = "mm", 
       bg = "white")




### [6] modify calibration summary table from csv ------------------


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
       filename = "./R/03_output/Tables/Cali_model_tab.pdf")




#create subdataset for Sci Dat. overview tables ---------------------------------

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
gtsave(Sum_tab,        # save table as pdf
       filename = "./R/03_output/Tables/Sum_tab.pdf")





# [7] create supplementary table with data on the experimental phases-----------------------


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
       filename = "./R/03_output/Tables/Sample_phase_tab.pdf")




