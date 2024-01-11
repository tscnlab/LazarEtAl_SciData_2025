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

load(file="./R/05_analysis/subdata/conf_subdata.rda")

### [2] Source ggplot functions-------------------------------------------------------

#use ggplot functions defined in the functions-script
source("./R/05_analysis/ggplot_functions.R")

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
  scale_fill_manual(values=c("#E69F00", "darkslategray3", "#999999"))+
  scale_color_manual(values=c("#E69F00", "darkslategray3", "#999999"))+
  geom_hline(aes(yintercept=Maxpup, colour = id
                 ),
             data=hline_dat)+
  geom_hline(aes(yintercept=Minpup, colour = id
                 ),
             data=hline_dat)+
  labs()
agecomp_plot

ggsave("R/06_output/agecomp_plot.pdf", plot = agecomp_plot ,
       width = 159.2 *(2.998/3), height = 80, units = "mm", 
       bg = "white")

ggsave("R/06_output/agecomp_plot.png", plot = agecomp_plot ,
       width = 159.2 *(2.998/3), height = 80, units = "mm", 
       bg = "white")


### [4]Plot example spectra FIg 1 Sci Dat.-----------------------------------------

exalight<-read_csv("./R/05_analysis/subdata/example_spectra.csv")

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


ggsave("R/06_output/example_spectra.pdf", plot = exa ,
       width = 159.2 *(2.998/3), height = 80, units = "mm", 
       bg = "white")

ggsave("R/06_output/example_spectra.png", plot = exa ,
       width = 159.2 *(2.998/3), height = 80, units = "mm", 
       bg = "white")


### [5] Autocorrelation ------------------
#Suppl Figure 4 - Autocorrelations

#using the ggacf helper function to create Suppl Figure 4 A
#get rid of axis labels for subplot for larger Figure
mel_acf_plot<- ggacf(mel_acf)+ 
  labs(x = "", 
       y = "",
       title = "A",
       subtitle = "Autocorr. melanopic EDI")

#using the ggacf helper function to create Suppl Figure 4 B

pupil_acf_plot <- ggacf(pupil_acf)+ 
  labs(x = "", 
       y = "",
       title = "B",
       subtitle = "Autocorr. pupil size")

#create layout
lay3 <- rbind( 
  c(1, 2)
  
)    

#align the axes of the subplots 
autocor_plots <- align_plots(mel_acf_plot, pupil_acf_plot,
                             align = "hv", axis = 'b')


#Y-Axis Label for Suppl. Figure 4 
yleft3 <- textGrob("Autocorrelation", 
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
ggsave("06_output/suppl/autocor_panels.pdf", plot = autocor_panel ,
       width = 159.2*(2.28242/3), height = 80, units = "mm", 
       bg = "white")
