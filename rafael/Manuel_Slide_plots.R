library(ggplot2); library(plotly); library(gridExtra); library(imager); library(ggthemes);
library(cowplot); library(drc)

#set folder directory
whichfolder<-"C:/Users/Nils/Desktop/data analysis/ANALYZED DATA"
setwd(whichfolder)

#load data
load("allfiles.Rda")

#figure 1
#autocorrelation averaged across IDs


all_acf <- sapply(split(allfiles, allfiles$ID), 
                  FUN = function (allfiles) c(acf(allfiles$melirrad,plot = F, 
                                                  lag.max = 30,na.action =na.exclude)$acf))

#observations used to compute the correlations
n.used_acf<- sapply(split(allfiles, allfiles$ID), 
                    FUN = function (allfiles) c(acf(allfiles$melirrad,plot = F, 
                                                    lag.max = 30,na.action =na.exclude)$n.used))
#print
n.used_acf

# restructure the data intro a matrix than dataframe and name the variables
all_autocor<-matrix(all_acf,nrow=775, ncol = 1 )
all_autocor<- data.frame(all_autocor)
all_autocor$lag<-rep(c(0:30),25)
all_autocor$ID<-rep(c("DP01_01", "DP01_02","DP01_03","DP01_04",
                      "DP01_05", "DP01_06","DP01_07","DP01_08",
                      "DP01_09", "DP01_10","DP02_01", "DP02_02",
                      "DP02_03","DP02_04","DP02_06","DP02_07",
                      "DP02_08","DP02_09","DP02_10","DP02_11",
                      "PSS10","PSS11", "PSS12", "PSS13","PSS14"
), each=31)

#compute weighted mean autocorr across all IDs
mean_acf<-data.frame(0:30)
names(mean_acf)<- "lag"
weighted<-all_acf
mean_acf$cor<-NA

for (i in 1:ncol(all_acf))
{weighted[,i]<-all_acf[,i]* (n.used_acf[i]/sum(n.used_acf)) }

for (i in 1:nrow(weighted))
{mean_acf$cor[i]<-sum(weighted[i,]) }



#plot the autocorrelation data -> here WEIGHTED MEAN across all IDs  

figone<- ggplot(data = mean_acf, mapping = aes(x = lag, y = cor)) +
        #scale_x_continuous(limits=c(0,60))+
        geom_hline(aes(yintercept = 0)) +
        geom_segment(mapping = aes( xend = lag, yend = 0))+
        #facet_wrap(~ ID)+
        scale_x_continuous(breaks=seq(from=0, to=30, by=6),
                           labels=seq(0,300,60))+
        labs(x="sample lag, t0 - [seconds]", y = "autocorrelation")+
        theme_bw() 
figone






#figure 2
figtwo<-ggplot(allfiles, aes(x= allfiles$phot_lux, y= allfiles$melirrad, fill=allfiles$studyType)) +
        geom_point(shape=21, colour="black", alpha=0.5)+
        scale_x_log10(name=expression(paste(" Photopic Illuminance [log " [10] , "W/m"^2,"]")),
                      limits=c(1,55000), 
                      breaks =c(0,1,5,10,50,100,500,1000,5000,10000,50000 ),
                      labels =c(0,1,5,10,50,100,500,"1k","5k","10k","50k"))+
        scale_y_log10(name=expression(paste(" Melanopic Irradiance [log " [10] , "W/m"^2,"]")),
                      limits=c(1,55000), 
                      breaks =c(0,1,5,10,50,100,500,1000,5000,10000,50000 ),
                      labels =c(0,1,5,10,50,100,500,"1k","5k","10k","50k"))+
        #facet_grid(.~ studyType)+
        theme_cowplot()+
        theme(legend.position = "none")

figtwo

#DOSE RESPONSE MELIRRAD

figthree<-ggplot(allfiles, aes(x = allfiles$melirrad, y = allfiles$diameter_3d,
                            fill=allfiles$studyType))+ 
        scale_x_log10(limits=c(1,55000), 
                      breaks =c(1,10,100,1000,10000,50000 ),
                      labels =c(1,10,100,"1k","10k","50k"))+
        scale_y_continuous(name = "", 
                           breaks = c(1:9), limits = c(1,9))+
        geom_point(shape=21, size=2.25, alpha=0.6, colour="black")+
        #geom_smooth()+
        facet_wrap(.~ subject, strip.position = "bottom", ncol = 4, nrow=2, scales = "free")+
        labs(x=" ")+
        #x=expression(paste(" Melanopic Irradiance [log " [10] , "W/m"^2,"]")))+
        theme_cowplot()+
        theme(legend.position = "none")
#print
figthree

#for fig4 see EXCEL sheet "eb_corr_PLOT.xlsx"

