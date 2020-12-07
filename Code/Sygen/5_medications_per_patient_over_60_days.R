## ---------------------------
##
## Script name: 5_medications_per_patient_over_60days
##
## Purpose of script: To describe and visualize the medications administered per patient over the first 60 days post injury
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2020-12-5
##
## Copyright (c) Catherine Jutzeler, 2020
## Email: catherine.jutzeler@bsse.ethz.ch
##
## ---------------------------
##
## Data source: Sygen Clinical Trial
##
## Notes: This analysis is for the publication Jutzeler et al, 2021 published in XX
##   
#### ---------------------------

## set working directory

setwd("/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/")

## ---------------------------
## load up the packages we will need:  
library(plyr)
library(tidyr)
library(ggplot2)
library(dplyr)

## ----------------------------
## Install packages needed:  (uncomment as required)

#if(!require(plyr)){install.packages("plyr")}
#if(!require(tidyr)){install.packages("tidyr")}
#if(!require(ggplot2)){install.packages("ggplot2")}

#### ---------------------------
#Clear working space

rm(list = ls())

#### ---------------------------
#Set output directorypaths

outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/Sygen'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen'


#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

#load original sygen medication dataset
masterfile.medication.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/df_drugs_per_days.csv", sep=',', header = TRUE)

#Create copy to work with

medication.per.patient <- masterfile.medication.data

#Assign new ID for the patients (for data protection reason)
medication.per.patient<-medication.per.patient %>% 
  dplyr::mutate(ID = group_indices_(medication.per.patient, .dots="NEW_ID")) 

##Add the letter P in front of the newly created ID variable
medication.per.patient$ID <- sub("^", "P", medication.per.patient$ID )
medication.per.patient

#Split the masterfile by pid
id <- medication.per.patient[order(medication.per.patient$ID),] 

id_split <- split(id, id$ID)

new_names <- as.character(unique(medication.per.patient$ID))

for (i in 1:length(id_split)) {
  id_split[[i]]
  write.csv(id_split[[i]], paste("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/pid_graphs/",new_names[i],".csv", sep=""))
}


#---- Plot drug exposure pattern for each patient#

setwd("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/pid_graphs/")

file_list <- list.files()

for(file in file_list)    #repeat for all files in dir folder
{
  data <- read.csv(file, header=TRUE, sep=',')
  
  data <- read.csv(file.choose())
  
  data.select <-dplyr::select(data,-c(1,2)) #remove first column as it is not needed
  
  cols_to_change = c(2:367)    #change columns 4:368 to numerics class format
  for(i in cols_to_change){
    class(data.select[, i]) = "numeric"
  }
  
  data.select [data.select>0] <- 1
  data.select  <-  data.select [c(1:62,368)]
  
  
  datan<-ddply(data.select,.(generic_name), function(x) colSums(x[,-c(1,63)], na.rm = TRUE))
  
  data_long<-datan%>%
    gather(time, dose, X0:X60)
  
  colnames(data_long)[2] <- "time"
  colnames(data_long)[3] <- "daily_dose"
  data_long$time<- sub("X","",data_long$time)
  data_long$time<- as.numeric(data_long$time)
  data_long$daily_dose<- as.numeric(data_long$daily_dose)
  
  
  myplot1<- ggplot(data_long, aes(time, generic_name, fill=daily_dose))+geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high="black") +
    theme_linedraw()+scale_x_continuous(expand = c(0, 0), breaks = c(0,10,20,30,60))+ 
    ggtitle("Number of Administrations per Drug per Day")+ 
    labs(x="Days Post-Injury")+ 
    theme(panel.grid.major = element_blank(),axis.title.x = element_text(size = 12),
          axis.text.x = element_text(color="black", size=10), 
          axis.text.y = element_text( color="gray28", size=9), 
          axis.title.y  = element_blank(), legend.position = "none")
  
  
  ggsave(myplot1,filename=paste("myplot",file,".pdf",sep=""),path='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/Sygen/Polypharmacy')
  
}







#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

