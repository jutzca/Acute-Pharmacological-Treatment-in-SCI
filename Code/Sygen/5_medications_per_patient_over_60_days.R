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
## ---------------------------
##
## load up the packages we will need:  
##
library(plyr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
#if(!require(plyr)){install.packages("plyr")}
#if(!require(tidyr)){install.packages("tidyr")}
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(stringr)){install.packages("stringr")}
##
## ---------------------------
##
## R Studio Clean-Up:
cat("\014") # clear console
rm(list=ls()) # clear workspace
gc() # garbage collector
##
## ---------------------------
##
## Set working directory 
setwd("/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/")
##
## ---------------------------
##
## Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/Sygen'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen'
##
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load original sygen medication dataset
masterfile.medication.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/drugs_per_day_shiny_app.csv", sep=',', header = TRUE)

# Create copy to work with
medication.per.patient <- masterfile.medication.data

#----------  Add the injury characteristics and demographics ---------- 
sygen.original<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/Original_data/df_sygen_formatted.csv", sep = ',', header = T,  na.strings=c("","NA"))

# Select columns to be used
sygen.original.subset <- sygen.original[,c(2:10,25:27)]

# Subset data to only patients with valid entry at Time 0 or Time 1 and remove duplicate patient numbers
sygen.original.subset1 <- distinct(subset(sygen.original.subset, Time==0 | Time==1) , ID, .keep_all = TRUE)

# Merge medication file and demographics file
joined_mediction.df <- merge(medication.per.patient, sygen.original.subset1, by.x = "NEW_ID", 
                   by.y = "ID", all.x = TRUE, all.y = FALSE)

#---------- Assign new ID for the patients (for data protection reason) ---------- 
joined_mediction.df.new.id<-joined_mediction.df %>% 
  dplyr::mutate(ID = group_indices(joined_mediction.df, n_groups=NEW_ID)) 

# Add the letter P in front of the newly created ID variable
joined_mediction.df.new.id$ID <- sub("^", "P", joined_mediction.df.new.id$ID)
joined_mediction.df.new.id

# Split the masterfile by pid
id <- joined_mediction.df.new.id[order(joined_mediction.df.new.id$ID),] 

id_split <- split(id, id$ID)

new_names <- as.character(unique(joined_mediction.df.new.id$ID))

for (i in 1:length(id_split)) {
  id_split[[i]]
  write.csv(id_split[[i]], paste("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/pid_graphs/",new_names[i],".csv", sep=""))
}


#---------- Plot drug exposure pattern for each patient ---------- 

setwd("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/pid_graphs/")

file_list <- list.files()

for(file in file_list)    #repeat for all files in dir folder
{
  data <- read.csv(file, header=TRUE, sep=',')
  
  data<- read.csv(file.choose())
  data1 <- data

  cols_to_change = c(5:368)    #change columns 4:368 to numerics class format
  for(i in cols_to_change){
    data1[, i][(data1[, i]>0)] <- 1
    class(data1[, i]) = "numeric"
    }
  

  datan<-plyr::ddply(data1,.(ID, generic_name, indication,Sex, Age, AIS, Cause, NLI, NLI_raw, YEARDOI,Time_wks, New_timeline ), function(x) colSums(x[,-c(1,2,3,4,66:382)], na.rm = TRUE))
  
  data_long<-datan%>%
    gather(day, daily_dose, X0:X60)

  data_long$day<- sub("X","",data_long$day)
  data_long$day<- as.numeric(data_long$day)
  data_long$daily_dose<- as.numeric(data_long$daily_dose)
  
  
  
  
  myplot1<- ggplot(data_long, aes(day, generic_name, fill=daily_dose))+geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high="black") +
    theme_linedraw()+scale_x_continuous(expand = c(0, 0), breaks = c(0,10,20,30,60))+ 
    # ggtitle(stringr::str_sub(file, end=-5))+
     #ggtitle(paste(file))+ 
    labs(x="Days Post-Injury")+ 
    theme(panel.grid.major = element_blank(),axis.title.x = element_text(size = 12),
          axis.text.x = element_text(color="black", size=10), 
          axis.text.y = element_text( color="gray28", size=9), 
          axis.title.y  = element_blank(), legend.position = "none")
  myplot1
  
  ggsave(myplot1,filename=paste(stringr::str_sub(file, end=-5),".pdf",sep=""),path='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/Sygen/Polypharmacy')
  
}







#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

