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
                   by.y = "ID", all.x = FALSE, all.y = TRUE)

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
# plegia <- as.character(unique(joined_mediction.df.new.id$NLI))
# nli <- as.character(unique(joined_mediction.df.new.id$NLI_raw))

for (i in 1:length(id_split)) {
  id_split[[i]]
  ais.grades <- unique(id_split[[i]]$AIS)
  sex <- unique(id_split[[i]]$Sex)
  nli <- unique(id_split[[i]]$NLI_raw)
  plegia <- unique(id_split[[i]]$NLI)
  write.csv(id_split[[i]], paste("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/pid_graphs/",new_names[i],"_",ais.grades,"_", sex,"_", nli,"_",plegia,".csv", sep=""))
}






#---------- Plot drug exposure pattern for each patient ---------- 

setwd("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/pid_graphs/")

file_list <- list.files()

for(file in file_list)    #repeat for all files in dir folder
{
  data <- read.csv(file, header=TRUE, sep=',')
  
  #data<- read.csv(file.choose())
  data1 <- data

  cols_to_change = c(5:368)    #change columns 4:368 to numerics class format
  for(i in cols_to_change){
    data1[, i][(data1[, i]>0)] <- 1
    class(data1[, i]) = "numeric"
    }
  
  # Sum up all lines with same drugs per patient
  datan<-plyr::ddply(data1,.(ID, generic.name, indication,Sex, Age, AIS, Cause, NLI, NLI_raw, YEARDOI,Time_wks, New_timeline), function(x) colSums(x[,-c(1,2,3,4,66:382)], na.rm = TRUE))

# Reformat data from wide to long
  data_long<-datan%>%
    gather(day, daily_dose, X0:X60)

  data_long$day<- sub("X","",data_long$day)
  data_long$day<- as.numeric(data_long$day)

  ais.grade.plot <-unique(data_long$AIS)
  sex.plot <-unique(data_long$Sex)
  cause.plot <-unique(data_long$Cause)
  plegia.plot <-unique(data_long$NLI)
  nli.plot <-unique(data_long$NLI_raw)
  
  # colors <- colorRampPalette(c("white", "#bca0dc", "#b491c8", "#663a82", "#3c1361"))(8)
  
  colors <- colorRampPalette(c("white", "#0000ff"))(7)
  
# Create plot  
myplot1<- ggplot(data_long, aes(day, generic.name, fill=as.factor(daily_dose)))+geom_tile(color = "white") +
  scale_fill_manual(values=colors)+theme_linedraw()+scale_x_continuous(expand = c(0, 0), breaks = c(0,15,30,45,60))+ 
    ggtitle(paste(sex.plot,", ",ais.grade.plot,", ",plegia.plot," (",nli.plot,"), ",cause.plot, sep = ""))+ 
    labs(x="Days Post-Injury", fill = "Number of\n Doses")+ 
    theme(panel.grid.major = element_blank(),axis.title.x = element_text(size = 12, family = 'Times'),
          plot.title =  element_text(size = 14, family = 'Times', face='bold'),
          axis.text.x = element_text(color="black", size=10, family = 'Times'), 
          axis.text.y = element_text( color="black", size=10, family = 'Times'), 
          axis.title.y  = element_blank(), legend.key = element_rect(fill = "black", color = NA))
  myplot1
  
  # Save Plot
  ggsave(myplot1,filename=paste(stringr::str_sub(file, end=-5),".pdf",sep=""),path='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/Sygen/Polypharmacy')
  
}







#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

