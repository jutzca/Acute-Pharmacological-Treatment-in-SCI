## ---------------------------
##
## Script name: 4_polar_plots_sygen
##
## Purpose of script: To visualize the medications administered per indication
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2020-12-2
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
library(ggplot2)
library(dplyr)


## ----------------------------
## Install packages needed:  (uncomment as required)

#if(!require(data.table)){install.packages("ggplot2")}


#### ---------------------------
#Clear working space

rm(list = ls())

#### ---------------------------
#Set output directorypaths

outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/Sygen'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen'


#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

#load original dataset

#load file
dataframe <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/df2_drugs_freq_disorder.csv", sep = ',', header = T)

#Remove Duplicates based on three rows
dataframe_rm_duplicates <- dataframe[!duplicated(dataframe[c(1:3)]),]

#Count number of drugs per indication
df_1 <- dataframe_rm_duplicates %>%
  count(generic_name, indication, sort = TRUE) %>%
  count(indication, sort = TRUE)

#Plot number of drugs per indication
nr.medications.indication.plot.sygen <-ggplot(data=df_1, aes(x=indication, y=n, fill=n)) +
  geom_bar(stat='identity')+theme_light() +
  scale_fill_gradient(low='red', high='blue', limits=c(0,150)) +
  theme(axis.title.y=element_text(angle=0))+ coord_polar()+theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
nr.medications.indication.plot.sygen

##Save plot
ggsave(
  "nr.medications.indication.plot.pdf",
  plot = nr.medications.indication.plot.sygen,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

dev.off()


#Count number of patients per indication
df_2 <- dataframe_rm_duplicates %>%
  count(NEW_ID, indication, sort = TRUE)%>%
  count(indication, sort = TRUE)

#Plotnumber of patients per indication
nr.patient.indication.plot.sygen <- ggplot(data=df_2, aes(x=indication, y=n, fill=n)) +
  geom_bar(stat='identity')+theme_light() +
  scale_fill_gradient(low='yellow', high='red', limits=c(0,800)) +
  theme(axis.title.y=element_text(angle=0)) + coord_polar()+theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
nr.patient.indication.plot.sygen


##Save plot
ggsave(
  "nr.patient.indication.plot.sygen.pdf",
  plot = nr.patient.indication.plot.sygen,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

dev.off()


