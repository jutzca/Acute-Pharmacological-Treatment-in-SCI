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
## ---------------------------
##
## load up the packages we will need:  
library(ggplot2)
library(dplyr)
library(easyGgplot2)
library(tidytext)
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(easyGgplot2)){install.packages("easyGgplot2")}
#if(!require(tidytext)){install.packages("tidytext")}
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
setwd("/Users/jutzelec/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/")
##
## ---------------------------
##
## Set output directorypaths
outdir_figures='/Users/jutzelec/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/Sygen'
outdir_tables='/Users/jutzelec/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen'
##
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load original dataset
dataframe <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/df2_drugs_freq_disorder.csv", sep = ',', header = T)

# Remove Duplicates based on three rows
dataframe_rm_duplicates <- dataframe[!duplicated(dataframe[c(2:4)]),]


#---------- List all medications per indication  ---------- 
drugs.per.indication <- dataframe_rm_duplicates %>%
  dplyr::select(indication, generic_name)%>%
  dplyr::group_by(indication)%>%
  dplyr::distinct()%>%
  summarise(
    allindication = paste(generic_name, collapse=", ")
  )%>%
  as.data.frame()
drugs.per.indication

# Save table
#write.csv(drugs.per.indication, "/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen/All_medications_per_organ_system.csv", row.names = F)



#---------- List all indications per medication ---------- 
indications.per.medication <- dataframe_rm_duplicates %>%
  dplyr::select(generic_name, indication)%>%
  dplyr::group_by(generic_name)%>%
  dplyr::distinct()%>%
  summarise(
    allindication = paste(indication, collapse=", ")
  )%>%
  as.data.frame()
indications.per.medication

# Save table
write.csv(indications.per.medication, "/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen/indications.per.medication.csv", row.names = F)


#---------- Number of drugs per indication overall ---------- 
# Count number of drugs per indication
number.of.drugs.per.indication.overall <- dataframe_rm_duplicates %>%
  dplyr::count( generic_name, indication, sort = TRUE) %>%
  dplyr::count(indication, sort = TRUE) %>%
  as.data.frame()
number.of.drugs.per.indication.overall

# Save table
#write.csv(number.of.drugs.per.indication.overall, "/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen/Supplementary_Table_3_overall.csv", row.names = F)

# Plot number of drugs per indication overall
number.of.drugs.per.indication.overall.sygen <-ggplot(data=number.of.drugs.per.indication.overall, aes(x=indication, y=n, fill=n)) +
  geom_bar(stat='identity')+
  theme_light() +
  scale_fill_gradient(low='red', high='blue', limits=c(0,150)) +
  theme(axis.title.y=element_text(angle=0))+ coord_polar()+
  theme(axis.title.y=element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x=element_text(size=12, family = 'Times'), 
        strip.text = element_text(size=14, family = "Times", face='bold'))
number.of.drugs.per.indication.overall.sygen

# Save plot
ggsave(
  "polar_plots.nr.medications.indication.sygen.pdf",
  plot = number.of.drugs.per.indication.overall.sygen,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Number of drugs per indication stratified by AIS grades ---------- 

# Add demographics and injury characteristics
demographics.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/demographics_injury_characteristics.csv", header=T, sep = ',')
dataframe_rm_duplicates.extended <- merge(dataframe_rm_duplicates,demographics.data)

# Count number of drugs per indication stratified by AIS grades
number.of.drugs.per.indication.stratified.by.ais.grades <- dataframe_rm_duplicates.extended %>%
  dplyr::count(ais1, generic_name, indication, sort = TRUE) %>%
  dplyr::count(ais1, indication, sort = TRUE) %>%
  as.data.frame()
number.of.drugs.per.indication.stratified.by.ais.grades

# Save table
#write.csv(number.of.drugs.per.indication.stratified.by.ais.grades, "/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen/Supplementary_Table_3_AIS_grades.csv", row.names = F)

# Plot number of drugs per indication stratified by AIS grades
polar_plots.nr.medications.indication.by.ais.grade.sygen <-ggplot(data=number.of.drugs.per.indication.stratified.by.ais.grades, aes(x=indication, y=n, fill=n)) +
  geom_bar(stat='identity')+facet_wrap(.~ais1, nrow = 2)+
  theme_light() +
  scale_fill_gradient(low='red', high='blue', limits=c(0,140)) +
  theme(axis.title.y=element_text(angle=0))+ coord_polar()+
  theme(axis.title.y=element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x=element_text(size=12, family = 'Times'), 
        strip.text = element_text(size=14, family = "Times", face='bold'))
polar_plots.nr.medications.indication.by.ais.grade.sygen

##Save plot
ggsave(
  "polar_plots.nr.medications.indication.by.ais.grade.sygen.pdf",
  plot = polar_plots.nr.medications.indication.by.ais.grade.sygen,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 15,
  height = 15,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Number of patients per indication overall ---------- 

#Count number of patients per indication
number.of.patient.per.indication <- dataframe_rm_duplicates %>%
  dplyr::count( NEW_ID, indication, sort = TRUE)   %>%
  dplyr::count(indication, sort = TRUE)%>%
  as.data.frame()
number.of.patient.per.indication

# Save table
#write.csv(number.of.patient.per.indication, "/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen/Supplementary_Table_2_overall.csv")

# Plot number of patients per indication
polar_plots.nr.patient.indication.sygen <- ggplot(data=number.of.patient.per.indication, aes(x=indication, y=n, fill=n)) +
  geom_bar(stat='identity')+theme_light() +
  scale_fill_gradient(low='yellow', high='red', limits=c(0,800)) +
  theme(axis.title.y=element_text(angle=0)) + coord_polar()+
  theme(axis.title.y=element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x=element_text(size=12, family = 'Times'), 
        strip.text = element_text(size=14, family = "Times", face='bold'))
polar_plots.nr.patient.indication.sygen


# Save plot
ggsave(
  "polar_plots.nr.patient.indication.sygen.pdf",
  plot = polar_plots.nr.patient.indication.sygen,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

dev.off()



#---------- Number of patients per indication stratified by AIS grades ---------- 

# Add demographics and injury characteristics
demographics.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/demographics_injury_characteristics.csv", header=T, sep = ',')
dataframe_rm_duplicates.extended <- merge(dataframe_rm_duplicates, demographics.data)

# Count number of patients per indication
number.of.patient.per.indication.per.ais.grades <- dataframe_rm_duplicates.extended %>%
  dplyr::count(ais1, NEW_ID, indication, sort = TRUE)   %>%
  dplyr::count(ais1, indication, sort = TRUE)%>%
  as.data.frame()
number.of.patient.per.indication.per.ais.grades

# Save table
#write.csv(number.of.patient.per.indication.per.ais.grades, "/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen/Supplementary_Table_2_per_ais_grade.csv")


# Plot number of patients per indication
polar_plots.nr.patient.indication.per.ais.grades.sygen <- ggplot(data=number.of.patient.per.indication.per.ais.grades, aes(x=indication, y=n, fill=n)) +
  geom_bar(stat='identity')+theme_light() +facet_wrap(.~ais1, nrow = 2)+
  scale_fill_gradient(low='yellow', high='red', limits=c(0,430)) +
  theme(axis.title.y=element_text(angle=0)) + coord_polar()+
  theme(axis.title.y=element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x=element_text(size=12, family = 'Times'), 
        strip.text = element_text(size=14, family = "Times", face='bold'))
polar_plots.nr.patient.indication.per.ais.grades.sygen


# Save plot
ggsave(
  "polar_plots.nr.patient.indication.per.ais.grades.sygen.pdf",
  plot = polar_plots.nr.patient.indication.per.ais.grades.sygen,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 12,
  height = 12,
  units = "in",
  dpi = 300
)

dev.off()


#---------- Determine the medications that were most frequently administred ---------- 

#Count number of patients per indication
medications.frequency <- dataframe_rm_duplicates %>%
  dplyr::count(generic_name, sort = TRUE)%>%
  dplyr::mutate(proportion=n/791*100)%>%
  dplyr::top_n(20)%>%
  as.data.frame()%>%
  ggplot2::ggplot(aes(x=reorder(generic_name, n), y=n))+geom_bar(stat = 'identity', fill='black')+
  coord_flip()+ylab("Number of Patients")+
  geom_text(aes(label = round(n, 1)), vjust = 0.5,hjust = -0.2, size=3)+
  theme_bw()+theme(axis.title.y = element_blank(), 
                   axis.text.y = element_text(size = 10, color = 'black', family = 'Times'),
                   axis.text.x = element_text(size = 10, color = 'black', family = 'Times'),
                   axis.title.x = element_text(size = 12, color = 'black', family = 'Times'))
medications.frequency


# Save plot
ggsave(
  "medications.frequency.pdf",
  plot = medications.frequency,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

dev.off()

