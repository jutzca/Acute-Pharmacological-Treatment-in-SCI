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

# Load original dataset
dataframe <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/df2_drugs_freq_disorder.csv", sep = ',', header = T)

# Remove Duplicates based on three rows
dataframe_rm_duplicates <- dataframe[!duplicated(dataframe[c(2:4)]),]

# Add demographics and injury characteristics
demographics.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/demographics_injury_characteristics.csv", header=T, sep = ',')
dataframe_rm_duplicates.extended <- merge(dataframe_rm_duplicates,demographics.data)

# Count number of drugs per indication overall
number.of.drugs.per.indication.overall <- dataframe_rm_duplicates.extended %>%
  dplyr::count(ais1, generic_name, indication, sort = TRUE) %>%
  dplyr::count(ais1, indication, sort = TRUE) %>%
  as.data.frame()
number.of.drugs.per.indication.overall

# Save table
write.csv(number.of.drugs.per.indication.overall, "/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen/Supplementary_Table_3_overall.csv", row.names = F)

# Count number of drugs per indication stratified by AIS grades
number.of.drugs.per.indication.stratified.by.ais.grades <- dataframe_rm_duplicates.extended %>%
  dplyr::count(ais1, generic_name, indication, sort = TRUE) %>%
  dplyr::count(ais1, indication, sort = TRUE) %>%
  as.data.frame()
number.of.drugs.per.indication.stratified.by.ais.grades

# Save table
write.csv(number.of.drugs.per.indication.stratified.by.ais.grades, "/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen/Supplementary_Table_3_AIS_grades.csv", row.names = F)

# Plot number of drugs per indication stratified by AIS grades
polar_plots.nr.medications.indication.by.ais.grade.sygen <-ggplot(data=number.of.drugs.per.indication.stratified.by.ais.grades, aes(x=indication, y=n, fill=n)) +
  geom_bar(stat='identity')+facet_wrap(.~ais1, nrow = 3)+
  theme_light() +
  scale_fill_gradient(low='red', high='blue', limits=c(0,140)) +
  theme(axis.title.y=element_text(angle=0))+ coord_polar()+
  theme(axis.title.y=element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank() )+force_panelsizes(cols = c(0.3, 1)) 
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


#Count number of patients per indication
df_2 <- dataframe_rm_duplicates %>%
  dplyr::count( NEW_ID, indication, sort = TRUE)   %>%
  dplyr::count(indication, sort = TRUE)%>%
  as.data.frame()

write.csv(df_2, "/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen/Supplementary_Table_2.csv")


#Plotnumber of patients per indication
polar_plots.nr.patient.indication.sygen <- ggplot(data=df_2, aes(x=indication, y=n, fill=n)) +
  geom_bar(stat='identity')+theme_light() +
  scale_fill_gradient(low='yellow', high='red', limits=c(0,800)) +
  theme(axis.title.y=element_text(angle=0)) + coord_polar()+theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
polar_plots.nr.patient.indication.sygen


##Save plot
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



# Barplot of drugs per indication
dataframe_rm_duplicates %>% subset(!(indication=="Cardiac System")) %>% subset(!(indication=="Immune system  "))%>%  subset(!(indication=="Renal and urinary system  "))%>%                      
  dplyr::count(indication, generic_name) %>%  
  dplyr::filter(n>= 20) %>%   
  dplyr::arrange(desc(n))%>%  
  dplyr::arrange(.,indication) %>%    
  ggplot(aes(tidytext::reorder_within(indication, -n, generic_name), n))+
  geom_bar(stat = "identity")+
  labs(x = NULL, y = "n") +
  facet_wrap(.~indication, scales = "free", ncol=3) +theme_bw()+theme()











