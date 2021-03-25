
## ---------------------------
##
## Script name: 3_polarplots_scirehab
##
## Purpose of script: To describe and quantify the number of medications patients received within the first 60 days post injury.
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2021-3-2
##
## Copyright (c) Catherine Jutzeler, 2021
## Email: catherine.jutzeler@bsse.ethz.ch
##
## ---------------------------
##
## Data source: SCI Rehab Study
##
## Notes: This analysis is for the publication Jutzeler et al, 2021 published in XX
##   
## ---------------------------
##
## load up the packages we will need:  
library("dplyr") 
library(tidyr)
library(tidyverse)
library(hrbrthemes)
library(formattable)
library(viridis)
library(ggplot2)
library(ggpubr)
library(naniar)
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(tidyr)){install.packages("tidyr")}
#if(!require(tidyverse)){install.packages("tidyverse")}
#if(!require(hrbrthemes)){install.packages("hrbrthemes")}
#if(!require(formattable)){install.packages("formattable")}
#if(!require(viridis)){install.packages("viridis")}
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(ggpubr)){install.packages("ggpubr")}
#if(!require(naniar)){install.packages("naniar")}
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
outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/SCI_Rehab/'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/SCI_Rehab/'

#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load original scirehab medication dataset
scirehab.medication.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/drug_addep/masterfile/masterfile.csv", header = T, sep = ',')
names(scirehab.medication.data)

# Make copy of data file to work with
scirehab.medication.data.copy <- scirehab.medication.data

#---------- Determine the medications that were most frequently administred ---------- 

# Subset to first 60 days
scirehab.medication.data.subset<-scirehab.medication.data.copy[,c(1:63)]

# Replace all values greater than 0 with a 1 and all na's will be replaced with a 0
scirehab.medication.data.subset.formatted<-scirehab.medication.data.subset %>%
  mutate_if(is.numeric, ~1 * (. != 0)) %>% 
  mutate_if(is.numeric, ~replace_na(., 0))

# Create data file and plot figure
scirehab.medication.data.subset.formatted.final <- scirehab.medication.data.subset.formatted%>%
  dplyr::mutate(total = rowSums(across(where(is.numeric))))%>%
  select(c(1,2,64))%>%
  mutate_if(is.numeric, ~1 * (. != 0))%>%
  filter(total>0)%>%
  count(generic_name) %>%
  dplyr::arrange(-n)%>%
  dplyr::distinct()%>%
  top_n(20)%>%
  as.data.frame()%>%
  ggplot2::ggplot(aes(x=reorder(generic_name, n), y=n))+geom_bar(stat = 'identity', fill='black')+
    coord_flip()+ylab("Number of Patients")+
    geom_text(aes(label = round(n, 1)), vjust = 0.5,hjust = -0.2, size=3)+
    theme_bw()+theme(axis.title.y = element_blank(), 
                     axis.text.y = element_text(size = 10, color = 'black', family = 'Times'),
                     axis.text.x = element_text(size = 10, color = 'black', family = 'Times'),
                     axis.title.x = element_text(size = 12, color = 'black', family = 'Times'))
  
scirehab.medication.data.subset.formatted.final

# Save plot
ggsave(
  "medications.frequency.scirehab.pdf",
  plot = scirehab.medication.data.subset.formatted.final,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 10,
  height = 6,
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