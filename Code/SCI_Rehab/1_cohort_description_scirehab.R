## ---------------------------
##
## Script name: cohort_description_scirehab
##
## Purpose of script: To describe the SCIREHAB cohort and create a table for publication
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
## Data source: SCIRehab Drug Data
##
## Notes: This analysis is for the publication Jutzeler et al, 2021 published in XX
##   
## ---------------------------
##   
## load up the packages we will need:  
library(data.table)
library(table1)
##   
## ----------------------------
##   
## Install packages needed:  (uncomment as required)
##
#if(!require(data.table)){install.packages("data.table")}
#if(!require(table1)){install.packages("table1")}
##
#### ---------------------------
##
# R Studio Clean-Up:
cat("\014") # clear console
rm(list=ls()) # clear workspace
gc() # garbage collector
##
#### ---------------------------
##
## Set working directory 
setwd("/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/")
##
#### ---------------------------
##
## Set output directorypaths
outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/SCI_Rehab'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/SCI_Rehab'
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load data files
scirehab.drug.df <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Drug_addep/masterfile/masterfile.csv", sep = ',', header = T,  na.strings=c("","NA"))
scirehab.demographics.df <-read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Drug_addep/df_rehab_formatted.csv", sep = ',', header = T,  na.strings=c("","NA"))

# Subset scirehab.demographics.df based on the variable newid from the scirehab.drug.df
sci.rehab.cohort <- subset(scirehab.demographics.df, unique(NEWID) %in% scirehab.drug.df$newid)

# Subset the cohort for admission
sci.rehab.cohort.admission <- subset(sci.rehab.cohort, Time=='admission')


#-----------Create Table of Included Cohort for Publication

# 1. Format Table: Customize levels, labels, and units of listed variables
# Change names of levels of variables
levels(sci.rehab.cohort.admission$Sex) <- c("Female", "Male")
levels(sci.rehab.cohort.admission$AIS) <- c("A", "B", "C", "D")
levels(sci.rehab.cohort.admission$NLI) <- c("Cervical", "Thoracic", 'Lumbar')
levels(sci.rehab.cohort.admission$Cause) <- c("Automobile", "Fall", "Gunshot wound", "Motorcycle", "Other sports", "Others", "Pedestrian", 'person-to-person contac', "Water-related")

# Relable variables
label(sci.rehab.cohort.admission$Sex) <- "Sex, n (%)"
label(sci.rehab.cohort.admission$Age) <- "Age Groups"
# label(emsci.trauma.sex.va.a1$NLI_level)<- "Neurological level of injury"
label(sci.rehab.cohort.admission$AIS) <- "AIS, n (%)"
label(sci.rehab.cohort.admission$NLI) <- "Neurological level of injury, n (%)"
label(sci.rehab.cohort.admission$Cause) <- "Cause, n (%)"


# 2. Create Table
table1::table1(~ Sex+Age+AIS+NLI+Cause, data = sci.rehab.cohort.admission)


#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

