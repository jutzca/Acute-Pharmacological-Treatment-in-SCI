## ---------------------------
##
## Script name: 1_cohort_description_sygen
##
## Purpose of script: To describe the Sygen cohort and create a table for publication
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
##
library(data.table)
library(table1)
library(dplyr)
##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
#if(!require(data.table)){install.packages("data.table")}
#if(!require(table1)){install.packages("table1")}
#if(!require(dplyr)){install.packages("dplyr")}
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
sygen.original<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/Original_data/df_sygen_formatted.csv", sep = ',', header = T,  na.strings=c("","NA"))

sygen.pid <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/PID/pid_sygen.csv", sep = ',', header = T,  na.strings=c("","NA"))

joined_df <- merge(sygen.pid, sygen.original, by.x = "NEW_ID", 
                   by.y = "ID", all.x = TRUE, all.y = FALSE)


joined_df2 <- subset(joined_df, Time==4)
sygen<-distinct(joined_df, NEW_ID, .keep_all = TRUE)


sygen$YEARDOI <- as.factor(sygen$YEARDOI)

#---------- Create Table of Included Cohort for Publication --------

# 1. Format Table: Customize levels, labels, and units of listed variables
# Change names of levels of variables
levels(sygen$Sex) <- c("Female", "Male")
levels(sygen$AIS) <- c("A", "B", "C", "D")
levels(sygen$NLI) <- c("Cervical", "Thoracic")
levels(sygen$Cause) <- c("Automobile", "Blunt trauma", "Fall", "Gunshot wound", "Motorcycle", "Other sports", "Others", "Pedestrian", "Water-related")

# Relable variables
label(sygen$Sex) <- "Sex, n (%)"
label(sygen$Age) <- "Age"
# label(emsci.trauma.sex.va.a1$NLI_level)<- "Neurological level of injury"
label(sygen$AIS) <- "AIS Grade, n (%)"
label(sygen$NLI) <- "Neurological level of injury, n (%)"
label(sygen$Cause) <- "Cause, n (%)"

# Assign units to Age at Injury and Year of Injury
units(sygen$Age) <- "years"

# 2. Create table
table1::table1(~ Sex+Age+AIS+NLI+Cause, data = sygen)

#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

