## ---------------------------
##
## Script name: 2_medication_desription_sygen
##
## Purpose of script: To describe and quantify the number of medications patients received within the first 60 days post injury.
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
library("dplyr") 
library(tidyr)
library(tidyverse)
library(hrbrthemes)
library(formattable)
library(viridis)
library(ggplot2)
library(ggpubr)
library(naniar)

## ----------------------------
## Install packages needed:  (uncomment as required)

#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(tidyr)){install.packages("tidyr")}
#if(!require(tidyverse)){install.packages("tidyverse")}
#if(!require(hrbrthemes)){install.packages("hrbrthemes")}
#if(!require(formattable)){install.packages("formattable")}
#if(!require(viridis)){install.packages("viridis")}
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(ggpubr)){install.packages("ggpubr")}
#if(!require(naniar)){install.packages("naniar")}

#### ---------------------------
#Clear working space

rm(list = ls())

#### ---------------------------
#Set output directorypaths

outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/Sygen'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen'


#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

#load original sygen medication dataset
sygen.medication.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_medications/masterfile/masterfile.csv", header = T, sep = ',')
names(sygen.medication.data)


#-------------------------Prepare data file for analysis------------------------------------------------------------------------------------------------------


#Calculate point prevalance of medication administration (i.e., number of medications administered per day per patient)

#Make copy of data file to work with
sygen.medication.data.2 <- sygen.medication.data


#Replace all values greater than 0 with a 1 and all na's will be replaced with a 0
sygen.medication.data.2[sygen.medication.data.2>0] <- 1
sygen.medication.data.2[is.na(sygen.medication.data.2)] <- 0 

#change columns to numerics class format
cols_to_change = c(4:6)    
for(i in cols_to_change){
  aggregate(sygen.medication.data.2[,i], by=list(Category=sygen.medication.data.2$generic_name), FUN=sum)
}

#Subset Data 
sygen.medication.data.2.subset <- sygen.medication.data.2[c(4:64)]

#Aggregate data: Number of medications per day for each patient
new_tab_pid<-aggregate(sygen.medication.data.2.subset[-1], sygen.medication.data.2["NEW_ID"], FUN=sum)

#Reformat data from wide to long
new_tab_pid_long <- gather(new_tab_pid, day, prevalence, X1:X60, factor_key=TRUE)
new_tab_pid_long 

#change columns 4:368 to numerics class format
cols_to_change = c(2:3)    
for(i in cols_to_change){
  class(new_tab_pid_long[, i]) = "numeric"
}

##Replace 0s with na
new_tab_pid_long_withna<- new_tab_pid_long %>% replace_with_na(replace = list(prevalence = 0))

#Plot point prevalence
point.prevalence.sygen <-ggplot(data=new_tab_pid_long_withna, aes(x = day, y = NEW_ID, fill = prevalence)) +
  viridis::scale_fill_viridis(name="Number of \nMedications Administered",
                              option = 'plasma',
                              direction = 1,
                              na.value = "white") +
  geom_tile(color = 'white', size = 0.1) + scale_x_continuous(
    expand = c(0, 0), breaks = c(0, 30,60)) +
  ggtitle("Sygen trial (n=791)")+ labs(x="Days Post-Injury", y="")+ theme_classic()+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),axis.title.x = element_text(size = 10) ,axis.text.x = element_text(color="black", size=8),  axis.ticks.y =element_blank(),  axis.text.y = element_blank(), axis.title.y  = element_blank())

point.prevalence.sygen









#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####
