## ---------------------------
##
## Script name: 3_Data_preprocessing_for_network_graph_scirehab
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
## Data source: SCI Rehab Study
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

outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/SCI_Rehab/'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/SCI_Rehab/'


#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

#load original scirehab medication dataset
scirehab.medication.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/drug_addep/masterfile/masterfile.csv", header = T, sep = ',')
names(scirehab.medication.data)

#-------------------------Calculate and visualize point prevalence of medication administration (i.e., number of medications administered per day per patient)-----------------

#Make copy of data file to work with
scirehab.medication.data.network <- scirehab.medication.data

#Replace all values greater than 0 with a 1 and all na's will be replaced with a 0
scirehab.medication.data.network.2<-scirehab.medication.data.network %>%
  dplyr::mutate_if(is.numeric, ~1 * (. != 0)) %>% 
  dplyr::mutate_if(is.numeric, ~replace_na(., 0))


#Create the vector generic_name
generic_name <- unique(scirehab.medication.data.network.2$generic_name)

#Create template for final data frame
df_total.scirehab = data.frame()

#Create loop: For each of the generic names, create a subset and then replace the 1's with the generic drug name.
for (i in 1:length(generic_name)){ 
  temp <- scirehab.medication.data.network.2[scirehab.medication.data.network.2$generic_name==generic_name[i],]
  generic_name2<-as.character(unique(temp$generic_name))
  temp[temp==1] <- generic_name2
  temp[temp==0] <- ''
  df <- temp
  df_total.scirehab <- rbind(df_total.scirehab,df)
}

write.csv(df_total.scirehab, '/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/drug_names_per_day_scirehab_for_network_graph.csv')

#Read file
df_scirehab.for.network <- read.csv('/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/drug_names_per_day_scirehab_for_network_graph.csv', header = T, sep = ',' )
head(df_scirehab.for.network)


#rearrange the data frame from wide to long for the days X0:X365
df_scirehab.for.network_long <- tidyr::gather(df_scirehab.for.network, day, measurement, X0:X365, factor_key=TRUE)
tibble::glimpse(df_scirehab.for.network_long)
head(df_scirehab.for.network_long)

# Subset data
df_scirehab.for.network_long_subset =df_scirehab.for.network_long[,-c(1,3)]

# Remove lines that have no value in the measurement column
df_scirehab.for.network_long_subset_rm_na <- subset(df_scirehab.for.network_long_subset, (!(measurement == '')) )

df_scirehab.for.network_long_subset_rm_na.unique.values <- df_scirehab.for.network_long_subset_rm_na %>% distinct()

# Create the vector generic_name
days <- unique(df_scirehab.for.network_long_subset_rm_na.unique.values$day)

# Create template for final data frame
df_all_days_pooled.scirehab = data.frame()

for (i in 1:length(days)){ 
  temp <- df_scirehab.for.network_long_subset_rm_na.unique.values[df_scirehab.for.network_long_subset_rm_na.unique.values$day==days[i],]
  day_added <-as.character(unique(temp$day))
  
  df <- temp %>% mutate(n = 1) %>% 
    spread(measurement, n, fill=0) %>% 
    select(-newid, -day) %>% 
    {crossprod(as.matrix(.))} %>% 
    replace(lower.tri(., diag=T), NA) %>%
    reshape2::melt(na.rm=T) %>%
    unite('Pair', c('Var1', 'Var2'), sep=", ")
  
  df$day <- day_added
  
  df_all_days_pooled.scirehab <- rbind(df_all_days_pooled.scirehab,df)
}

df_all_days_pooled_scirehab.final <- subset(df_all_days_pooled.scirehab, value>0)


# Create the dataframe for the edges --> separate the pairs
edge_df<-df_all_days_pooled_scirehab.final %>% separate(Pair, c("Source", "Target"), ", ")

# Write file
write.csv(edge_df, '/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Network_graph/edges_for_graph.scirehab.csv', row.names = F)


