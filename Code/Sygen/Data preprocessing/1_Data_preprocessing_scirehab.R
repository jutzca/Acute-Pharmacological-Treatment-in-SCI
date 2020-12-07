
# ---------------------------
##
## Script name: 1_Data_preprocessing_scirehab
##
## Purpose of script: Data preprocessing of the scirehab data
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2020-12-7
##
## Copyright (c) Catherine Jutzeler, 2020
## Email: catherine.jutzeler@bsse.ethz.ch
##
## ---------------------------
##
## Data source: SCIRehab Data
##
## Notes: This analysis is for the publication Jutzeler et al, 2021 published in XX
##   
#### ---------------------------

## set working directory

setwd("/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/")

Sys.setlocale(category = "LC_CTYPE", locale = "C")

## ---------------------------
## load up the packages we will need:  
library(plyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(hablar)
library(dplyr)
library(data.table)


## ----------------------------
## Install packages needed:  (uncomment as required)

#if(!require(plyr)){install.packages("plyr")}
#if(!require(tidyr)){install.packages("tidyr")}
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(reshape2)){install.packages("reshape2")}
#if(!require(hablar)){install.packages("hablar")}
#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(data.table)){install.packages("data.table")}

#### ---------------------------
#Clear working space

rm(list = ls())

#### ---------------------------
#Set output directorypaths

outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/SCI_Rehab'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/SCI_Rehab'


#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

#Load original data
drug <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Drug_addep/masterfile/catherine_drug_file_new.csv", header = T, sep = ',')

##Add character c to ID
drug$newid<- sprintf('C%i', drug$newid)

drug <- drug[order(drug$generic_name),] 

drug_split <- split(drug, drug$generic_name)


new_names <- as.character(unique(drug$generic_name))

for (i in 1:length(drug_split)) {
  assign(new_names[i],  drug_split[[i]])
  write.csv(drug_split[[i]], paste("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Drug_addep/data/",new_names[i],".csv"))
}



#--- Set working directory
setwd("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Drug_addep/data/")


#all files in folder
file_list <- list.files()

for(file in file_list)    #repeat for all files in dir folder
{
  data <- read.csv(file, header=TRUE, sep=',')
  
  data <-select(data,-c(1)) #remove first column as it is not needed
  
  drug_name <- unique(data$generic_name) #extract name of drug 
  
  cols_to_change = c(2:368)    #change columns 2:368 to numerics class format
  for(i in cols_to_change){
    class(data[, i]) = "numeric"
  }
  
  
  data_new<-ddply( data,.(newid, generic_name), function(x) colSums(x[,-1], na.rm = TRUE))
  
  dataset <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Drug_addep/pid/pid.csv", header = T, sep = ',')
  df_final <-merge(data_new, dataset, by="newid", all.y =TRUE ) #extend NEW_ID to all 791 IDs
  
  
  df_final$generic_name <- drug_name #add drug name to all 791 IDs
  
  df_final[df_final==0] <- NA #replace NAs with Os
  
  df_final2 <-  df_final[order(df_final$newid),] 
  
  
  write.csv(df_final2, paste("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Drug_addep/new/",file)) #export each merged and modified file
  
}

###merge all drug files

setwd("/Users/localadmin/Documents/5_R/Drug_addep/new/")


file_list2 <- list.files()

all_files <- Reduce(rbind, lapply(file_list2, read.csv))

data <-select(all_files,-c(1))

data <-  data[order(data$newid),] 

write.csv(data, paste("/Users/localadmin/Documents/5_R/Drug_addep/masterfile/masterfile.csv")) #export each merged and modified file

View(data)

#####Change exposed to 1 and non-exposed to 0

df_test <- read.csv("/Users/localadmin/Documents/5_R/Drug_addep/masterfile/masterfile.csv", sep=',', header = TRUE)


names(df_test)

cols_to_change = c(4:368)    #change columns 4:368 to numerics class format
for(i in cols_to_change){
  class(df_test[, i]) = "numeric"
}




##Every valid dose entry will be converted to a 1 and NAs will be set 0
df_test[df_test>0] <- 1
df_test[is.na(df_test)] <- 0 


a <-sum(as.numeric(df_test$X0), na.rm = TRUE)

a <- df_test$X0
c <- colSums(df_test$X0)
c <- colSums(df_test[-1])


for(var in colnames(df_test)[-c(1:3)]) {
  df[[paste0(var, '_t')]] <- sum(df_test[[var]])
}



df_test_30days <- df_test[c(1:36)]

df_test_30days$exposure=NA

df_test_30days$nr_of_days_exposed=rowSums(df_test_30days[c(4:36)])

df_test_30days$exposure_status <- ifelse(df_test_30days$nr_of_days_exposed > 0, 1, ifelse((df_test_30days$nr_of_days_exposed == 0), 0, "fair"))

head(df_test_30days)


##Create x.dat file with drugs as rows and PID as columns
df_final_30days <- df_test_30days[c(2,3,39)]
names(df_final_30days)

df_final_30days<- df_final_30days[order(df_final_30days$newid),]

df_final_30days_wide <-reshape(df_final_30days , idvar = "generic_name", timevar = "newid", direction = "wide")
names(df_final_30days_wide) <- gsub("exposure_status.", "", names(df_final_30days_wide))

write.csv(df_final_30days_wide, "/Users/localadmin/Documents/5_R/Drug_addep/masterfile/masterfile_30_Days.csv")


##Every valid dose entry will be converted to a 1 and NAs will be set 0
df_test[df_test>0] <- 1
df_test[is.na(df_test)] <- 0 

df_test_7days <- df_test[c(1:11)]

df_test_7days$exposure=NA

df_test_7days$nr_of_days_exposed=rowSums(df_test_7days[c(4:11)])

df_test_7days$exposure_status <- ifelse(df_test_7days$nr_of_days_exposed > 0, 1, ifelse((df_test_7days$nr_of_days_exposed == 0), 0, "fair"))

head(df_test_7days)


##Create x.dat file with drugs as rows and PID as columns
df_final_7days <- df_test_7days[c(2,3,14)]
names(df_final_7days)

df_final_7days<- df_final_7days[order(df_final_7days$newid),]

df_final_7days_wide <-reshape(df_final_7days , idvar = "generic_name", timevar = "newid", direction = "wide")
names(df_final_7days_wide) <- gsub("exposure_status.", "", names(df_final_7days_wide))

write.csv(df_final_7days_wide, "/Users/localadmin/Documents/5_R/Drug_addep/masterfile/masterfile_7_Days.csv")



##Every valid dose entry will be converted to a 1 and NAs will be set 0
df_test[df_test>0] <- 1
df_test[is.na(df_test)] <- 0 

df_test_14days <- df_test[c(1:18)]

df_test_14days$exposure=NA

df_test_14days$nr_of_days_exposed=rowSums(df_test_14days[c(4:18)])

df_test_14days$exposure_status <- ifelse(df_test_14days$nr_of_days_exposed > 0, 1, ifelse((df_test_14days$nr_of_days_exposed == 0), 0, "fair"))

head(df_test_14days)


##Create x.dat file with drugs as rows and PID as columns
df_final_14days <- df_test_14days[c(2,3,21)]
names(df_final_14days)

df_final_14days<- df_final_14days[order(df_final_14days$newid),]

df_final_14days_wide <-reshape(df_final_14days , idvar = "generic_name", timevar = "newid", direction = "wide")
names(df_final_14days_wide) <- gsub("exposure_status.", "", names(df_final_14days_wide))

write.csv(df_final_14days_wide, "/Users/localadmin/Documents/5_R/Drug_addep/masterfile/masterfile_14_Days.csv")






