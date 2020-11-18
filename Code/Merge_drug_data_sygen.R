##### Code created by C. Jutzeler Noveber 10th, 2020
##### Cohort: Sygen study
#### Data cleaning and preprocessing drug data


library(plyr)
library(tidyr)
library(ggplot2)


Sys.setlocale(category = "LC_CTYPE", locale = "C")

#------Create individual data files per drug ------

#Load the data set
drug <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/df_drugs_indication_per_day.csv", header = T, sep = ',')

drug <-select(drug,-c(3))
 
#Order the data set according to the drug name
drug <- drug[order(drug$generic_name),] 

#Split the drug data set into subsets that only contain a single drug
drug_split <- split(drug, drug$generic_name)
 
#Create unique drug names 
new_names.drugs <- as.character(unique(drug$generic_name))

#Create a file for each drug 
 for (i in 1:length(drug_split)) {
   assign(new_names.drugs[i],  drug_split[[i]]) #assign drug name to the file
   write.csv(drug_split[[i]], paste("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/Drugs/",new_names.drugs[i],".csv", sep=""))
 }
 
#------Create individual data files per drug for all patients ------

###Load data
setwd("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/Drugs/")

#libraries requested
library(plyr)
library(reshape2)
library(hablar)
library(dplyr)
library(data.table)


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
  
  data_new<-ddply(data,.(NEW_ID, generic_name), function(x) colSums(x[,-1], na.rm = TRUE))
  
  dataset <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/PID/pid_sygen.csv", header = T, sep = ',')
  df_final <-merge(data_new, dataset, by="NEW_ID", all.y =TRUE ) #extend NEW_ID to all 791 IDs

  
  
  df_final$generic_name <- drug_name #add drug name to all 791 IDs
 
  df_final[df_final==0] <- NA #replace NAs with Os
  
  df_final2 <-  df_final[order(df_final$NEW_ID),] 
  
  
  write.csv(df_final2, paste("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/new/",file)) #export each merged and modified file

}

#------Merge all drug file ------
#set pwd
setwd("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/new/")

#select all files from pwd
file_list2 <- list.files()

#Rowbind all files
all_files <- Reduce(rbind, lapply(file_list2, read.csv))

#Remove first column
data.merged <-select(all_files,-c(1))

#Sort data according to NEW_ID
data.merged <-  data.merged[order(data.merged$NEW_ID),] 

#export merged and modified file
write.csv(data.merged, paste("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/Merged_cleaned/merged_cleaned_sygen.csv")) 


#####Change exposed to 1 and non-exposed to 0
df_test <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/Merged_cleaned/merged_cleaned_sygen.csv", sep=',', header = TRUE)

cols_to_change = c(4:368)    #change columns 4:368 to numerics class format
for(i in cols_to_change){
  class(df_test[, i]) = "numeric"
}


##Every valid dose entry will be converted to a 1 and NAs will be set 0
df_test[df_test>0] <- 1
df_test[is.na(df_test)] <- 0 

df_test_30days <- df_test[c(1:36)]

df_test_30days$exposure=NA

df_test_30days$nr_of_days_exposed=rowSums(df_test_30days[c(4:36)])

df_test_30days$exposure_status <- ifelse(df_test_30days$nr_of_days_exposed > 0, 1, ifelse((df_test_30days$nr_of_days_exposed == 0), 0, "fair"))

head(df_test_30days)


##Create x.dat file with drugs as rows and PID as columns
df_final_30days <- df_test_30days[c(2,3,39)]
names(df_final_30days)

df_final_30days<- df_final_30days[order(df_final_30days$NEW_ID),]

df_final_30days_wide <-reshape(df_final_30days , idvar = "generic_name", timevar = "NEW_ID", direction = "wide")
names(df_final_30days_wide) <- gsub("exposure_status.", "", names(df_final_30days_wide))

write.csv(df_final_30days_wide, "/Users/localadmin/Documents/5_R/masterfile/masterfile_30_Days.csv")


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

df_final_7days<- df_final_7days[order(df_final_7days$NEW_ID),]

df_final_7days_wide <-reshape(df_final_7days , idvar = "generic_name", timevar = "NEW_ID", direction = "wide")
names(df_final_7days_wide) <- gsub("exposure_status.", "", names(df_final_7days_wide))

write.csv(df_final_7days_wide, "/Users/localadmin/Documents/5_R/masterfile/masterfile_7_Days.csv")



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

df_final_14days<- df_final_14days[order(df_final_14days$NEW_ID),]

df_final_14days_wide <-reshape(df_final_14days , idvar = "generic_name", timevar = "NEW_ID", direction = "wide")
names(df_final_14days_wide) <- gsub("exposure_status.", "", names(df_final_14days_wide))

write.csv(df_final_14days_wide, "/Users/localadmin/Documents/5_R/masterfile/masterfile_14_Days.csv")













#------Create individual data files per indication ------
#Load the data set
indication <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/df_drugs_indication_per_day.csv", header = T, sep = ',')

indication <-select(indication,-c(2))

#Order the data set according to the indication name
indication <- indication[order(indication$indication),] 

#Split the indication data set into subsets that only contain a single indication
indication_split <- split(indication, indication$indication)

#Create unique indication names 
new_names.indications <- as.character(unique(indication$indication))

#Create a file for each indication 
for (i in 1:length(indication_split)) {
  assign(new_names.indications[i],  indication_split[[i]]) #assign indication name to the file
  write.csv(indication_split[[i]], paste("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/Indications/",new_names.indications[i],".csv", sep=""))
}

#------Create individual data files per indication for all patients ------

#clean working space
rm(list = ls())

###Load data
setwd("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/Indications/")

#libraries requested
library(plyr)
library(reshape2)
library(hablar)
library(dplyr)
library(data.table)


#all files in folder
file_list <- list.files()

for(file in file_list)    #repeat for all files in dir folder
{
  data.indication <- read.csv(file, header=TRUE, sep=',')
  
  data.indication <-select(data.indication,-c(1)) #remove first column as it is not needed
  
  indication_name <- unique(data.indication$indication) #extract name of drug 
  
  cols_to_change = c(2:368)    #change columns 2:368 to numerics class format
  for(i in cols_to_change){
    class(data.indication[, i]) = "numeric"
  }
  
  data.indication.new<-ddply(data.indication,.(NEW_ID, indication), function(x) colSums(x[,-1], na.rm = TRUE))
  
  dataset <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/PID/pid_sygen.csv", header = T, sep = ',')
  df_final <-merge(data.indication.new, dataset, by="NEW_ID", all.y =TRUE ) #extend NEW_ID to all 791 IDs
  
  
  
  df_final$indication <- indication_name #add drug name to all 791 IDs
  
  df_final[df_final==0] <- NA #replace NAs with Os
  
  df_final2 <-  df_final[order(df_final$NEW_ID),] 
  
  
  write.csv(df_final2, paste("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/new_indications/",file)) #export each merged and modified file
  
}

#------Merge all indication files ------
#set pwd
setwd("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/new_indications/")

#select all files from pwd
file_list2 <- list.files()

#Rowbind all files
all_files <- Reduce(rbind, lapply(file_list2, read.csv))

#Remove first column
data.merged <-select(all_files,-c(1))

#Sort data according to NEW_ID
data.merged <-  data.merged[order(data.merged$NEW_ID),] 

#export merged and modified file
write.csv(data.merged, paste("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/Merged_cleaned/merged_cleaned_indications_sygen.csv")) 


#####Change exposed to 1 and non-exposed to 0
df_test <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/Merged_cleaned/merged_cleaned_indications_sygen.csv", sep=',', header = TRUE)

cols_to_change = c(4:368)    #change columns 4:368 to numerics class format
for(i in cols_to_change){
  class(df_test[, i]) = "numeric"
}


##Every valid dose entry will be converted to a 1 and NAs will be set 0
df_test[df_test>0] <- 1
df_test[is.na(df_test)] <- 0 

df_test_30days <- df_test[c(1:34)]

df_test_30days$exposure=NA

df_test_30days$nr_of_days_exposed=rowSums(df_test_30days[c(4:34)])

df_test_30days$exposure_status <- ifelse(df_test_30days$nr_of_days_exposed > 0, 1, ifelse((df_test_30days$nr_of_days_exposed == 0), 0, "fair"))

head(df_test_30days)


data_long <- gather(df_test_30days, condition, measurement, X0:X30, factor_key=TRUE)
data_long


##Create x.dat file with drugs as rows and PID as columns
df_final_30days <- df_test_30days[c(2,3,34)]
names(df_final_30days)

df_final_30days<- df_final_30days[order(df_final_30days$NEW_ID),]

df_final_30days_wide <-reshape(df_final_30days , idvar = "indication", timevar = "NEW_ID", direction = "wide")
names(df_final_30days_wide) <- gsub("indication_status.", "", names(df_final_30days_wide))

write.csv(df_final_30days_wide, "/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/Merged_cleaned/merge_cleaned_indications_30_Days.csv")






library(data.table)  # faster fread() and better weekdays()
library(dplyr)       # consistent data.frame operations
library(purrr)       # consistent & safe list/vector munging
library(tidyr)       # consistent data.frame cleaning
library(lubridate)   # date manipulation
library(countrycode) # turn country codes into pretty names
library(ggplot2)     # base plots are for Coursera professors
library(scales)      # pairs nicely with ggplot2 for plot label formatting
library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.
library(knitr)       # kable : prettier data.frame output



ggplot(data_long, aes(x=condition, y=NEW_ID, fill=as.factor(exposure_status)))+
  geom_tile(color="white", size=0.1)+scale_fill_manual(values = c('gray','red'))+facet_wrap(.~data_long$indication)

names(data_long)
 