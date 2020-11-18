##### Code created by C. Jutzeler Noveber 10th, 2020
##### Cohort: Sygen study
#### Summary statistics of cohorts


#Clear workspace
rm(list = ls())

#Set local drive
Sys.setlocale(category = "LC_CTYPE", locale = "C")

#The following commands will install these packages if they are not already installed:

if(!require(magrittr)){install.packages("magrittr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(gghighlight)){install.packages("gghighlight")}


#List of libraries required for the analyses below
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr) 
library(ggplot2)
library(gghighlight)
library(data.table)
library(table1)

#where libraries are stored
.libPaths()

#paths
outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables'


#-------------------------Sygen Data------------------------------------------------------------------------------------------------------
#load original dataset
sygen.original<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/Original_data/df_sygen_formatted.csv", sep = ',', header = T,  na.strings=c("","NA"))

sygen.pid <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Data/Sygen/PID/pid_sygen.csv", sep = ',', header = T,  na.strings=c("","NA"))

joined_df <- merge(sygen.pid, sygen.original, by.x = "NEW_ID", 
                   by.y = "ID", all.x = TRUE, all.y = FALSE)


joined_df2 <- subset(joined_df, Time==4)
sygen<-distinct(joined_df, NEW_ID, .keep_all = TRUE)


sygen$YEARDOI <- as.factor(sygen$YEARDOI)

#Create Summary Table
#Formatting of table: Customize levels, labels, and units of listed variables
#Change names of levels of variables
levels(sygen$Sex) <- c("Female", "Male")
levels(sygen$AIS) <- c("A", "B", "C", "D")
levels(sygen$NLI) <- c("Cervical", "Thoracic")
levels(sygen$Cause) <- c("Automobile", "Blunt trauma", "Fall", "Gunshot wound", "Motorcycle", "Other sports", "Others", "Pedestrian", "Water-related")

#Relable variables
label(sygen$Sex) <- "Sex, n (%)"
label(sygen$Age) <- "Age"
# label(emsci.trauma.sex.va.a1$NLI_level)<- "Neurological level of injury"
label(sygen$YEARDOI) <- "Year of injury, n (%)"
label(sygen$AIS) <- "AIS, n (%)"
label(sygen$NLI) <- "Neurological level of injury, n (%)"
label(sygen$Cause) <- "Cause, n (%)"

#Assign units to Age at Injury and Year of Injury
units(sygen$Age) <- "years"

#Print table
table1::table1(~ Sex+Age+AIS+NLI+Cause+YEARDOI, data = sygen)

