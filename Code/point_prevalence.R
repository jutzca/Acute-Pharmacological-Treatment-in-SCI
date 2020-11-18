##### Code created by C. Jutzeler November 10th, 2020
####Calculation and visualization of point prevalence of drugs (number of patients per drug per day) in addep and sygen datasets.

#Clear workspace
rm(ls=list())

#The following commands will install these packages if they are not already installed:

#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(tidyr)){install.packages("tidyr")}
#if(!require(tidyverse)){install.packages("tidyverse")}
#if(!require(hrbrthemes)){install.packages("hrbrthemes")}
#if(!require(formattable)){install.packages("formattable")}
#if(!require(viridis)){install.packages("viridis")}
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(ggpubr)){install.packages("ggpubr")}
#if(!require(naniar)){install.packages("naniar")}

#List of libraries required for the analyses below
library("dplyr") 
library(tidyr)
library(tidyverse)
library(hrbrthemes)
library(formattable)
library(viridis)
library(ggplot2)
library(ggpubr)
library(naniar)

#where libraries are stored
.libPaths()

#paths
outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables'


#-------------------------SCIRehab Data------------------------------------------------------------------------------------------------------

#load original scirehab drug dataset
scirehab.drug.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Drug_addep/masterfile/masterfile.csv", header = T, sep = ',')
names(scirehab.drug.data)


#-------------------------Prepare data file for analysis------------------------------------------------------------------------------------------------------

#Make copy of data file to work with
scirehab.drug.data.2 <- scirehab.drug.data

#Replace all values greater than 0 with a 1 and all na's will be replaced with a 0
scirehab.drug.data.2[scirehab.drug.data.2>0] <- 1
scirehab.drug.data.2[is.na(scirehab.drug.data.2)] <- 0 

#change columns to numerics class format
cols_to_change = c(4:6)    
for(i in cols_to_change){
  aggregate(scirehab.drug.data.2[,i], by=list(Category=scirehab.drug.data.2$generic_name), FUN=sum)
}

#Subset Data 
scirehab.drug.data.2.subset <- scirehab.drug.data.2[c(4:64)]

#Aggregate data: Number of drugs per day for each patient
new_tab_pid<-aggregate(scirehab.drug.data.2.subset[-1], scirehab.drug.data.2["newid"], FUN=sum)

#Reformat data from wide to long
new_tab_pid_long <- gather(new_tab_pid, day, prevalence, X1:X60, factor_key=TRUE)
new_tab_pid_long 


#Add new column with prevalence measure: Number of subjetcs in addep dataset = 1257
new_tab_pid_long$prevalence1 <- (new_tab_pid_long$prevalence/1257)*100

#change columns 4:368 to numerics class format
cols_to_change = c(2:3)    
for(i in cols_to_change){
  class(new_tab_pid_long[, i]) = "numeric"
}

##Replace 0s with na
new_tab_pid_long_withna<- new_tab_pid_long %>% replace_with_na(replace = list(prevalence1 = 0))

#Plot point prevalence
point.prevalence.scirehab <-ggplot(data=new_tab_pid_long_withna, aes(x = day, y = newid, fill = prevalence1)) +
  viridis::scale_fill_viridis(name="Number of \nMedications Administered",
                              option = 'plasma',
                              direction = 1,
                              na.value = "white") +
  geom_tile(color = 'white', size = 0.1) + scale_x_continuous(
    expand = c(0, 0), breaks = c(0, 30,60)) +
  ggtitle("SCI Rehab Study (n=1257)")+ labs(x="Days Post-Injury", y="")+ theme_classic()+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),axis.title.x = element_text(size = 10) ,axis.text.x = element_text(color="black", size=8),  axis.ticks.y =element_blank(),  axis.text.y = element_blank(), axis.title.y  = element_blank())

point.prevalence.scirehab






#-------------------------Sygen Data------------------------------------------------------------------------------------------------------

#load original sygen drug dataset
sygen.drug.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/masterfile.csv", header = T, sep = ',')
names(sygen.drug.data)


#-------------------------Prepare data file for analysis------------------------------------------------------------------------------------------------------

#Make copy of data file to work with
sygen.drug.data.2 <- sygen.drug.data


#Replace all values greater than 0 with a 1 and all na's will be replaced with a 0
sygen.drug.data.2[sygen.drug.data.2>0] <- 1
sygen.drug.data.2[is.na(sygen.drug.data.2)] <- 0 

#change columns to numerics class format
cols_to_change = c(4:6)    
for(i in cols_to_change){
  aggregate(sygen.drug.data.2[,i], by=list(Category=sygen.drug.data.2$generic_name), FUN=sum)
}

#Subset Data 
sygen.drug.data.2.subset <- sygen.drug.data.2[c(4:64)]

#Aggregate data: Number of drugs per day for each patient
new_tab_pid<-aggregate(sygen.drug.data.2.subset[-1], sygen.drug.data.2["NEW_ID"], FUN=sum)

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



