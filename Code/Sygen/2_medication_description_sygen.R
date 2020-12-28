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
  sygen.medication.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/masterfile.csv", header = T, sep = ',')
  names(sygen.medication.data)
  
  
  #-------------------------Calculate and visualize point prevalance of medication administration (i.e., number of medications administered per day per patient)-----------------
  
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
  
  
  #-------------------------Calculate and visualize number of drugs per patient with 7,14, and 30 days respectively-----------------
  
  #Make copy of data file to work with
  sygen.medication.data.3 <- sygen.medication.data
  
  
  #Replace all values greater than 0 with a 1 and all na's will be replaced with a 0
  sygen.medication.data.3[sygen.medication.data.3>0] <- 1
  sygen.medication.data.3[is.na(sygen.medication.data.3)] <- 0 
  
  #change columns to numerics class format
  cols_to_change = c(4:6)    
  for(i in cols_to_change){
    aggregate(sygen.medication.data.3[,i], by=list(Category=sygen.medication.data.3$generic_name), FUN=sum)
  }
  
  #Subset Data for 60 days post injury
  sygen.medication.data.3.subset <- sygen.medication.data.3[c(4:64)]
  
  #Aggregate data: Number of medications per day for each patient
  sygen_medication_wide<-aggregate(sygen.medication.data.3.subset[-1],by=list(sygen.medication.data.3$generic_name, sygen.medication.data.3$NEW_ID), FUN=sum)
  
  
  ####------ Calcualte number of patients
  sygen_medication_wide.sum = sygen_medication_wide%>%rowwise%>% dplyr::mutate(sum_7_days = sum(c(X1,X2,X3,X4,X5,X6,X7)))
  sygen_medication_wide.sum =sygen_medication_wide.sum %>% rowwise%>% dplyr::mutate(sum_14_days = sum(c(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14)))
  sygen_medication_wide.sum =sygen_medication_wide.sum %>% rowwise%>% dplyr::mutate(sum_30_days = sum(c(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30)))
  sygen_medication_wide.sum =sygen_medication_wide.sum %>% rowwise%>% dplyr::mutate(sum_60_days = sum(c(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X1,
                                                                                                        X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,
                                                                                                        X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60)))
  
  
  #Covert all numbers greater than 1 to 1
  sygen_medication_wide.sum$sum_7_days[sygen_medication_wide.sum$sum_7_days>0] <- 1
  sygen_medication_wide.sum$sum_14_days[sygen_medication_wide.sum$sum_14_days>0] <- 1
  sygen_medication_wide.sum$sum_30_days[sygen_medication_wide.sum$sum_30_days>0] <- 1
  sygen_medication_wide.sum$sum_60_days[sygen_medication_wide.sum$sum_60_days>0] <- 1
  
  ####------ 7-day average and standard deviation
  #Subset data
  sygen_medication_wide.sum.subset.7d <- sygen_medication_wide.sum[c(1,2,63)]
  
  #Calcualte number of unique drugs given per patient within first 7 days post injury
  x7_days_mean <- sygen_medication_wide.sum.subset.7d %>% 
    group_by(Group.2)%>% 
    summarise(Frequency.7days = sum(sum_7_days))
  
  #Calculate mean and sd for 7 days
  mean(x7_days_mean$Frequency.7days)
  sd(x7_days_mean$Frequency.7days)
  min(x7_days_mean$Frequency.7days)
  max(x7_days_mean$Frequency.7days)
  
  ####------ 14-day average and standard deviation
  
  #Subset data
  sygen_medication_wide.sum.subset.14d <- sygen_medication_wide.sum[c(1,2,64)]
  
  #Calcualte number of unique drugs given per patient within first 14 days post injury
  x14_days_mean <- sygen_medication_wide.sum.subset.14d %>% 
    group_by(Group.2)%>% 
    summarise(Frequency.14days = sum(sum_14_days))
  
  #Calculate mean and sd for 14 days
  mean(x14_days_mean$Frequency.14days)
  sd(x14_days_mean$Frequency.14days)
  min(x14_days_mean$Frequency.14days)
  max(x14_days_mean$Frequency.14days)
  
  
  ####------ 30-day average and standard deviation
  
  #Subset data
  sygen_medication_wide.sum.subset.30d <- sygen_medication_wide.sum[c(1,2,65)]
  
  #Calcualte number of unique drugs given per patient within first 30 days post injury
  x30_days_mean <- sygen_medication_wide.sum.subset.30d %>% 
    group_by(Group.2)%>% 
    summarise(Frequency.30days = sum(sum_30_days))
  
  #Calculate mean and sd for 30 days
  mean(x30_days_mean$Frequency.30days)
  sd(x30_days_mean$Frequency.30days)
  min(x30_days_mean$Frequency.30days)
  max(x30_days_mean$Frequency.30days)
  
  ####------ 60-day average and standard deviation
  
  #Subset data
  sygen_medication_wide.sum.subset.60d <- sygen_medication_wide.sum[c(1,2,66)]
  
  #Calcualte number of unique drugs given per patient within first 60 days post injury
  x60_days_mean <- sygen_medication_wide.sum.subset.60d %>% 
    group_by(Group.2)%>% 
    summarise(Frequency.60days = sum(sum_60_days))
  
  #Calculate mean and sd for 60 days
  mean(x60_days_mean$Frequency.60days)
  sd(x60_days_mean$Frequency.60days)
  min(x60_days_mean$Frequency.60days)
  max(x60_days_mean$Frequency.60days)
  
  ####------ Plot the average drugs per 7, 14, 30, and 60 days
  
  
  sygen_medication.plot <-cbind(x7_days_mean,x14_days_mean[c(2)],x30_days_mean[c(2)],x60_days_mean[c(2)])
  
  #Wide to long format
  data_long <- gather(sygen_medication.plot, condition, measurement, Frequency.7days:Frequency.60days, factor_key=TRUE)
  data_long
  
  library(plyr)
  data_long$condition<-revalue(data_long$condition, c("Frequency.7days"="7 Days", "Frequency.14days"="14 Days","Frequency.30days"="30 Days","Frequency.60days"="60 Days"))
  
  
  prevalence.plot <- data_long%>%ggplot( aes(x=condition, y=measurement, fill=condition)) +
    geom_violin(trim=FALSE)+
    geom_boxplot(width=0.1, fill="white")+  #scale_fill_viridis(discrete = TRUE, option='plasma', direction =-1) +
    theme_classic() +
    theme(
      legend.position="none",
      axis.title.y = element_text(size=10)
    ) +scale_fill_brewer(palette="Blues") + 
    xlab("")+ylab("Average Number of Medications")
  prevalence.plot
  
  
  ##Save plot
  ggsave(
    "drug.prevalence.7-14-30-60days.plot.sygen.pdf",
    plot = prevalence.plot,
    device = 'pdf',
    path = outdir_figures,
    scale = 1,
    width = 4,
    height = 4,
    units = "in",
    dpi = 300
  )
  
  dev.off()
  
  
  
  
  #### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####
  
  
