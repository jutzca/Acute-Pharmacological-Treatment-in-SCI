## ---------------------------
##
## Script name: 2_medication_desription_scirehab
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
scirehab.medication.data.1 <- scirehab.medication.data

#Replace all values greater than 0 with a 1 and all na's will be replaced with a 0
scirehab.medication.data.2<-scirehab.medication.data.1 %>%
  dplyr::mutate_if(is.numeric, ~1 * (. != 0)) %>% 
  dplyr::mutate_if(is.numeric, ~replace_na(., 0))
  
#change columns to numerics class format
# cols_to_change = c(4:6)    
# for(i in cols_to_change){
#   aggregate(scirehab.medication.data.2[,i], by=list(Category=scirehab.medication.data.2$generic_name), FUN=sum)
# }

#Subset Data 
scirehab.medication.data.2.subset <- scirehab.medication.data.2[c(2:63)]

new_tab_pid_long.scirehab <- gather(scirehab.medication.data.2.subset, day, prevalence, X1:X60, factor_key=TRUE)
new_tab_pid_long.scirehab 



#Aggregate data: Number of medications per day for each patient
new_tab_pid.scirehab<-aggregate(scirehab.medication.data.2.subset[-1], scirehab.medication.data.2["newid"], FUN=sum)

# Subset data according to PIDs

scirehab.pid <-read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Drug_addep/Demographics_injury_charact/rehab_summary_stats_patients_with_C0_to_L2_injuries.csv", sep = ',', header = T,  na.strings=c("","NA"))


new_tab_pid.scirehab.subset <- merge(scirehab.pid, new_tab_pid.scirehab, by.x = "newid", 
                   by.y = "newid", all.x = TRUE, all.y = F)


#Reformat data from wide to long
new_tab_pid_long.scirehab <- gather(new_tab_pid.scirehab.subset, day, prevalence, X0:X60, factor_key=TRUE)
new_tab_pid_long.scirehab 


#Add new column with prevalence measure: Number of subjetcs in addep dataset = 1243
new_tab_pid_long.scirehab$prevalence1 <- (new_tab_pid_long.scirehab$prevalence/1243)*100

#change columns 4:368 to numerics class format
cols_to_change = c(10:12)    
for(i in cols_to_change){
  class(new_tab_pid_long.scirehab[, i]) = "numeric"
}

##Replace 0s with 'NA'
new_tab_pid_long_withna.scirehab<- new_tab_pid_long.scirehab %>% replace_with_na(replace = list(prevalence = 0))

#Plot point prevalence
point.prevalence.scirehab <-ggplot(data=new_tab_pid_long_withna.scirehab, aes(x =day, y = newid, fill = prevalence)) +
  viridis::scale_fill_viridis(name="Number of \nMedications Administered",
                              option = 'plasma',
                              direction = 1,
                              na.value = "white") +
  geom_tile(color = 'white', size = 0.1) + 
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 30,60)) +
  ggtitle("SCI Rehab Study (n=1243)")+ labs(x="Days Post-Injury", y="")+ theme_classic()+
  theme(plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(color="black", size=8),  
        axis.ticks.y =element_blank(),  
        axis.text.y = element_blank(), 
        axis.title.y= element_blank())

point.prevalence.scirehab


#---------- Calculate and visualize the number of medications per day per patients  ---------- 

number.of.drug.perday.scirehab <- new_tab_pid_long.scirehab %>%
  dplyr::group_by(day, AIS) %>%
  dplyr::mutate(n = n(),
                mean = mean(prevalence,na.rm=TRUE),
                median = median(prevalence, na.rm=TRUE),
                sd = sd(prevalence,na.rm=TRUE),
                max = max(prevalence, na.rm=TRUE),
                min = min(prevalence, na.rm=TRUE)
  ) %>%
  dplyr::mutate(sem = sd / sqrt(n - 1),
                CI_lower = mean + qt((1-0.95)/2, n - 1) * sem,
                CI_upper = mean - qt((1-0.95)/2, n - 1) * sem)
number.of.drug.perday.scirehab



number.of.drug.perday.scirehab2 <- number.of.drug.perday.scirehab %>%
  select(mean, median, sd, max, min, day, AIS) %>%
  distinct() %>%
  arrange(day)%>%
  arrange(AIS)
number.of.drug.perday.scirehab2

write.csv(number.of.drug.perday.scirehab2, '/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/SCI_Rehab/number.of.drug.perday.scirehab2.csv')


# Create color list  
color_list <- c("#FFA500", "#EE6677", "#228833", "#4477AA", "#4B0082")

# Create plot  
number.of.drug.perday.scirehab.plot <- ggplot(number.of.drug.perday.scirehab, aes(x=as.numeric(day), y=mean, color = AIS))+
  geom_line(aes(x=as.numeric(day), y=mean, color=AIS), size=1)+
  geom_ribbon(aes(ymin=min,ymax=max,fill=AIS),color="grey",alpha=0.4) +  theme_bw(base_size = 12, base_family = "Times") + xlim(1,60) +
  scale_fill_manual(values=color_list) + scale_color_manual(values=color_list) +
  facet_wrap(.~AIS, ncol = 1)+ theme(legend.position="none", axis.text = element_text(color = 'black'), axis.title = element_text(color = 'black'), strip.text = element_text(color = 'black'))
number.of.drug.perday.scirehab.plot

# Save plot
ggsave(
  "number.of.drug.perday.scirehab.plot.pdf",
  plot = number.of.drug.perday.scirehab.plot,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 3,
  height = 6,
  units = "in",
  dpi = 300
)

dev.off()



#-------------------------Calculate and visualize number of medications per patient with 7,14, and 30 days respectively-----------------

# Make copy of data file to work with
scirehab.medication.data.3 <- scirehab.medication.data

# Replace all values greater than 0 with a 1 and all NA's will be replaced with a 0
scirehab.medication.data.3.without.na<-scirehab.medication.data.3%>% 
  dplyr::mutate_if(is.numeric, ~1 * (. > 0))%>% 
  replace(is.na(.), 0)

# Subset Data for 60 days post injury
scirehab.medication.data.3.without.na.subset <- scirehab.medication.data.3.without.na[c(1:63)]

# Calculate number of patients
scirehab_medication_wide.sum = scirehab.medication.data.3.without.na.subset%>%
  rowwise%>% 
  dplyr::mutate(sum_7_days = sum(c(X0,X1,X2,X3,X4,X5,X6)))

scirehab_medication_wide.sum = scirehab_medication_wide.sum %>% rowwise%>% dplyr::mutate(sum_14_days = sum(c(X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13)))
scirehab_medication_wide.sum = scirehab_medication_wide.sum %>% rowwise%>% dplyr::mutate(sum_30_days = sum(c(X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29)))
scirehab_medication_wide.sum = scirehab_medication_wide.sum %>% rowwise%>% dplyr::mutate(sum_60_days = sum(c(X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,
                                                                                                      X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,
                                                                                                                                                                                              X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59)))
# Covert all numbers greater than 1 to 1
scirehab_medication_wide.sum$sum_7_days[scirehab_medication_wide.sum$sum_7_days>0] <- 1
scirehab_medication_wide.sum$sum_14_days[scirehab_medication_wide.sum$sum_14_days>0] <- 1
scirehab_medication_wide.sum$sum_30_days[scirehab_medication_wide.sum$sum_30_days>0] <- 1
scirehab_medication_wide.sum$sum_60_days[scirehab_medication_wide.sum$sum_60_days>0] <- 1


####------ 7-day average, standard deviation, min,and max
d7_data <-scirehab_medication_wide.sum %>%
  dplyr::group_by(newid) %>% 
  dplyr::summarise(final.sum.7d= sum(sum_7_days))
d7_data

mean(d7_data$final.sum.7d)
sd(d7_data$final.sum.7d)
min(d7_data$final.sum.7d)
max(d7_data$final.sum.7d)


####------ 14-day average, standard deviation, min,and max
d14_data <-scirehab_medication_wide.sum %>%
  dplyr::group_by(newid) %>% 
  dplyr::summarise(final.sum.14d= sum(sum_14_days))
d14_data

mean(d14_data$final.sum.14d)
sd(d14_data$final.sum.14d)
min(d14_data$final.sum.14d)
max(d14_data$final.sum.14d)

####------ 30-day average, standard deviation, min,and max
d30_data <-scirehab_medication_wide.sum %>%
  dplyr::group_by(newid) %>% 
  dplyr::summarise(final.sum.30d= sum(sum_30_days))
d30_data

mean(d30_data$final.sum.30d)
sd(d30_data$final.sum.30d)
min(d30_data$final.sum.30d)
max(d30_data$final.sum.30d)

####------ 60-day average and standard deviation
d60_data <-scirehab_medication_wide.sum %>%
  dplyr::group_by(newid) %>% 
  dplyr::summarise(final.sum.60d= sum(sum_60_days))
d60_data

mean(d60_data$final.sum.60d)
sd(d60_data$final.sum.60d)
min(d60_data$final.sum.60d)
max(d60_data$final.sum.60d)


####------ Plot the average medications per 7, 14, 30, and 60 days

scirehab_medication.plot<-cbind(d7_data,d14_data[-1],d30_data[-1], d60_data[-1])


#Wide to long format
data_long <- gather(scirehab_medication.plot, condition, measurement, final.sum.7d:final.sum.60d, factor_key=TRUE)
data_long

library(plyr)
data_long$condition<-revalue(data_long$condition, c("final.sum.7d"="7 Days", "final.sum.14d"="14 Days","final.sum.30d"="30 Days","final.sum.60d"="60 Days"))


prevalence.plot.scirehab <- data_long%>%ggplot( aes(x=condition, y=measurement, fill=condition)) +
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.07, fill="white")+  #scale_fill_viridis(discrete = TRUE, option='plasma', direction =-1) +
  theme_classic() +
  theme(
    legend.position="none",
    axis.title.y = element_text(size=10)
  ) +scale_fill_brewer(palette="Blues") + 
  xlab("")+ylab("Number of Medications")
prevalence.plot.scirehab


##Save plot
ggsave(
  "medication.prevalence.7-14-30-60days.plot.scirehab.pdf",
  plot = prevalence.plot.scirehab,
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









