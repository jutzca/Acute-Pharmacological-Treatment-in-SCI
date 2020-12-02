## ---------------------------
##
## Script name: 3_prophylactic_drug_use_sygen
##
## Purpose of script: To describe and quantify the number of medications that were administered for prophylaxis or prevention.
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
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr) 
library(ggplot2)
library(gghighlight)

## ----------------------------
## Install packages needed:  (uncomment as required)

# if(!require(magrittr)){install.packages("magrittr")}
# if(!require(dplyr)){install.packages("dplyr")}
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(gghighlight)){install.packages("gghighlight")}

#### ---------------------------
#Clear working space

rm(list = ls())

#### ---------------------------
#Set output directorypaths

outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/Sygen'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen'


#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

#----- Number of unique drugs administered to for prophylaxis of secondary health complications -----
#Load original data set
prevent.indication <- read.csv("Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Prophylactic_drug_use/prophylactic_drug_use_indications.csv", header = T, sep = ',')
names(prevent.indication)


prevent.indication.2 <- prevent.indication  %>%
  count(indication, sort = TRUE)


drugs_per_indication <-ggplot(data=prevent.indication.2, aes(x=reorder(indication,-n),y=n)) +
  geom_bar(stat="identity", position="dodge", color="#D5D8DC", fill="#D5D8DC", width = 0.8)+
  theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), text=element_text(family='Times',size = 12))+ 
  ylab("Number of Prophlyactic Indications")+scale_y_continuous(limits = c(0,1200), expand = c(0,0))+
  geom_text(aes(label=n), vjust=-0.3, size=3, family='Times') 
drugs_per_indication

##Save plot
ggsave(
  "prevention_drugs_per_indication.pdf",
  plot = drugs_per_indication,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 14,
  height = 6,
  units = "in",
  dpi = 300
)

dev.off()


#----- Number of indications per organ system ----- 

#load original dataset
prevent.organsystem <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Prophylactic_drug_use/Unique_drugs_prophylaxis.csv", header = T, sep = ',')
names(prevent.organsystem)

prevent.organsystem.2 <- prevent.organsystem  %>%
  count(Organ_system, sort = TRUE)


drugs_per_orgsystem <-ggplot(data=prevent.organsystem.2, aes(x=reorder(Organ_system,-n),y=n)) +
  geom_bar(stat="identity", position="dodge", color="#808B96", fill="#808B96", width = 0.8)+
  theme_classic()+theme(axis.text.x = element_text(angle = 60, hjust = 1), axis.title.x = element_blank(), text=element_text(family='Times',size = 12))+ ylab("Number of Drugs")+scale_y_continuous(limits = c(0,80), expand = c(0,0))+
  geom_text(aes(label=n), vjust=-0.3, size=3, family='Times')
drugs_per_orgsystem


##Save plot
ggsave(
  "prevention_drugs_per_orgsystem.pdf",
  plot = drugs_per_orgsystem,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

dev.off()


#----- Number of indications per drugs ----- 


prevent.indication <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Prophylactic_drug_use/prophylactic_drug_use_indications.csv", header = T, sep = ',')
names(prevent.indication)


prevent.indication.per.drug <- as.data.frame( prevent.indication  %>%
  count(generic_name, sort = TRUE))%>%
  filter(n>14)

prevent.indication.per.drug.plot <-ggplot(data=prevent.indication.per.drug, aes(x=reorder(generic_name,-n),y=n)) +
  geom_bar(stat="identity", position="dodge", color="#2C3E50", fill="#2C3E50", width = 0.8)+
  theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), text=element_text(family='Times',size = 12))+ ylab("Number of Indications")+scale_y_continuous(limits = c(0,500), expand = c(0,0))+
  geom_text(aes(label=n), vjust=-0.3, size=3, family='Times')

prevent.indication.per.drug.plot




##Save plot
ggsave(
  "prevention.indication.per.drug.plotn.pdf",
  plot = prevent.indication.per.drug.plot,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 14,
  height = 6,
  units = "in",
  dpi = 300
)

dev.off()

#----- Number of patients that received prophylactic treatment per organ system ----- 

prevent.indication <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Prophylactic_drug_use/prophylactic_drug_use_indications.csv", header = T, sep = ',')
names(prevent.indication)

#Number of patients per indication

prevent.indication.drugs.per.patient <- prevent.indication  %>%
  count(X...NEW_ID, indication, sort = TRUE)  %>%
  count(indication, sort = TRUE)


prevent.indication.drugs.per.patient.plot <-ggplot(data=prevent.indication.drugs.per.patient, aes(x=reorder(indication,-n),y=n)) +
  geom_bar(stat="identity", position="dodge", color="#212F3C", fill="#212F3C", width = 0.8)+
  theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), text=element_text(family='Times',size = 12))+ ylab("Number of Patients")+scale_y_continuous(limits = c(0,520), expand = c(0,0))+
  geom_text(aes(label=n), vjust=-0.3, size=3, family='Times')

prevent.indication.drugs.per.patient.plot



#----- Combine all plots ----- 

library(ggpubr)
prevention.combined.plot <-ggarrange(drugs_per_indication, drugs_per_orgsystem, prevent.indication.per.drug.plot,prevent.indication.drugs.per.patient.plot,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2, align = "h")
prevention.combined.plot


##Save plot
ggsave(
  "prevention.combined.plot.pdf",
  plot = prevention.combined.plot,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 24,
  height = 20,
  units = "in",
  dpi = 300
)

dev.off()


#### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####

