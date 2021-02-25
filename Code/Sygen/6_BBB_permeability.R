## ---------------------------
##
## Script name: 6_BBB_permeability
##
## Purpose of script: To assess the ability of medications to cross the BBB
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2021-2-22
##
## Copyright (c) Catherine Jutzeler, 2021
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
library(dplyr) 
library(ggplot2)
library(waffle)

##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(waffle)){install.packages("waffle")}
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

# Load original sygen medication dataset
bbb.sygen<- read.csv("/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Data/bbb_permeability.csv", header = T, sep = ',')
names(bbb.sygen)

# Reoder the factors of variable BBB
bbb.sygen$BBB<-factor(bbb.sygen$BBB, levels = c("Yes", "No", "Unknown"))

# 1. Count number of medications that can or can not cross the BBB
bbb.count <- bbb.sygen %>%
  dplyr::count(BBB)%>%
  dplyr::mutate(proportions=(100/775*n))
 bbb.count

# 2. Plot number of medications that can or can not cross the BBB

# Bar plot
 bbb.bar.plot <- ggplot(bbb.count) +
  geom_col(aes(x = BBB, y = proportions, fill=BBB), width = 0.8) +
  scale_fill_brewer(palette = "Blues", direction = 1) +theme_minimal() + theme(text = element_text(size = 16))+
   xlab("Blood brain barrier permeability")+  ylab("Porportions [%]")+ theme(legend.position = 'none', axis.title = element_text(size=12, family = 'Times', color = 'black'), 
                                                                             axis.text = element_text(size=10, family = 'Times', color = 'black'))
 bbb.bar.plot
 
 # Save plot
 ggsave(
   "bbb.bar.plot.pdf",
   plot = bbb.bar.plot,
   device = 'pdf',
   path = outdir_figures,
   scale = 1,
   width = 3,
   height = 3,
   units = "in",
   dpi = 300
 )
 
 dev.off()

# 2. Waffle plot

parts <- c(`Yes`=60,
           `No`=20, 
           `Unknown`=20)

bbb.waffle.plot <- waffle::waffle(parts, rows=5, size=1, colors=c("#b30099", "#1879bf", "#969696"), 
       title="Blood Brain Barrier Permeability", 
       xlab="One square == 1%")+theme(plot.title = element_text(size=12, family = 'Times', face = 'bold'), 
                                      axis.title = element_text(family = "Times"),
                                      legend.text = element_text(family = "Times"))

bbb.waffle.plot

# Save plot
ggsave(
  "bbb.waffle.plot.pdf",
  plot = bbb.waffle.plot,
  device = 'pdf',
  path = outdir_figures,
  scale = 1,
  width = 6,
  height = 8,
  units = "in",
  dpi = 300
)

dev.off()






