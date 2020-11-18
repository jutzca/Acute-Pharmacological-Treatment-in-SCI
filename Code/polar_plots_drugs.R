#--------------Create polar plots--------------#
#Code create by C. Jutzeler 12.3.2020

#Clear workspace
rm(list = ls())

#libraries required
library(dplyr)
library(ggplot2)

#load file
dataframe <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/df2_drugs_freq_disorder.csv", sep = ',', header = T)

#Remove Duplicates based on three rows
dataframe_rm_duplicates <- dataframe[!duplicated(dataframe[c(1:3)]),]

#Count number of drugs per indication
df_1 <- dataframe_rm_duplicates %>%
  count(generic_name, indication, sort = TRUE) %>%
  count(indication, sort = TRUE)

#Plot number of drugs per indication
ggplot(data=df_1, aes(x=indication, y=n, fill=n)) +
  geom_bar(stat='identity')+theme_light() +
  scale_fill_gradient(low='red', high='blue', limits=c(0,150)) +
  theme(axis.title.y=element_text(angle=0))+ coord_polar()+theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

#Count number of patients per indication
df_2 <- dataframe_rm_duplicates %>%
  count(NEW_ID, indication, sort = TRUE)%>%
  count(indication, sort = TRUE)

#Plotnumber of patients per indication
ggplot(data=df_2, aes(x=indication, y=n, fill=n)) +
  geom_bar(stat='identity')+theme_light() +
  scale_fill_gradient(low='yellow', high='red', limits=c(0,800)) +
  theme(axis.title.y=element_text(angle=0)) + coord_polar()+theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

                        