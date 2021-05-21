## ---------------------------
##
## Script name: 4_Visualization_of_network_graph_scirehab
##
## Purpose of script: To create the network graph
##
## Author: Dr. Catherine Jutzeler
##
## Date Created: 2021-2-26
##
## Copyright (c) Catherine Jutzeler, 2021
## Email: catherine.jutzeler@bsse.ethz.ch
##
## ---------------------------
##
## Data source: SCIRehab Observational Study Drug Data
##
## Notes: For the publication in XXX
##   
## ---------------------------
##
## load up the packages we will need:  
##
library(dplyr)
library(tidyr)
library(tibble)
library(tidyverse)
# library(ggnet)
library(network)
library(sna)
library(ggplot2)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)
library(networkD3)

##
## ----------------------------
##
## Install packages needed:  (uncomment as required)
##
# if(!require(dplyr)){install.packages("dplyr")}
# if(!require(tidyr)){install.packages("tidyr")}
# if(!require(tibble)){install.packages("tibble")}
# if(!require(tidyverse)){install.packages("tidyverse")}
# if(!require(ggnet)){install.packages("ggnet")}
# if(!require(network)){install.packages("network")}
# if(!require(sna)){install.packages("sna")}
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(igraph)){install.packages("igraph")}
# if(!require(tidygraph)){install.packages("tidygraph")}
# devtools::install_github("thomasp85/ggraph", dependencies=TRUE)
# if(!require(visNetwork)){install.packages("visNetwork")}
# if(!require(networkD3)){install.packages("networkD3")}
# devtools::install_github('slowkow/ggrepel')

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
outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/SCI_Rehab'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/SCI_Rehab'
##
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load network data
network_data.scirehab<-read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Network_graph/edges_for_graph.scirehab.csv", header = T, sep = ',')

# Data on medication
scirehab.medication.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/drug_addep/masterfile/masterfile.csv", header = T, sep = ',', stringsAsFactors = F)
names(scirehab.medication.data)

#-------------------------Calculate and visualize point prevalence of medication administration (i.e., number of medications administered per day per patient)-----------------

# Make copy of data file to work with
scirehab.medication.data.copy <- scirehab.medication.data

# Subset data to 60 days due to memory issues
scirehab.medication.data.copy.subset <-scirehab.medication.data.copy[, c(1:63)]

# Format file from wide to long
scirehab.information.on.medication.long <- gather(scirehab.medication.data.copy.subset, day, dose, X0:X60, factor_key=TRUE)
scirehab.information.on.medication.long

#Replace all values greater than 0 with a 1 and all na's will be replaced with a 0
scirehab.medication.data.2.long<-scirehab.information.on.medication.long%>%
  tidyr::drop_na("dose")%>% 
  dplyr::mutate_if(is.numeric, ~1 * (. != 0)) %>% 
  dplyr::mutate_if(is.numeric, ~replace_na(., 0))

# Add demographics and injury characteristics
demographics.data.scirehab <-read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Drug_addep/Demographics_injury_charact/rehab_summary_stats_patients_with_C0_to_L2_injuries.csv", sep = ',', header = T,  na.strings=c("","NA"))

# Merge drug file and demographics
scirehab.information.on.medication.long2.extended <- merge(scirehab.medication.data.2.long,demographics.data.scirehab, by="newid")

# Create list with number of patients per drug per day
scirehab.nr.of.patients.per.drug.per.day <- scirehab.information.on.medication.long2.extended %>%
  dplyr::filter(dose != 0)%>%
  dplyr::select(newid,generic_name,day)%>%
  distinct()%>%
  dplyr::group_by(day, generic_name) %>%
  distinct()%>%
  dplyr::select(-"newid")%>%
  dplyr::mutate(n.source = n()) %>%
  dplyr::select(generic_name,day,n.source)%>%
  distinct()
scirehab.nr.of.patients.per.drug.per.day

# Make day a numeric variable
scirehab.nr.of.patients.per.drug.per.day$day <-as.numeric(as.factor(scirehab.nr.of.patients.per.drug.per.day$day))

#---------- Create Network Graph ---------- 

# Create subset of weights, which will be assigned to the node of the graph (node size = number of patients receiving a drug) 
scirehab.nr.of.patients.per.drug.per.day.X7 <- scirehab.nr.of.patients.per.drug.per.day%>% subset(day==7)%>%
  as.data.frame()%>%
  select(-c("day"))

# 1. Node list

# Create source
source <- network_data.scirehab %>% subset(day=="X7" & value >20 )%>% 
  distinct(Source) %>%
  dplyr::rename(label = Source)
head(source)

# Create target
target <- network_data.scirehab %>%subset(day=="X7" & value >20 )%>% 
  distinct(Target) %>%
  dplyr::rename(label = Target)
head(target)

# To create a single dataframe with a column with the unique locations we need to use a full join
nodes <- full_join(source, target, by = "label")
head(nodes)

# To have unique IDs for each city, we add an “id” column
nodes <- nodes %>% rowid_to_column("id")
nodes

nodes2 <- merge(nodes, scirehab.nr.of.patients.per.drug.per.day.X7, by.x = "label", by.y = "generic_name")
nodes2

# 2. Edge list
edge.data <- network_data.scirehab %>%  subset(day=="X7" & value >20 )%>% 
  group_by(Source, Target) %>%
  dplyr::summarise(weight = value) %>% 
  ungroup()
head(edge.data)

edges <- edge.data %>% 
  left_join(nodes, by = c("Source" = "label")) %>% 
  dplyr::rename(from = id) %>% 
  left_join(nodes, by = c("Target" = "label")) %>% 
  dplyr::rename(to = id) %>% 
  select(from, to, weight)
edges

edges$weight.grp <- cut(edges$weight, c(-1,50,100,150),
                        labels=c("0-50", "51-100","l"))

# 3. Creating network objects
set.seed(100)

igraph_layouts <- c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
                    'randomly', 'fr', 'kk', 'drl', 'lgl')

color_list <- c("#FFA500", "#EE6677", "#228833", "#4477AA", "#4B0082")

g <- tbl_graph(nodes2, edges, directed = FALSE)%>%
  mutate(degree = n.source)%>%
  ggraph(layout = "kk") +
  # geom_edge_link(aes(width = weight),
  #                # edge_colour = "red",
  #                position = "identity",
  #                alpha = 0.8,
  #                # colour = 'gray', 
  #                arrow = NULL) +
  geom_edge_link2(aes(
    width = weight),
    color='gray',
    alpha = 0.8)+
  scale_edge_width(range = c(0.1, 2)) +
  # scale_edge_colour_brewer(palette = "Set1")+
  geom_node_point(aes(size = degree),color='red') +
  geom_node_text(aes(#size = n.source, 
    label = label), repel = TRUE, 
    max.overlaps = getOption("ggrepel.max.overlaps", default = 100), family = "Times") +  ggtitle('Day 60')+
  theme_graph(title_size = 18,
              title_face = "bold",
              title_margin = 10)
g




