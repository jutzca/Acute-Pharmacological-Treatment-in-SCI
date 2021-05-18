## ---------------------------
##
## Script name: 7_Visualization_of_network_graph_sygen
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
## Data source: Sygen Clinical Trial Drug Data
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
outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/Sygen'
outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen'
##
##
#### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####

# Load data

network_data<-read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Network_graph/edges_for_graph.csv", header = T, sep = ',')

information.on.medication <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/df_drugs_indication_per_day2.csv")

# Format file from wide to long
information.on.medication.long <- gather(information.on.medication, day, dose, X0:X365, factor_key=TRUE)
information.on.medication.long 

# Replace all values greater than 0 with a 1 and all na's will be replaced with a 0
information.on.medication.long2<-information.on.medication.long %>%
  dplyr::mutate_if(is.numeric, ~1 * (. != 0)) %>% 
  dplyr::mutate_if(is.numeric, ~replace_na(., 0))


# Add demographics and injury characteristics
demographics.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/demographics_injury_characteristics2.csv", header=T, sep = ',')

information.on.medication.long2.extended <- merge(information.on.medication.long2,demographics.data, by="NEW_ID")


write.csv(information.on.medication.long2.extended,"/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/information.on.medication.long.csv", row.names = F )

# Create list with number of patients per drug per day

nr.of.patients.per.drug.per.day <- information.on.medication.long2 %>%
  dplyr::filter(dose != 0)%>%
  dplyr::select(-"indication")%>%
  dplyr::group_by(day, generic_name) %>%
  distinct()%>%
  dplyr::mutate(n.source = n()) %>%
  dplyr::select(-"NEW_ID")%>%
  distinct()%>%
  dplyr::mutate_if(is.numeric, ~1 * (. != 0)) %>% 
  dplyr::mutate_if(is.numeric, ~replace_na(., 0))%>%
  dplyr::count(day)
nr.of.patients.per.drug.per.day


nr.of.patients.per.drug.per.day$day <-as.numeric(as.factor(nr.of.patients.per.drug.per.day$day))

plot_ly(nr.of.patients.per.drug.per.day, x = ~day, y = ~n, type = 'bar')


# Create list with number of patients per drug per day per indication
nr.of.patients.per.drug.per.day.per.indiction <- information.on.medication.long2 %>%
  dplyr::filter(dose != 0)%>%
  # dplyr::select(-"indication")%>%
  dplyr::group_by(day, generic_name, indication) %>%
  distinct()%>%
  dplyr::mutate(n.source = n(),
                mean = mean(dose,na.rm=TRUE),
                median = median(dose, na.rm=TRUE),
                sd = sd(dose,na.rm=TRUE),
                max = max(dose, na.rm=TRUE),
                min = min(dose, na.rm=TRUE)
  ) 

nr.of.patients.per.drug.per.day.per.indiction






nr.of.patients.per.drug.per.day.X7 <- nr.of.patients.per.drug.per.day%>% subset(day=="X60")%>%
  as.data.frame()%>%
  select(-c("day", "dose"))


# 1. Node list

# Create source
source <- network_data %>% subset(day=="X60" & value >10 )%>% 
  distinct(Source) %>%
  dplyr::rename(label = Source)
head(source)

# Create target
target <- network_data %>%subset(day=="X60" & value >20 )%>% 
  distinct(Target) %>%
  dplyr::rename(label = Target)
names(target)

# To create a single dataframe with a column with the unique locations we need to use a full join
nodes <- full_join(source, target, by = "label")
names(nodes)

# To have unique IDs for each city, we add an “id” column
nodes <- nodes %>% rowid_to_column("id")
nodes

nodes2 <- merge(nodes, nr.of.patients.per.drug.per.day.X7, by.x = "label", by.y = "generic_name")
nodes2

# 2. Edge list
edge.data <- network_data %>%  subset(day=="X60" & value >20 )%>% 
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

# 
# #---------- network graph ---------- 
# # Create network
# routes_network <- network::network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)
# routes_network
# 
# # Class network
# class(routes_network)
# 
# # Plot network
# plot(routes_network, vertex.cex = 3)        # or plot(routes_network, vertex.cex = 3, mode = "circle")
# 
# #---------- igraph ---------- 
# detach(package:network)
# rm(routes_network)
# 
# # Create igraph
# routes_igraph <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
# 
# # Plot igraph
# plot(routes_igraph, edge.arrow.size = 0.2)
# 
# 
# #---------- tidygraph and ggraph ---------- 
# # To create a network object 
# routes_tidy <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
# 
# # To convert an igraph or network object
# routes_igraph_tidy <- tidygraph::as_tbl_graph(routes_igraph)
# 
# # To verify the classes of objects
# class(routes_tidy)
# class(routes_igraph_tidy)
# class(routes_igraph)
# 
# # To print out the tbl_graph object
# routes_tidy
# 
# # The nodes tibble is activated by default, but you can change which tibble is active with the activate() function.
# routes_tidy %>% 
#   tidygraph::activate(edges) %>% 
#   arrange(desc(weight)) # to rearrange the rows in the edges tibble to list those with the highest “weight” first
# 
# # Plot the graph
# library(ggplot2)
# ggraph::ggraph(routes_tidy) + ggraph::geom_edge_link(aes(alpha = weight)) + ggraph::geom_node_point(aes(color = "red",
#                                                                                                         size = "weight")) + ggraph::theme_graph()
# 
# 
# #---------- networkD3 ---------- 
# # To recreate the current columns, while subtracting 1 from each ID
# nodes_d3 <- dplyr::mutate(nodes, id = id - 1)
# edges_d3 <- dplyr::mutate(edges, from = from - 1, to = to - 1)
# 
# 
# # Create network graph
# networkD3::forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
#              NodeID = "label", Group = "id", Value = "weight", linkDistance=150,linkColour="lightgray",
#              opacity = 1, charge=-300,fontSize = 16, zoom = TRUE, opacityNoHover=1, width = 1000, height = 800)
# 
# #---------- sankeyNetwork ---------- 
# sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
#               NodeID = "label", Value = "weight", fontSize = 16, unit = "Drugs", nodeWidth = 10)
# 
