  ## ---------------------------
  ##
  ## Script name: 3_Data_preprocessing_for_network_graph_sygen
  ##
  ## Purpose of script: To create a dataframe to later create the edges and nodes for the network graph
  ##
  ## Author: Dr. Catherine Jutzeler
  ##
  ## Date Created: 2020-12-13
  ##
  ## Copyright (c) Catherine Jutzeler, 2020
  ## Email: catherine.jutzeler@bsse.ethz.ch
  ##
  ## ---------------------------
  ##
  ## Data source: Sygen Clinical Trial Drug Data
  ##
  ## Notes: For the publication in XXX
  ##   
  #### ---------------------------
  
  ## set working directory for Mac and PC
  
  setwd("/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/")
  
  Sys.setlocale(category = "LC_CTYPE", locale = "C")
  
  
  ## ---------------------------
  ## load up the packages we will need:  
  
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(tidyverse)
  #install.packages("GGally")
  library(GGally)
  
  #devtools::install_github("briatte/ggnet")
  library(ggnet)
  
  
  library(network)
  library(sna)
  library(ggplot2)
  
  
  
  ## ----------------------------
  ## Install packages needed:  (uncomment as required)
  
  ##if(!require(dplyr)){install.packages("dplyr")}
  ##if(!require(tidyr)){install.packages("tidyr")}
  ##if(!require(tibble)){install.packages("tibble")}
  
  #### ---------------------------
  #Clear working space
  
  rm(list = ls())
  
  #### ---------------------------
  #Set output directorypaths
  outdir_figures='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Figures/Sygen'
  outdir_tables='/Users/jutzca/Documents/Github/Acute-Pharmacological-Treatment-in-SCI/Tables/Sygen'
  
  
  #### -------------------------------------------------------------------------- CODE START ------------------------------------------------------------------------------------------------####
  
  #Lod original data
  sygen.medication.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/masterfile.csv", header = T, sep = ',')
  names(sygen.medication.data)
  
  #Make copy of data file to work with
  sygen.medication.data.network <- sygen.medication.data
  
  
  #Replace all values greater than 0 with a 1 and all na's will be replaced with a 0
  sygen.medication.data.network[sygen.medication.data.network>0] <- 1
  sygen.medication.data.network[is.na(sygen.medication.data.network)] <- 0 
  
  #change columns to numerics class format
  cols_to_change = c(4:6)    
  for(i in cols_to_change){
    aggregate(sygen.medication.data.network[,i], by=list(Category=sygen.medication.data.network$generic_name), FUN=sum)
  }
  
  #Create the vector generic_name
  generic_name <- unique(sygen.medication.data.network$generic_name)
  
  #Create template for final data frame
  df_total = data.frame()
  
  #Create loop: For each of the generic names, create a subset and then replace the 1's with the generic drug name.
  for (i in 1:length(generic_name)){ 
    temp <- sygen.medication.data.network[sygen.medication.data.network$generic_name==generic_name[i],]
    generic_name2<-as.character(unique(temp$generic_name))
    temp[temp==1] <- generic_name2
    temp[temp==0] <- ''
    df <- temp
    df_total <- rbind(df_total,df)
  }
  
  write.csv(df_total, '/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/drug_names_per_day.csv')
  
  
  
  #Read file
  df_total <- read.csv('/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/drug_names_per_day.csv', header = T, sep = ',' )
  head(df_total)
  
  
  #rearrange the data frame from wide to long for the days X0:X365
  data_long <- tidyr::gather(df_total, day, measurement, X0:X365, factor_key=TRUE)
  tibble::glimpse(data_long)
  head(data_long)
  
  #Subset data
  data_long_subset =data_long[,-c(1,2,4)]
  
  #Remove lines that have no value in the measurement column
  data_long_subset_rm_na <- subset(data_long_subset, (!(measurement == '')) )
  
  #Create the vector generic_name
  days <- unique(data_long_subset_rm_na$day)
  
  #Create template for final data frame
  df_all_days_pooled = data.frame()
  
    for (i in 1:length(days)){ 
      temp <- data_long_subset_rm_na[data_long_subset_rm_na$day==days[i],]
      day_added <-as.character(unique(temp$day))
      
      df <- temp %>% mutate(n = 1) %>% 
        spread(measurement, n, fill=0) %>% 
        select(-NEW_ID, -day) %>% 
        {crossprod(as.matrix(.))} %>% 
        replace(lower.tri(., diag=T), NA) %>%
        reshape2::melt(na.rm=T) %>%
        unite('Pair', c('Var1', 'Var2'), sep=", ")
     
      df$day <- day_added
  
        df_all_days_pooled <- rbind(df_all_days_pooled,df)
    }
    
  
  #Create the dataframe for the edges --> separate the pairs
  edge_df<-df_all_days_pooled %>% separate(Pair, c("Source", "Target"), ", ")
  
  #Write file
  write.csv(edge_df, '/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Network_graph/edges_for_graph.csv')
  
  
  #----Plotting network
  
  # Lod data
  #e1 = read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Network_graph/edges_for_graph.csv", header = T, sep = ',')
  
  e<-edge_df
  
  #Subset to day of interest
  e1 <- subset(e, day == 'X7' & value > 20)
  names(e1)
  
  # Create network object
  e2 <- e1[,-c(3,4)]
  
  net = network(e2, directed = FALSE)
  
  #----Calcualte the edge weight----#
  edge_weight<- dplyr:: mutate(e1, 
                    edge_weight_pct = (100/791*value)/10)
  
  
  #----Calculate the node size----
  
  #Subset the data_long file: Remove all the lines with empty cells in column 'measurement'
  data_long_subset_rm_na <- subset(data_long, (!(measurement == '')) )
  
  #Count number of patients that received a certain drug per day
  drugs_per_day <- data_long_subset_rm_na %>%
    group_by(measurement,day, generic_name) %>%
    summarise(count=n())
    
  drugs_per_day_X <- subset(drugs_per_day, day == 'X7')
  
  node_size<-e2 %>% 
    select(Target, Source) %>% 
    t %>% c %>% unique
  
  node_size2 <-drugs_per_day_X[drugs_per_day_X$generic_name %in% node_size, ]
  
  # network plot
  
  set.seed(200)
  ggnet2(net, 
         node.size =node_size2$count, 
         node.color = "darkorange",
         edge.size = edge_weight$edge_weight_pct,
         edge.alpha = 0.6,
         edge.color = 'lightgray',
         label.color = "black",
         label = TRUE, legend.position = "none")+
    theme_light()+theme(axis.title = element_blank())+ggtitle('Day 7 Post-Injury')+
    guides(color = FALSE, size = FALSE)
  
  
  
  
  
  
  #### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####
