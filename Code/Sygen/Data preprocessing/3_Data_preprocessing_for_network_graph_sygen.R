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
  ## ---------------------------
  ##
  ## load up the packages we will need:  
  ##
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
  ##
  ## ----------------------------
  ##
  ## Install packages needed:  (uncomment as required)
  ##
  ##if(!require(dplyr)){install.packages("dplyr")}
  ##if(!require(tidyr)){install.packages("tidyr")}
  ##if(!require(tibble)){install.packages("tibble")}
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
  
  # Lod original data
  sygen.medication.data <- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/df_drugs_indication_per_day.csv", header = T, sep = ',')
  names(sygen.medication.data)
  
  # Make copy of data file to work with
  sygen.medication.data.network.copy <- sygen.medication.data
  
  # Replace all values greater than 0 with a 1 and all na's will be replaced with a 0
  sygen.medication.data.network<-sygen.medication.data.network.copy %>%
    dplyr::mutate_if(is.numeric, ~1 * (. != 0)) %>% 
    dplyr::mutate_if(is.numeric, ~replace_na(., 0))

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
  
  write.csv(df_total, '/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/drug_names_per_day.csv', row.names = F)
  
  
  #Read file
  df_total1 <- read.csv('/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/masterfile/drug_names_per_day.csv', header = T, sep = ',' )
  head(df_total)
  

  #rearrange the data frame from wide to long for the days X0:X365
  data_long <- tidyr::gather(df_total1, day, measurement, X0:X365, factor_key=TRUE)
  tibble::glimpse(data_long)
  head(data_long)
  
  # Subset data
  data_long_subset =data_long[,-c(1, 3:4)]
  
  # Remove lines that have no value in the measurement column
  data_long_subset_rm_na <- subset(data_long_subset, (!(measurement == '')) )
  
  data_long_subset_rm_na2 <- data_long_subset_rm_na %>% distinct()
  
  # Create the vector generic_name
  days <- unique(data_long_subset_rm_na2$day)
  
  # Create template for final data frame
  df_all_days_pooled = data.frame()
  
    for (i in 1:length(days)){ 
      temp <- data_long_subset_rm_na2[data_long_subset_rm_na2$day==days[i],]
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
    
  
  df_all_days_pooled_final <- subset(df_all_days_pooled, value>0)
  
  
  #Create the dataframe for the edges --> separate the pairs
  edge_df<-df_all_days_pooled_final %>% separate(Pair, c("Source", "Target"), ", ")
  
  #Write file
  write.csv(edge_df, '/Volumes/jutzelec$/8_Projects/1_Ongoing/3_Drugs/Network_graph/edges_for_graph2.csv', row.names = F)
  

  #### -------------------------------------------------------------------------- CODE END ------------------------------------------------------------------------------------------------####
