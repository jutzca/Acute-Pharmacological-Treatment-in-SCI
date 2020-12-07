#Split the masterfile by pid

df_test <- read.csv("/Users/jutzca/Documents/5_R/masterfile/df1.csv", sep=',', header = TRUE)


id <- df_test[order(df_test$NEW_ID),] 


id_split <- split(id, id$NEW_ID)


new_names <- as.character(unique(df_test$NEW_ID))

for (i in 1:length(id_split)) {
  id_split[[i]]
  write.csv(id_split[[i]], paste("/Users/localadmin/Documents/5_R/pid_graphs/",new_names[i],".csv", sep=""))
}


setwd("/Users/localadmin/Documents/5_R/pid_graphs/")

file_list <- list.files()



###################Plot drug exposure pattern for each patient#####################


library(plyr)
library(tidyr)
library(ggplot2)


setwd("/Users/localadmin/Documents/5_R/pid_graphs/")

file_list <- list.files()

for(file in file_list)    #repeat for all files in dir folder
{
    data <- read.csv(file, header=TRUE, sep=',')
  
  data <-select(data,-c(1,2)) #remove first column as it is not needed
  
 cols_to_change = c(3:365)    #change columns 4:368 to numerics class format
for(i in cols_to_change){
class( data[, i]) = "numeric"
  }

  data [data>0] <- 1
  #acetazolamide[is.na(acetazolamide)] <- 0 
  
  data  <-  data [c(1:62)]
  
  
  datan<-ddply(data,.(generic_name), function(x) colSums(x[,-1], na.rm = TRUE))
  
  data_long<-  datan%>%
    gather(time, dose, X0:X60)
  
  colnames(data_long)[2] <- "time"
  colnames(data_long)[3] <- "daily_dose"
  data_long$time<- sub("X","",data_long$time)
  data_long$time<- as.numeric(data_long$time)
  data_long$daily_dose<- as.numeric(data_long$daily_dose)
  
    myplot1<- ggplot(data_long, aes(time, generic_name, fill=daily_dose ))+geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high="black") +theme_linedraw()+
      scale_x_continuous(expand = c(0, 0), breaks = c(0,10,20,30,60))+  
      ggtitle(paste(file))+ labs(x="Days Post-Injury")+ 
    theme(panel.grid.major = element_blank(),axis.title.x = element_text(size = 12),
          axis.text.x = element_text(color="black", size=10), 
          axis.text.y = element_text( color="gray28", size=9), 
          axis.title.y  = element_blank(), legend.position = "none")
  
   ggsave(myplot1,filename=paste("myplot",file,".pdf",sep=""))
   
}







