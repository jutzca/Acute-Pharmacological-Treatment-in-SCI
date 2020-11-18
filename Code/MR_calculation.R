

mod <- read.csv("/Users/localadmin/Documents/5_R/masterfile/mr.csv", sep=',', header = TRUE)

##with only new AIS grade####
####4 weeks###
mod$new_MR4<- ifelse(mod$ais1=="AIS A" & (mod$ais4=="AIS C"| mod$ais4=="AIS D"),"Y", 
                     ifelse(mod$ais1=="AIS B" & mod$ais4=="AIS D","Y", 
                            ifelse(mod$ais1=="AIS C" & mod$modben04>=5, "Y", 
                                   ifelse(mod$ais1=="AIS D" & mod$modben04>=6, "Y", 
                                          ifelse(is.na(mod$ais1), NA, "N")))))


####16 weeks###
mod$new_MR16<- ifelse(mod$ais1=="AIS A" & (mod$ais16=="AIS C"| mod$ais16=="AIS D"),"Y", 
                     ifelse(mod$ais1=="AIS B" & mod$ais16=="AIS D","Y", 
                            ifelse(mod$ais1=="AIS C" & mod$modben16>=5, "Y", 
                                   ifelse(mod$ais1=="AIS D" & mod$modben16>=6, "Y", 
                                          ifelse(is.na(mod$ais1), NA, "N")))))



####26 weeks###
mod$new_MR26<- ifelse(mod$ais1=="AIS A" & (mod$ais26=="AIS C"| mod$ais26=="AIS D"),"Y", 
                      ifelse(mod$ais1=="AIS B" & mod$ais26=="AIS D","Y", 
                             ifelse(mod$ais1=="AIS C" & mod$modben26>=5, "Y", 
                                    ifelse(mod$ais1=="AIS D" & mod$modben26>=6, "Y", 
                                           ifelse(is.na(mod$ais1), NA, "N")))))



####52 weeks###
mod$new_MR52<- ifelse(mod$ais1=="AIS A" & (mod$ais52=="AIS C"| mod$ais52=="AIS D"),"Y", 
                      ifelse(mod$ais1=="AIS B" & mod$ais52=="AIS D","Y", 
                             ifelse(mod$ais1=="AIS C" & mod$modben52>=5, "Y", 
                                    ifelse(mod$ais1=="AIS D" & mod$modben52>=6, "Y", 
                                           ifelse(is.na(mod$ais1), NA, "N")))))

names(mod)

write.csv(mod, "/Users/localadmin/Documents/5_R/masterfile/outcome_demographics.csv")







##using a combination of old and new AIS baseline grades####
mod$combo_new_MR4<- ifelse(mod$ais1_combo=="A" & (mod$ais4=="AIS C"| mod$ais4=="AIS D"),"Y", 
                           ifelse(mod$ais1_combo=="B" & mod$ais4=="AIS D","Y", 
                                  ifelse(mod$ais1_combo=="C" & mod$modben04>=5, "Y", 
                                         ifelse(mod$ais1_combo=="D" & mod$modben04>=6, "Y",
                                                ifelse(is.na(mod$ais1_combo), NA, "N")))))

##old marked recovery with old AIS grade and modben scores only##
mod$old_MR4<- ifelse(mod$ASIMPC01_A =="A" & mod$modben04>=3,"Y", 
                     ifelse(mod$ASIMPC01_A =="B" & mod$modben04>=4,"Y", 
                            ifelse(mod$ASIMPC01_A =="C" & mod$modben04>=5, "Y", 
                                   ifelse(mod$ASIMPC01_A =="D" & mod$modben04>=6, "Y", 
                                          ifelse(is.na(mod$ASIMPC01_A), NA, "N")))))