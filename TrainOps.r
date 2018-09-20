setwd("J:/Shared/Kazi Wadud/TrainOps Simulation/")
homedir <- getwd()

library(dplyr)
library(readxl)
library(zoo)
library(xlsx)
library(tidyr)


dirlist <-  list.dirs(path=".", full.names = FALSE, recursive = FALSE)

myls1 <- vector("list", length(dirlist) )


for(d in 1:length(dirlist)) {   #outer most loop @ folder level
  
  setwd(paste0(homedir,"/", dirlist[d]))   #enter individual folder
  
  
  print(getwd())  #check folder
  
  fl <- list.files(path = ".", pattern ="Trip Table",  full.names = FALSE, recursive = FALSE)
  
  myls <- vector("list", length(fl) )
  
  for (f in 1:length(fl)) {
    
    df <- read_excel(fl[f], sheet = 1)
    

    #RouteDir <- df[2, c(1,2)]
    
    #colnames(RouteDir) <- c("Name", "Direction")
    
    df2 <- df[7:nrow(df), c(1,2,3,16)]   ###Start from Time
    
    colnames(df2) <- df2[1,] 
    
    df2 <- df2[-1,]  
    
    
    df_check <- df2
    
    fl_check <-  df_check  %>% filter(!grepl("_INT_", `Station Name`)) %>%  select(`Station Name`) %>% unique() %>% nrow()
    
    if(fl_check==1) {
      
    next
      
    }
    
    
    df2 <- rename(df2, Voltage=Train) 
    
    print(f)
    
    ###df3 <- filter(df2, Mode=="Accel")
    
    df2$`Station Name`[1]<-  ifelse(is.na(df2$`Station Name`[1]), "Start_Station",  df2$`Station Name`[1]) 
    ##sometimes Start Station can be null
    
    df3 <- filter(df2, !is.na(Voltage))
    
    
    
    df3$`Station Name` <- gsub("_INT_[a-zA-Z]+", NA, df3$`Station Name` )  ##Covert Interlocking to NA
    
    df4 <- transform(df3, `Station Name` = na.locf(`Station Name`))
    
    df4$Sequence <- seq.int(1, nrow(df4), by=1)
    
    df4$Voltage <- as.numeric(df4$Voltage) 
    
    
  
    df5 <- df4 %>% filter(Mode=="Accel") %>% group_by(`Station.Name`) %>% 
                     summarise(avg_volt=mean(Voltage))
    
    df5_1 <- df4 %>% group_by(`Station.Name`) %>% 
                     summarise(min_volt=min(Voltage))
    
    
    df6 <- left_join(df4, df5, by = c("Station.Name" = "Station.Name") )
    df66 <- left_join(df6, df5_1, by = c("Station.Name" = "Station.Name") )
    
    
    df7 <- df66[ , c(2,5:7)]
    
    
    df8 <- df7 %>% group_by(`Station.Name`, avg_volt, min_volt) %>% summarise(MAX_SEQ=max(Sequence))
    
    
    df9 <- arrange(df8, df8$MAX_SEQ)
    
    df9 <-  as.data.frame(df9)
    
    df10 <- mutate(df9, nextst = lead(`Station.Name`) )
    
    df_final <-  select(df10, Station.Name, nextst, avg_volt, min_volt) %>% rename(Start_Station=Station.Name,
                                                                                       End_Station=nextst)
    
     LineInfo <-   gsub("_[[:print:]]+","",  gsub("Trip Table_", "",fl[1] ) )   
    
    df_final$End_Station <- ifelse(is.na(df_final$End_Station), LineInfo, df_final$End_Station)
    
    df_final$FileName <- fl[f]
    
    
    myls[[f]] <- df_final
    
    
  }
    
  TrainOps <- do.call(rbind, lapply(myls, data.frame, stringsAsFactors=FALSE))
  
  TrainOps <-   TrainOps %>% filter(!is.na(avg_volt)) 
  
  write.csv(TrainOps, paste0(dirlist[d], "level_2.csv"), row.names = F)
  
  
  
  TrainOps$StartEndStation <- paste0(TrainOps$Start_Station, "-", TrainOps$End_Station)
  
  Final_TrainOps <- TrainOps %>% group_by(StartEndStation) %>% 
    summarise(Avg_Voltage=mean(avg_volt), Min_Voltage=min(min_volt))
  
  
  Final_TrainOps$Scenerio <- dirlist[d]
  
  Final_TrainOps <- Final_TrainOps %>% separate(StartEndStation, c("Start_Station", "End_Station"), "-" )
  
  
  myls1[[d]]  <- Final_TrainOps
  
  write.xlsx(Final_TrainOps, paste0(homedir, "/Summary_Data.xlsx"), sheetName =dirlist[d], append = T )
  
  
}
  

#Grand_summary <- do.call(rbind, lapply(myls1, data.frame, stringsAsFactors=FALSE))

  
setwd(homedir)













