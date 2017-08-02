

library(sp)
library(readxl)
library(stringr)
library(dplyr)


# Read Ploygon file and process to tidy format ----------------------------

polygonfile <- list.files(path=".", pattern = "MasterPolygon")
polygondata <- as.data.frame(read_excel(polygonfile, sheet = 2))
polygondata$Coordinates <-  gsub('<[[:print:]]{1,}=', "", polygondata$Description )
polygondata$Coordinates <-  gsub('/>', "", polygondata$Coordinates)
polygondata$Coordinates <-  gsub('\'', "", polygondata$Coordinates)


MaxPoints <- max(as.integer(str_extract(polygondata$NAME, '\\d+')))

polygondata$Coordinates <- paste0(polygondata$Caption, ",", polygondata$Coordinates)

polygondata_final <-  as.data.frame(str_split_fixed(polygondata$Coordinates, ",", (MaxPoints*2)+1), stringsAsFactors = F)

str(polygondata_final)


dim(polygondata_final)

#View(polygondata_final)

myls <- list()
myls1 <- list()

for (j in 1:nrow(polygondata_final)) {
  
  i=2
  r=2
  
  while (i <= dim(polygondata_final)[2]) {
    
    myls[[r]] <- paste0(polygondata_final[j,1], ":",polygondata_final[j,i], ",", polygondata_final[j,i+1])
    r = r + 1
    i = i + 2  
  }
  
  myls1[[j]] <- myls
}  
  


finallist <- list()

for(k in 1:length(myls1)) {
  
 df<- as.data.frame(unlist(myls1[[k]]))
 
 finallist[[k]]  <- df 
  
}

 

Results <- do.call(rbind, lapply(finallist, data.frame, stringsAsFactors=FALSE))


clean_polygondata <-  as.data.frame(str_split_fixed(Results[,1], ":", 2), stringsAsFactors = F)

#clean_polygondata

library(tidyr)
Final_Polygon <- clean_polygondata %>%  separate(V2, c("longitude", "latitude"), ",", extra ="merge")

Final_Polygon  <- rename(Final_Polygon, polygonid=V1)
Final_Polygon <- filter(Final_Polygon, longitude!="" )

Final_Polygon$longitude <- as.numeric(Final_Polygon$longitude)
Final_Polygon$latitude <- as.numeric(Final_Polygon$latitude)

# Final Poligon data in tall format
#View(Final_Polygon)



#write.csv(Final_Polygon, "Final_Polygon.csv", row.names = F)



rcmdata <- read.csv("RCM_All_Low_Volatge_Event_July2017.CSV", stringsAsFactors = F)

rcmdata <- filter(rcmdata, !is.na(latitude))


rcmdata$RowID <- seq.int(nrow(rcmdata))

#View(rcmdata)
str(rcmdata)



ll <- list()

for (i in 1:nrow(rcmdata)) {
  
   for(j in 1:length(unique(Final_Polygon$polygonid))) {
     
     pol1 <- Final_Polygon[Final_Polygon$polygonid==unique(Final_Polygon$polygonid)[j],2:3]
     
      if(point.in.polygon(rcmdata[i,8],rcmdata[i,9], pol1[,1],pol1[,2])>0){
        
       ll[[i]] <- paste0(point.in.polygon(rcmdata[i,8],rcmdata[i,9], pol1[,1],pol1[,2]), "-", 
                          unique(Final_Polygon$polygonid)[j], ":", rcmdata$RowID[i] )
      
      }
        
     
    }

}


all_data <- do.call(rbind, lapply(ll, data.frame, stringsAsFactors=FALSE))

names(all_data)  <- c("Name")


all_data_section <- all_data %>% separate(Name, c("Check_Section", "RowID"), ":", extra ="merge") %>% 
                          separate(Check_Section, c("CheckResult", "Section"), "-", extra ="merge")



all_data_section$RowID <- as.integer(all_data_section$RowID)

str(rcmdata)
str(all_data_section)

                        
joineddata <-   left_join(rcmdata,all_data_section, by=c('RowID' = 'RowID') )

#View(joineddata)



MidPointfile <- list.files(path=".", pattern = "MidPoints.xlsx")
MidPointdata <- as.data.frame(read_excel(MidPointfile, sheet = 2))
MidPointdata$Coordinates <-  gsub('<[[:print:]]{1,}=', "", MidPointdata$Description)
MidPointdata$Coordinates <-  gsub('/>', "", MidPointdata$Coordinates)
MidPointdata$Coordinates <-  gsub('\'', "", MidPointdata$Coordinates)

#View(MidPointdata)


MidPointdata$Coordinates <- paste0(MidPointdata$Caption, ",", MidPointdata$Coordinates)

MidPointdata_final <-  as.data.frame(str_split_fixed(MidPointdata$Coordinates, ",", 3), stringsAsFactors = F)

finaldata <-   left_join(joineddata,MidPointdata_final, by=c('Section' = 'V1') )


#View(finaldata)


str(finaldata)

df <- select(finaldata, vehicle, event_name, start_time,  end_time, Section, V2, V3)

fdata <- rename(df, longitude=V2, latitude=V3 )

#View(fdata)

write.csv(fdata, "fdata.csv", row.names = F)



