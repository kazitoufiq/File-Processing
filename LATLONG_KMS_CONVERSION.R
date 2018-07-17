library(dplyr)

library(sp)
library(rgeos)


setwd("E:/R code/OBD n IEV")


df <- read.csv("Track_Code_Assignment_22062018.csv", stringsAsFactors = F)


setwd("E:/R code/OBD n IEV/coordinates to kms")

fl <- list.files(".", pattern = "Track_Code")

myls <- list()

for(i in 1:length(fl)){
  
  myls[[i]]  <- read.csv(fl[i], stringsAsFactors = F)
  

}

latlong_kms_ref <- do.call(rbind, lapply(myls, data.frame, stringsAsFactors=FALSE))



str(latlong_kms_ref)
str(df)

df<-  df[!is.na(df$Track_Code),]


myls <- list()

for(i in 1:nrow(df)) {
        
         print(i)
       
       
        kmsref <- latlong_kms_ref %>%  filter(Track_Code==df$Track_Code[i])
        
        if(nrow(kmsref)!=0){
          
        p_df <-  as.data.frame(cbind(df$longitude[i], df$latitude[i]))  
        
        colnames(p_df)  <- c("Long", "Lat")
        obd_sp <- SpatialPoints(p_df)
        
        Ref_sp <- SpatialPoints(kmsref[, c(4,3)]) 
        
        index_kmsref <- apply(gDistance(obd_sp, Ref_sp, byid=TRUE), 2, which.min)
        
        ff <- kmsref[index_kmsref,]
        rownames(ff) <- c()
        
        nearest_point <- cbind(df$RowID[i],  ff)
        names(nearest_point)
        
        colnames(nearest_point)  <- c("RowID", "Track_Code", "Km", "Ref_Lat", "Ref_Long" )
        
        myls[[i]]  <- nearest_point
        
        } 
        
}




SNAPPED_KMS  <- do.call(rbind, lapply(myls, data.frame, stringsAsFactors=FALSE))



unique(SNAPPED_KMS$Track_Code)




