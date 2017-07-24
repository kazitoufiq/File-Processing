#    1) This R Script will download data from system based on Date Selection.         
#    2) Authentication to PRS is via Windows user authentication. To run this  
#       script user need to have access to PRS.
#
#    3) This script download data by URL request and by one day for each day within the 
#          selected dates. This approach has been adopted to generate "Service_Date"
#    4) This script contains User Defined function and also has exception handling procedure 
#
#    5) This script does all the data processing necessary to derive the final data model 
#        output to do various performance analysis.
#   
#    6) At the end this script connects to SQL Server Database and load/append the data model to a table       
#
# Script Written By -    Kazi Toufiq Wadud, 
#                    
#                     
#                        
#=============================================================================================



library(lubridate)
ReportServer <- "http://prsreports.metrotrains.com.au/ReportServer?%2fPRS%2f" #PRS Report Server
DateArray <- seq(as.Date("2017/7/17"), as.Date("2017/7/23"), by =1)    ####Type Date(Y/M/D) --User Input

StartDate <- DateArray[1]
StratDateFormatted <- format(StartDate, "%d-%b-%Y")

EndDate <- DateArray[length(DateArray)]
EndDateFormatted <- format(EndDate, "%d-%b-%Y")

StartDateValue <- paste(month(DateArray[1]),day(DateArray[1]),year(DateArray[1]),sep="/")
EndDateValue <- paste(month(EndDate),day(EndDate),year(EndDate),sep="/")



# Downlaod OPR Daily Report -----------------------------------------------


myls <- vector("list", length(DateArray)) 

for (d in 1:length(DateArray))  {

StartDateValue <- paste(month(DateArray[d]),day(DateArray[d]),year(DateArray[d]),sep="/")  
StratDateFormatted <- format(DateArray[d], "%d-%b-%Y")  

print("Downloading OPR Daily Results") 
ReportName <- "OPR+Daily+Results&rs:Command=Render&rs:format=csv"
DatePart <- paste("&DateFrom=",StartDateValue,"&DateTo=", StartDateValue, sep="")
u <- paste(ReportServer,ReportName,DatePart, sep="")
FileName <- paste0("OPR Daily Results", " ", StratDateFormatted, " To ", StratDateFormatted,".csv")
download.file(u, FileName, mode="wb")


df <- read.csv(FileName, header = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
df$Service_Date <-format(DateArray[d], "%d-%m-%Y")
file.remove(FileName)
myls[[d]] <- df

}

OPRDailyResults <- do.call(rbind, lapply(myls, data.frame, stringsAsFactors=FALSE))
write.csv (OPRDailyResults, "OPR Daily Results.csv", row.names = F)



# Download Incident Data Recategorised Report -----------------------------




myls <- vector("list", length(DateArray)) 

for (d in 1:length(DateArray))  {

StartDateValue <- paste(month(DateArray[d]),day(DateArray[d]),year(DateArray[d]),sep="/")  

StratDateFormatted <- format(DateArray[d], "%d-%b-%Y")
  
print("Start Downloading Incident Data Report")

ReportName<- "Incident+Reports%2fIncident+Data+recategorised&rs:Command=Render&rs:format=csv"
DatePart <- paste("&datefrom=",StartDateValue,"%2012:00:00%20AM","&dateto=", StartDateValue, "%2012:00:00%20AM", sep="")
u <-paste(ReportServer,ReportName,DatePart, sep="")
FileName <- paste0("Incident Data Report Recategorised", " ", StratDateFormatted, " To ", StratDateFormatted,".csv")
download.file(u, FileName, mode="wb")

df <- read.csv(FileName, header = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
df$Service_Date <-format(DateArray[d], "%d-%m-%Y")

file.remove(FileName)

myls[[d]] <- df


}

IncidentDataRecategorised <- do.call(rbind, lapply(myls, data.frame, stringsAsFactors=FALSE))
write.csv (IncidentDataRecategorised, "Incident Data Recategorised.csv", row.names = F)




# Download Performance Data Report Revised --------------------------------



myls <- vector("list", length(DateArray)) 

for (d in 1:length(DateArray))  {
  
  StartDateValue <- paste(month(DateArray[d]),day(DateArray[d]),year(DateArray[d]),sep="/")  
  
  StratDateFormatted <- format(DateArray[d], "%d-%b-%Y")
  
  print("Start Downloading Performance Data Report Revised")
  
  ReportName<- "Performance+Data+Report+-+Revised&rs:Command=Render&rs:Format=csv"
  DatePart <- paste("&DateFrom=",StartDateValue,"%2012:00:00%20AM","&DateTo=", StartDateValue, "%2012:00:00%20AM", sep="")
  u <-paste(ReportServer,ReportName,DatePart, sep="")
  FileName <- paste0("Performance Data Report Revised", " ", StratDateFormatted, " To ", StratDateFormatted,".csv")
  download.file(u, FileName, mode="wb")
  
  df <- read.csv(FileName, header = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
  
  df$Service_Date <-format(DateArray[d], "%d-%m-%Y")
  file.remove(FileName)
  myls[[d]] <- df

  
}

PerformanceDataRevised <- do.call(rbind, lapply(myls, data.frame, stringsAsFactors=FALSE))

write.csv (PerformanceDataRevised, "Performance Data Revised.csv", row.names = F)




# Download Train Fleet Reliability Lost Report ----------------------------





myls <- vector("list", length(DateArray)) 

for (d in 1:length(DateArray))  {
  
  StartDateValue <- paste(month(DateArray[d]),day(DateArray[d]),year(DateArray[d]),sep="/")  
  
  StratDateFormatted <- format(DateArray[d], "%d-%b-%Y")
  
  print("Start downloading Train Fleet Reliability Lost by Incident Report")
  
  ReportName<- "Incident+Reports%2fTrain+Fleet+Reliability+Lost+Report+Version+2&rs:Command=Render&rs:format=csv"
  DatePart <- paste("&DateFrom=",StartDateValue,"%2012:00:00%20AM","&DateTo=", StartDateValue, "%2012:00:00%20AM", sep="")
  u <-paste(ReportServer,ReportName,DatePart, sep="")
  FileName <- paste0("Train Fleet Reliability Lost", " ", StratDateFormatted, " To ", StratDateFormatted,".csv")
  download.file(u, FileName, mode="wb")
  
  df <- read.csv(FileName, header = T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")
  df$Service_Date <-format(DateArray[d], "%d-%m-%Y")
  
  file.remove(FileName)
  
  myls[[d]] <- df
  
  
}

TrainFleetReliabilityLost <- do.call(rbind, lapply(myls, data.frame, stringsAsFactors=FALSE))
write.csv (TrainFleetReliabilityLost, "Train Fleet Reliability Lost.csv", row.names = F)



# Load Incident & Performance Data ----------------------------------------



setwd("J:/Asset Management/Asset Performance and Investment/12_Data Repository")


incdata <- read.csv("Incident Data Recategorised.csv", stringsAsFactors = FALSE, fileEncoding="latin1")
perfdata <- read.csv("Performance Data Revised.csv", stringsAsFactors = FALSE, fileEncoding="latin1")



dim(incdata)
dim(perfdata)



# Date format Checking  ---------------------------------------------------



UDF_DateFormatChecker <- function(x){
  
  try (if(sum(grepl("\\d{1,2}-\\d{1,2}-\\d{4}", x)==FALSE &    
              as.integer(substr(x,4,5))<=12)!=0) { 
    
    stop ("Data format issue. Pls check")
    
  } else {print("Date format is OK. Proceeding to next code for execution.")
  
         }
  )
  
}

UDF_DateFormatChecker(incdata$Service_Date)
UDF_DateFormatChecker(perfdata$Service_Date)


incdata$Service_Date <- as.Date(incdata$Service_Date, "%d-%m-%Y")
perfdata$Service_Date <- as.Date(perfdata$Service_Date, "%d-%m-%Y")


print(paste0("Incident Data: ", max(incdata$Service_Date), " Class: ", class(incdata$Service_Date)))
print(paste0("Performance Data: ", max(perfdata$Service_Date), " Class: ", class(perfdata$Service_Date)))



# Left Join :  L :Performance Data  R: Incident Data  ---------------------



library(dplyr)

perf_inc_data <-  left_join(perfdata, incdata, by= c('Service_Date'= 'Service_Date', 'TDN' = 'TDN'))
## After joinining multiple record for same TDN+Service_Date can be repeated multiple 
##times due to TDN+Incident+Resp combination 

##View(perf_inc_data)

data1 <-  perf_inc_data[,c("Service_Date", "TDN", "Incident_No")]
#View(data1)

Inc_factor_per_tdn <- data1 %>% group_by(Service_Date,TDN) %>% summarise(Incident_Factor=1/n_distinct(Incident_No))

str(data1)

str(Inc_factor_per_tdn)

##View(Inc_factor_per_tdn)

data2 <-  perf_inc_data[,c("Service_Date", "TDN", "Incident_No", "Responsibility")]

#data2 <- filter(data2, !is.na(data2$Responsibility))

str(data2)


Resp_factor_per_inc <- data2 %>% group_by(Service_Date,TDN, Incident_No) %>% summarise(Resp_Factor=1/n_distinct(Responsibility))


combined_kpi_factor_stg1 <- left_join(Resp_factor_per_inc, Inc_factor_per_tdn , by= c('Service_Date'= 'Service_Date', 'TDN'='TDN'))


combined_kpi_factor_stg1$combined_kpi_factor <- combined_kpi_factor_stg1$Resp_Factor * combined_kpi_factor_stg1$Incident_Factor


##View(combined_kpi_factor_stg1)

#kpi_factor_tab <- summarise(group_by(perf_inc_data, TDN), KPI_Factor=1/n())

#str(perf_inc_data)

perf_inc_data_Stg1 <-  left_join(perf_inc_data, combined_kpi_factor_stg1 , by= c('Service_Date'= 'Service_Date', 'TDN'='TDN', 'Incident_No'='Incident_No'))

#str(perf_inc_data_Stg1)

##View(perf_inc_data_Stg1)  261715




# Handle uneven allocation across multiple Responsibilities ---------------



TrainFleetRel <-  read.csv("Train Fleet Reliability Lost.csv", stringsAsFactors = FALSE, fileEncoding="latin1")

#str(TrainFleetRel)


TrainFleetRel$Service_Date <- as.Date(TrainFleetRel$Service_Date, "%d-%m-%Y")

TrainFleetRel_stg2 <- TrainFleetRel[, c("Service_Date", "Incident_No", "Resp", "Percentage")]

TrainFleetRel_stg3 <- rename(TrainFleetRel_stg2, Resp_TFRel=Resp, Percentage_TFRel=Percentage,
                             Incident_No_TFRel=Incident_No)

str(TrainFleetRel_stg3)

##View(TrainFleetRel_stg3)


library(tidyr)  # to use separate function - to split Resp and Percenatge 

df1 <- TrainFleetRel_stg3 %>% separate(Resp_TFRel, c("Resp1", "Resp2", "Resp3", "Resp4"), sep = "\\,") %>% separate(Percentage_TFRel, 
                                                                                                                    c("Percentage1", "Percentage2", "Percentage3", "Percentage4"), sep = "\\,")



#View(df1)

## filter records other than 50% and 100%

non_even_allocation <-  filter(df1, grepl("50%|100%", df1$Percentage1)==FALSE)


#non_even_allocation$Incident_No_TFRel <-as.character(non_even_allocation$Incident_No_TFRel)
#str(non_even_allocation)



for (i in 3:ncol(non_even_allocation) ) {
  non_even_allocation[,c(i)] <- trimws(non_even_allocation[, c(i)], which = c("both", "left", "right"))
}

### convert -example - 70% to 0.70

non_even_allocation$Percentage1 <-as.numeric(sub("^",".", sub("%", "", non_even_allocation$Percentage1)))
non_even_allocation$Percentage2 <-as.numeric(sub("^",".", sub("%", "", non_even_allocation$Percentage2)))
non_even_allocation$Percentage3 <-as.numeric(sub("^",".", sub("%", "", non_even_allocation$Percentage3)))
non_even_allocation$Percentage4 <-as.numeric(sub("^",".", sub("%", "", non_even_allocation$Percentage4)))





# Left Join Uneven allocation to derive KPI factor---------------------------------------------





interim1 <- left_join(perf_inc_data_Stg1,non_even_allocation, by= c('Service_Date'= 'Service_Date', 'Incident_No'='Incident_No_TFRel'))

#View(interim1)


interim1$Resp_Factor1 <-  interim1$Resp_Factor

interim1$Resp_Factor1 <- ifelse(interim1$Responsibility==interim1$Resp1 & !is.na(interim1$Resp1), interim1$Percentage1, 
                                interim1$Resp_Factor1)

interim1$Resp_Factor1 <- ifelse(interim1$Responsibility==interim1$Resp2 & !is.na(interim1$Resp2), interim1$Percentage2, 
                                interim1$Resp_Factor1)

interim1$Resp_Factor1 <- ifelse(interim1$Responsibility==interim1$Resp3 & !is.na(interim1$Resp3), interim1$Percentage3, 
                                interim1$Resp_Factor1)


interim1$Resp_Factor1 <- ifelse(interim1$Responsibility==interim1$Resp4 & !is.na(interim1$Resp4), interim1$Percentage4, 
                                interim1$Resp_Factor1)


interim1$KPI_Factor <- interim1$Resp_Factor1 * interim1$Incident_Factor





# Derive KPI/measures from KPI_Factor  ------------------------------------





#str(interim1)


interim1$Unplanned_Late_Arrival <- ifelse(interim1$on_time ==0, interim1$KPI_Factor,0)

interim1$Unplanned_Early_Arrival <- ifelse(interim1$on_time ==0 & interim1$Arr_Mins_Early < 0,
                                           interim1$KPI_Factor,0)


interim1$Unplanned_Westona_Bypass <- ifelse(grepl("BW", interim1$AAmex) & !grepl("S", interim1$AAmex),
                                            interim1$KPI_Factor,0)


interim1$Unplanned_Loop_Bypass  <- ifelse(grepl("BL", interim1$AAmex) & !grepl("S", interim1$AAmex),
                                          interim1$KPI_Factor,0)


interim1$Unplanned_Short_Departure <- ifelse(grepl("SD", interim1$AAmex),
                                             interim1$KPI_Factor,0)


interim1$Unplanned_Short_Arrival <- ifelse(grepl("SA", interim1$AAmex),
                                           interim1$KPI_Factor,0)


interim1$Unplanned_Cancellation  <- ifelse(grepl("C", interim1$AAmex),
                                           interim1$KPI_Factor,0)


interim1$Planned_Short_Arrival <- ifelse(grepl("SA", interim1$PAmex),
                                         interim1$KPI_Factor,0)


interim1$Planned_Cancellation <- ifelse(grepl("C", interim1$PAmex),
                                        interim1$KPI_Factor,0)

interim1$Timetabled_Services <- interim1$KPI_Factor



interim1$Total_Unplanned_Cancellation <- interim1$Unplanned_Cancellation + 
  (interim1$Unplanned_Short_Arrival*0.25) +  (interim1$Unplanned_Short_Departure*0.25) +
  (interim1$Unplanned_Loop_Bypass*0.125)  + (interim1$Unplanned_Westona_Bypass*0.125) 


#New colum Resp_Final to add logic :   If TDN has no Incident No, 
#but has been delayed or has impact on Total Unplanned cancellation, then "Unallocated"

interim1$Resp_Final <- ifelse((interim1$Unplanned_Late_Arrival>0 | interim1$Total_Unplanned_Cancellation>0) 
                              & is.na(interim1$Incident_No), "Unallocated", interim1$Responsibility )

interim1$Incident_No_Final <- ifelse((interim1$Unplanned_Late_Arrival>0 | interim1$Total_Unplanned_Cancellation>0) 
                                     & is.na(interim1$Incident_No), paste0(interim1$TDN,"-", interim1$Service_Date) , interim1$Incident_No )



str(interim1)

##View(interim1)

write.csv(interim1, "Asset Performance Model.csv", row.names = F)





# Load Processed Data to Database -----------------------------------------

print("Loading Data to Database")


library(RODBC) 

dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=MT-LT02600\\SQLEXPRESS;
                              database=SAM;trusted_connection=true')


q <- sqlQuery(dbhandle, "SELECT MAX(Service_Date) from Asset_Performance_Model")

MaxDBDate <- as.Date(q[1,1])

try(
if(max(interim1$Service_Date)<=MaxDBDate) { 
  
  stop ("Record already Updated")
  
} else {print("Records may be updated. Proceeding to next code for execution")
  
  sqlSave(dbhandle, interim1, tablename ="Asset_Performance_Model", append = T,
          rownames = F, colnames = F, verbose = FALSE)
}

)



odbcCloseAll()

# End of Script -----------------------------------------------------------








