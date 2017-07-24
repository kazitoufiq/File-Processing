

library(xlsx)
library(lubridate)
library(dplyr)


# FMP Data processing -----------------------------------------------------


print("Start of FMP data processing...")

df1 <- read.xlsx("ALL PRS Data JUNE.xls", sheetIndex = 1, colClasses=NA , check.names=F)


try(if(class(df1$`Service Date`)!="Date"){stop ("Date format not correct")
  
} else print("Date Format Correct! Proceeding to next code for execution")
)


##Car Type Name
ColSelectecd <- c("Service Date", "TPMIS No", "Car No", "Car Type Name","System Description", "Component Code", "Driver Report")

df2 <- df1[!df1$Status %in% c("NF"), ColSelectecd]

df2 <-   rename(df2, Fleet=`Car Type Name`)


DuplicateCheck <-  df2[duplicated(df2[,c('TPMIS No')]),]

try(if(nrow(DuplicateCheck)>0) {stop("Data has duplicate value! Please check.")
} else print("No Duplicate! Proceeding to next code for execution.")
)


write.csv(df2, "FMP.csv", row.names = F)

print("End of FMP data processing.")



# Infra Data Processing ---------------------------------------------------

print("Start of Infra data processing...")


wb<- loadWorkbook("1- Passenger Weighted Minutes - Actuals vs. Targets V2.xls")
sheets <- getSheets(wb)
sheet <- sheets[[1]]

df1 <- readColumns(sheet, startRow=6, startColumn=1, endColumn =13, 
                   header=T, colClasses=NA, check.names=F)



str(df1)
HeaderNames <-colnames(df1[, c(1:6,9:13)])
HeaderNames[1]  <- "Date"
df2 <- df1[, c(1:6,9:13)]
colnames(df2) <- HeaderNames


try(if(sum(is.na(df2$Date))<2) {stop("Last Blanks Rows have not been identified.")
} else {print("Blank rows identified. Proceeding to next code for execution.") }
)

df3 <- df2[(!is.na(df2$Date)), ]


df3$Date <- as.Date(as.POSIXct(df3$Date, 'GMT'))
str(df3)

try(if(ncol(df3)==11){
  write.csv(df3, "INFRA.csv", row.names = F)
  
}else { stop("Unexpected Columns!! Pls check")
  
}
)


print("End of INFRA data processing.")


# Signalling Data Processing ----------------------------------------------


print("Start of SIGNALLING data processing...")


df1 <- read.xlsx2("03- Signalling Failure Cause Analysis - Manager V2.xls", 
                  sheetIndex = 2, colClasses=NA , check.names=F)

df1$Date <- as.Date(as.POSIXct(df1$Date, 'GMT'))
df1$`Time Avail` <- strptime("1970-01-01", "%Y-%m-%d", tz="GMT") + df1$`Time Avail`
df1$`Time OK` <- strptime("1970-01-01", "%Y-%m-%d", tz="GMT") + df1$`Time OK`
df1$`Time OS` <- strptime("1970-01-01", "%Y-%m-%d", tz="GMT") + df1$`Time OS`
df1$`Time Rep` <- strptime("1970-01-01", "%Y-%m-%d", tz="GMT") + df1$`Time Rep`

df2 <- df1[(!is.na(df1$Date)), ]

str(df2)

write.csv(df2, "SIGNALLING.csv", row.names = F, na="")

print("End of SIGNAL data processing.")



rm(list = ls())
getwd()
gc()




# Import INFRA and SIGNALLING and Check Date format -----------------------


INF <- read.csv("INFRA.csv", stringsAsFactors = F, check.names=F)
SIGNALLING <- read.csv("SIGNALLING.csv", stringsAsFactors = F, check.names=F)


print(paste0("Infra: ", max(INF$Date)))
print(paste0("Signalling: ",max(SIGNALLING$Date)))


#library(lubridate)

UDF_Date <- function(x,df) {
  try(
    if (sum(grepl( "\\d{1,2}-\\d{1,2}-\\d{4}", df[,x]))!=0) {
      
      
      df[,x] <- as.Date( df[,x], "%d-%m-%Y")
      return(df)
      
    } else if(sum(grepl( "\\d{4}-\\d{1,2}-\\d{1,2}",  df[,x]))!=0) {
      
      
      df[,x] <- as.Date( df[,x], "%Y-%m-%d")
      return(df)
      
    } else {
      
      stop("Data Format Issue. Please check")
      
    }
  )
}


INF <- UDF_Date('Date',INF)
SIGNALLING <- UDF_Date('Date',SIGNALLING)

print(paste0("Infra: ", max(INF$Date), " Class: ", class(INF$Date)))
print(paste0("Signalling: ", max(SIGNALLING$Date), " Class: ", class(SIGNALLING$Date)))



# Start of Data Processing to generate Expr for INFRA and Signalling-------------------------------


library(dplyr)

##Get Infra records present only in INF_PWM_Report, Not in Signalling Data

INF_Only <-  anti_join(INF, SIGNALLING, by = c("Incident No"="Incident No"))

str(INF_Only)

INF_Only_Expr <-transmute(INF_Only,
                              Service_Date=Date,
                              Incident_No=`Incident No`,
                              Line=NA,
                              Location=Location,
                              Asset_ID=NA,
                              Expr1 = "Asset", Expr2="Infrastructure", 
                              Expr3=`System Desc`, 
                              Expr4= case_when(
                                INF_Only$`System Desc`=="Signals" ~ "Other",
                                TRUE ~  INF_Only$`Subsystem Desc`), 
                              Expr5=NA, 
                              Expr6=NA,
                              Expr7=NA,
                              Reported_Fault=`Incident Desc`,
                              Action_Taken=NA
                              )                        


##View(INF_Only_Expr)

Signal <- filter(SIGNALLING, !(`Resp Code` %in%  c(29,30,31,32,47)))
Track <- filter(SIGNALLING, `Resp Code` %in%  c(47))
Power <- filter(SIGNALLING, `Resp Code` %in%  c(29,30,31,32))

#str(Signal)


Signal_Expr <- transmute(Signal, 
                           Service_Date=Date,
                           Incident_No=`Incident No`,
                           Line=`Train Line`,
                           Location=`Locn Desc`,
                           Asset_ID=`Equip No`,
                           Expr1 ="Asset", 
                           Expr2="Infrastructure", 
                           Expr3="Signal", 
                           Expr4=`L1 - System`, Expr5=`L2 - Sub-System`, Expr6=`L3 - Sub-System`,
                           Expr7=`L4 - Part List`,
                           Reported_Fault=`Reported Fault`,
                           Action_Taken=`Action Taken`
                           ) 



Track_Expr <- transmute(Track, 
                         Service_Date=Date,
                         Incident_No=`Incident No`,
                         Line=`Train Line`,
                         Location=`Locn Desc`,
                         Asset_ID=`Equip No`,
                         Expr1 = "Asset", Expr2="Infrastructure", Expr3="Track", 
                         Expr4=`L4 - Part List`, Expr5=NA, Expr6=NA, Expr7=NA,
                         Reported_Fault=`Reported Fault`,
                         Action_Taken=`Action Taken`
                         ) 

##View(Track_Expr)

Power_Expr <- transmute(Power, 
                         Service_Date=Date,
                         Incident_No=`Incident No`,
                         Line=`Train Line`,
                         Location=`Locn Desc`,
                         Asset_ID=`Equip No`,
                         Expr1 = "Asset", Expr2="Infrastructure", Expr3="Power Supply", 
                         Expr4="Signal Power", Expr5=NA, Expr6=`L4 - Part List`,
                         Expr7=NA,
                         Reported_Fault=`Reported Fault`,
                         Action_Taken=`Action Taken`) 

str(Power_Expr)


##FMP data 


# Import FMP data and check date  -----------------------------------------



FMP <- read.csv("FMP.csv", stringsAsFactors = F, check.names=F)

str(FMP)
## Watch out :  Fleet or Car type name!!   ###  SERVICE DATE
##Service Date or date



FMP_Expr <- transmute(FMP, 
                        Service_Date=`Service Date`,
                        Incident_No=`TPMIS No`,
                        Line=NA,
                        Location=NA,
                        Asset_ID=`Car No`,
                        Expr1 = "Asset", Expr2="Rolling Stock", Expr3=`Fleet`, 
                        Expr4=`System Description`, Expr5=`Component Code`, Expr6=NA,
                        Expr7=NA,
                        Reported_Fault=`Driver Report`,
                        Action_Taken=NA
                        ) 

### change this based on date format

str(FMP_Expr)


FMP_Expr <- UDF_Date('Service_Date', FMP_Expr)

max(FMP_Expr$Service_Date)




# Combine all the data sets -INFRA, SIGNALLING, FMP -----------------------




Consolidated_Expr1 <- rbind(INF_Only_Expr, Signal_Expr, Track_Expr, Power_Expr, FMP_Expr)

Consolidated_Expr2 <- mutate(Consolidated_Expr1, Location_From=NA, Location_To=NA) 

Consolidated_Expr <- select(Consolidated_Expr2,1:2,15,16,3:14)


sample_n(Consolidated_Expr, 1, replace = FALSE)


# Get PRS Incident Data for EXPR --------------------------------------------------




#==============================================================================================#


PRS_Incident <- read.csv("Incident Data Recategorised.csv", stringsAsFactors = F, check.names=F)



PRS_Incident


library(RODBC) 

dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=MT-LT02600\\SQLEXPRESS;
                              database=SAM;trusted_connection=true')


sqlSave(dbhandle, PRS_Incident , tablename ="Monthly_PRS_Incident_Temp", append = T,
        rownames = F, colnames = F, verbose = FALSE)









str(PRS_Incident)

PRS_Incident_Expr <-    transmute(PRS_Incident, 
                              Service_Date=Service_Date,  
                             `Incident_No`=Incident_No,
                              Location_From=Location_From,
                              Location_To=Location_To,
                              Line=Line_Description,
                              Location=NA,
                              Asset_ID=NA,
                              
                              Expr1=case_when(
                              PRS_Incident$Responsibility=='PRJ' ~ 'Projects',
                              PRS_Incident$Responsibility=='INF' ~ 'Asset', 
                              PRS_Incident$Responsibility=='TRN' ~ 'Asset', 
                              TRUE ~ "Operational"),
                              
                              Expr2=case_when(
                                PRS_Incident$Responsibility=='INF' ~ 'Infrastructure',
                                PRS_Incident$Responsibility=='TRN' ~ 'Rolling Stock', 
                                TRUE ~ PRS_Incident$Responsibility),
                              
                              Expr3=case_when(
                                PRS_Incident$Responsibility=='INF' ~ PRS_Incident$Responsibility,
                                PRS_Incident$Responsibility=='TRN' ~ PRS_Incident$Responsibility, 
                                TRUE ~ PRS_Incident$CancelCause),
                              
                              Expr4=case_when(
                                 PRS_Incident$Responsibility=='INF' ~ PRS_Incident$CancelCause,
                                 PRS_Incident$Responsibility=='TRN' ~ PRS_Incident$CancelCause),
                              
                              Expr5=NA, 
                              Expr6=NA, 
                              Expr7=NA,
                              
                               Reported_Fault=Reasons,
                               Action_Taken=NA
                              )
                            


str(PRS_Incident_Expr)


PRS_Incident_Expr <- UDF_Date('Service_Date',PRS_Incident_Expr)



PRS_Incident_Distinct <- unique(PRS_Incident_Expr)    # Get Distinct records                       

OnlyInPRS <-anti_join(PRS_Incident_Distinct,Consolidated_Expr, by=c("Incident_No"="Incident_No")) 


str(OnlyInPRS)

# PRS_Incident_Distinct$Service_Date   <- as.Date(PRS_Incident_Distinct$Service_Date, "%d-%m-%Y")
# Consolidated_Expr$Service_Date  <- as.Date(Consolidated_Expr$Service_Date, "%Y-%m-%d")


PRSAndConsolidated <- inner_join(PRS_Incident_Distinct,Consolidated_Expr, by=c("Incident_No"="Incident_No"))

##View(PRSAndConsolidated_NonAssetinPRS )
PRSAndConsolidated_NonAssetinPRS <-PRSAndConsolidated[PRSAndConsolidated$Expr1.x!='Asset',]


##View(PRSAndConsolidated_NonAssetinPRS)

NonAssetIncident<- select(PRSAndConsolidated_NonAssetinPRS, -ends_with(".y"))

SelectedColumns <- sub("\\.x", "", colnames(NonAssetIncident)) 

colnames(NonAssetIncident) <- SelectedColumns


NonAsset <-   rbind(OnlyInPRS, NonAssetIncident)

Consolidated_Expr_Final <- rbind(NonAsset,Consolidated_Expr)

#View(Consolidated_Expr_Final)

str(Consolidated_Expr_Final)

#---------------------------------------------


Incident_Data_Ext1 <-left_join(PRS_Incident[,c("Service_Date","Incident_No", "TDN")],
                                     Consolidated_Expr_Final, by=c("Incident_No"="Incident_No"))
str(Incident_Data_Ext1)

Incident_Data_Ext2 <- select(Incident_Data_Ext1, -Service_Date.y)



Incident_Data_Ext3 <- rename(Incident_Data_Ext2, Service_Date=Service_Date.x)

##View(Incident_Data_Ext3)


#write.csv(Incident_Data_Ext1, "temp_test.csv", row.names = F)


Incident_Data_Ext <- unique(Incident_Data_Ext3)

#View(Incident_Data_Ext)

Incident_Data_Ext_Final <- mutate(Incident_Data_Ext, Responsibility=case_when(
                        Incident_Data_Ext$Expr2=="Rolling Stock" ~ "TRN",
                        Incident_Data_Ext$Expr2=="Infrastructure" ~ "INF",
                        TRUE ~ Incident_Data_Ext$Expr2 ) ) 



dim(PRS_Incident)
dim(Incident_Data_Ext1)
dim(Incident_Data_Ext)
#View(Incident_Data_Ext2)


sample_n(Incident_Data_Ext_Final, 5, replace = FALSE)

write.csv(Incident_Data_Ext_Final, "Incident_Data_Ext_Final.csv", row.names = F)





# Import Incident Data with EXPR and Performance Data ---------------------



incdata <- read.csv("Incident_Data_Ext_Final.csv", stringsAsFactors = FALSE)  #, fileEncoding="latin1")
perfdata <- read.csv("Performance Data Revised.csv", stringsAsFactors = FALSE, fileEncoding="latin1")


###View(incdata)
###View(perfdata)

str(incdata)
str(perfdata)



incdata <- UDF_Date('Service_Date', incdata)
perfdata <- UDF_Date('Service_Date', perfdata)



library(dplyr)

perf_inc_data <-  left_join(perfdata, incdata, by= c('Service_Date'= 'Service_Date', 'TDN' = 'TDN'))

## After joinining multiple record for same TDN+Service_Date can be repeated multiple 
##times due to TDN+Incident+Resp combination 

###View(perf_inc_data)

data1 <-  perf_inc_data[,c("Service_Date", "TDN", "Incident_No")]
##View(data1)

Inc_factor_per_tdn <- data1 %>% group_by(Service_Date,TDN) %>% summarise(Incident_Factor=1/n_distinct(Incident_No))

str(data1)

str(Inc_factor_per_tdn)

###View(Inc_factor_per_tdn)

data2 <-  perf_inc_data[,c("Service_Date", "TDN", "Incident_No", "Responsibility")]

#data2 <- filter(data2, !is.na(data2$Responsibility))

str(data2)


Resp_factor_per_inc <- data2 %>% group_by(Service_Date,TDN, Incident_No) %>% summarise(Resp_Factor=1/n_distinct(Responsibility))


combined_kpi_factor_stg1 <- left_join(Resp_factor_per_inc, Inc_factor_per_tdn , by= c('Service_Date'= 'Service_Date', 'TDN'='TDN'))



combined_kpi_factor_stg1$combined_kpi_factor <- combined_kpi_factor_stg1$Resp_Factor * combined_kpi_factor_stg1$Incident_Factor




perf_inc_data_Stg1 <-  left_join(perf_inc_data, combined_kpi_factor_stg1 , by= c('Service_Date'= 'Service_Date', 'TDN'='TDN', 'Incident_No'='Incident_No'))




# insert code to adjust uneven allocation ---------------------------------



TrainFleetRel <-  read.csv("Train Fleet Reliability Lost.csv", stringsAsFactors = FALSE, fileEncoding="latin1")

#str(TrainFleetRel)


TrainFleetRel$Service_Date <- as.Date(TrainFleetRel$Service_Date, "%d-%m-%Y")

TrainFleetRel_stg2 <- TrainFleetRel[, c("Service_Date", "Incident_No", "Resp", "Percentage")]

TrainFleetRel_stg3 <- rename(TrainFleetRel_stg2, Resp_TFRel=Resp, Percentage_TFRel=Percentage,
                             Incident_No_TFRel=Incident_No)

str(TrainFleetRel_stg3)

###View(TrainFleetRel_stg3)


library(tidyr)  # to use separate function - to split Resp and Percenatge 

df1 <- TrainFleetRel_stg3 %>% separate(Resp_TFRel, c("Resp1", "Resp2", "Resp3", "Resp4"), sep = "\\,") %>% separate(Percentage_TFRel, 
                                                                                                                    c("Percentage1", "Percentage2", "Percentage3", "Percentage4"), sep = "\\,")

##View(df1)

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

###View(non_even_allocation)


###View(non_even_allocation)


########### end of non even allocation code block


interim1 <- left_join(perf_inc_data_Stg1,non_even_allocation, by= c('Service_Date'= 'Service_Date', 'Incident_No'='Incident_No_TFRel'))

##View(interim1)


# 2nd Level of KPI calculation --------------------------------------------





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




# Derive the measurements -------------------------------------------------



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

###View(interim1)

write.csv(interim1, "Performance Model_Extended.csv", row.names = F)



# Load Data to RDMS -------------------------------------------------------



library(RODBC) 

dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=MT-LT02600\\SQLEXPRESS;
                              database=SAM;trusted_connection=true')


#APM_data <- read.csv("Asset Performance Model.csv", stringsAsFactors = F, fileEncoding="latin1")
##View(F_OPR_DAILY)

##View(APM_data)

# ALTER TABLE [dbo].[Asset_Performance_Model] ALTER COLUMN [Reasons] VARCHAR(2000);
# ALTER TABLE [dbo].[Asset_Performance_Model] ALTER COLUMN [Service_Date] date;
#
#sqlClear(dbhandle, "Performance_Model_Extended", errors = TRUE)


#sqlClear(dbhandle, "Incremental_Performance_Model_Extended", errors = TRUE)

#sqlDrop(dbhandle, "Incremental_Performance_Model_Extended", errors = TRUE)




sqlSave(dbhandle, interim1, tablename ="Incremental_Performance_Model_Extended", append = T,
        rownames = F, colnames = F, verbose = FALSE)




sqlQuery( dbhandle, "UPDATE [dbo].[Incremental_Performance_Model_Extended]
SET Expr2 =
          CASE
          WHEN Expr2 = 'EXT' THEN 'EXT - External Factors'
          WHEN Expr2 = 'MET' THEN 'MET - Metro Trains'
          WHEN Expr2 = 'OPS' THEN 'OPS - Operations'
          WHEN Expr2 = 'OTH' THEN 'OTH - Others'
          WHEN Expr2 = 'PNL' THEN 'PNL - Pacific National Limited'
          WHEN Expr2 = 'PRJ' THEN 'PJT - Projects'
          WHEN Expr2 = 'PSI' THEN 'PSI - Passenger Impact'
          WHEN Expr2 = 'QBE' THEN 'QBE - Qube Logistics'
          WHEN Expr2 = 'VAN' THEN 'VAN - Vandals'
          WHEN Expr2 = 'VLP' THEN 'VLP - V/Line Passenger'
          ELSE Expr2
          END")


sqlQuery(dbhandle, 
         
         "UPDATE [dbo].[Incremental_Performance_Model_Extended]
         SET Expr3 =
         CASE
         WHEN Expr3 = 'AO' THEN 'AO - Stations'
         WHEN Expr3 = 'CRP' THEN 'CRP - Craigieburn Rail Project'
         WHEN Expr3 = 'DAO' THEN 'DAO - Drivers Allocation Officer'
         WHEN Expr3 = 'DER' THEN 'DER - Derailment'
         WHEN Expr3 = 'DRC' THEN 'DRC - Dandenong Rail Corridor'
         WHEN Expr3 = 'DTRS' THEN 'DTRS - Digital Train Radio System'
         WHEN Expr3 = 'FGT' THEN 'FGT - Freight Train'
         WHEN Expr3 = 'FT' THEN 'FT - Faulty Train'
         WHEN Expr3 = 'GRF' THEN 'GRF - Graffiti'
         WHEN Expr3 = 'ILL' THEN 'ILL - ILL Passenger'
         WHEN (Expr3 = 'INF' AND Expr2 != 'Infrastructure') THEN 'INF - Infrastructure'
         WHEN (Expr3 = 'INF' AND Expr2 = 'Infrastructure') THEN 'Infrastructure Unallocated'
         WHEN Expr3 = 'LX' THEN 'LXI - Level crossing Incident'
         WHEN Expr3 = 'MET' THEN 'MET - Metro Trains'
         WHEN Expr3 = 'MTL' THEN 'MTL - Metrol'
         WHEN Expr3 = 'OTH' THEN 'OTH - Others'
         WHEN Expr3 = 'PAS' THEN 'PAS - Passenger Train'
         WHEN Expr3 = 'PJT' THEN 'PJT - Projects'
         WHEN Expr3 = 'PLO' THEN 'PLO - Police Operations'
         WHEN Expr3 = 'RRL' THEN 'RRL - Regional Rail Link Project'
         WHEN Expr3 = 'SIG' THEN 'SIG - Signaller'
         WHEN Expr3 = 'TCF' THEN 'TCF - Track Circuit Failure'
         WHEN Expr3 = 'TPS' THEN 'TPS - Trespasser Struck'
         WHEN (Expr3 = 'TRN' AND Expr2 != 'Rolling Stock') THEN 'TRN - Fleet Maintenance' 
         WHEN (Expr3 = 'TRN' AND Expr2 = 'Rolling Stock') THEN 'Rolling Stock Unallocated'
         WHEN Expr3 = 'TRS' THEN 'TRS - Trespasser'
         WHEN Expr3 = 'TS' THEN 'TS - Train Services'
         WHEN Expr3 = 'TTC' THEN 'TTC - Timetable Constraints'
         WHEN Expr3 = 'UPG' THEN 'UPG - Unruly Passenger'
         WHEN Expr3 = 'VAN' THEN 'VAN - Vandalism'
         WHEN Expr3 = 'WHC' THEN 'WHC - Wheelchairs'
         WHEN Expr3 = 'WHR' THEN 'WHR - Weather'
         WHEN Expr3 = 'WPL' THEN 'WPL - Wait Passenger Load'
         ELSE Expr3
         END")



sqlQuery(dbhandle, "UPDATE [dbo].[Incremental_Performance_Model_Extended]
         SET Expr3 =
         CASE
         WHEN UPPER(Expr3) LIKE '%TRAP%' THEN 'XTrap'
         ELSE Expr3
         END")




sqlQuery(dbhandle, "UPDATE Incremental_Performance_Model_Extended SET Location_From=Monthly_PRS_Incident_Temp.Location_From
         FROM Monthly_PRS_Incident_Temp
         WHERE Incremental_Performance_Model_Extended.Incident_No= Monthly_PRS_Incident_Temp.Incident_No")



sqlQuery(dbhandle, "UPDATE Incremental_Performance_Model_Extended SET Location_To=Monthly_PRS_Incident_Temp.Location_To
         FROM Monthly_PRS_Incident_Temp
         WHERE Incremental_Performance_Model_Extended.Incident_No= Monthly_PRS_Incident_Temp.Incident_No")






sqlQuery(dbhandle, "UPDATE Incremental_Performance_Model_Extended SET Expr4 = Monthly_PRS_Incident_Temp.recategorised 
         FROM Monthly_PRS_Incident_Temp
         WHERE Incremental_Performance_Model_Extended.Incident_No = Monthly_PRS_Incident_Temp.Incident_No
         AND Expr1 = 'Operational'")




sqlQuery(dbhandle, "UPDATE [dbo].[Incremental_Performance_Model_Extended]
         SET Expr2 = 
         CASE
         WHEN Expr2 = 'VAN - Vandals' THEN 'EXT - External Factors'
         WHEN Expr2 = 'MET - Metro Trains' THEN 'OPS - Operations'
         ELSE Expr2
         END")




sqlQuery(dbhandle, "UPDATE [dbo].[Incremental_Performance_Model_Extended]
         SET Expr4 = 
         CASE
         WHEN (Expr4 = 'Other' AND Expr2 = 'PSI - Passenger Impact') THEN 'Others - Passenger Related (Vomit etc)'
         WHEN (Expr4 = 'Other' AND Expr2 = 'VLP - V/Line Passenger') THEN 'V/Line (Others)'
         WHEN (Expr4 = 'Other' AND Expr3 = 'WHR - Weather') THEN 'Others - Weather'
         WHEN (Expr4 = 'Other' AND Expr3 = 'LXI - Level crossing Incident') THEN 'Trespasser'
         ELSE Expr4
         END")
sqlQuery(dbhandle, "UPDATE [dbo].[Incremental_Performance_Model_Extended]
         SET Expr5 = Expr2 WHERE Expr3 = 'FGT - Freight Train'")




sqlQuery(dbhandle, "UPDATE [dbo].[Incremental_Performance_Model_Extended]
         SET Expr3 =
         CASE
         WHEN Expr4 = 'Wait Pass Loading' THEN 'Wait Passenger Loading'
         WHEN Expr4 = 'Ill Pass' THEN 'Pass Behaviour (ill pass, unruly, pass related)'
         WHEN Expr4 = 'Unruly Pass' THEN 'Pass Behaviour (ill pass, unruly, pass related)'
         WHEN Expr4 = 'Wheelchair Traffic' THEN 'Pass Behaviour (ill pass, unruly, pass related)'
         WHEN Expr4 = 'Event Traffic' THEN 'Pass Behaviour (ill pass, unruly, pass related)'
         WHEN Expr4 = 'Others - Passenger Related (Vomit etc)' THEN 'Pass Behaviour (ill pass, unruly, pass related)'
         WHEN Expr4 = 'Staff Error' THEN 'Operations'
         WHEN Expr4 = 'Metro' THEN 'Operations'
         WHEN Expr4 = 'Train Driver Related' THEN 'Operations'
         WHEN Expr4 = 'Storm' THEN 'Inclement Weather'
         WHEN Expr4 = 'Rain' THEN 'Inclement Weather'
         WHEN (Expr4 = 'Others - Weather' AND Expr3 = 'WHR - Weather') THEN 'Inclement Weather'
         WHEN Expr4 = 'Graffiti' THEN 'Extl Factors (graffiti, vandalism, police ops, trespasser)'
         WHEN Expr4 = 'Vandalism' THEN 'Extl Factors (graffiti, vandalism, police ops, trespasser)'
         WHEN Expr4 = 'Police ops' THEN 'Extl Factors (graffiti, vandalism, police ops, trespasser)'
         WHEN Expr4 = 'Trespasser' THEN 'Extl Factors (graffiti, vandalism, police ops, trespasser)'
         WHEN (Expr3 = 'FGT - Freight Train' AND Expr1 = 'Operational') THEN 'Extl Factors (graffiti, vandalism, police ops, trespasser)'
         WHEN ((Expr3 = 'ELL' OR Expr3 = 'ELE') AND Expr1 = 'Operational') THEN 'Extl Factors (graffiti, vandalism, police ops, trespasser)'
         WHEN (Expr3 = 'PAS - Passenger Train' AND Expr1 = 'Operational' AND Expr2 != 'VLP - V/Line Passenger') THEN 'Extl Factors (graffiti, vandalism, police ops, trespasser)'
         WHEN (Expr3 = 'LXI - Level crossing Incident' AND Expr1 = 'Operational') THEN 'Extl Factors (graffiti, vandalism, police ops, trespasser)'
         WHEN (Expr3 = 'MIS' AND Expr1 = 'Operational') THEN 'Extl Factors (graffiti, vandalism, police ops, trespasser)'
         WHEN Expr4 = 'Late Departure / Presentation' THEN 'VLP'
         WHEN Expr4 = 'Wait Passengers / Interchange / Congestion' THEN 'VLP'
         WHEN Expr4 = 'V/Line (Others)' THEN 'VLP'
         WHEN (Expr4 = 'Other' AND Expr3 = 'MIS') THEN 'Extl Factors (graffiti, vandalism, police ops, trespasser)'
         WHEN (Expr4 = 'Other' AND Expr3 = 'MISC') THEN 'Extl Factors (graffiti, vandalism, police ops, trespasser)'
         WHEN (Expr4 = 'Other' AND Expr3 = 'OTH - Others') THEN 'Extl Factors (graffiti, vandalism, police ops, trespasser)'
         WHEN (Expr4 = 'Other' AND Expr3 = 'TCF - Track Circuit Failure') THEN 'Extl Factors (graffiti, vandalism, police ops, trespasser)'
         ELSE Expr3
         END")




# Correction of Rolling Stock's Expr4 labels ------------------------------



sqlQuery(dbhandle,"UPDATE Incremental_Performance_Model_Extended
SET Expr4 =
  CASE
WHEN Expr4 LIKE '%Power Conversion (MA)%' THEN 'Power Conversion (Motor Alternator)'
ELSE Expr4
END
WHERE Expr2='Rolling Stock'")	



sqlQuery(dbhandle,"UPDATE Incremental_Performance_Model_Extended
SET Expr4 =
  CASE
WHEN Expr4 LIKE '%Body & External Fittings%' THEN 'Body and External Fittings'
ELSE Expr4
END
WHERE Expr2='Rolling Stock'")	



sqlQuery(dbhandle,"UPDATE Incremental_Performance_Model_Extended
SET Expr4 =
  CASE
WHEN Expr4 LIKE '%Internal Fittings & Furnishing%' THEN 'Internal Fittings & Furnishings'
ELSE Expr4
END
WHERE Expr2='Rolling Stock'")



sqlQuery(dbhandle,"UPDATE Incremental_Performance_Model_Extended
SET Expr4 =
  CASE
WHEN Expr4 LIKE '%Power Supply (Panto & Battery)%' THEN 'Power Supply (Pantograph & Battery)'
ELSE Expr4
END
WHERE Expr2='Rolling Stock'")	



sqlQuery(dbhandle,"UPDATE Incremental_Performance_Model_Extended
SET Expr4 =
  CASE
WHEN Expr4 LIKE '%Enviromental Control%' THEN 'Environmental Control'
ELSE Expr4
END
WHERE Expr2='Rolling Stock'")	



sqlQuery(dbhandle,"UPDATE Incremental_Performance_Model_Extended
SET Expr4 =
  CASE
WHEN Expr4 LIKE '%Wheelslip%' THEN 'Wheel Slip (Drive Control)'
ELSE Expr4
END
WHERE Expr2='Rolling Stock'")	



# Append the Incremental data to master data table  -----------------------



sqlQuery(dbhandle, "INSERT INTO Performance_Model_Extended_WIP SELECT * FROM  Incremental_Performance_Model_Extended")


sqlClear(dbhandle, "Incremental_Performance_Model_Extended", errors = TRUE)

sqlClear(dbhandle, "Monthly_PRS_Incident_Temp", errors = TRUE)


odbcCloseAll()


