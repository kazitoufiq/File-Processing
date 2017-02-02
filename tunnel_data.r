setwd("J:/Shared/_Kia/Kazi/Normal Condition")
source("http://www.omegahat.net/RDCOMClient/examples/excelUtils3.R")
library(RDCOMClient)

inputdata <- read.csv("MTP MM day 1 normal condition feeder currents for Thermal calculation.csv", header = TRUE, stringsAsFactors = FALSE)
str(inputdata)
#View(inputdata)
col_name <- colnames(inputdata)


for (i in 1:length(col_name)) {
  
  col_data  <-  as.data.frame(inputdata[, c(i)])
  col_data_1 <- as.data.frame(trimws(col_data[,1]))

  colnames(col_data_1) <- c(col_name[i])
  
  f_name <- paste0(gsub("\\.{1,}", " ",col_name[i]),".xlsx")

  
  #selected_f_name <- list.files(pattern =f_name)
  #selected_file <- list.files(pattern =selected_f_name)
  
  
  xlApp <- COMCreate("Excel.Application")
  print(col_name[i])
  print(f_name)
  wb<- xlApp[["Workbooks"]]$Open(paste0("J:/Shared/_Kia/Kazi/Normal Condition/",f_name))
  sheet <- wb$Worksheets("Raw Data")
  
  #source("http://www.omegahat.net/RDCOMClient/examples/excelUtils3.R")
  
  rdcomexport <- function(x) {
    #sh = wb[["Worksheets"]]$Add()
    sheet <- wb$Worksheets("Raw Data")
    #sh[["Name"]] <- as.character(x$Species[1])
    exportDataFrame(x, at = sheet$Range("A1"))
  }
  
  rdcomexport(col_data_1)
  
  wb$Save()   
  xlApp$Quit() 
  
  
  
}
