#The script to automate the data processing for Power Modelling/track
#Written by - Kazi Toufiq Wadud    Date : 26-Sept-2016

library("xlsx")    # To write .xls file
library("readxl")  # To read .xls file

setwd("J:/xxx/Volts")

# List all the files name in the directory
xlsfiles <- list.files(pattern ="*.xls")


for (i in 1:length(xlsfiles))  {
  
  base_data <- read_excel(xlsfiles[i], sheet="Results for all train types", col_names = FALSE)
  base_data <- base_data[995:nrow(base_data),] 
  df <-  base_data[, c(1,2,23,27,21,25)]
  df <- df[order(df$X1),]
  colnames(df) <-c("From Sation", "To Station",  "Min V1", "Min V2", "AV Pwd V1", "AV Pwd V2")
  df <- as.data.frame(df)
  df$`Min V1` <- as.numeric(df$`Min V1`)
  df$`Min V2` <- as.numeric(df$`Min V2`)
  df$`AV Pwd V1` <- as.numeric(df$`AV Pwd V1`) 
  df$`AV Pwd V2` <- as.numeric(df$`AV Pwd V2`)
  write.xlsx(df , paste0("Track -", xlsfiles[i]), row.names=FALSE) 
  
  write.xlsx(df, "./output/Master.xls", sheetName=xlsfiles[i], append = TRUE, row.names = FALSE )

  }
