#The script to automate the data processing for Network Modelling
#Written by - Kazi Toufiq Wadud 



library("xlsx")    # To write .xls file
library("readxl")  # To read .xls file


setwd("C:/Users/kazi.wadud/Desktop/KIA")

# Read Normal Condition Data
base_data <- read_excel("2. Normal Condition.xls", sheet="SUBSTATIONS (2)", col_names = FALSE, skip=10)

#Move file to another directory to avoid conflict while reading other .xls files later in the script
file.rename("3. Normal Condition.xls", "./output/3. Normal Condition.xls")

#Select required columns
df <-  base_data[, c(1,53,56)]

#Sort column 1 alphabatically 
df <- df[order(df$X1),]

colnames(df) <-c("Substation", "Rating (kW)", "Loading (%)")

df <- as.data.frame(df)

write.xlsx(df, "./output/base_data.xls", row.names=FALSE)



# List all the files name in the directory
xlsfiles <- list.files(pattern ="*.xls")


# Run Loop to read relevent data from each of the file and write the output to separate files and also in different sheets 
#in the same work book 
# Also write the data table with append mode to a CSV for later read and aggregation


for (i in 1:length(xlsfiles))  {
  
  N_min_1_data <- read_excel(xlsfiles[i], sheet="SUBSTATIONS (2)", col_names = FALSE, skip=10)
  N_min_1_data <-N_min_1_data[, c(1,53,56)]
  
  N_min_1_data <- N_min_1_data[order(N_min_1_data$X1),]
  colnames(N_min_1_data) <-c("Substation", "Rating1 (kW)", "Loading1 (%)")
  
  N_min_1_data <- as.data.frame(N_min_1_data)
  
  
  N_min_1.result  <- merge(x=df, y=N_min_1_data, all.x=TRUE)
  
  
  write.xlsx(N_min_1.result , paste0("n-1 Report ", xlsfiles[i]), row.names=FALSE)  
  
  write.table(N_min_1.result, file='Master_ALL.csv', append = TRUE,  sep = ",", row.names = FALSE, col.names = FALSE )
    
  write.xlsx(N_min_1.result , file="N-1 Report.xlsx" , sheet= paste0(xlsfiles[i]), row.names=FALSE, append=TRUE)  
  
  
}


master_df <- read.csv("Master_ALL.csv", header = FALSE, stringsAsFactors = FALSE)

colnames(master_df) <-c("Substation", "Rating (kW)", "Loading (%)", "Rating1 (kW)",  "Loading1 (%)" )

head(master_df)


master_df <-  master_df[!(is.na(master_df$`Loading1 (%)`)),]

agg_master_df <- aggregate(master_df$`Loading1 (%)`, by=list(Substation=master_df$Substation), FUN=max)

colnames(agg_master_df) <- c("Substation", "Max_Loading")


final <- merge(x=df, y=agg_master_df, all.x = TRUE)

write.xlsx(final, "./output/final_report.xls", row.names=FALSE)










