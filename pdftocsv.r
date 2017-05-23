setwd("C:/Users/kazi.wadud/Desktop/Lifts & Escalators - PDF file collection")
backupdir <- "C:/Users/kazi.wadud/Desktop/Lifts & Escalators - PDF file collection/backup/"

library(pdftables)

fl <- list.files(pattern ="*.pdf")   #list all pdf files

for (i in 1:length(fl)) {

convert_pdf(fl[i], paste0(gsub(".pdf","",fl[i]), ".csv"), api_key="******")

file.copy(fl[i], paste0(backupdir,fl[i]))
   
rpt_date <-gsub( (".pdf"), "", sub("\\D{1,}","",fl[i]))

df <- read.csv(paste0(gsub(".pdf","",fl[i]), ".csv"), stringsAsFactors = FALSE)


df$RptDate <- rpt_date  
write.csv (df,paste0(gsub(".pdf","",fl[i]), ".csv"), row.names = FALSE)

                        }

#Read CSV one by one



fl <- list.files(pattern ="*.csv") 

library(dplyr)

for (i in 1:length(fl))  {
  
  df1 <- read.csv(fl[i], stringsAsFactors = FALSE)
  
  #str(df1)
  
  df2 <- df1[-c(1:5),]
  
  df3 <- df2[!df2$X=="",]  ##Exclude Rows with 1st column blank
  
  colnames(df3) <- df3[1,] 
  
  #View(df4) 
  #str(df4)
  df4 <- df3[!df3$Date=="Date",] #Remove the first row which has has been assigned as column name
  colnames(df4)[ncol(df4)] <- "RptDate"  #Rename last column as RptDate
  
  n1 <- colnames(df4)
  
  colnames(df4) <- ifelse(n1=="" | n1=="NA","Blank", n1)
  
  ColumnNames <- c()  #Define empty vector
  
  for (c in 1:ncol(df4)) {
    
    ColumnNames[c] <- ifelse(colnames(df4)[c]=="Blank", paste0("Blank",c),colnames(df4)[c])
    
  }
  
  colnames(df4) <- ColumnNames
  
  df4<-  select(df4, -contains("Blank"))
  
  
  if (ncol(df4)==11) {
    
    df4$UnitReturendToService <- NA
    df4 <- df4[, c(1:10,12,11)]
  } 
    
  
  write.table(df4,"combinedintable.csv", append = T, sep=',', col.names = F , row.names = F )
  #write.xlsx(df4 , file="data.xlsx" , sheet="T", row.names=FALSE, col.names = TRUE,  append=T)
  
}
