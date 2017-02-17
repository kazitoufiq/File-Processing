setwd("J:/Asset Management/Asset Performance and Investment/11_Analytics/Dwell Time Analysis")
library(readxl)
library(xlsx)
library(chron)
library(dplyr)

t1 <- read.csv("2016-01-13DwellTimeRawData.csv", header=T, stringsAsFactors = F, fileEncoding="UTF-8-BOM")

t1$TimeOnLine <-ifelse(t1$Planned_Departure_Time=="", t1$Planned_Arrival_Time, t1$Planned_Departure_Time)
t1$TimeOnLine <- as.POSIXlt(t1$TimeOnLine, format = "%H:%M:%S")

t1 <- t1[!is.na(t1$TimeOnLine),]

View(t1)

EndTime <- setNames(aggregate(t1$TimeOnLine, by=list(TDN=t1$TDN, Line=t1$Line), FUN=function(x) max(x, na.rm=TRUE)), c("TDN", "Line", "End"))
StartTime <- setNames(aggregate(t1$TimeOnLine, by=list(TDN=t1$TDN, Line=t1$Line), FUN=function(x) min(x, na.rm=TRUE)), c("TDN", "Line", "Start"))

str(EndTime)
str(StartTime)

TimeByTDN <- inner_join(StartTime,EndTime, by=c("TDN", "Line"))

TimeByTDN <- filter(TimeByTDN, Line %in% c("WER"))


View(TimeByTDN)

str(TimeByTDN)

myls <- vector("list", length=nrow(TimeByTDN))

for (i in 1:nrow(TimeByTDN)) {

  r1 <- seq(TimeByTDN[i,3], TimeByTDN[i,4], by="min")
  
  myls[[i]]   <- r1
  
   }


indx <- sapply(myls, length)
str(myls)

res <- as.data.frame(do.call(rbind,lapply(myls, `length<-`, max(indx))))
head(res)

str(res)
                    
df2 <- cbind(TimeByTDN$TDN, TimeByTDN$Line, res)

str(df2)

gg <- as.data.frame(table(as.vector(t(df2[,-c(1,2)]))),stringsAsFactors = FALSE)

tt1 <- as.POSIXct(as.numeric(gg$Var1), origin = "1970-01-01")

linebusy <-  cbind(tt1,gg)

View(linebusy)




