setwd("C:/Users/Documents/R Script")
incident_data <- read.csv("INCIDENT_AUG_SEPT.csv")
station_ref <- read.csv("STATION_CODE_MASTER.csv")


station.seq <- read.csv("STATION_SEQ.csv")

incident_data$From_Location <- trimws(incident_data$From_Location)
incident_data$To_Location <- trimws(incident_data$To_Location)


View(incident_data)
View(station_ref)

View(station.seq)



FI1 <-  merge(x=incident_data[incident_data$Line=='WER',], y=station.seq, by.x=c('From_Location', 'Line'), by.y=c('STATION_CODE', 'LINE'), all.x=TRUE)

FI2 <-  merge(x=FI1[FI1$Line=='WER',], y=station.seq, by.x=c('To_Location', 'Line'), by.y=c('STATION_CODE', 'LINE'), all.x=TRUE)


FI2 <- FI2[!is.na(FI2$SEQ_NO.x), ]


FI2 <- FI2[!is.na(FI2$SEQ_NO.y), ]



View(FI2)


##3incident_data[incident_data$Line=='WER',]


froms <- FI2$SEQ_NO.x

str(froms)

tos <- FI2$SEQ_NO.y

str(tos)


from <- as.data.frame(froms)

to <- as.data.frame(tos)


zz <- cbind (from, to)


z1_max <- apply(zz, 1, max)

z1_min <- apply(zz, 1, min)


z <- cbind(z1_min, z1_max)




#View(FI2_SEQ  <- (c(FI2$SEQ_NO.x, FI2$SEQ_NO.y)))





myls <- vector("list", length = nrow(z))



for(i in 1:nrow(z)){
  
  myls[[i]] <- seq(from=z[i,1], to=z[i,2])
  
}

myls

indx <- sapply(myls, length)

res <- as.data.frame(do.call(rbind,lapply(myls, `length<-`,
                                          max(indx))))

#mat_res <- as.matrix(res)

#class(mat_res)

#table(mat_res)


myfunction <- function(x){
  1/length(x[!is.na(x)])
  
}

row.sums <- apply(res, 1, myfunction)


row.sums

t <-cbind(res, row.sums)

t


myf <- function(x){
  paste(x, sep="", collapse="-")
  
}



myls1 <- vector("list", length = nrow(t)) 

for(i in 1:nrow(t)) {
  
  myls1[[i]] <- combn(as.matrix(t)[i,],2, myf , simplify = TRUE)
  
  
}


data.f1 <-as.data.frame(unlist(myls1))

colnames(data.f1) <- c("Seq") 

data.f1

foo <- data.frame(do.call('rbind', strsplit(as.character(data.f1$Seq),'-',fixed=TRUE)))

indx <- sapply(foo, is.factor)

foo[indx] <- lapply(foo[indx], function(x) as.numeric(as.character(x)))

str(foo)

foo
colnames(foo) <- c("Seq1", "Seq2") 

foo1 <- foo[which((foo$Seq1<=1 & foo$Seq2>1) | (foo$Seq1>1 & foo$Seq2<=1 )),]

foo1

aggregate(foo1$Seq2, by=list(Seq1=foo1$Seq1), FUN=sum)


