# A SIMPLE R Script to get final output with records which are missing in one of the files
# Scope: compare between two files

setwd("C:/Users/kazi.wadud/Documents/R Script/COMPARE")
old_df = read.csv("old.csv", stringsAsFactors = FALSE)
new_df = read.csv("new.csv", stringsAsFactors = FALSE)

nrow(old_df)
nrow(new_df)

require(dplyr) 

str(new_df)
str(old_df)

dd <-anti_join(old_df, new_df)  # output will ONLY show the record which is EXCLUSIVELY present in left dataframe
#but not in right data frame

dd$comment <- "PRESENT IN OLDDF ONLY"

View(dd)

tt <-anti_join(new_df, old_df)
tt$comment <- "PRESENT IN newdf ONLY"

View(tt)

final_df <- rbind(dd, tt)
View(final_df)
