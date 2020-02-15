## Handy History obtained as a .txt file from 888
# rm(list = ls())
getwd()
# setwd("~/GitHub/888-Poker-Hand-History-Analysis/2NL Hands")
### Importing hand history .txt files ### 
?read.delim
x <- list.files() # Hands from each session 50 in total
# create list of sessions
sesh <- list()
sesh <- lapply(x, read.delim)
# obtain the dates of the recorded sessions
date <- sapply(x, function(x){
  gsub("888poker", "", unlist(strsplit(x, " "))[1], fixed = T)
})
# convert to date and assign to list
date <- as.Date(date, "%Y%m%d")
names(sesh) <- date
# unlist sessions within list
sesh <- lapply(sesh, as.vector)



# setwd("~/GitHub/888-Poker-Hand-History-Analysis/R Data")
saveRDS(sesh, file ="Hand_History_List.rds")
##########################################################################################################################


