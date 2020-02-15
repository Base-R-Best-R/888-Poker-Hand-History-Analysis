## Handy History obtained as a .txt file from 888
# rm(list = ls())
library(stringr)
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
for(i in 1:length(sesh)){
  sesh[[i]] <- as.character(unlist(sesh[[i]]))
}
# total number of hands 2 NL hands played
k <- lapply(sesh, function(x){
  grep("*****", x, fixed = T)
})
HandS <- sapply(k, length)
sum(HandS) # 2878 Hands of 2NL
#
# setwd("~/GitHub/888-Poker-Hand-History-Analysis/R Data")
saveRDS(sesh, file ="Hand_History_List.rds")
##########################################################################################################################


