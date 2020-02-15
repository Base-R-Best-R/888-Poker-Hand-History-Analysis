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
date <- lapply(x, function(x){
  gsub("888poker", "", unlist(strsplit(x, " "))[1], fixed = T)
})
# Utilize anydate to convert class
library(anytime)
anydate(date[1])

?gsub
saveRDS(sesh, file ="Hand_History_List.rds")
#####################################################################################################################
?file.info
