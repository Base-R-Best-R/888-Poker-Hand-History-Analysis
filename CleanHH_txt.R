## Handy History obtained as a .txt file from 888
# rm(list = ls())
library(stringr)
getwd()
# setwd("~/GitHub/888-Poker-Hand-History-Analysis/2NL Hands")
### Importing hand history .txt files ### 
?read.delim
x <- list.files() # Hands from each session 50 in total
# rm files from folder that 888 saves the files to in order to avoid duplicates later
# setwd("~/888poker/HandHistory/B2B_Suckouts")
# sum(list.files() %in% x)
# file.remove(x)
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
k <- lapply(k, function(x) x[-1] - 1 )
HandS <- sapply(k, length)
sum(HandS) # 2878 Hands of 2NL
## Hands Played Barplot ##
Handbd <- tapply(HandS, names(sesh), sum)
names(HandS)
barplot(Handbd, main = "2NL Hands Played", ylim = c(0,900))
abline(h = mean(Handbd), col = "red") # average is 200 hands a day
# split session into hands
splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
# Handwise
eval(parse(text = paste(c("seshBh<-list(",rep("list(),", 49), "list())"), collapse = ""))) # empty list of lists
names(seshBh) <- names(sesh) # keep names
## split by hand
for(i in seq.int(50)){
  seshBh[[i]] <- splitAt(sesh[[i]], k[[i]])
}
## Write function that filters hands into pre-flop / flop / turn / river 
mutatehand <- function(x){
  upper <- grep("big blind", x, fixed = T) # find Index that splits info into 
  Hand <- splitAt(x, upper + 1) 
  Hand_info <- Hand[[1]]
  ## Split the remainder by ** to obtain downcards, flop, river and turn ##
  remain <- Hand[[2]]
  sp <- splitAt(remain, grep("**", remain, fixed = T))
  ## return list of info plus play 
  re <- c(list(Hand_info),
             sp)
  return(re)
}
mutatehand()
# setwd("~/GitHub/888-Poker-Hand-History-Analysis/R Data")
# saveRDS(seshBh, file ="Hand_History_List.rds")
##########################################################################################################################
#eval(parse(text = paste(c("mutatedHands<-list(",rep("list(),", 49), "list())"), collapse = ""))) # empty list of lists
#names(mutatedHands) <- names(sesh)

#mutatedHands <- lapply(seshBh[1:14], function(x){
#  lapply(x, mutatehand)
#})
################# debugging ###########
# deosnt work for session 15
seshBh[[15]][[1]]
mutatehand(seshBh[[15]][[1]])
##
# setwd("~/GitHub/888-Poker-Hand-History-Analysis/R Data")
# saveRDS(mutatedHands, file ="Hands_organized.rds")
