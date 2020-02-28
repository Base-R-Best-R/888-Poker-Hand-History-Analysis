## Statistics ##
getwd()
setwd("C:/Users/blasc/OneDrive/Documents/GitHub/888-Poker-Hand-History-Analysis/R Data") # can be found in the R Data folder
Hands <- readRDS(list.files()[2])
# VPIP - measures how often one voluntarily pays money into a pot before seeing the flop
f <- lapply(Hands, function(x){
  lapply(x, grep, pattern = "B2B_Suckouts bets|B2B_Suckouts raises")
})
#
m <- lapply(f, function(x){
  lapply(x, function(y){
    2 %in% y
  })
})
# VPIP
VPIP <- sum(unlist(m)) / sum(sapply(Hands, length)) # Interestingly quite a bit tighter than expected

