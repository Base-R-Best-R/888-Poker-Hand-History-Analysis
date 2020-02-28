## Statistics ##
getwd()
setwd("C:/Users/blasc/OneDrive/Documents/GitHub/888-Poker-Hand-History-Analysis/R Data") # can be found in the R Data folder
Hands <- readRDS(list.files()[2])
# VPIP - measures how often one voluntarily pays money into a pot before seeing the flop
f <- lapply(Hands, function(x){
  lapply(x, grep, pattern = "B2B_Suckouts bets|B2B_Suckouts raises|B2B_Suckouts calls")
})
#
m <- lapply(f, function(x){
  lapply(x, function(y){
    2 %in% y
  })
})
# VPIP
VPIP <- sum(unlist(m)) / sum(sapply(Hands, length)) # note that not all recorded sessions are full-ring
VPIP # For a tight aggressive player a value between 15 % and 20 % is considered optimal 
############################################################################################################################
# PFR - indicates how often one raised before the flop is seen
f1 <- lapply(Hands, function(x){
  lapply(x, grep, pattern = "B2B_Suckouts bets|B2B_Suckouts raises")
})
#
m1 <- lapply(f1, function(x){
  lapply(x, function(y){
    2 %in% y
  })
})
# PFR
PFR <- sum(unlist(m1)) / sum(sapply(Hands, length))
(VPIP - PFR) / VPIP # 35% of played hands are called raises or limps, this is objectively to passive even when considering that 
# almost all of those hands are not limps but calls to a raise to 2x or 3x BB







