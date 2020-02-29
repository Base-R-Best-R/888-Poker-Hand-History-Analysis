## Statistics ##
library(stringr)
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
# 21.50799 %
VPIP <- sum(unlist(m)) / sum(sapply(Hands, length)) # note that not all recorded sessions are full-ring
VPIP # For a tight aggressive player a value between 15 % and 20 % is considered optimal 
###############################################################################################################################
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
# 13.86379 %
PFR <- sum(unlist(m1)) / sum(sapply(Hands, length))
(VPIP - PFR) / VPIP # 35% of played hands are called raises or limps, this is objectively to passive even when considering that 
# almost all of those hands are not limps but calls to a raise to 2x or 3x BB
###############################################################################################################################
## Profitability ##
# One can calculate any stat and without doubt certain values will show asociation with a winning player, however it is 
# crucial to know how profitable ones playstyle is, accordingly I will calculate a confidence inervall for the 
# past ~ 3000 hands.
# 888 rake for 2 NL is 1 cent every 16 cents and $ 4 max
Win <- function(Hand, Name, list = F){
  # $$$ list
  dol <- list("Blinds" = c(rep(0, 10)),
              "Pre-flop" = c(rep(0, 10)),
              "Flop" = c(rep(0, 10)),
              "Turn" = c(rep(0, 10)),
              "River" = c(rep(0, 10)))
  ## Cover SB and BB ##
  # SB
  if(any(grepl(paste(Name, "posts small blind"), x[[1]]))){
    indSB <- grep(paste(Name, "posts small blind"), x[[1]])
    s1 <- unlist(strsplit(x[[1]][indSB], "\\$|\\]"))[2]
    dol[["Blinds"]][1] <- (-as.numeric(s1))
  }
  # BB
  if(any(grepl(paste(Name, "posts big blind"), x[[1]]))){
    indSB <- grep(paste(Name, "posts big blind"), x[[1]])
    s1 <- unlist(strsplit(x[[1]][indSB], "\\$|\\]"))[2]
    dol[["Blinds"]][1] <- (-as.numeric(s1))
  }
 ## Scan the rest for checks, bets and raises and collected money
  if(any(unlist(sapply(x, grepl, pattern = paste(paste(Name, c("bets", "raises", "calls", "collected")), collapse  = "|"))))){
    z <- sapply(x, grep, pattern =  paste(paste(Name, c("bets", "raises", "calls", "collected")), collapse  = "|"))
    l <- list()
    for(i in seq.int(length(x))){
      l[[i]] <- x[[i]][z[[i]]]
    }
    l <- l[-1]
      
  }
  else{
    return(sum(unlist(dol)))
  }
}
##
z <- sapply(x, grep, pattern =  paste(paste(Name, c("bets", "raises", "calls", "collected")), collapse  = "|"))
z
l <- list()
for(i in seq.int(length(x))){
 l[[i]] <- x[[i]][z[[i]]]
}
l <- l[-1]
strsplit(l[[1]], "\\$|\\]")
r <- lapply(l, strsplit, split = "\\$|\\]")

View(unlist(r))
k <- as.numeric(unlist(r))[!is.na(as.numeric(unlist(r)))]
##

dif <- function(p, x){
  # first if checks if money was collected
  if(any(unlist(sapply(p, grepl, pattern =  paste(paste(Name, "collected"), collapse  = "|"))))){
    # second if checks if villain folded to last bet or called it off
    if(x[length(x)] != sum(x[-length(k)])*2 + 0.03 - round(sum(x[-length(k)])*2 + 0.03 / 0.16) / 100){
        a <- sum(x[-c(length(x) - 1, length(k))])*2 + 0.03 
        b <- round(a / 0.16) / 100
        a - b - sum(x[-c(length(x) - 1, length(x))])
    }
    
  }
  else{
    -sum(x)
  }
}
sum(k[-c(length(k) - 1, length(k))])*2 + 0.03 
#- round(0.85/0.16)/100

