#databank preprocessing

setwd("C:/Users/580377/Documents/Personal/GitHub/baseball")
setwd("C:/Users/Katie/Documents/GitHub/baseball")
source("preprocessing_code/calcpoints.R")
library(tidyverse)
library(mlr)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

bats_input <- readRDS("clean_data/retrobats2017.RDS")
names(bats_input)

bats_sum <- bats_input %>%
  select(-game.key, -game.date, -team.alignment, -team.key, -opponent.key, -slot, -seq,
         team.key.home, -team.key.away, -homescore, -awayscore) %>%
  group_by(person.key) %>%
  summarise_if(is.numeric, funs(sum=sum), na.rm=TRUE)

bats_mean <- bats_input %>%
  select(-game.key, -game.date, -team.alignment, -team.key, -opponent.key, -slot, -seq,
         team.key.home, -team.key.away, -homescore, -awayscore, -B_G_DH, -B_G_PH, -B_G_PR, -F_1B_POS,
         -F_2B_POS, -F_3B_POS, -F_SS_POS, -F_OF_POS, -F_LF_POS, -F_CF_POS, -F_RF_POS, F_C_POS, -F_P_POS) %>%
  group_by(person.key) %>%
  summarise_if(is.numeric, funs(mean=mean), na.rm=TRUE)

bats_sd <- bats_input %>%
  select(-game.key, -game.date, -team.alignment, -team.key, -opponent.key, -slot, -seq,
         team.key.home, -team.key.away, -homescore, -awayscore, -B_G_DH, -B_G_PH, -B_G_PR, -F_1B_POS,
         -F_2B_POS, -F_3B_POS, -F_SS_POS, -F_OF_POS, -F_LF_POS, -F_CF_POS, -F_RF_POS, F_C_POS, -F_P_POS) %>%
  group_by(person.key) %>%
  summarise_if(is.numeric, funs(sd=sd), na.rm=TRUE)

bats_mode <- bats_input %>%
  select(person.key, team.key, slot) %>%
  group_by(person.key) %>%
  summarise_all(funs(mode=getmode))

bats_all <- merge(bats_sum, bats_mean, by="person.key")
bats_all <- merge(bats_all, bats_sd, by="person.key")
bats_all <- merge(bats_all, bats_mode, by="person.key")
