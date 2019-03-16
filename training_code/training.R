#databank preprocessing

setwd("C:/Users/580377/Documents/Personal/GitHub/baseball")
setwd("C:/Users/Katie/Documents/GitHub/baseball")
source("preprocessing_code/calcpoints.R")
library(tidyverse)
library(mlr)

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
  summarise_if(is.numeric, list(mean=mean, sd=sd), na.rm=TRUE)



bats_mode <- bats_input %>%
  select(person.key, )