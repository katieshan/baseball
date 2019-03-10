setwd("C:/Users/580377/Documents/Personal/GitHub/baseball")
library(tidyverse)
library(mlr)

retrosplits0 <- read_csv("raw_data/retrosplits-master/retrosplits-master/daybyday/playing-2017.csv")
retrovars <- read_csv("raw_data/retrosplits_vars.csv")
retrocols <- retrovars$varname[retrovars$in_preprocess==1]
pitchcols <- retrovars$varname[retrovars$pitch==1]
batcols <- retrovars$varname[retrovars$bat==1]
retro1 <- retrosplits0 %>% select(c(retrocols)) %>% filter(season.phase=="R")

retrogames <- retro1 %>% select(game.key, team.key, opponent.key, team.alignment, B_R)
retrogames_h <- retrogames %>% filter(team.alignment==1) %>% 
  group_by(game.key, team.key) %>%
  summarise(homescore=sum(B_R)) %>%
  rename(team.key.home=team.key)
retrogames_a <- retrogames %>% filter(team.alignment==0) %>% 
  group_by(game.key, team.key) %>%
  summarise(awayscore=sum(B_R)) %>%
  rename(team.key.away=team.key)
retrogames <- merge(retrogames_h, retrogames_a, by="game.key")

retropitches <- retro1 %>% 
  select(pitchcols) %>%
  filter(P_G==1) %>%
  merge(., retrogames, by="game.key")

retropitches <- retropitches %>%
  mutate(teamwon = if_else((team.alignment==1 & homescore>awayscore) | (team.alignment==0 & awayscore > homescore), 1, 0),         
         teamscore = if_else (team.alignment==1, homescore, awayscore),
         oppscore = if_else(team.alignment==1, awayscore, homescore)) %>%
  select(-homescore, -awayscore, -team.alignment, -team.key.away, -team.key.home)

retropitches <- retropitches %>%
  arrange(game.key, team.key, seq) %>%
  group_by(game.key, team.key) %>%
  mutate(prevruns = cumsum(P_R) - P_R,
         prevwin = cumsum(P_W) - P_W,
         prevloss = cumsum(P_L) - P_L) %>%
  arrange(game.key, team.key, -seq) %>%
  group_by(game.key, team.key) %>%
  mutate(subruns = cumsum(P_R) - P_R,
         subwin = cumsum(P_W) - P_W,
         subloss = cumsum(P_L) - P_L) %>%
  arrange(game.key, team.key, seq) %>%
  ungroup()

holdcands <- retropitches %>%
  filter(P_GS==0 & P_W==0 & P_L==0 & P_SV==0 & P_GF==0 & P_OUT>=1) %>%
  mutate(holdwins= if_else(prevwin==1, 1, 0),
         holdloss=if_else(subloss==1 & (oppscore - subruns < teamscore),1,0)) %>%
  select(game.key, person.key, holdwins, holdloss)

retropitches <- merge(retropitches, holdcands, by=c("game.key", "person.key"), all.x=TRUE)
retropitches$holdsest <- retropitches$holdloss + retropitches$holdwins
retropitches$holdsest[is.na(retropitches$holdsest)]<- 0
retropitches$IP <- retropitches$P_OUT/3

retropitches$points <-  calcpitch(df=retropitches, GS="P_GS", IP="IP",ER="P_ER",PBB="P_BB",K="P_SO",SV="P_SV",HLD="holdsest")
