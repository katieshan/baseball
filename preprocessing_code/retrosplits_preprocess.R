setwd("C:/Users/580377/Documents/Personal/GitHub/baseball")
setwd("C:/Users/Katie/Documents/GitHub/baseball")
source("preprocessing_code/calcpoints.R")
library(tidyverse)
library(lubridate)

#this function calculates modes
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#This function cleans the retrosheet data
retrorun <- function(year){
  
  infile = paste0("raw_data/retrosplits-master/retrosplits-master/daybyday/playing-", year ,".csv")
  outpitch = paste0("clean_data/retropitches", year ,".csv")
  outbat = paste0("clean_data/retrobats", year ,".csv")
  outpitchrds = paste0("clean_data/retropitches", year ,".rds")
  outbatrds = paste0("clean_data/retrobats", year ,".rds")
  outbattrain = paste0("clean_data/battrain", year ,".rds")
  outpitchtrain = paste0("clean_data/pitchtrain", year ,".rds")
  start <- paste(year, 04, 01, sep="-") %>% ymd() %>% as.Date()
  
  people <- readRDS("clean_data/databank_people.rds")
  
  #bring in retrosplit data
  retrosplits0 <- read_csv(infile)
  
  #bring in varable info and pull preprocessing, pitching, and batting fields
  retrovars <- read_csv("raw_data/retrosplits_vars.csv")
  retrocols <- retrovars$varname[retrovars$in_preprocess==1]
  pitchcols <- retrovars$varname[retrovars$pitch==1]
  batcols <- retrovars$varname[retrovars$bat==1]
  
  #filter to preprocessing fields
  retro1 <- retrosplits0 %>% select(c(retrocols)) %>% 
    filter(season.phase=="R") %>%
    mutate(game.date = as.Date(game.date), team.key = as.factor(team.key), opponent.key = as.factor(opponent.key))
  
  summary(retro1)
  
  #create game win and loss info (total runs per team per game)
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
  
  #pull out pitches and merge on games
  retropitches <- retro1 %>% 
    select(pitchcols) %>%
    filter(P_G==1) %>%
    merge(., retrogames, by="game.key")
  
  #get game scores into pitch data
  retropitches <- retropitches %>%
    mutate(teamwon = if_else((team.alignment==1 & homescore>awayscore) | (team.alignment==0 & awayscore > homescore), 1, 0),         
           teamscore = if_else (team.alignment==1, homescore, awayscore),
           oppscore = if_else(team.alignment==1, awayscore, homescore)) %>%
    select(-homescore, -awayscore, -team.alignment, -team.key.away, -team.key.home)
  
  #add up to impute holds; look at runs scored before and after each pitcher,
  #and if previous or subsequent pitcher got the win or loss
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
  
  #pull out possible holds--neither wins nor losses, middle relievers, at least one out
  holdcands <- retropitches %>%
    filter(P_GS==0 & P_W==0 & P_L==0 & P_SV==0 & P_GF==0 & P_OUT>=1) %>%
    #calculate holds
    #if a previous pitcher got the win, it's a hold
    #if a subsequent pitcher got the loss, and the opponent's score at the end
    #of the the pitcher's turn was less than the home team's final score,
    #it might have been a hold.  give it to them and test later.
    mutate(holdwins= if_else(prevwin==1, 1, 0),
           # holdloss=if_else(subloss==1 & (oppscore - subruns < teamscore),1,0)) %>%
           holdloss=0) %>%
    select(game.key, person.key, holdwins, holdloss)
  
  #merge the holds back on and calculate final holds and innings pitched
  retropitches <- merge(retropitches, holdcands, by=c("game.key", "person.key"), all.x=TRUE)
  retropitches$holdsest <- retropitches$holdloss + retropitches$holdwins
  retropitches$holdsest[is.na(retropitches$holdsest)]<- 0
  retropitches$IP <- retropitches$P_OUT/3
  
  #run the point calculation function
  retropitches$points <-  calcpitch(df=retropitches, GS="P_GS", IP="IP",ER="P_ER",PBB="P_BB",K="P_SO",SV="P_SV",HLD="holdsest")
  
  summary(retropitches)
  
  rm(holdcands, retrogames_a, retrogames_h) 
  
  #now batters
  #pull out batters and merge on games
  retrobats <- retro1 %>% 
    filter(P_G==0) %>%
    select(batcols) %>%
    merge(., retrogames, by="game.key")
  
  retrobats <- retrobats %>%
    mutate(B_1B = B_H - B_2B - B_3B - B_HR,
           NSB = B_SB - B_CS)
  
  retrobats$points <-  calcbat(df=retrobats, R="B_R", S="B_1B", D="B_2B", Tr="B_3B", HR="B_HR", RBI="B_RBI", BBB="B_BB", NSB = "NSB")
  
  retropitches <- retropitches %>%
    select(-slot, -P_G, -season.phase, -prevruns, -prevwin, -prevloss, -subruns, -subwin, -subloss, -holdwins, -holdloss)
  
  nrow(retropitches)
  # retropitches <- merge(retropitches, people, by.x = "person.key", by.y="retroID", all.x = TRUE)
  nrow(retropitches)
  sum(is.na(retropitches$playerID))
  
  write.csv(retropitches, outpitch)
  saveRDS(retropitches, outpitchrds)
  
  retrobats <- retrobats %>%
    select(-season.phase)
  
  # nrow(retrobats)
  # retrobats <- merge(retrobats, people, by.x = "person.key", by.y="retroID", all.x = TRUE)
  # nrow(retrobats)
  # sum(is.na(retrobats$playerID))
  
  write.csv(retrobats, outbat)
  saveRDS(retrobats, outbatrds)
  
  #Now get game numbers
  bats_sum <- retrobats %>%
    select(-game.key, -game.date, -team.alignment, -team.key, -opponent.key, -slot, -seq,
           team.key.home, -team.key.away, -homescore, -awayscore) %>%
    group_by(person.key) %>%
    summarise_if(is.numeric, funs(sum=sum), na.rm=TRUE)
  
  bats_mean <- retrobats %>%
    select(-game.key, -game.date, -team.alignment, -team.key, -opponent.key, -slot, -seq,
           team.key.home, -team.key.away, -homescore, -awayscore, -B_G_DH, -B_G_PH, -B_G_PR, -F_1B_POS,
           -F_2B_POS, -F_3B_POS, -F_SS_POS, -F_OF_POS, -F_LF_POS, -F_CF_POS, -F_RF_POS, F_C_POS, -F_P_POS) %>%
    group_by(person.key) %>%
    summarise_if(is.numeric, funs(mean=mean), na.rm=TRUE)
  
  bats_sd <- retrobats %>%
    select(-game.key, -game.date, -team.alignment, -team.key, -opponent.key, -slot, -seq,
           team.key.home, -team.key.away, -homescore, -awayscore, -B_G_DH, -B_G_PH, -B_G_PR, -F_1B_POS,
           -F_2B_POS, -F_3B_POS, -F_SS_POS, -F_OF_POS, -F_LF_POS, -F_CF_POS, -F_RF_POS, F_C_POS, -F_P_POS) %>%
    group_by(person.key) %>%
    summarise_if(is.numeric, funs(sd=sd), na.rm=TRUE)
  
  bats_mode <- retrobats %>%
    select(person.key, team.key, slot) %>%
    group_by(person.key) %>%
    summarise_all(funs(mode=getmode))
  
  bats_all <- merge(bats_sum, bats_mean, by="person.key")
  bats_all <- merge(bats_all, bats_sd, by="person.key")
  bats_all <- merge(bats_all, bats_mode, by="person.key")
  
  mean_points <- arrange(bats_all, desc(points_sum)) %>%
    select(points_sum, points_mean) %>%
    top_n(90, points_sum) %>%
    summarise(mean(points_mean))
  
  games_above  <- retrobats %>%
    select(person.key, points) %>%
    mutate(games=1, games_above=if_else(points>as.numeric(mean_points), 1, 0)) %>%
    select(-points) %>%
    group_by(person.key) %>%
    summarise_all(funs(count=sum), na.rm=TRUE)
  
  bats_all <- merge(bats_all, games_above, by="person.key") %>%
    merge(., people, by.x="person.key", by.y="retroID", all.x=TRUE)%>%
    mutate(team.key_mode=as.factor(team.key_mode), 
           slot_mode = as.factor(slot_mode),
           bats=as.factor(bats),
           throws=as.factor(throws),
           debut=as.Date(debut),
           birthdate=as.Date(birthdate),
           since_debut = (start-debut)/365.25,
           age=(start-birthdate)/365.25) %>%
    select(-playerID, -bbrefID, nameFirst, -nameLast, -nameGiven,-debut, -birthdate)
  
  rm(retrobats, bats_mean, bats_mode, bats_sd, bats_sum, games_above, mean_points)
  
  saveRDS(bats_all, outbattrain)
  
  #now pitchers
  pitches_sum <- retropitches %>%
    select(-game.key, -game.date, -team.key, -opponent.key, -seq, teamwon, -teamscore, -oppscore) %>%
    group_by(person.key) %>%
    summarise_if(is.numeric, funs(sum=sum), na.rm=TRUE)
  
  pitches_mean <- retropitches %>%
    select(-game.key, -game.date, -team.key, -opponent.key, -seq, teamwon, -teamscore, -oppscore) %>%
    group_by(person.key) %>%
    summarise_if(is.numeric, funs(mean=mean), na.rm=TRUE)
  
  pitches_sd <- retropitches %>%
    select(-game.key, -game.date, -team.key, -opponent.key, -seq, teamwon, -teamscore, -oppscore) %>%
    group_by(person.key) %>%
    summarise_if(is.numeric, funs(sd=sd), na.rm=TRUE)
  
  pitches_mode <- retropitches %>%
    select(person.key, team.key) %>%
    group_by(person.key) %>%
    summarise_all(funs(team.key_mode=getmode))
  
  pitches_all <- merge(pitches_sum, pitches_mean, by="person.key")
  pitches_all <- merge(pitches_all, pitches_sd, by="person.key")
  pitches_all <- merge(pitches_all, pitches_mode, by="person.key")
  
  mean_points <- arrange(pitches_all, desc(points_sum)) %>%
    select(points_sum, points_mean) %>%
    top_n(90, points_sum) %>%
    summarise(mean(points_mean))
  
  games_above  <- retropitches %>%
    select(person.key, points) %>%
    mutate(games=1, games_above=if_else(points>as.numeric(mean_points), 1, 0)) %>%
    select(-points) %>%
    group_by(person.key) %>%
    summarise_all(funs(count=sum), na.rm=TRUE)
  
  pitches_all <- merge(pitches_all, games_above, by="person.key") %>%
    merge(., people, by.x="person.key", by.y="retroID", all.x=TRUE)%>%
    mutate(team.key_mode=as.factor(team.key_mode), 
           bats=as.factor(bats),
           throws=as.factor(throws),
           debut=as.Date(debut),
           birthdate=as.Date(birthdate),
           since_debut = (start-debut)/365.25,
           age=(start-birthdate)/365.25) %>%
    select(-playerID, -bbrefID, -nameFirst, -nameLast, -nameGiven, -debut, -birthdate)
  
  saveRDS(pitches_all, outpitchtrain)
}

retrorun(2010)
retrorun(2011)
retrorun(2012)
retrorun(2013)
retrorun(2014)
retrorun(2015)
retrorun(2016)
retrorun(2017)
retrorun(2018)