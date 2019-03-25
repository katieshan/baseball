setwd("C:/Users/580377/Documents/Personal/GitHub/baseball")
setwd("C:/Users/Katie/Documents/GitHub/baseball")
source("preprocessing_code/calcpoints.R")
library(tidyverse)
library(mlr)
library(lubridate)
library(glmnet)
library(caret)

batimport <- function(year){
  infile = paste0("clean_data/battrain", year ,".rds")
  df <- readRDS(infile)
  return(df)
}

#this function calculates modes
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

bat2018 <- batimport(2018) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut))

# fanbat <- read_csv("raw_data/FanGraphs Leaderboardbat.csv") %>%
#   mutate(mergename=tolower(gsub("[^[:alnum:]]","", Name)))

fanc <- read_csv("raw_data/FanGraphs Leaderboardcatcher.csv") %>%
     mutate(fanpos="C", mergename=tolower(gsub("[^[:alnum:]]","", Name)))
fan1b <- read_csv("raw_data/FanGraphs Leaderboard1b.csv") %>%
  mutate(fanpos="1B", mergename=tolower(gsub("[^[:alnum:]]","", Name)))
fan2b <- read_csv("raw_data/FanGraphs Leaderboard2b.csv") %>%
  mutate(fanpos="2B", mergename=tolower(gsub("[^[:alnum:]]","", Name)))
fan3b <- read_csv("raw_data/FanGraphs Leaderboard3b.csv") %>%
  mutate(fanpos="3B", mergename=tolower(gsub("[^[:alnum:]]","", Name)))
fanss <- read_csv("raw_data/FanGraphs Leaderboardss.csv") %>%
  mutate(fanpos="SS", mergename=tolower(gsub("[^[:alnum:]]","", Name)))
fanof <- read_csv("raw_data/FanGraphs Leaderboardof.csv") %>%
  mutate(fanpos="OF", mergename=tolower(gsub("[^[:alnum:]]","", Name)))

fanbat <- rbind(fanc, fan1b, fan2b, fan3b, fanss, fanof)
fanbat <- fanbat[!duplicated(fanbat[c("mergename")]),]

people <- readRDS("clean_data/databank_people.rds")
colnames(people)
bat2018ppl <- bat2018 %>% select(person.key, F_1B_POS_sum, F_2B_POS_sum, F_3B_POS_sum, 
                                 F_SS_POS_sum, F_OF_POS_sum, F_C_POS_sum, age, since_debut) 
vector <- apply(select(bat2018ppl, -person.key, -age, -since_debut), 1, which.max) 

pos_xwalk <- read_csv("clean_data/pos_xwalk.csv")

bat2018ppl <- cbind(bat2018ppl, posindex=vector) %>%
  merge(., people, by.x="person.key", by.y="retroID") %>%
  select(-weight, -height, -bats, -throws, -playerID, -debut, -bbrefID, -birthdate,
         -F_1B_POS_sum, -F_2B_POS_sum, -F_3B_POS_sum, 
         -F_SS_POS_sum, -F_OF_POS_sum, -F_C_POS_sum) %>%
  mutate(fullname = paste0(nameFirst, " ", nameLast), mergename=tolower(gsub("[^[:alnum:]]","", fullname))) %>%
  merge(., pos_xwalk, all.x=TRUE)

batnew <- merge(bat2018ppl, fanbat, by="mergename", all.y=TRUE)
batnew$single <- batnew$H - batnew$`2B`-batnew$`3B`-batnew$HR
batnew$nsb <- batnew$SB - batnew$CS

batnew$fanpoints <- calcbat(batnew, R="R", S="single", D="2B", Tr="3B", HR="HR", RBI="RBI", BBB="BB", NSB="nsb")
teams <- read_csv("clean_data/team_xwalk.csv")
batnew<- merge(batnew, teams)

fanpitch <- read_csv("raw_data/FanGraphs Leaderboardpitchfull.csv") %>%
  mutate(mergename=tolower(gsub("[^[:alnum:]]","", Name)))


pitchimport <- function(year){
  infile = paste0("clean_data/pitchtrain", year ,".rds")
  df <- readRDS(infile)
  return(df)
}

pitch2018 <- pitchimport(2018) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut))

pitch2018ppl <- pitch2018 %>% select(person.key, P_GS_sum, P_GF_sum, games_count, age, since_debut) %>%
  mutate(start_share=P_GS_sum/games_count, close_share=P_GF_sum/games_count,
         position=if_else(start_share>0.5, "S", if_else(close_share>0.5, "C", "R"))) %>%
  merge(., people, by.x="person.key", by.y="retroID") %>%
  select(-weight, -height, -bats, -throws, -playerID, -debut, -bbrefID, -birthdate,
         -P_GS_sum, -P_GF_sum, -games_count, -start_share, -close_share) %>%
  mutate(fullname = paste0(nameFirst, " ", nameLast), mergename=tolower(gsub("[^[:alnum:]]","", fullname)))

pitchnew <- merge(pitch2018ppl, fanpitch, by="mergename", all.y=TRUE)
pitchnew$fanpoints <- calcpitch(pitchnew, GS="GS", IP="IP", ER="ER", PBB="BB", K="SO", SV="SV", HLD="HLD")
pitchnew<- merge(pitchnew, teams)

colnames(batnew)

batpred <- batnew %>%
  filter(!is.na(person.key)) %>%
  select(person.key, age, since_debut, team.key_mode=team.key)

bat2017 <- batimport(2017) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut))

lag1 <- bat2018 %>% 
  select(-age, -since_debut) %>%
  rename_at(vars(-person.key),function(x) paste0(x,"_lag1"))

lag2 <- bat2017 %>% 
  select(-age, -since_debut) %>%
  rename_at(vars(-person.key),function(x) paste0(x,"_lag2"))

batpred <- merge(batpred, lag1, by="person.key")
batpred <- merge(batpred, lag2, by="person.key")

batpred <- lapply(batpred, function(col){
  if(is.factor(col) | is.character(col) | is.logical(col)){
    col[is.na(col)] = getmode(col)
    return(as.factor(col))
  }else{
    col[is.na(col)]= mean(col, na.rm=TRUE)
    return(as.numeric(col))
  }
}) %>% as.data.frame()


pitchpred <- pitchnew %>%
  filter(!is.na(person.key)) %>%
  select(person.key, age, since_debut, team.key_mode=team.key)

pitch2017 <- pitchimport(2017) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut))

lag1 <- pitch2018 %>% 
  select(-age, -since_debut) %>%
  rename_at(vars(-person.key),function(x) paste0(x,"_lag1"))

lag2 <- pitch2017 %>% 
  select(-age, -since_debut) %>%
  rename_at(vars(-person.key),function(x) paste0(x,"_lag2"))

pitchpred <- merge(pitchpred, lag1, by="person.key")
pitchpred <- merge(pitchpred, lag2, by="person.key")

pitchpred <- lapply(pitchpred, function(col){
  if(is.factor(col) | is.character(col) | is.logical(col)){
    col[is.na(col)] = getmode(col)
    return(as.factor(col))
  }else{
    col[is.na(col)]= mean(col, na.rm=TRUE)
    return(as.numeric(col))
  }
}) %>% as.data.frame()

#fix factor levels to match those in the model

fixFactorLevels_pred = function(model, newdata, targetvar) {
 
  #get factor levels from model
  factor.levels = model$factor.levels
  #fix each column in newdata
  for (varname in names(factor.levels)){   #for each factored variable in the model...
    #if variable names is the target doesn't apply
    if(varname == targetvar) {
      newdata[[varname]] = NULL
    }
    #For all other variables...
    #if ".merged" is contained within that factor's levels...
    else if (".merged" %in% factor.levels[[varname]]){  
      #convert variable in newdata to character to allow manipulation of data
      newdata[[varname]] = as.character(newdata[[varname]])
      #overwrite entries in new data varname with .merged if they are unknown factor levels from the model
      #change new data variable back to factor with correct levels and overwrite in newdata
      newdata[[varname]] = factor(newdata[[varname]],
                                   levels = factor.levels[[varname]])
    }
    
    #if ".merged" is not one of the original factor levels it needs to be replaced with na
    else if(!(".merged" %in% factor.levels[[varname]])) {
      
      #convert variable in newdata to character to allow manipulation of data
      
      newdata[[varname]] = as.character(newdata[[varname]])
      
      newdata[[varname]][!(newdata[[varname]] %in% factor.levels[[varname]])] = NA
      
      #change new data variable back to factor with correct levels and overwrite in newdata
      
      newdata[[varname]] = factor(newdata[[varname]], levels = factor.levels[[varname]])
    }
  }
  return(newdata)
}

batpred0 <- fixFactorLevels_pred(b1, batpred0, "points_sum")

load("clean_data/models.RData")

batpred0 <- select(batpred, -person.key)
batpred0$points_sum <- NULL
pb1 <- predict(b1, newdata=batpred0)
