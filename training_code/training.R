#databank preprocessing

setwd("C:/Users/580377/Documents/Personal/GitHub/baseball")
# setwd("C:/Users/Katie/Documents/GitHub/baseball")
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


bat2010 <- batimport(2010) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut)) %>% select(-nameFirst)
bat2011 <- batimport(2011) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut)) %>% select(-nameFirst)
bat2012 <- batimport(2012) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut)) %>% select(-nameFirst)
bat2013 <- batimport(2013) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut)) %>% select(-nameFirst)
bat2014 <- batimport(2014) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut)) %>% select(-nameFirst)
bat2015 <- batimport(2015) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut)) %>% select(-nameFirst)
bat2016 <- batimport(2016) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut)) %>% select(-nameFirst)
bat2017 <- batimport(2017) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut)) %>% select(-nameFirst)
bat2018 <- batimport(2018) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut)) %>% select(-nameFirst)

addup <- function(year, df0, df1, df2){
  lag1<-year-1
  lag2<-year-2
  out <- df0 %>%
    select(person.key, games_count, games_above_count, points_sum, points_mean, points_sd, team.key_mode, age, since_debut) %>%
    mutate(year=year)
  lag1 <- df1 %>% 
    select(-age, -since_debut) %>%
    rename_at(vars(-person.key),function(x) paste0(x,"_lag1"))
  lag2 <- df2 %>% 
    select(-age, -since_debut) %>%
    rename_at(vars(-person.key),function(x) paste0(x,"_lag2"))
  df <- merge(out, lag1, by="person.key")
  df <- merge(df, lag2, by="person.key")
  return(df)
}
train2018 <- addup(2018, bat2018, bat2017, bat2016)
train2017 <- addup(2017, bat2017, bat2016, bat2015)
train2016 <- addup(2016, bat2016, bat2015, bat2014)
train2015 <- addup(2015, bat2015, bat2014, bat2013)
train2014 <- addup(2014, bat2014, bat2013, bat2012)
train2013 <- addup(2013, bat2013, bat2012, bat2011)
train2012 <- addup(2012, bat2012, bat2011, bat2010)
trainbats <- rbind(train2018, train2017, train2016, train2015, train2014, train2013, train2012)
  

trainbats <- lapply(trainbats, function(col){
  if(is.factor(col) | is.character(col) | is.logical(col)){
    col[is.na(col)] = getmode(col)
    return(as.factor(col))
  }else{
    col[is.na(col)]= 0
    return(as.numeric(col))
  }
}) %>% as.data.frame()

pitchimport <- function(year){
  infile = paste0("clean_data/pitchtrain", year ,".rds")
  df <- readRDS(infile)
  return(df)
}

pitch2010 <- pitchimport(2010) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut))
pitch2011 <- pitchimport(2011) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut))
pitch2012 <- pitchimport(2012) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut))
pitch2013 <- pitchimport(2013) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut))
pitch2014 <- pitchimport(2014) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut))
pitch2015 <- pitchimport(2015) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut))
pitch2016 <- pitchimport(2016) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut))
pitch2017 <- pitchimport(2017) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut))
pitch2018 <- pitchimport(2018) %>% mutate(age=as.numeric(age), since_debut=as.numeric(since_debut))

train2018 <- addup(2018, pitch2018, pitch2017, pitch2016)
train2017 <- addup(2017, pitch2017, pitch2016, pitch2015)
train2016 <- addup(2016, pitch2016, pitch2015, pitch2014)
train2015 <- addup(2015, pitch2015, pitch2014, pitch2013)
train2014 <- addup(2014, pitch2014, pitch2013, pitch2012)
train2013 <- addup(2013, pitch2013, pitch2012, pitch2011)
train2012 <- addup(2012, pitch2012, pitch2011, pitch2010)
trainpitch <- rbind(train2018, train2017, train2016, train2015, train2014, train2013, train2012) 

trainpitch <- lapply(trainpitch, function(col){
  if(is.factor(col) | is.character(col) | is.logical(col)){
    col[is.na(col)] = getmode(col)
    return(as.factor(col))
  }else{
    col[is.na(col)]= 0
    return(as.numeric(col))
  }
}) %>% as.data.frame()

write_rds(trainbats, "clean_data/trainbats.rds")
write_rds(trainpitch, "clean_data/trainpitch.rds")

#train test split
n <- nrow(trainbats)
shuffled_df <- trainbats[sample(n), ]
train_indices <- 1:round(0.8 * n)
train <- shuffled_df[train_indices, ]
test_indices <- (round(0.8 * n) + 1):n
test <- shuffled_df[test_indices, ]

ppbat = preProcess(train, method = "range")
train <- predict(ppbat, train)
test <- predict(ppbat, test)

write_rds(train, "clean_data/trainbats_prepped.rds")
write_rds(test, "clean_data/testbats_prepped.rds")

outcome_cols <- c("person.key", "games_count", "games_above_count", "points_sum", "points_mean", "points_sd")
train0 <- train %>% select(-outcome_cols)
trainout <- train %>% select(outcome_cols)
test0 <- test %>% select(-outcome_cols)
testout <- test %>% select(outcome_cols)
batout <- testout

glmnet_params=makeParamSet(
  makeDiscreteParam("alpha", values=c(0, 0.25, 0.5, 0.75, 1.0)),
  makeDiscreteParam("nlambda", values=c(100, 250, 500, 750, 1000))
)

set.seed(42)

tuneGLMnet <- function(type, data, iters=3, seed=42, targetVar){

  set.seed(seed)
  
  traintemp <- cbind(data, trainout[[targetVar]])
  colnames(traintemp)[ncol(traintemp)]<-targetVar
  task=makeRegrTask(id=type, data=traintemp, target=targetVar)
  
  lrn=makeLearner("regr.glmnet")
  
  ps=glmnet_params
  ctrl = makeTuneControlGrid(resolution=4)
  rdesc = makeResampleDesc(method = "CV", iters = iters, stratify = F)
  results = tuneParams(lrn, task, rdesc, measures=mse, par.set=ps, control=ctrl, show.info=T)
  
  lrn1 = setHyperPars(makeLearner("regr.glmnet"), par.vals = results$x)
  m = mlr::train(lrn1, task)

  return(m)
}

b1<- tuneGLMnet("bat", train0, targetVar="points_sum")
b2<- tuneGLMnet("bat", train0, targetVar="games_count")
b3<- tuneGLMnet("bat", train0, targetVar="games_above_count")

predicttest <- function(targetVar, model){
  testtemp <- cbind(test0, testout[[targetVar]])
  colnames(testtemp)[ncol(testtemp)]<-targetVar

  preds=predict(model, newdata=testtemp)
  return(preds)
}

pb1<- predicttest("points_sum", b1)
pb2<- predicttest("games_count", b2)
pb3<- predicttest("games_above_count", b3)

#now pitching
n <- nrow(trainpitch)
shuffled_df <- trainpitch[sample(n), ]
train_indices <- 1:round(0.8 * n)
train <- shuffled_df[train_indices, ]
test_indices <- (round(0.8 * n) + 1):n
test <- shuffled_df[test_indices, ]


pppitch = preProcess(train, method = "range")
train <- predict(pppitch, train)
test <- predict(pppitch, test)

write_rds(train, "clean_data/trainpitch_prepped.rds")
write_rds(test, "clean_data/testpitch_prepped.rds")

train0 <- train %>% select(-outcome_cols)
trainout <- train %>% select(outcome_cols)
test0 <- test %>% select(-outcome_cols)
testout <- test %>% select(outcome_cols)
pitchout <- testout



p1<- tuneGLMnet("pitch", train0, targetVar="points_sum")
p2<- tuneGLMnet("pitch", train0, targetVar="games_count")
p3<- tuneGLMnet("pitch", train0, targetVar="games_above_count")

predicttest <- function(targetVar, model){
  testtemp <- cbind(test0, testout[[targetVar]])
  colnames(testtemp)[ncol(testtemp)]<-targetVar
  
  preds=predict(model, newdata=testtemp)
  return(preds)
}

pp1<- predicttest("points_sum", p1)
pp2<- predicttest("games_count", p2)
pp3<- predicttest("games_above_count", p3)

save("b1", "b2", "b3", "p1", "p2", "p3", "pp1",
     "pp2", "pp3", "pb1", "pb2", "pb3","ppbat", "pppitch",
     file = "clean_data/models.RData")

     