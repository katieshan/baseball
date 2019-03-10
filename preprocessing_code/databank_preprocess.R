#databank preprocessing

setwd("C:/Users/580377/Documents/Personal/GitHub/baseball")
setwd("C:/Users/Katie/Documents/GitHub/baseball")
source("preprocessing_code/calcpoints.R")
library(tidyverse)

people <- read_csv("raw_data/baseballdatabank-2019.2/baseballdatabank-2019.2/core/People.csv")
appearances <- read_csv("raw_data/baseballdatabank-2019.2/baseballdatabank-2019.2/core/Appearances.csv")

people <- people %>%
  select(-birthCountry, -birthState, -birthCity, -deathYear, -deathMonth, -deathDay, -deathCountry, -deathState, -deathCity) %>%
  mutate(birthdate = as.Date(with(., paste(birthYear, birthMonth, birthDay,sep="-")), "%Y-%m-%d"), debut=as.Date(debut))

summary(people)
people$bats <- as.factor(people$bats)
people$throws <- as.factor(people$throws)

people <- select(people, -finalGame, -birthYear, -birthMonth, -birthDay)

#not using appearances, we have position data

write.csv(people, "clean_data/databank_people.csv")
saveRDS(people, "clean_data/databank_people.rds")
