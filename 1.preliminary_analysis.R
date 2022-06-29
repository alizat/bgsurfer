

library(tidyverse)
library(lubridate)
library(plotly)
library(tictoc)
library(glue)

options(dplyr.summarise.inform = FALSE)



## LOAD DATA ----

dta <- read_csv('data/1.mrpantherson/bgg_db_1806.csv')
# dta_2017 <- read_csv('data/1.mrpantherson/bgg_db_2017_04.csv')
# dta_2018 <- read_csv('data/1.mrpantherson/bgg_db_2018_01.csv')



## FIRST LOOK ----

skimr::skim_without_charts(dta) %>% 
#arrange(n_missing, character.n_unique, factor.n_unique, sd)
do({
  df <- .
  
  df <- df %>% arrange(n_missing)
  
  classes <- df %>% sapply(class) %>% unlist() %>% unique()
  
  if (any(classes %in% c('character','factor','ordered','Date','POSIXct','POSIXt'))) {
    arrange_with_these <- colnames(df)[colnames(df) %>% str_detect('unique')]
    df <- df %>% arrange(across(arrange_with_these))
  }
    
  if (any(classes %in% c('numeric','integer','double'))) {
    arrange_with_these <- colnames(df)[colnames(df) %>% str_detect('sd')]
    df <- df %>% arrange(across(arrange_with_these))
  }
  
  df
})



## CATEGORIES ----

categories <- 
  dta$category %>% 
  map(~ .x %>% str_split(', ') %>% unlist()) %>% 
  unlist() %>% 
  unique()

# category_counts <- c()
# for (current_category in categories) {
#   category_counts[current_category] <- 
#     dta %>% 
#     filter(category %>% str_detect(current_category)) %>% 
#     pull(game_id) %>% 
#     n_distinct()
# }

category_counts <- tibble(category = categories, n = 0)
for (current_category in categories) {
  category_counts$n[category_counts$category == current_category] <-
    dta %>%
    filter(category %>% str_detect(current_category)) %>%
    pull(game_id) %>%
    n_distinct()
}



## MECHANICS ----

## list of game mechanics
mechanics <- 
  dta$mechanic %>% 
  map(~ .x %>% str_split(', ') %>% unlist()) %>% 
  unlist() %>% 
  unique()

## number of games per mechanic
# mechanic_counts <- c()
# for (current_mechanic in mechanics) {
#   mechanic_counts[current_mechanic] <- 
#     dta %>% 
#     filter(mechanic %>% str_detect(current_mechanic)) %>% 
#     pull(game_id) %>% 
#     n_distinct()
# }
mechanic_counts <- tibble(mechanic = mechanics, n = 0)
for (current_mechanic in mechanics) {
  mechanic_counts$n[mechanic_counts$mechanic == current_mechanic] <-
    dta %>% 
    filter(mechanic %>% str_detect(current_mechanic)) %>% 
    pull(game_id) %>% 
    n_distinct()
}



## DESIGNERS ----

## list of designers
designers <- 
  dta$designer %>% 
  map(~ .x %>% str_split(', ') %>% unlist()) %>% 
  unlist() %>% 
  unique()

## number of games per designer
# designer_counts <- c()
# for (current_designer in designers) {
#   designer_counts[current_designer] <- 
#     dta %>% 
#     filter(designer %>% str_detect(current_designer)) %>% 
#     pull(game_id) %>% 
#     n_distinct()
# }
designer_counts <- tibble(designer = designers, n = 0)
for (current_designer in designers) {
  designer_counts$n[designer_counts$designer == current_designer] <-
    dta %>% 
    filter(designer %>% str_detect(current_designer)) %>% 
    pull(game_id) %>% 
    n_distinct()
}

## example query
designer_counts %>% keep(~ .x >= 7) %>% sort(decreasing = TRUE) %>% names()



## PLAYER COUNT ----

dta %>% 
  select(min_players, max_players) %>% 
  mutate(max_players = pmax(min_players, max_players)) %>% 
  mutate(min2max_players = max_players - min_players) %>% 
  summary()



## PLAYTIME ----

dta %>% 
  select(min_time, max_time, avg_time) %>% 
  mutate(max_time = pmax(min_time, max_time)) %>% 
  summary()



## YEAR ----

dta %>% count(year) %>% arrange(desc(year))



## RATING ----

dta %>% 
  mutate(avg_rating_rounded = round(avg_rating)) %>% 
  count(avg_rating_rounded)

dta %>% 
  mutate(avg_rating_rounded = round(avg_rating)) %>% 
  filter(avg_rating_rounded == 9) %>% 
  select(names, year, rank, num_votes) %>% 
  print(n = 40)



## WEIGHT ----

dta %>% 
  select(weight) %>% 
  summary()

