

library(tidyverse)
library(lubridate)
library(plotly)
library(tictoc)
library(glue)

options(dplyr.summarise.inform = FALSE)

Sys.setlocale(locale = "French")



## PARAMETERS ----

## number of top N games to analyze
# num_games <- Inf
num_games <- 100



## LOAD DATA ----

dta <- read_csv('data/1.mrpantherson/bgg_db_1806.csv', show_col_types = FALSE)
# dta_2017 <- read_csv('data/1.mrpantherson/bgg_db_2017_04.csv')
# dta_2018 <- read_csv('data/1.mrpantherson/bgg_db_2018_01.csv')

dta <- dta %>% head(num_games)



## FIRST LOOK ----

print(glue('\n\n*** FYI ***\n\n'))

print(glue('> Most recent game in this dataset was published in {max(dta$year)}'))
print(glue('> Data being analyzed can be obtained from:  https://www.kaggle.com/datasets/mrpantherson/board-game-data'))
print(glue('> Analyses below are for the top {nrow(dta)} games as per the BGG average user rating'))



## FIRST LOOK ----

print(glue('\n\n\n\n*** Overview ***\n\n'))

skimr::skim_without_charts(dta) %>% 
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
  }) %>% 
  print()



## CATEGORIES ----

print(glue('\n\n\n\n*** Categories ***\n\n'))

categories <- 
  dta$category %>% 
  map(~ .x %>% str_split(', ') %>% unlist()) %>% 
  unlist() %>% 
  unique()

category_counts <- tibble(category = categories, n = 0)
for (current_category in categories) {
  category_counts$n[category_counts$category == current_category] <-
    dta %>%
    filter(category %>% str_detect(current_category)) %>%
    pull(game_id) %>%
    n_distinct()
}

print(glue('# games that fall under each category'))
print(glue('(note that games can fall under multiple categories)'))
print(glue(''))
category_counts %>% 
  arrange(desc(n)) %>% 
  print(n = 50)



## MECHANICS ----

print(glue('\n\n\n\n*** Mechanics ***\n\n'))

## list of game mechanics
mechanics <- 
  dta$mechanic %>% 
  map(~ .x %>% str_split(', ') %>% unlist()) %>% 
  unlist() %>% 
  unique()

mechanic_counts <- tibble(mechanic = mechanics, n = 0)
for (current_mechanic in mechanics) {
  mechanic_counts$n[mechanic_counts$mechanic == current_mechanic] <-
    dta %>% 
    filter(mechanic %>% str_detect(current_mechanic)) %>% 
    pull(game_id) %>% 
    n_distinct()
}

print(glue('# games that fall under each game mechanic'))
print(glue('(note that games can have multiple game mechanics)'))
print(glue(''))
mechanic_counts %>% 
  arrange(desc(n)) %>% 
  print(n = 50)



## DESIGNERS ----

print(glue('\n\n\n\n*** Designers ***\n\n'))

## list of designers
designers <- 
  dta$designer %>% 
  map(~ .x %>% str_split(', ') %>% unlist()) %>% 
  unlist() %>% 
  unique()

designer_counts      <- tibble(designer = designers, contributions = 0)
designer_counts_main <- tibble(designer = designers, main_designer = 0)
for (current_designer in designers) {
  designer_counts$contributions[designer_counts$designer == current_designer] <-
    dta %>% 
    filter(designer %>% str_detect(current_designer)) %>% 
    pull(game_id) %>% 
    n_distinct()
  designer_counts_main$main_designer[designer_counts_main$designer == current_designer] <-
    dta %>% 
    filter(designer %>% str_detect(paste0('^', current_designer))) %>% 
    pull(game_id) %>% 
    n_distinct()
}

print(glue('# games that each designer was contributor OR the main designer for'))
designer_counts %>% 
  inner_join(designer_counts_main, by = 'designer') %>% 
  arrange(desc(main_designer)) %>% 
  print(n = 50)



## PLAYER COUNT ----

print(glue('\n\n\n\n*** Player Count ***\n\n'))
dta %>% 
  select(names, rank, min_players, max_players) %>% 
  mutate(min_players = pmax(min_players, 1)) %>% 
  mutate(max_players = pmax(min_players, max_players)) %>% 
  arrange(max_players) %>% 
  print(n = 50)



## PLAYTIME ----

print(glue('\n\n\n\n*** Playtime ***\n\n'))
dta %>% 
  select(names, rank, min_time, max_time, avg_time) %>% 
  mutate(max_time = pmax(min_time, max_time)) %>% 
  arrange(max_time) %>% 
  print(n = 50)



## YEAR ----

print(glue('\n\n\n\n*** Year It Came Out ***\n\n'))
dta %>% 
  count(year) %>% 
  arrange(desc(year)) %>% 
  print(n = 50)



## RATING ----

print(glue('\n\n\n\n*** Average Rating ***\n\n'))
dta %>% 
  mutate(avg_rating_rounded = round(avg_rating*2) / 2) %>% 
  count(avg_rating_rounded) %>% 
  print()
print(glue(''))
dta %>% 
  mutate(avg_rating_rounded = round(avg_rating*2) / 2) %>% 
  select(names, rank, num_votes, avg_rating_rounded) %>% 
  arrange(rank) %>% 
  print(n = 50)



## WEIGHT ----

print(glue('\n\n\n\n*** Complexity ***\n\n'))
dta %>% 
  select(names, weight) %>% 
  mutate(weight_rounded = round(weight * 2) / 2) %>% 
  count(weight_rounded) %>%
  print()
print(glue(''))
dta %>% 
  mutate(weight_rounded = round(weight * 2) / 2) %>% 
  select(names, rank, weight_rounded) %>% 
  arrange(rank) %>% 
  print(n = 50)

