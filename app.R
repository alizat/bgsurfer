

library(tidyverse)
library(lubridate)
library(plotly)
library(tictoc)
library(glue)

library(shiny)
library(DT)

options(dplyr.summarise.inform = FALSE)


dta <- read_csv('data/1.mrpantherson/bgg_db_1806.csv')
categories <- 
  dta$category %>% 
  map(~ .x %>% str_split(', ') %>% unlist()) %>% 
  unlist() %>% 
  unique()
mechanics <- 
  dta$mechanic %>% 
  map(~ .x %>% str_split(', ') %>% unlist()) %>% 
  unlist() %>% 
  unique()
designers <- 
  dta$designer %>% 
  map(~ .x %>% str_split(', ') %>% unlist()) %>% 
  unlist() %>% 
  unique()


ui <- navbarPage(
  title = 'Analyzing the top tabletop games (according to BGG)',
  
  ## overview
  tabPanel(
    'Games List',
    DTOutput('games_list')
  ),
  
  ## categories
  tabPanel(
    'Categories',
    numericInput(
      inputId = 'topn_categories', 
      label = 'Select the top N games to analyze', 
      min = 0, 
      max = 5000, 
      value = 100
    ),
    br(),
    fluidRow(column(width = 6, DTOutput('games_counts_categories'), offset = 3)),
    br(),
    br(),
    DTOutput('games_list_categories')
  ),
  tabPanel('Game Mechanics'),
  tabPanel('Designers')
)

server <- function(input, output, session) {
  
  header_renamer <- function(df) {
    df %>% 
      set_names(colnames(.) %>% 
                  map_chr(~ .x %>% str_split('_') %>% unlist() %>% paste(collapse = ' ')) %>% 
                  str_to_title())
  }
  
  dta <- read_csv('data/1.mrpantherson/bgg_db_1806.csv')
  dta_basic_view <- 
    dta %>% 
    select(rank,
           names,
           year,
           avg_rating,
           num_votes,
           designer, 
           weight)
  
  output$games_list <- renderDT({
    dta_basic_view %>% 
      header_renamer() %>% 
      datatable(filter = 'top',
                rownames = '',
                options = list(
                  dom = 'tip',
                  columnDefs = list(list(width = '8%', targets = c(1,3,4,5,7)))
                ))
  })
  
  category_counts <- reactive({
    cat_counts <- tibble(category = categories, n = 0)
    for (current_category in categories) {
      cat_counts$n[cat_counts$category == current_category] <-
        dta %>%
        head(input$topn_categories) %>%
        filter(category %>% str_detect(current_category)) %>%
        pull(game_id) %>%
        n_distinct()
    }
    cat_counts
  })
  
  output$games_counts_categories <- renderDT({
    category_counts() %>%
      header_renamer() %>% 
      datatable(filter = 'top',
                rownames = FALSE,
                options = list(dom = 'tip', pageLength = 5))
  })
  
  output$games_list_categories <- renderDT({
    selected_categories <- 
      category_counts()$category[input$games_counts_categories_rows_selected]
    
    selected_games <- 
      dta %>% 
      filter(category %>% str_detect(selected_categories %>% paste(collapse = '|'))) %>% 
      pull(names)
    
    dta_basic_view %>% 
      head(input$topn_categories) %>% 
      filter(names %in% selected_games) %>% 
      header_renamer() %>% 
      datatable(filter = 'top',
                rownames = '',
                options = list(
                  dom = 'tip',
                  columnDefs = list(list(width = '8%', targets = c(1,3,4,5,7)))
                ))
  })
}

shinyApp(ui, server)