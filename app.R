

library(tidyverse)
library(lubridate)
library(plotly)
library(tictoc)
library(glue)

library(shiny)
library(shinyWidgets)
library(shinydashboard)
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
col_renamer <- function(col_header_names) {
  col_header_names %>% 
    map_chr(~ .x %>% str_split('_') %>% unlist() %>% paste(collapse = ' ')) %>% 
    str_to_title()
}


ui <- fluidPage(
  
  titlePanel('Analyzing the top tabletop games (according to BGG)'),
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput(
        inputId = 'topk', 
        label = 'Top K games to analyze', 
        min = 0, 
        max = 5000, 
        value = 100
      ),
      pickerInput(
        inputId = 'selected_cols', 
        label = 'Features of Interest', 
        choices = colnames(dta) %>% set_names(col_renamer(.)), 
        selected = c('rank',
                     'names',
                     'year',
                     'avg_rating',
                     'num_votes',
                     'designer', 
                     'weight') %>% set_names(col_renamer(.)), 
        multiple = TRUE
      ),
      br(),
      h3('Filters'),
      pickerInput(
        inputId = 'selected_categories', 
        label = 'Categories', 
        choices = categories, 
        selected = categories, 
        multiple = TRUE
      ),
      pickerInput(
        inputId = 'selected_mechanics', 
        label = 'Mechanics', 
        choices = mechanics, 
        selected = mechanics, 
        multiple = TRUE
      ),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        
        ## Overview
        tabPanel(
          'Games List',
          DTOutput('games_list')
        ),
        
        ## Categories
        tabPanel(
          'Categories',
          br(),
          helpText('Highlight categories to see relevant games.'),
          helpText('If multiple categories are highlighted, you may select whether games are to have one or all of the higlighted categories'),
          br(),
          fluidRow(
            column(width = 6, DTOutput('games_counts_categories')),
            column(width = 6, htmlOutput('txt_highlighted_categories'))
          ),
          br(),
          br(),
          radioButtons(
            inputId  = 'rb_any_or_all_categories', 
            label    = 'Games below to contain...', 
            choices  = c('any highlighted categories', 'all highlighted categories'), 
            selected = 'any highlighted categories'
          ),
          br(),
          br(),
          DTOutput('games_list_categories')
        ),
        
        ## Mechanics
        tabPanel(
          'Mechanics',
          br(),
          helpText('Highlight mechanics to see relevant games.'),
          helpText('If multiple mechanics are highlighted, you may select whether games are to have one or all of the higlighted mechanics'),
          br(),
          fluidRow(
            column(width = 6, DTOutput('games_counts_mechanics')),
            column(width = 6, htmlOutput('txt_highlighted_mechanics'))
          ),
          br(),
          br(),
          radioButtons(
            inputId  = 'rb_any_or_all_mechanics', 
            label    = 'Games below to contain...', 
            choices  = c('any highlighted mechanics', 'all highlighted mechanics'), 
            selected = 'any highlighted mechanics'
          ),
          br(),
          br(),
          DTOutput('games_list_mechanics')
        ),
        
        ## Designers
        tabPanel('Designers')
        
      ),
      width = 9
    )
    
  )

)

server <- function(input, output, session) {
  
  header_renamer <- function(df) {
    df %>% 
      set_names(colnames(.) %>% 
                  map_chr(~ .x %>% str_split('_') %>% unlist() %>% paste(collapse = ' ')) %>% 
                  str_to_title())
  }
  
  dta <- read_csv('data/1.mrpantherson/bgg_db_1806.csv')
  dta_reactive <- reactive({
    dta %>% 
      filter(category %>% str_detect(input$selected_categories %>% paste(collapse = '|'))) %>% 
      filter(mechanic %>% str_detect(input$selected_mechanics  %>% paste(collapse = '|')))
  })
  dta_compact <- reactive({
    dta_reactive() %>% select(all_of(input$selected_cols))
  })
  
  ## *******
  
  output$games_list <- renderDT({
    dta_compact() %>% 
      header_renamer() %>% 
      datatable(filter = 'top',
                rownames = '',
                options = list(
                  dom = 'tip',
                  columnDefs = list(list(width = '8%', targets = c(1,3,4,5,7)))
                ))
  })
  
  ## *******
  
  ## CATEGORIES
  
  category_counts <- reactive({
    cat_counts <- tibble(category = categories, number_of_games = 0)
    for (current_category in categories) {
      if (current_category %in% dta_reactive()$category) {
        cat_counts$number_of_games[cat_counts$category == current_category] <-
          dta_reactive() %>%
          head(input$topk) %>%
          filter(category %>% str_detect(current_category)) %>%
          pull(game_id) %>%
          n_distinct()
      }
    }
    cat_counts
  })
  
  output$games_counts_categories <- renderDT({
    category_counts() %>%
      header_renamer() %>% 
      datatable(rownames = FALSE,
                options = list(dom = 'tip')) %>% 
      formatStyle(c('Category', 'Number Of Games'), lineHeight = '40%')
  })
  
  output$txt_highlighted_categories <- renderUI({
    HTML(glue(
      '
      <center>
      <H3>Highlighted Categories</H3>
      {unique(category_counts()[input$games_counts_categories_rows_selected,]$category) %>% paste(collapse = "<BR>")}
      </center>
      '
    ))
  })
  
  output$games_list_categories <- renderDT({
    selected_categories <- 
      category_counts()$category[input$games_counts_categories_rows_selected]
    
    selected_games <- c()
    if (length(selected_categories) > 0) {
      if (input$rb_any_or_all_categories == 'any highlighted categories') {
        selected_games <- 
          dta_reactive() %>% 
          filter(category %>% str_detect(selected_categories %>% paste(collapse = '|'))) %>% 
          pull(names)
      } else if (input$rb_any_or_all_categories == 'all highlighted categories') {
        selected_games <- dta_reactive()
        for (current_category in selected_categories) {
          selected_games <- 
            selected_games %>% 
            filter(category %>% str_detect(current_category))
        }
        selected_games <- selected_games %>% pull(names)
      }
    }
    
    if (length(selected_games) > 0) {
      dta_compact() %>% 
        head(input$topk) %>% 
        filter(names %in% selected_games) %>% 
        header_renamer() %>% 
        datatable(filter = 'top',
                  rownames = '',
                  options = list(
                    dom = 'tip',
                    columnDefs = list(list(width = '8%', targets = c(1,3,4,5,7)))
                  ))
    }
  })
  
  ## *******
  
  ## MECHANICS
  
  mechanic_counts <- reactive({
    mech_counts <- tibble(mechanic = mechanics, number_of_games = 0)
    for (current_mechanic in mechanics) {
      if (current_mechanic %in% dta_reactive()$mechanic) {
        mech_counts$number_of_games[mech_counts$mechanic == current_mechanic] <-
          dta_reactive() %>%
          head(input$topk) %>%
          filter(mechanic %>% str_detect(current_mechanic)) %>%
          pull(game_id) %>%
          n_distinct()
      }
    }
    mech_counts
  })
  
  output$games_counts_mechanics <- renderDT({
    mechanic_counts() %>%
      header_renamer() %>% 
      datatable(rownames = FALSE,
                options = list(dom = 'tip')) %>% 
      formatStyle(c('Mechanic', 'Number Of Games'), lineHeight = '40%')
  })
  
  output$txt_highlighted_mechanics <- renderUI({
    HTML(glue(
      '
      <center>
      <H3>Highlighted Mechanics</H3>
      {unique(mechanic_counts()[input$games_counts_mechanics_rows_selected,]$mechanic) %>% paste(collapse = "<BR>")}
      </center>
      '
    ))
  })
  
  output$games_list_mechanics <- renderDT({
    selected_mechanics <- 
      mechanic_counts()$mechanic[input$games_counts_mechanics_rows_selected]
    
    selected_games <- c()
    if (length(selected_mechanics) > 0) {
      if (input$rb_any_or_all_mechanics == 'any highlighted mechanics') {
        selected_games <- 
          dta_reactive() %>% 
          filter(mechanic %>% str_detect(selected_mechanics %>% paste(collapse = '|'))) %>% 
          pull(names)
      } else if (input$rb_any_or_all_mechanics == 'all highlighted mechanics') {
        selected_games <- dta_reactive()
        for (current_mechanic in selected_mechanics) {
          selected_games <- 
            selected_games %>% 
            filter(mechanic %>% str_detect(current_mechanic))
        }
        selected_games <- selected_games %>% pull(names)
      }
    }
    
    if (length(selected_games) > 0) {
      dta_compact() %>% 
        head(input$topk) %>% 
        filter(names %in% selected_games) %>% 
        header_renamer() %>% 
        datatable(filter = 'top',
                  rownames = '',
                  options = list(
                    dom = 'tip',
                    columnDefs = list(list(width = '8%', targets = c(1,3,4,5,7)))
                  ))
    }
  })
  
  ## *******
  
  ## DESIGNERS
  
  
}

shinyApp(ui, server)