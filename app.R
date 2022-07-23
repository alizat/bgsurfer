

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
p25 <- function(x) {quantile(x, probs = 0.25, na.rm = TRUE)}
p50 <- function(x) {quantile(x, probs = 0.50, na.rm = TRUE)}
p75 <- function(x) {quantile(x, probs = 0.75, na.rm = TRUE)}


ui <- fluidPage(
  
  titlePanel('Analyzing the top tabletop games (according to BGG)'),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3('Features'),
      
      pickerInput(
        inputId = 'selected_cols', 
        label = 'Features of Interest', 
        choices = 
          colnames(dta) %>% 
          set_names(col_renamer(.)) %>% 
          c('Num Categories', 'Num Mechanics', 'Num Designers'), 
        selected = c('rank',
                     'names',
                     'year',
                     'avg_rating',
                     'num_votes',
                     'designer', 
                     'weight') %>% set_names(col_renamer(.)), 
        options = list(`actions-box` = TRUE,
                       `selected-text-format`= "count",
                       `count-selected-text` = "{0} features (out of {1})",
                       `none-selected-text` = "None selected"), 
        multiple = TRUE
      ),
      
      br(),
      h3('Filters'),
      
      ## rank filter
      numericInput(
        inputId = 'rank', 
        label = 'Rank (at most)', 
        min = 0, 
        max = 5000, 
        value = 1000
      ),
      
      ## categories filter
      pickerInput(
        inputId = 'selected_categories', 
        label = 'Categories', 
        choices = categories, 
        selected = categories, 
        options = list(`actions-box` = TRUE,
                       `selected-text-format`= "count",
                       `count-selected-text` = "{0} categories (out of {1})",
                       `none-selected-text` = "None selected"), 
        multiple = TRUE
      ),
      
      ## mechanics filter
      pickerInput(
        inputId = 'selected_mechanics', 
        label = 'Mechanics', 
        choices = mechanics, 
        selected = mechanics, 
        options = list(`actions-box` = TRUE,
                       `selected-text-format`= "count",
                       `count-selected-text` = "{0} mechanics (out of {1})",
                       `none-selected-text` = "None selected"), 
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
        
        ## EDA
        tabPanel(
          'EDA',
          br(),
          fluidRow(
            column(width = 4,
                   selectInput(
                     inputId  = 'x_eda',
                     label    = 'X',
                     choices  = c('year', 'mechanic', 'category'),
                     selected = 'year',
                     multiple = FALSE)),
            column(width = 4,
                   selectInput(
                     inputId  = 'y_eda',
                     label    = 'Y',
                     choices  = c('count', dta %>% select_if(is.numeric) %>% colnames()),
                     selected = 'count',
                     multiple = FALSE)),
            column(width = 4, uiOutput('agg_control'))
          ),

          tabsetPanel(
            tabPanel(
              'Table',
              br(),
              HTML('<center>'),
              DTOutput('table_eda'),  #, width = '80%'
              HTML('</center>'),
              br(),
              hr(),
              br(),
              fluidRow(
                column(width = 3,
                       radioButtons(
                         inputId  = 'rb_any_or_all_values',
                         label    = 'Games below to contain...',
                         choices  = c('any highlighted values', 'all highlighted values'),
                         selected = 'any highlighted values'
                       ),
                       textOutput('num_games_values_selected')
                ),
                column(width = 9, htmlOutput('txt_highlighted_values'))
              ),
              
              DTOutput('table_breakdown_eda')
            ),
            tabPanel(
              'Visual',
              plotOutput('viz_eda')
            )
          )
        ),
        
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
    df <- 
      dta %>% 
      filter(rank <= input$rank) %>%
      filter(category %>% str_detect(input$selected_categories %>% paste(collapse = '|'))) %>% 
      filter(mechanic %>% str_detect(input$selected_mechanics  %>% paste(collapse = '|')))
    
    count_categories <- function(x) {
      x %>% 
        str_split(', ') %>% 
        map_dbl(~ .x %>% length())
    }
    
    df <- 
      df %>% 
      mutate(`Num Categories` = count_categories(category),
             `Num Mechanics`  = count_categories(mechanic),
             `Num Designers`  = count_categories(designer))
    
    df
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
  
  table_eda_df <- reactive({
    
    df <- dta_reactive()
    
    if (input$x_eda %in% c('category', 'mechanic'))
      df <- df %>% separate_rows(!!input$x_eda, sep = ', ')
    
    if (input$y_eda == 'count') {
      df %>% count(across(all_of(input$x_eda)), name = 'Count')
      
    } else {
      df <- 
        df %>% 
        select(all_of(c(input$x_eda, input$y_eda))) %>% 
        group_by(across(all_of(input$x_eda))) %>% 
        summarise(across(all_of(input$y_eda), c(min, p25, p50, mean, p75, max))) %>% 
        ungroup()
      
      colnames(df)[2:ncol(df)] <- 
        colnames(df)[2:ncol(df)] %>% 
        str_replace('_[1-9]+$', glue("_[{c('min', 'Q1', 'median', 'avg', 'Q3', 'max')}]"))
      
      df
    }
  })
  
  output$table_eda <- renderDT({
    display_me = table_eda_df()
    colnames(display_me) <- 
      colnames(display_me) %>% 
      str_split('_') %>% 
      map_chr(~ .x %>% unlist() %>% paste(collapse = ' ')) %>% 
      str_to_title()
    display_me <- 
      display_me %>% 
      mutate_if(is.numeric, round, digits = 1)
    display_me %>% 
      datatable(
        filter = 'top',
        rownames = FALSE,
        options = list(
          dom = 'tip',
          columnDefs = list(
            list(className = 'dt-center', targets = '_all'),
            list(width = '30%', targets = 0)
          )
        )
      ) %>% 
      formatStyle(1:ncol(display_me), lineHeight = '40%')
  })

  values_selected <- reactive({
    selected_values <-
      table_eda_df()[input$table_eda_rows_selected,1] %>% pull()
    selected_values
  })

  values_selected_games <- reactive({
    selected_values <- values_selected()
    selected_games <- c()
    if (length(selected_values) > 0) {
      if (input$rb_any_or_all_values == 'any highlighted values') {
        selected_games <- dta_reactive()
        selected_games <-
          selected_games %>%
          filter(
            as.character(selected_games[[input$x_eda]]) %>% str_detect(selected_values %>% paste(collapse = '|'))
          ) %>%
          pull(names)
      } else if (input$rb_any_or_all_values == 'all highlighted values') {
        selected_games <- dta_reactive()
        for (current_value in selected_values) {
          selected_games <-
            selected_games %>%
            filter(
              as.character(selected_games[[input$x_eda]]) %>% str_detect(as.character(current_value))
            )
        }
        selected_games <- selected_games %>% pull(names)
      }
    }

    selected_games
  })

  output$txt_highlighted_values <- renderUI({
    HTML(glue(
      '
      <center>
      <H4>Highlighted values</H4>
      {unique(table_eda_df()[input$table_eda_rows_selected,1] %>% pull()) %>% paste(collapse = ", ")}
      </center>
      '
    ))
  })

  output$num_games_values_selected <- renderText({
    req(values_selected_games())
    values_selected_games() %>% length() %>% paste('games')
  })

  output$table_breakdown_eda <- renderDT({
    selected_categories <- values_selected()
    selected_games <- values_selected_games()

    if (length(selected_games) > 0) {
      display_me <-
        dta_compact() %>%
        filter(names %in% selected_games) %>%
        header_renamer()
      numeric_variables_indices <-
        display_me %>%
        map_lgl(~ is.numeric(.x)) %>%
        which() %>%
        unname()
      display_me %>%
        datatable(filter = 'top',
                  rownames = '',
                  options = list(
                    dom = 'tip',
                    columnDefs = list(list(width = '8%', targets = numeric_variables_indices))
                  )
        )
    }
  })
  
}

shinyApp(ui, server)