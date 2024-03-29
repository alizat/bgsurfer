

library(tidyverse)
library(lubridate)
library(plotly)
library(tictoc)
library(glue)

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)

library(bslib)
library(thematic)
library(showtext)
library(patchwork)

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


# # Builds theme object to be supplied to ui
# my_theme <- bs_theme(
#   bootswatch = "cerulean",
#   base_font = font_google("Righteous")
# ) %>%
#   bs_add_rules(sass::sass_file("styles.scss"))
# 
# # Let thematic know to use the font from bs_lib
# thematic_shiny(font = "auto")
# 
# usage: fluidPage(..., theme = my_theme, ...)


dbHeader <- dashboardHeader(title = 'BG Surfer')
dbHeader$children[[2]]$children <- tags$img(src='navbar-logo-bgg-b2.svg')
ui <- dashboardPage(
  skin = 'purple',
  dbHeader,
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = 'Features',
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
                         `count-selected-text` = "{0} out of {1} features",
                         `none-selected-text` = "None selected"),
          multiple = TRUE
        )
      ),
      menuItem(
        text = 'Filters',
        
        ## rank filter
        menuItem(
          'Rank',
          sliderInput(
            inputId = 'rank',
            label = '',
            min = 0,
            max = 5000,
            value = c(0, 1000)
          )
        ),
        
        ## number of players filter
        menuItem(
          'Number of players',
          sliderInput(
            inputId = 'num_players',
            label = '',
            min = min(dta$min_players),
            max = max(dta$max_players),
            value = c(min(dta$min_players), max(dta$max_players))
          )
        ),
        
        ## year filter
        menuItem(
          'Year',
          sliderInput(
            inputId = 'year',
            label = '',
            min = min(dta$year),
            max = max(dta$year),
            value = c(min(dta$year), max(dta$year))
          )
        ),
        
        ## avg_rating filter
        menuItem(
          'Average Rating',
          sliderInput(
            inputId = 'avg_rating',
            label = '',
            min = round(min(dta$avg_rating), 2) - 0.01,
            max = round(max(dta$avg_rating), 2) + 0.01,
            value = c(round(min(dta$avg_rating), 2) - 0.01,
                      round(max(dta$avg_rating), 2) + 0.01)
          )
        ),
        
        ## num_votes filter
        menuItem(
          'Number of Votes',
          sliderInput(
            inputId = 'num_votes',
            label = '',
            min = min(dta$num_votes),
            max = max(dta$num_votes),
            value = c(min(dta$num_votes),
                      max(dta$num_votes))
          )
        ),
        
        ## num_votes filter
        menuItem(
          'Age',
          sliderInput(
            inputId = 'age',
            label = '',
            min = min(dta$age),
            max = max(dta$age),
            value = c(min(dta$age),
                      max(dta$age))
          )
        ),
        
        ## owned filter
        menuItem(
          'Owned by how many',
          sliderInput(
            inputId = 'owned',
            label = '',
            min = min(dta$owned),
            max = max(dta$owned),
            value = c(min(dta$owned),
                      max(dta$owned))
          )
        ),
        
        ## categories filter
        menuItem(
          'Categories',
          pickerInput(
            inputId = 'selected_categories',
            label = '',
            choices = categories,
            selected = categories,
            options = list(`actions-box` = TRUE,
                           `selected-text-format`= "count",
                           `count-selected-text` = "{0} out of {1} categories",
                           `none-selected-text` = "None selected"),
            multiple = TRUE
          )
        ),
        
        ## mechanics filter
        menuItem(
          'Mechanics',
          pickerInput(
            inputId = 'selected_mechanics',
            label = '',
            choices = mechanics,
            selected = mechanics,
            options = list(`actions-box` = TRUE,
                           `selected-text-format`= "count",
                           `count-selected-text` = "{0} out of {1} mechanics",
                           `none-selected-text` = "None selected"),
            multiple = TRUE
          )
        )
      )
    )
    
  ),
  dashboardBody(
    
    includeCSS("styling.css"),
    
    shinyjs::useShinyjs(),
    
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
          column(
            width = 2,
            selectInput(
              inputId  = 'x_eda',
              label    = 'X',
              choices  = c('year', 'mechanic', 'category', 'designer', 'age', 'avg_rating') %>% set_names(col_renamer(.)),
              selected = 'year',
              multiple = FALSE),
            selectInput(
              inputId  = 'y_eda',
              label    = 'Y',
              choices  = c('count',
                           dta %>% select_if(is.numeric) %>% colnames(),
                           'category', 'mechanic', 'designer') %>% set_names(col_renamer(.)),
              selected = 'count',
              multiple = FALSE),
            selectInput(
              inputId  = 'plot_type',
              label    = 'Plot Type',
              choices  = c('Bar chart', 'Pie chart', 'Histogram'),
              selected = 'Bar chart',
              multiple = FALSE
            ),
            selectInput(
              inputId  = 'sort_by',
              label    = 'Sort by ...',
              choices  = 'Count',
              selected = 'Count',
              multiple = FALSE
            ),
            selectInput(
              inputId  = 'color_by',
              label    = 'Color by ...',
              choices  = 'Count',
              selected = 'Count',
              multiple = FALSE
            ),
            numericInput(
              inputId = 'top_n',
              label   = 'Show Top N',
              min     = 1,
              max     = 100,
              value   = 10
            )
          ),
          column(
            width = 10,
            style = 'border-left-width: 2px; border-left-color: rgb(96, 92, 168); border-left-style: dotted',
            tabsetPanel(
              id = 'visuals',
              tabPanel(
                'Table',
                br(),
                helpText('Highlight categories to see relevant games.'),
                HTML('<center>'),
                DTOutput('table_eda'),
                HTML('</center>'),
                br(),
                br(),
                helpText('If multiple categories are highlighted, you may select whether games are to have one or all of the higlighted categories'),
                fluidRow(
                  column(width = 3,
                         offset = 1,
                         radioButtons(
                           inputId  = 'rb_any_or_all_values',
                           label    = 'Games below to contain...',
                           choices  = c('any highlighted values', 'all highlighted values'),
                           selected = 'any highlighted values'
                         ),
                         textOutput('num_games_values_selected')
                  ),
                  column(width = 7, htmlOutput('txt_highlighted_values'))
                ),
                
                DTOutput('table_breakdown_eda')
              ),
              tabPanel(
                'Visual',
                br(),
                # fluidRow(
                #   column(
                #     width = 3,
                #     selectInput(
                #       inputId = 'facet_by',
                #       label   = 'Facet by ...',
                #       choices  = c('mechanic', 'category', 'designer') %>% set_names(col_renamer(.)),
                #       selected = 'mechanic',
                #       multiple = FALSE
                #     )
                #   ),
                #   column(
                #     width = 3,
                #     numericInput(
                #       inputId = 'facet_n',
                #       label   = 'Max Number of Facets',
                #       min     = 1,
                #       max     = 10,
                #       value   = 4
                #     )
                #   )
                # ),
                # br(),
                plotlyOutput('viz_eda'),
                numericInput(
                  inputId = 'num_bins',
                  label   = 'Number of Bins',
                  min     = 1,
                  max     = 30,
                  value   = 10
                )
              )
            )
          )
        ),
      ),
      
    ),
    
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
      filter(rank %>% between(input$rank[[1]], input$rank[[2]]) ) %>%
      filter(min_players %>% between(input$num_players[[1]], input$num_players[[2]]) | 
               max_players %>% between(input$num_players[[1]], input$num_players[[2]])) %>%
      # filter(min_players <= input$num_players[[1]], 
      #        max_players >= input$num_players[[2]]) %>%
      filter(year       %>% between(       input$year[[1]], input$year[[2]]       )) %>%
      filter(avg_rating %>% between( input$avg_rating[[1]], input$avg_rating[[2]] )) %>%
      filter(num_votes  %>% between(  input$num_votes[[1]], input$num_votes[[2]]  )) %>%
      filter(age        %>% between(        input$age[[1]], input$age[[2]]        )) %>%
      filter(owned      %>% between(      input$owned[[1]], input$owned[[2]]      )) %>%
      filter(category   %>% str_detect(input$selected_categories %>% paste(collapse = '|'))) %>%
      filter(mechanic   %>% str_detect(input$selected_mechanics  %>% paste(collapse = '|')))
    
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
  
  observeEvent(input$visuals, {
    if (input$visuals == 'Visual') {
      shinyjs::showElement('plot_type')
      shinyjs::showElement('top_n')
      shinyjs::showElement('sort_by')
      shinyjs::showElement('color_by')
      
    } else {
      shinyjs::hideElement('plot_type')
      shinyjs::hideElement('top_n')
      shinyjs::hideElement('sort_by')
      shinyjs::hideElement('color_by')
    }
    
  })
  
  observeEvent(input$plot_type, {
    if (input$plot_type == 'Histogram') {
      shinyjs::showElement('num_bins')
    } else {
      shinyjs::hideElement('num_bins')
    }
  })
  
  observeEvent(input$x_eda, {
    if (input$y_eda == 'count') {
      updateNumericInput(inputId = 'top_n',   value    = 10)
      updateSelectInput(inputId = 'sort_by',  choices  = c('Count', input$x_eda) %>% set_names('Count', col_renamer(input$x_eda)))
      updateSelectInput(inputId = 'color_by', choices  = c('Count', input$x_eda) %>% set_names('Count', col_renamer(input$x_eda)))
    }
  })
  
  table_eda_df <- reactive({
    
    df <- dta_reactive()
    
    if (input$x_eda %in% c('category', 'mechanic', 'designer'))
      df <- df %>% separate_rows(!!input$x_eda, sep = ', ')
    
    if (input$y_eda == 'count') {
      df %>% count(across(all_of(input$x_eda)), name = 'Count')
      
    } else if (input$y_eda %in% c('category', 'mechanic', 'designer')) {
      df <- 
        df %>% 
        select(all_of(c(input$x_eda, input$y_eda))) %>% 
        group_by(across(all_of(input$x_eda))) %>% 
        summarise(across(all_of(input$y_eda), n_distinct)) %>% 
        ungroup()
      
      colnames(df)[2:ncol(df)] <-
        colnames(df)[2:ncol(df)] %>%
        paste('[# unique values]')
      
      df
      
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
  
  # output$num_games_values_selected <- renderText({
  #   req(values_selected_games())
  #   values_selected_games() %>% length() %>% paste('games')
  # })
  
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
  
  observeEvent(input$y_eda, {
    if (input$y_eda == 'count') {
      updateSelectInput(inputId = 'plot_type', 
                        choices = c('Bar chart', 'Pie chart', 'Histogram'), 
                        selected = 'Bar chart')
    }
  })
  
  output$viz_eda <- renderPlotly({
    req(input$x_eda, input$y_eda, input$top_n, input$sort_by, input$color_by)
    
    if (input$y_eda == 'count' && input$plot_type == 'Bar chart') {
      
      df <- table_eda_df()
      if (length(input$x_eda) > 1) {
        my_x <- input$x_eda %>% paste(collapse = '-')
        df <- df %>% unite(input$x_eda %>% paste(collapse = '_'))
      } else {
        my_x <- input$x_eda
      }
      
      p <- 
        df %>% 
        arrange(across(all_of(input$sort_by))) %>% 
        tail(input$top_n) %>% 
        mutate(across(all_of(my_x), ~ factor(.x, levels = .x))) %>% 
        mutate(Info = glue('
                           <BR>{str_to_title(my_x)}: {get(my_x)}
                           Count: {Count}
                           ')) %>% 
        ggplot(aes_string(my_x, 'Count', fill = input$color_by, label = 'Info')) +
        geom_bar(stat = 'identity') + 
        coord_flip() + 
        theme(legend.position = 'none') + 
        labs(title = glue('{col_renamer(input$y_eda)} ~ {col_renamer(input$x_eda)}'),
             x = glue('{col_renamer(input$x_eda)}'),
             y = glue('{col_renamer(input$y_eda)}'))
      
      if (input$color_by == 'Count') {
        p <- p + scale_fill_gradient(low = '#383663', high = '#605CA8')
      }
      if (input$color_by == input$x_eda) {
        p <- p + scale_fill_manual(values = rainbow(input$top_n)) + scale_fill_hue(l = 40)
        #+ scale_fill_brewer(palette = 'Spectral')
      }
      
      p <- ggplotly(p, tooltip = 'Info')
      
      p
      
    } else if (input$y_eda == 'count' && input$plot_type == 'Pie chart') {
      
      df <- table_eda_df()
      if (length(input$x_eda) > 1) {
        my_x <- input$x_eda %>% paste(collapse = '-')
        df <- df %>% unite(input$x_eda %>% paste(collapse = '_'))
      } else {
        my_x <- input$x_eda
      }
      
      df_tmp <- 
        df %>% 
        arrange(across(all_of(input$sort_by))) %>% 
        tail(input$top_n) %>% 
        mutate(across(all_of(my_x), ~ factor(.x, levels = .x))) %>% 
        mutate(Info = glue('
                           <BR>{str_to_title(my_x)}: {get(my_x)}
                           Count: {Count}
                           '))
      
      if (input$color_by == input$x_eda) {
        my_colors <- rainbow(input$top_n)
      } else {
        my_colors <- colorRampPalette(colors = c('#383663', '#7f7cb9'))(input$top_n)
      }
      
      t <- list(
        family = "Arial Rounded MT",
        # color = 'white',
        size = 14)
      
      p <- plot_ly(df_tmp, 
                   labels = as.formula(paste0('~', my_x)), 
                   values = as.formula(paste0('~', 'Count')), 
                   textinfo = 'label',  # 'label+percent'
                   hoverinfo = 'text',
                   insidetextfont = list(color = '#FFFFFF'),
                   text = ~paste(Count, ' games'),
                   marker = list(colors = my_colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   type = 'pie') %>% 
        layout(title = glue('{col_renamer(input$y_eda)} ~ {col_renamer(input$x_eda)}'), font = t)
      
      p
    }
  })
  
}

shinyApp(ui, server)