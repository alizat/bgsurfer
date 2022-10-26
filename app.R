

library(tidyverse)
library(lubridate)
library(plotly)
library(tictoc)
library(glue)

library(shiny)
library(shinyjs)
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

categorical_vars <- 
    c(
        'age',
        'min_players',
        'max_players',
        'year'
    )
multivalued_cat_vars <- 
    c(
        'category',
        'designer',
        'mechanic'
    )
numerical_vars <- 
    c(
        'avg_time',
        'min_time',
        'max_time',
        'avg_rating',
        'geek_rating',
        'num_votes',
        'owned',
        'weight'
    )


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


## header for shiny app below
dbHeader <- dashboardHeader(title = 'BG Surfer')
dbHeader$children[[2]]$children <- tags$img(src='navbar-logo-bgg-b2.svg')

## UI ----
ui <- dashboardPage(
    
    #### color scheme ----
    skin = 'purple',
    
    #### header: BGG logo ----
    dbHeader,
    
    #### sidebar ----
    dashboardSidebar(
        sidebarMenu(
            
            ## Features: features to display in welcoming games table
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
            
            ## Filters: filters the overall data that is used everywhere in the app
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
    
    #### body ----
    dashboardBody(
        
        ## coloring, look and feel of the app
        includeCSS("styling.css"),
        
        ## enables show/hide of specific shiny controls
        useShinyjs(),
        
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
                        hidden(selectInput(
                            inputId  = 'plot_type',
                            label    = 'Plot Type',
                            choices  = c('Scatterplot', 'Bar Chart', 'Histogram', 'Box Plot'), # 'Pie chart'
                            selected = 'Scatterplot',
                            multiple = FALSE
                        )),
                        selectInput(
                            inputId  = 'x_eda',
                            label    = 'X',
                            choices  = numerical_vars %>% set_names(col_renamer(.)),
                            selected = 'weight',
                            multiple = FALSE
                        ),
                        selectInput(
                            inputId  = 'y_eda',
                            label    = 'Y',
                            choices  = numerical_vars %>% set_names(col_renamer(.)),
                            selected = 'owned',
                            multiple = FALSE
                        ),
                        hidden(numericInput(
                            inputId = 'num_bins',
                            label   = 'Number of Bins',
                            min     = 1,
                            max     = 30,
                            value   = 10
                        )),
                        hidden(selectInput(
                            inputId  = 'sort_by',
                            label    = 'Sort by ...',
                            choices  = c('Count', 'X'),
                            selected = 'Count',
                            multiple = FALSE
                        )),
                        hidden(selectInput(
                            inputId  = 'color_by',
                            label    = 'Color by ...',
                            choices  = c(' ', c(categorical_vars, numerical_vars)) %>% set_names(' ', col_renamer(c(categorical_vars, numerical_vars))),
                            multiple = FALSE
                        )),
                        hidden(selectInput(
                            inputId  = 'size',
                            label    = 'Size',
                            choices  = c(' ', c(categorical_vars, numerical_vars)) %>% set_names(' ', col_renamer(c(categorical_vars, numerical_vars))),
                            multiple = FALSE
                        )),
                        hidden(numericInput(
                            inputId = 'top_n',
                            label   = 'Show Top N',
                            min     = 1,
                            max     = 100,
                            value   = 10
                        ))#,
                        # uiOutput('histogram_notice')
                    ),
                    column(
                        width = 10,
                        style = 'border-left-width: 2px; border-left-color: rgb(96, 92, 168); border-left-style: dotted',
                        tabsetPanel(
                            id = 'eda_sub',
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
                                plotlyOutput('viz_eda')
                            )
                        )
                    )
                ),
            ),
            
        ),
        
    )
)

## SERVER ----
server <- function(input, output, session) {
    
    header_renamer <- function(df) {
        df %>% 
            set_names(colnames(.) %>% 
                          map_chr(~ .x %>% str_split('_') %>% unlist() %>% paste(collapse = ' ')) %>% 
                          str_to_title())
    }
    
    #### ---- load data ----
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
    
    #### ---- overview ----
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
    
    #### ---- eda ----
    
    ######## ---- show/hide controls based on selected eda tab & plot type ----
    observeEvent(c(input$eda_sub, input$plot_type), {
        
        hideElement('num_bins')
        hideElement('sort_by')
        hideElement('color_by')
        hideElement('size')
        hideElement('top_n')
        
        if (input$eda_sub == 'Table') {
            hideElement('plot_type')
            showElement('y_eda')
            
            x_vars <- c(multivalued_cat_vars, categorical_vars, numerical_vars)
            updateSelectInput(
                inputId = 'x_eda',
                choices = x_vars %>% set_names(col_renamer(x_vars))
            )
            y_vars <- c(categorical_vars, numerical_vars)
            updateSelectInput(
                inputId = 'y_eda',
                choices = c('count', y_vars) %>% set_names('Count', col_renamer(y_vars))
            )
            
        } else if (input$eda_sub == 'Visual') {
            showElement('plot_type')
            hideElement('y_eda')
            
            if (input$plot_type == 'Scatterplot') {
                showElement('y_eda')
                showElement('color_by')
                showElement('size')
                
                updateSelectInput(
                    inputId = 'x_eda',
                    choices = numerical_vars %>% set_names(col_renamer(numerical_vars)),
                    selected = 'weight'
                )
                updateSelectInput(
                    inputId = 'y_eda',
                    choices = numerical_vars %>% set_names(col_renamer(numerical_vars)),
                    selected = 'owned'
                )
                color_vars <- c(categorical_vars, numerical_vars)
                updateSelectInput(
                    inputId = 'color_by',
                    choices = color_vars %>% set_names(col_renamer(color_vars)),
                    selected = 'owned'
                )
                
            } else if (input$plot_type == 'Bar Chart') {
                showElement('color_by')
                showElement('sort_by')
                showElement('top_n')
                
                x_vars <- c(multivalued_cat_vars, categorical_vars)
                updateSelectInput(
                    inputId = 'x_eda',
                    choices = x_vars %>% set_names(col_renamer(x_vars))
                )
                color_vars <- c('Count', 'X')
                updateSelectInput(
                    inputId = 'color_by',
                    choices = color_vars %>% set_names(col_renamer(color_vars))
                )
                
            } else if (input$plot_type == 'Histogram') {
                showElement('num_bins')
                
            } else if (input$plot_type == 'Box Plot') {
                showElement('y_eda')
                showElement('top_n')
            }
        }
        
    })
    
    # observeEvent(input$x_eda, {
    #   if (input$y_eda == 'count') {
    #     updateNumericInput(inputId = 'top_n',   value    = 10)
    #     updateSelectInput(inputId = 'sort_by',  choices  = c('Count', input$x_eda) %>% set_names('Count', col_renamer(input$x_eda)))
    #     updateSelectInput(inputId = 'color_by', choices  = c('Count', input$x_eda) %>% set_names('Count', col_renamer(input$x_eda)))
    #   }
    # })
    
    ######## ---- reactive variable used for EDA/Table tab ----
    table_eda_df <- reactive({
        
        df <- dta_reactive()
        
        ## round numeric X
        if (class(df[[input$x_eda]]) == 'numeric')
            df[[input$x_eda]] <- df[[input$x_eda]] %>% round(1)
        
        ## multi-valued categorical variables  ==>  multiply rows
        if (input$x_eda %in% multivalued_cat_vars)
            df <- df %>% separate_rows(!!input$x_eda, sep = ', ')
        
        ## aggregate if Y == 'count'
        if (input$y_eda == 'count') {
            df %>% count(across(all_of(input$x_eda)), name = 'Count')
            
            ## multi-valued categorical variables  ==>  count carefully!
        } else if (input$y_eda %in% multivalued_cat_vars) {
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
            
            ## Y is assumed to be numeric
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
    
    ######## ---- render table in EDA/Table tab ----
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
    
    # output$histogram_notice <- renderUI({
    #   if (input$plot_type == 'Histogram') {
    #     HTML('<p style="text-align:center; color:rgb(96, 92, 168);">Y is ignored in histogram plots.</p>')
    #   } else {
    #     ''
    #   }
    # })
    
    ######## ---- eda/visuals tab ----
    output$viz_eda <- renderPlotly({
        req(input$x_eda)
        
        df <- dta_reactive()
        
        if (input$plot_type == 'Histogram') {
            df %>% 
                ggplot(aes_string(input$x_eda)) +
                geom_histogram(fill = '#605CA8', bins = input$num_bins) + 
                labs(x = input$x_eda %>% col_renamer()) + 
                theme_classic()
            
        } else if (input$plot_type == 'Bar Chart' && input$x_eda %in% c(multivalued_cat_vars, categorical_vars)) {
            
            ## multi-valued categorical variables  ==>  multiply rows
            if (input$x_eda %in% multivalued_cat_vars)
                df <- df %>% separate_rows(!!input$x_eda, sep = ', ')
            
            df <- 
                df %>% 
                count(across(all_of(input$x_eda)), name = 'Count')
            
            if (input$sort_by == 'Count') {
                df <- df %>% arrange(Count)
            } else {
                df <- 
                    df %>% 
                    arrange(across(all_of(input$x_eda)))
            }
            
            df <- 
                df %>% 
                tail(input$top_n) %>% 
                mutate(across(all_of(input$x_eda), ~ factor(.x, levels = .x))) %>% 
                mutate(Info = glue('
                           <BR>{str_to_title(input$x_eda)}: {get(input$x_eda)}
                           Count: {Count}
                           '))
            
            if (input$color_by == 'X') {
                my_colors <- rainbow(input$top_n)
                fill_by_this <- input$x_eda
            } else {
                my_colors <- colorRampPalette(colors = c('#383663', '#7f7cb9'))(input$top_n)
                fill_by_this <- 'Count'
            }
            
            p <- 
                df %>%
                ggplot(aes_string(input$x_eda, 'Count')) +
                # geom_bar(fill = '#605CA8', stat = 'identity') +
                geom_bar(aes_string(fill = fill_by_this), stat = 'identity') +
                labs(x = input$x_eda %>% col_renamer()) +
                theme_classic()
            if (input$color_by == 'Count') {
                p <- p + scale_fill_gradient(low = '#383663', high = '#605CA8')
            } else {
                p <- p + scale_fill_manual(values = my_colors) + scale_fill_hue(l = 40)
            }
            p
            
        } else if (input$plot_type == 'Scatterplot') {
            req(input$y_eda)
            if (input$y_eda != 'count') {
                p <- 
                    df %>% 
                    mutate(Game = glue('
                             {names}
                             {col_renamer(input$x_eda)}: {get(input$x_eda)}
                             {col_renamer(input$y_eda)}: {get(input$y_eda)}
                             {if (input$color_by != " ") paste0(col_renamer(input$color_by), ": ", get(input$color_by)) else ""}
                             {if (input$size != " ")     paste0(col_renamer(input$size),     ": ", get(input$size))     else ""}
                           ') %>% trim()) %>% 
                    ggplot(aes_string(input$x_eda, input$y_eda, label = 'Game'))
                
                if (input$color_by == ' ') {
                    if (input$size == ' ') {
                        p <- p + geom_point(                               color = '#605CA8', alpha = 0.3)
                    } else {
                        p <- p + geom_point(aes_string(size = input$size), color = '#605CA8', alpha = 0.3)
                    }
                } else {
                    if (input$size == ' ') {
                        p <- p + geom_point(aes_string(color = input$color_by),                    alpha = 0.3) + scale_color_gradient(low = '#1c1b32', high = '#9f9dca')
                    } else {
                        p <- p + geom_point(aes_string(color = input$color_by, size = input$size), alpha = 0.3) + scale_color_gradient(low = '#1c1b32', high = '#9f9dca')
                    }
                }
                
                if (input$size != ' ') {
                    
                }
                
                p <- 
                    p + 
                    labs(x = input$x_eda %>% col_renamer(),
                         y = input$y_eda %>% col_renamer()) + 
                    theme_classic()
                
                p %>% ggplotly(tooltip = 'Game')
            }
        }
    })
    
}

shinyApp(ui, server)