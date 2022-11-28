
library(shiny)
library(shinydashboard)
source('explore.r')
library(dplyr)
dashboardPage(skin = 'yellow', title = 'R shiny Application',
  dashboardHeader(titleWidth = '100%', 
                  title = span(
                    tags$img(src = "main.jpg", width = '100%'),
                    column(12, class = 'title-box',
                           tags$h1(class = 'primary-title', style = 'margin-top:10px; color:black; font-size:40px', 'VINHO VERDE'),
                           tags$h2(class = 'primary-subtitle', style = 'margin-top:20px;color:#100a22; font-size:25px', 'R- Shiny Web Application to Predict Quality of Wine'))
                    
                  )),
  dashboardSidebar(
    sidebarMenu(id = 'menu1',
      menuItem("About", tabName = "about", icon = icon("dashboard")),
      menuItem("Data Exploration", tabName = "explore", icon = icon("wpexplorer"))
    ),
    # extra options once user clicks the data exploration
    conditionalPanel(condition = "input.menu1== 'explore' ",
                     selectInput(inputId = 'features', label = 'Select Predictor Column', 
                     choices = colnames(df)[-12]),
                     selectInput(inputId = 'graphType', label = 'Select Type of Distribution',
                                 choices = c('Histogram', 'Density Plot', 'Box Plot', 'Count Plot'),
                                 selected = 'Count Plot'),
                     checkboxInput(inputId = 'missing', label = 'Check for Missing data', value = F)
                     ),
    sidebarMenu(id = 'menu2',
      menuItem("Modeling", tabName = "model", icon = icon("th"))
      ),
    sidebarMenu(id = 'menu3',
      menuItem("Data", tabName = "data", icon = icon("book"))
    )),
  dashboardBody(tags$style(type="text/css", "
/*    Move everything below the header */
    .content-wrapper {
        margin-top: 50px;
    }
    .content {
        padding-top: 60px;
    }
/*    Format the title/subtitle text */
    .title-box {
        position: absolute;
        text-align: center;
        top: 50%;
        left: 50%;
        transform:translate(-50%, -50%);
    }
    @media (max-width: 590px) {
        .title-box {
            position: absolute;
            text-align: center;
            top: 10%;
            left: 10%;
            transform:translate(-5%, -5%);
        }
    }
    @media (max-width: 767px) {
        .primary-title {
            font-size: 1.1em;
        }
        .primary-subtitle {
            font-size: 1em;
        }
    }
/*    Make the image taller */
    .main-header .logo {
        height: 120px;
    }
/*    Override the default media-specific settings */
    @media (max-width: 5000px) {
        .main-header {
            padding: 0 0;
            position: relative;
        }
        .main-header .logo,
        .main-header .navbar {
            width: 100%;
            float: none;
        }
        .main-header .navbar {
            margin: 0;
        }
        .main-header .navbar-custom-menu {
            float: right;
        }
    }
/*    Move the sidebar down */
    .main-sidebar {
        position: absolute;
    }
    .left-side, .main-sidebar {
        padding-top: 175px;
    }"
  ),
    tabItems(
      # First tab content
      tabItem(tabName = "about",
              fluidRow(
                box(status = 'success', width = 12, solidHeader = T, collapsible = F, background = 'light-blue',
                    h4('The primary purpose of this application is to develop a web based shiny 
                       application that can be used to explore "Wine Quality Dataset" publically available at
                       the UCI repository.', br(),
                       "To navigate into this app please go through the following instructions:",
                       br(),
                       "1. Data Exploration : ",
                       br(),
                       "2. Modeling:",
                       br(),
                       "3. Data : ",
                       br(), style = 'color:black; font-size:20px'
                       )),
      )
      ),
      
      # Second tab content
      tabItem(tabName = "explore",
              fluidRow(
                box(width = 12, h4("Distribution plots comes handy when you have multiple data points and you are looking to explore multiple type
                                   of distribution plots to analyse the dataset")),
                box(title = "Plots", width = 6, status = 'success',plotOutput("plot1", height = 250),
                    conditionalPanel(condition = "input.menu1.graphType == Histogram",
                                     checkboxInput('fd', label = 'Use Freedman-Diaconis Rule'))),
                box(title = 'Corr Plots', width = 6, plotOutput("plot3", height = 250),
                    sliderInput(inputId = 'power', label = "Power transformation", -3,3,1,step = 0.01 ))
                
              ),
              fluidRow(
                box(status = 'warning', width = 4, plotOutput("plot2", height = 200))
              ),
              fluidRow(
                box(h4("afaalfjalkjf"))
              )
      ),
      
      # third tab
      tabItem(tabName = 'model',
              h2('Data modeling ')
      ),
      
      # fourth tab content
      tabItem(tabName = 'data',
              h2('data set')
      )
    )
  )
)