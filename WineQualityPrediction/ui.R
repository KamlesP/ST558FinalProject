
library(shiny)
library(shinydashboard)
source('explore.r')
library(dplyr)
library(shinyalert)
library(mathjaxr)
library(shinythemes)
library(dashboardthemes)

dashboardPage(skin = 'yellow', title = 'R shiny Application',
  dashboardHeader(titleWidth = '100%', 
                  title = span(
                    tags$img(src = "main.jpg", width = '100%'),
                    column(12, class = 'title-box',
                           tags$h1(class = 'primary-title', style = 'margin-top:10px; color:black; font-size:40px', 'VINHO VERDE'),
                           tags$h2(class = 'primary-subtitle', style = 'margin-top:20px;margin-left:300px;margin-right:300px;color:white;background-color:black; font-size:25px', 'R- Shiny Web Application to Predict Quality of Wine'))
                    
                  )),
  dashboardSidebar(
    sidebarMenu(id = 'menu1',
      menuItem("About", tabName = "about", icon = icon("dashboard")),
      menuItem("Data Exploration", tabName = "explore", icon = icon("wpexplorer"))
    ),
    # extra options once user clicks the data exploration
    conditionalPanel(condition = "input.menu1== 'explore' ",
                     radioButtons(inputId = 'dfType', label = p('Select Type of Wine Data', style = "color:white;font-family: Times New Roman;font-weight:bold "), 
                                  choices = c('Red', 'White','Combined')),
                     selectInput(inputId = 'features', label = p('Select Predictor Column', style = "color:white;font-family: Times New Roman;font-weight:bold "), 
                     choices = colnames(df)[1:12]),
                     selectInput(inputId = 'graphType', label = p('Select Type of Distribution', style = "color:white;font-family: Times New Roman;font-weight:bold "),
                                 choices = c('Histogram', 'Density Plot', 'Box Plot', 'Count Plot'),
                                 selected = 'Histogram'),
                    # set a shiny alter
                     actionButton(inputId = 'missing', label = 'Check for Missing data')
                     ),
    sidebarMenu(id = 'menu2',
      menuItem("Modeling", tabName = "model", icon = icon("th"))
      ),
    # condtion open in modeling tab
    conditionalPanel(condition = 'input.menu2 == "model"',
                     radioButtons(inputId = 'dfType2', label = 'Select Type of Wine Data', 
                                  choices = c('Red', 'White','Combined'))),
    sidebarMenu(id = 'menu3',
      menuItem("Data", tabName = "data", icon = icon("book"))
    ), 
    conditionalPanel(condition = "input.menu3 == 'data'",
                     radioButtons(inputId = 'wineType', label = 'Select Wine Data', 
                                  choices = c('Red', 'White','Combined')))
    ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "purple_gradient"
    ),
  tags$style(type="text/css", "
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
                box(status = 'success', width = 12, solidHeader = T,
                    textOutput('about')),
      )
      ),
      
      # Second tab content
      tabItem(tabName = "explore",
              fluidRow(
                box(solidHeader = T, width = 12, uiOutput('dataExplore'), uiOutput('Hypothesis')),

                box(title = h4('Corr Plots', style = 'font-family: Times New Roman; text-align:center'), width = 6, 
                    plotOutput("plot3", height = 250),
                    background = 'maroon'),
                box(title = h4('Plot Interpretation', style = 'font-family: Times New Roman; text-align:center'), background = 'light-blue', width = 6, uiOutput('text1'), tableOutput('table1'))
              ),
              fluidRow(
                box(solidHeader = T, background = 'maroon', title = h4('Visualization', style = 'font-family: Times New Roman; text-align:center'), width = 6, plotOutput("plot1", height = 300),
                    conditionalPanel(condition = "input.menu1.graphType == 'Histogram' ",
                                     checkboxInput('fd', label = 'Use Freedman-Diaconis Rule'))),
                #box(title = 'Plot for Missing', status = 'warning', width = 4, plotOutput("plot2", height = 300)),
                box( solidHeader = T, background = 'maroon', title = h4('Correlation Plot', style = 'font-family: Times New Roman; text-align:center'), width = 6,
                     plotOutput("plot4", height = 300)),
              ),
      ),
      
      # third tab
      tabItem(tabName = 'model',
                 fluidRow(
                   tabBox(
                     title = "", id = 'tabset', width = '100%', 
                     tabPanel(title = h4('Modeling Info', style = 'font-family: Times New Roman; background-color:lighblue; font-weight: bold; font-size:3vw'), 
                              fluidRow(
                                box(title = h5('GLM (Generalized Linear Model)', style = 'margin-top: 10px; font-family: Times New Roman;font-weight: bold; text-align: center; font-size:1.5vw' ), width = 12, uiOutput('glm'), status = 'success', solidHeader = T, collapsible = T),
                                box(title = h5('Classification Tree', style = 'margin-top: 10px; font-family: Times New Roman;font-weight: bold; text-align: center; font-size:1.5vw'), width = 12, status = 'success', uiOutput('cart'), solidHeader = T, collapsible = T),
                                box(title = h5('Random Forest', style = 'margin-top: 10px; font-family: Times New Roman;font-weight: bold; text-align: center; font-size:1.5vw'), width = 12, status = 'success', solidHeader = T, collapsible = T, uiOutput('rf'))
                              )
                              ),
                     tabPanel(title = h4('Model Fit', style = 'font-family: Times New Roman; background-color:lighblue; font-weight: bold; font-size:3vw'),
                              fluidRow(
                                box(solidHeader = T, collapsible = T, background = 'orange', width = 4, selectInput(inputId = 'model', label = h5("Select Model", style = "color:black;font-family: Times New Roman;font-weight:bold; text-align:center "),
                                            choices = c('Generalized Model', "Classification Tree", "Random Forest")
                                            )),
                                box(solidHeader = T, collapsible = T, background = 'orange', width = 4, sliderInput(inputId = 'splitratio', label = h5("Select train - test split ratio ", style = "color:black;font-family: Times New Roman;font-weight:bold; text-align:center "),
                                            min = 0, max = 1, value = 0.8, step = 0.05)),
                                box(solidHeader = T, collapsible = T, background = 'orange',width = 2, numericInput(inputId = 'cv', label = h5("Select CV value", style = "color:balck;font-family: Times New Roman;font-weight:bold; text-align:center "), value = 2,
                                                            min = 2, max = 10)),
                                box(solidHeader = T, collapsible = T, background = 'orange', width = 2, checkboxInput(inputId = 'fit', label = h5('Fit Model', 
                                                                                         style = "color:black;font-family: Times New Roman;font-weight:bold; text-align:center "),value = FALSE,
                                                              width = '400px'
                                                                ))
                              ),
                              fluidRow(
                                box(width = 2, solidHeader = T, collapsible = T, background = 'orange',
                                    checkboxGroupInput(inputId = 'variables', inline = FALSE,
                                                       label = p("Select variable to include in the model", style = "color:black;font-family: Times New Roman;font-weight:bold; text-align:justify "),
                                                       choices = colnames(df[, c(1:11,14)]),
                                                       selected = list("high.Quality")),
                                    conditionalPanel(condition = "input.model == 'Classification Tree'",
                                                     checkboxInput(inputId = 'tuneP', 
                                                     label = p('Add cp for Tuning Parametr', style = "color:black;font-family: Times New Roman;font-weight:bold ")),
                                                     value = FALSE),
                                    ),
                                box(solidHeader = T, collapsible = T, background = 'orange',width = 6, 
                                    title = p('Model Summary', style = "color:black;font-family: Times New Roman;font-weight:bold; text-align:center "),
                                    conditionalPanel(condition = "input.tuneP ",
                                                     sliderInput(inputId = 'cp', label = "Select a range for cp",
                                                                 value = c(0.00,0.1),
                                                                 min = 0.0,
                                                                 max = 0.1,
                                                                 step = 0.001)),
                                    conditionalPanel(condition = "input.model == 'Random Forest'",
                                                     uiOutput('mtryParam')),
                                    verbatimTextOutput('summary')),
                                box(solidHeader = T, collapsible = T, background = 'orange',
                                  title = p("Model Evaluation & Diagnostics", style = "color:black;font-family: Times New Roman;font-weight:bold; text-align:justify "),
                                    width = 4,
                                    checkboxInput(inputId = 'diagnostic',
                                                  label = 'Generate Diagnostic Result', 
                                                  value = FALSE),
                                    verbatimTextOutput('diagTest'),
                                    plotOutput('tree')
                                    )
                              ),
                              fluidRow(
                                box(collapsible = T, solidHeader = T, background = 'orange', width = 3, 
                                    title = p("Model Perfromace on Test", style = "color:black;font-family: Times New Roman;font-weight:bold; text-align:justify "),
                                    checkboxInput(inputId = 'test', 
                                                 label = p('Model for Test', style = "color:black;font-family: Times New Roman;font-weight:bold "),
                                                 value = FALSE),
                                    conditionalPanel(condition = "input.model %in% c('Generalized Model', 'Classification Tree', 'Random Forest')",
                                                     checkboxInput(inputId = 'roc',
                                                                   label = p("Generate ROC Curve", style = "color:black;font-family: Times New Roman;font-weight:bold; text-align:justify "),
                                                                   value  = FALSE))),
                                box(collapsible = T, solidHeader = T, background = 'orange', width = 5,
                                    title = p("Confusion Matrix", style = "color:black;font-family: Times New Roman;font-weight:bold; text-align:justify"),
                                    verbatimTextOutput('testDf')),
                                box(collapsible = T, solidHeader = T, background = 'orange', width = 4, 
                                    title = p("AUC (Area under Curve)", style = "color:black;font-family: Times New Roman;font-weight:bold; text-align:justify "), 
                                    plotOutput('testPlot')),
                              ),
                              ),
                     tabPanel(title  = h4('Prediction', style = 'font-family: Times New Roman; background-color:lighblue; font-weight: bold; font-size:3vw'),
                              fluidRow(id = 'predict',
                                  box(title = 'Pred Vaiables from Fit', width = 2,
                                      radioButtons(inputId = 'predVar',
                                                    label = 'Pred Var',
                                                    choices = colnames(df[, c(1:11,13)]),
                                                    selected = "high.Quality"
                                                    )),
                                  # for fixed acidity
                                  box( title = "User Input", width =2,
                                       conditionalPanel(condition = "input.predVar =='density'",
                                                        print("KP")),
                                       
                                       
                                       
                                       
                                       
                                       
                                  # conditionalPanel(condition = 'input.predVar == "ab"',
                                  #                      textInput(inputId = 'fa', 
                                  #                                   label = 'Fixed Acidity',
                                  #                                   value = 0)),
                                  # #for volatile acidity
                                  # conditionalPanel(condition = 'input.predVar',
                                  #                       textInput(inputId = 'va', 
                                  #                                    label = 'Volatile Acidity',
                                  #                                    value = 0)),
                                  # conditionalPanel(condition = 'input.predVar == "citric.acid"',
                                  #                  textInput(inputId = 'ca', 
                                  #                               label = 'Citric Acid',
                                  #                               value = 0)),
                                  # conditionalPanel(condition = 'input.predVar == "residual.sugar"',
                                  #                  numericInput(inputId = 'rs', 
                                  #                               label = 'Residual Sugar',
                                  #                               value = 0)),
                                  # conditionalPanel(condition = 'input.predVar == "chlorides"',
                                  #                  numericInput(inputId = 'ch', 
                                  #                               label = 'Chloride',
                                  #                               value = 0)),
                                  # conditionalPanel(condition = 'input.predVar == "free.sulfur.dioxide"',
                                  #                  numericInput(inputId = 'so2', 
                                  #                               label = 'Sulfur Di-Oxide',
                                  #                               value = 0)),
                                  # conditionalPanel(condition = 'input.predVar == "total.sulfur.dioxide"',
                                  #                  numericInput(inputId = 'tso2', 
                                  #                               label = 'Total Sulfur Di-Oxide',
                                  #                               value = 0)),
                                  # conditionalPanel(condition = 'input.predVar == "density"',
                                  #                  numericInput(inputId = 'rho', 
                                  #                               label = 'Density',
                                  #                               value = 0)),
                                  # conditionalPanel(condition = 'input.predVar == "pH"',
                                  #                  numericInput(inputId = 'pH', 
                                  #                               label = 'pH',
                                  #                               value = 0)),
                                  # conditionalPanel(condition = 'input.predVar == "sulphates"',
                                  #                  numericInput(inputId = 'sulphate', 
                                  #                               label = 'Sulphate',
                                  #                               value = 0)),
                                  # conditionalPanel(condition = 'input.predVar == "alcohol"',
                                  #                  numericInput(inputId = 'alcohol', 
                                  #                               label = 'Alcohol',
                                  #                               value = 0)),
                                  # 
                                  actionButton("submit", "Submit")
                                  ),
                                  box(title = 'User Input Value', width = 8, uiOutput("fixed.acidity")
                                      )

                                  
                              )
                      )
                   )
                 )
      ),
      
      # fourth tab content
      tabItem(tabName = 'data', 
              h4('Dataset', icon("database",lib = "font-awesome"),style="font-family:Times New Roman;color:black;text-align:center; font-size:1.5vw",
                 fluidRow(
                          column(DT::dataTableOutput("WineData"),
                                 width = 12)),
                 hr(),
                 p(em("Made by"),br("KayP"),br("ST558 Project"),style="text-align:center; font-family: times")
                 )
      )
    )
  )
)