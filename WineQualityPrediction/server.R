
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DataExplorer)
source('explore.r')
library(plotly)
library(dplyr)
library(viridis)
library(PerformanceAnalytics)


function(input, output, session) {
  
  calBin <- reactive({
    Ft <- input$features
    binWidth = 2*IQR(dplyr::select(df, Ft))/(length(dplyr::select(df, Ft))^(1/3))
  })
  
  # shinyAlter####################
  observeEvent(input$missing, {
    showModal(modalDialog(
      title = "Info!!",
      uiOutput('text2'),
      easyClose = TRUE,
      footer = NULL
    ))
  })  
  output$plot1 <- renderPlot({
    # filter data based on wine radio button
    if (input$dfType == 'Red'){
      df <- df %>% dplyr::filter(Type == 'red')
    } else if(input$dfType == 'White') {
      df <- df %>% dplyr::filter(Type == 'white')
    } else {
      df <- df
    }
    if (input$graphType == "Density Plot"){
      ggplot(df, aes_string(x = input$features))  + 
        geom_density(aes(fill = factor(quality)), alpha = 0.6) +
        labs(title = "Density Plot",
             subtitle = paste0(str_to_sentence(input$features), " grouped by Quality Rating "),
             cation = "Source: Wine Quality Data",
             x = paste0(input$features),
             fill = "Quality Rating")+
        theme(plot.title = element_text(size = 30, color = "black"),
              plot.subtitle = element_text(size = 20, color = "black")
        )
    }
    else if (input$graphType == "Histogram"){
      if (input$fd == FALSE){
      ggplot(df, aes_string(x = input$features))  + scale_fill_brewer(palette = "Spectral") +
        geom_histogram(aes(fill = factor(quality)),
                       #binwidth = 0.1,
                       col = 'black') +
        labs(title = "Density Plot",
             subtitle = paste0(str_to_sentence(input$features), " grouped by Quality Rating "),
             cation = "Source: Wine Quality Data",
             x = paste0(input$features),
             fill = "Quality Rating")+
        theme(plot.title = element_text(size = 30, color = "black"),
              plot.subtitle = element_text(size = 20, color = "black")
        )
      }
      else if (input$fd){
        binWidth = calBin()
        print(unlist(binWidth))
        ggplot(df, aes_string(x = input$features))  + scale_fill_brewer(palette = "Spectral") +
        geom_histogram(aes(fill = factor(quality)),
                         binwidth = binWidth,
                         col = 'black') +
          labs(title = "Density Plot",
               subtitle = paste0(str_to_sentence(input$features), " grouped by Quality Rating "),
               cation = "Source: Wine Quality Data", color = 'red',
               x = paste0(input$features),
               fill = "Quality Rating")+
          theme(plot.title = element_text(size = 30, color = "black"),
                plot.subtitle = element_text(size = 20, color = "black")
          )
      }
    }
    else if (input$graphType == "Box Plot"){
      g <- ggplot(df, aes(x = factor(quality)))
      g + geom_boxplot( aes_string(y = input$features), varwidth=T, fill="plum") + 
        labs(title="Box plot", 
             subtitle=paste0(str_to_sentence(input$features), " grouped by Quality Rating "),
             caption="Source: Wine Quality Data",
             x= "Wine Quality",
             y= paste0(input$features)) +
            theme(plot.title = element_text(size = 30, color = "black"),
              plot.subtitle = element_text(size = 20, color = "black")
              )
    }
    else if(input$graphType == "Count Plot"){
      # removing type available from the dataset
      df <- dplyr::select(df,)
      g <- ggplot(df, aes(x = quality)) + 
        geom_bar(stat = 'count', fill = viridis(6)) + 
        labs(x = 'Quality')
      plot(g)
    }

  })


  # Data frame to get the main df for further calculations
  getDf <- reactive({
    # filter data based on wine radio button
    if (input$dfType == 'Red'){
      df <- df %>% dplyr::filter(Type == 'red')
    } else if(input$dfType == 'White') {
      df <- df %>% dplyr::filter(Type == 'white')
    } else {
      df <- df
    }
    newData <- df %>%dplyr::select(input$features, high.Quality)
    # add a generic name for the predictor column
    newData$featureCol <- newData[,1]
    finalDf <- as.data.frame(newData %>% select(high.Quality, featureCol))
  })
  # Del
  # getCorr <- reactive({
  #   data <- getDf()
  #   corr <- round(cor(data$power, data$quality),2)
  # })
  # 
  getPval <- reactive({
    # calcualte p-value for the plot
    getData = getDf()
    mod <- glm(high.Quality ~ featureCol, data = getData)
    modSum <- summary(mod)
    pVal <- format(modSum$coefficients, format = "e", digits = 1)
  })
  
  output$plot3 <- renderPlot({
    data <- getDf()
    g <- ggplot(data, aes(x = featureCol, y = high.Quality))
    g + geom_jitter( size = 2, color = 'blue', alpha = 0.3) + 
      stat_smooth(method = 'glm', 
                     color = 'red', method.args = list(family = binomial),lty = 2, se = TRUE) + 
      labs(x = paste0(input$features),
           y = paste0("Probability to High Quality (Quality>5)"))
  })
  
  output$table1 <- renderTable({
    table <- getPval()
  })
  
  output$text1 <- renderUI({
    table <- getPval()
    print(table)
    pred <- input$features
    if (as.numeric(table[2,4]) < 0.05){
      text <- paste0("To establish the inference from the predictor variable it is important to check 
                     p-value. First row in table belongs to the intercept and second row belongs to the 
                     predictor's coefficient after fitting a 'glm' model. For a significance level of 0.05,
                     we can estimate that ", pred ," is significant")
    } else {
      text <- paste0("To establish the inference from the predictor variable it is important to check 
                     p-value. First row in table belongs to the intercept and second row belongs to the 
                     predictor's coefficient after fitting a 'glm' model. For a significance level of 0.05,
                     we can estimate that ", pred ," is not significant")
    }
    h4(text)
  })
  
  output$text2 <- renderUI({
    data <- getDf()
    missVal <- sum(is.na(data))
    if (missVal == 0){
      text <- paste0("There is no missing value in the data frame")
    } else {
      text <- paste0("Total missing observations in the data frame ", missVal)
    }
    
    h4(text, style = 'color:red' )
  })
  
  output$plot4 <- renderPlot({
    # generate correlation plot
    chart.Correlation(df%>%dplyr::select(everything(), -Type, - high.Quality),
                      method = 'pearson',
                      histogram = TRUE,
                      pc = 16)
    
  })

  
}
