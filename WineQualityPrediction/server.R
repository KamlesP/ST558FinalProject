
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
  
  output$plot1 <- renderPlot({
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
  output$plot2 <- renderPlot({
    if (input$missing){
      plot_missing(df)
    }
  })
  
  # Data frame to get the main df for further calculations
  getDf <- reactive({
    newData <- df %>%dplyr::select(input$features, high.Quality)
    # add a generic name for the predictor column
    newData$featureCol <- newData[,1]
    finalDf <- as.data.frame(newData %>% select(high.Quality, featureCol))
  })
  
  getCorr <- reactive({
    data <- getDf()
    corr <- round(cor(data$power, data$quality),2)
  })
  
  output$plot3 <- renderPlot({
    data <- getDf()
    g <- ggplot(data, aes(x = featureCol, y = high.Quality))
    g + geom_jitter( size = 2, color = 'blue', alpha = 0.5) + 
       geom_smooth(method = 'glm', 
                     color = 'red', method.args = list(family = 'binomial'), se = TRUE)
  })
  
  output$plot4 <- renderPlot({
    # generate correlation plot
    chart.Correlation(df%>%dplyr::select(everything(), -Type, - high.Quality),
                      method = 'pearson',
                      histogram = TRUE,
                      pc = 16)
    
  })

  
}
