
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DataExplorer)
source('explore.r')
library(plotly)
library(dplyr)
library(viridis)

function(input, output, session) {

  output$plot1 <- renderPlot({
    if (input$graphType == "Density Plot"){
      ggplot(df, aes_string(x = input$features))  + 
        geom_density(aes(fill = factor(quality)), alpha = 0.6) +
        labs(title = "Density Plot",
             subtitle = paste0(input$features, " grouped by Quality Rating "),
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
                       #binwidth = input$slider,
                       col = 'black') +
        labs(title = "Density Plot",
             subtitle = paste0(input$features, " grouped by Quality Rating "),
             cation = "Source: Wine Quality Data",
             x = paste0(input$features),
             fill = "Quality Rating")+
        theme(plot.title = element_text(size = 30, color = "black"),
              plot.subtitle = element_text(size = 20, color = "black")
        )
      }
      else if (input$fd){
        #var = noquote(paste("df",noquote(input$features), sep = "$"))
        binWidth = 2*IQR(dplyr::select(df, input$features))/(length(dplyr::select(df, input$features))^(1/3))
        observe(IQR(dplyr::select(df, input$features)))
        ggplot(df, aes_string(x = input$features))  + scale_fill_brewer(palette = "Spectral") +
        geom_histogram(aes(fill = factor(quality)),
                         #binwidth = binWidth,
                         col = 'black') +
          labs(title = "Density Plot",
               subtitle = paste0(input$features, " grouped by Quality Rating "),
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
             subtitle=paste0(input$features, " grouped by Quality Rating "),
             caption="Source: Wine Quality Data",
             x= "Wine Quality",
             y= paste0(input$features)) +
            theme(plot.title = element_text(size = 30, color = "black"),
              plot.subtitle = element_text(size = 20, color = "black")
              )
    }
    else if(input$graphType == "Count Plot"){
      g <- ggplot(df, aes(quality)) + 
        geom_bar(stat = 'count', fill = viridis(6))
      plot(g)
    }

  })
  output$plot2 <- renderPlot({
    if (input$missing){
      plot_missing(df)
    }
  })
  
  getDf <- reactive({
    t <- input$power
    newData <- df %>%dplyr::select(input$features, quality)
    newData$power <- newData[, input$features]^(1/t)
    finalDf <- as.data.frame(newData)
  })
  
  getCorr <- reactive({
    data <- getDf()
    corr <- round(cor(data$power, data$quality),2)
  })
  
  output$plot3 <- renderPlot({
    data <- getDf()
    corr <- getCorr()
    print(corr)
    g <- ggplot(data, aes(x = power, y = quality))
    g + geom_point(size = 2) +
      geom_smooth(method = 'lm', color = 'red') + 
      geom_text(x = max(data$power) - 0.5, y = 8 ,
                label = paste0(corr), color = 'red', size = 8)
  })
  
  output$text <- renderText({
    corr <- getCorr()
    paste0("Correlation Coefficient : ", corr )
  })
  
}
