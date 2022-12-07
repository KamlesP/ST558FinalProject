
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DataExplorer)
source('explore.r')
library(plotly)
library(dplyr)
library(viridis)
library(PerformanceAnalytics)
library(mathjaxr)
library(caret)
library('rpart.plot')
library(rattle)

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
      #df <- dplyr::select(df,-Type)
      g <- ggplot(df, aes(x = quality)) + 
        geom_bar(stat = 'count', aes(fill = factor(quality))) + 
        labs(x = 'Quality',
             y = 'Count',
             fill = 'Quality')
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
  output$LR <- renderUI({
    content <- "Logistic regression models are extension of linear regression model
    that maps classification probelem with two possible outcomes. A linear regression model
    does not output probabilities so it cant be used for a classification problem i.e dog vs cat type 
    classification problem" 
    content1 <- "A potential solution for such problems is logistic regression where insted of 
    fitting a straight line or a hyperplane, we uses a fuction to squeeze the output of a 
    lienear equation between 0 and 1 "
    h4(content, br(), content1)
  })
  
  # mathematical equations
  output$eq1 <- renderUI({
    withMathJax('$$logistic(z) = \\frac{1}{1 + e^{-z}}$$')
  })
  
  # Model Fit
  # split the data set
  splitDf <- reactive({
    set.seed(42)
    # nee to add this becoz each df has different obs, else it will count the 
    # combined df as main one
    if (input$dfType2 == 'Red'){
      df <- df %>% dplyr::filter(Type == 'red')
    } else if(input$dfType2 == 'White') {
      df <- df %>% dplyr::filter(Type == 'white')
    } else {
      df <- df
    }
    index <- sample(dim(df)[1], size = dim(df)[1]*input$splitratio, replace = FALSE)
  })
  
  # MAIN MODEL ####################################################################
  model <- reactive({
    set.seed(42)
    # get the currect data frame
    if (input$dfType2 == 'Red'){
      df <- df %>% dplyr::filter(Type == 'red')
    } else if(input$dfType2 == 'White') {
      df <- df %>% dplyr::filter(Type == 'white')
    } else {
      df <- df
    }
    index <- splitDf()
    cols <- c(input$variables)
    df <- df%>%select(all_of(cols))
    # get the train df
    trainDf <- as.data.frame(df[index,])
    x <- trainDf %>% select(everything(), -high.Quality)
    y <- as.factor(trainDf$high.Quality)
    if (input$model == 'Generalized Model' && input$fit == TRUE){
      # control parameter
      trainControl <- trainControl(method = "cv",
                                number = input$cv,
                                savePredictions = "all",
                                )
      # model building
      set.seed(42)
      model <- caret::train(high.Quality ~ . , data = trainDf,
                        method = 'glm',
                        trControl = trainControl,
                        family = 'binomial',
                        preProcess = c("center", "scale")
                        )
      model
    }else if(input$model == 'Classification Tree' && input$fit == TRUE){
      trainControl <- trainControl(method = 'repeatedcv', 
                                  number = input$cv,
                                  repeats = 3)
      if (input$tuneP == FALSE) {
      model <- train(high.Quality ~ . , data = trainDf,
                     method = 'rpart', 
                     trControl = trainControl,
                     preProcess = c('center', 'scale'))
      }else if (input$tuneP){
        model <- train(high.Quality ~ . , data = trainDf,
                       method = 'rpart', 
                       trControl = trainControl,
                       preProcess = c('center', 'scale'),
                       tuneGrid = data.frame(cp = seq(input$cp[1], input$cp[2], 0.001)))
     }
    }else if(input$model == 'Random Forest' && input$fit == TRUE){
      trainControl <- trainControl(method = 'repeatedcv',
                                   number = input$cv,
                                   repeats = 3)
      model <- train(high.Quality ~ ., data = trainDf,
                     method = 'rf',
                     trControl = trainControl,
                     preProcess = c('center', 'scale'),
                     tuneGrid = data.frame(mtry = 1:(dim(trainDf)[2]-1))
                     )
    }else {
      
    }
  })

  # to show in app the model summary
  output$summary <- renderPrint(
    if (input$model == 'Generalized Model' && input$fit == TRUE){
      print(summary(model())) 
    } else if (input$model == 'Classification Tree' && input$fit == TRUE){
      print(model())
    }else if(input$model == 'Random Forest' && input$fit == TRUE){
      model <- model()
      print(model)
    },
    width = 10000
  )
  
  # model Diagnostics
  diagnostic <- reactive({
    if (input$model == 'Generalized Model' && input$fit == TRUE){
      model <- model()
      model$results[2:5]
    }else if (input$model == 'Classification Tree' && input$fit == TRUE){
      model <- model()
      min <- model$results[which.max(model$results[, "Accuracy"]), 'cp']
    } else if(input$model == 'Random Forest' && input$fit == TRUE){
      rfModel <- model()
      varImp(rfModel)
    }
  })
  

  
  #Diagnostic 
  output$diagTest <- renderPrint(
    if (input$model == 'Generalized Model' && input$diagnostic == TRUE){
      print(diagnostic())
    }else if(input$model == 'Random Forest' && input$diagnostic == TRUE){
      print(diagnostic())
    },
    width = 10000
  )
  
  # for classification tree
  output$tree <- renderPlot({
    if (input$model == 'Classification Tree' && input$diagnostic == TRUE){
      t <- model()
      rattle::fancyRpartPlot(t$finalModel, caption = paste0('Wine Data: ', input$dfType2))
    } else if (input$model == 'Random Forest' && input$diagnostic == TRUE){
      plot(model())
    }
  })
  
  # short note for mtry
  output$mtryParam <- renderText({
    text <- p("[INFO!!!] Model is using 'mtry' as a tuneGrid parameter. 
              The value will range from 1 to number of predictor variables
              selected", style = 'color:blue')
    paste0(text)
  })

  # Model Prediction

  
}
