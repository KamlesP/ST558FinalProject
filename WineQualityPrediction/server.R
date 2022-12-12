
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
library(pROC)
library(dashboardthemes)

function(input, output, session) {
  
  
  # Data Exploration 
  output$dataExplore <- renderUI({
    text1 <- "The primary objective of Null Hypothesis testing is to estimate p-value, 
    i.e probability of obtaining the observed results, or something more extreme if the
    null hypothesis is true. If the results are unlikely under the null hypothesis, then 
    you will reject the Null Hypothesis"
    h4(text1, style = 'font-family: Times New Roman; text-align:justify')
  })
  output$Hypothesis <-  renderUI({
    text2 <- "H0: Predictor variable has zero slope coefficient"
    text3 <- "Ha: Slope coeffiecient is non zero."
    
    h4(text2, br(),  text3, style = 'font-family: Times New Roman; text-align:center; background-color:rgba(0, 128, 0, 0.3); outline-style: double; font-style: italic' )
  })
  
  calBin <- reactive({
    Ft <- input$features
    binWidth = 2*IQR(dplyr::select(df, Ft))/(length(dplyr::select(df, Ft))^(1/3))
  })
  
  # shinyAlter
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
        labs(title = "Histogram Plot",
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
    # calculate p-value for the plot
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
    h4(text, style = 'font-family: Times New Roman; text-align:justify')
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
  
  # Modeling Info###########################################################################
  output$glm <- renderUI({
    con <- "The term 'gernarlized' linear model refers to a large class of models populaized by
                McCullagh and Nelder. In these models, the  response variable assumed to follow a exponential 
                family distribution. There are three main component in any GLM model:" 
    
    con1 <- "Random Component: It specifies the probability distribution of the response variable; i.e.
                  normal distribution for Y in classical linear regression model, or binomial distribution
                  for Y in binary logistic regresssion."
    con2 <- "Systematic Component: Specifies the linear combiantion among the predictor variables, similar
              to the linear combination "
    m <- withMathJax('$$\\beta0 + \\beta1*x1 + \\beta2*x2....$$')
    con3 <- "Link Function: It specifies a link between the random and systematic components. It expalins how
              the expected value of response is related to the linear combination of predictor variables"
    m2 <- withMathJax('$$\\mu = \\log(\\pi\frac{1-pi}$$')
    
    con4 <- p('Advantages of GLM over traditional OLS model: ', style = 'font-weight:bold')
    con5 <- p("* Firstly, no need to transform the response variable to have a normal distribution", 
              br(),
              br(),
              "* It provides much flexibility by providing an option to select a logit fucntion", br(),br(),
              "* All interference tools i.e. Wald and Likelihood ratios are applicable in generalized linear models too.", br(),br(),
              "* glm() from the base R package can be used to capture all GLM models")
    h4(con, br(), br() ,con1, br(), br(), con2, m,  br(), con3, m2, con4,con5,  style = 'font-family: Times New Roman; text-align:justify' )
  })
  
  output$cart <- renderUI({
    con1 <- "When the relationship between the predictor and response variable is linear then
              methods like multiple logistic regression can produce accurate results.
              However, if non linearity present in the model then these linear models are not accurate.
              To perform well in case of any non-linearity presnet in the model we oftern use classification
              and regression tree, often abbreviated as CART."
    img = tags$img(src = 'cart.png', style =  'display: block;
                                             margin-left: auto;
                                             margin-right: auto;
                                             width: 20%' )
    con2 <- p('The way to interpret the tree is as follows:', style = "font-weight:bold")
    con3 <- p("* player with years played less than 4.5 Years have a predicted salary of $225.8K",
              br(),
              br(),
              "* Player with years played more tahn 4.5 years and less than 16.5 average home run have
              a predicted slary of $557.6K",
              br(), 
              br(), 
              "* Player with years played more tahn 4.5 years and greater than 16.5 average home run have
              a predicted slary of $975.6K ")
    
    h4(con1, img, con2, con3,  style = 'font-family: Times New Roman; text-align:justify' )
  })
  
  output$rf <- renderUI({
    con1 <- p("Random forest are commonly used machine learning algorithm that combines the output of multiple
              decision trees to reach a single result. Random forest algorithm handels both regresssion and 
              classification problem", 
              br(), br(),
              "Since Random forest is made up of several decision trees , it is imporatnt to describe
              decision tree algorithm. Decision trees starts with a question and each question helps
              an individuals to arrive at a final decision which will be denoted by the leaf node.",
              br(),
              "Decision trees seek to find the best split to the data via CART algorith and metrics 
              such as Gini Impurity, information gain or MSE can be used to evaluate the quality 
              of the split.", br(), br(),
              " The random forest is the extension of bagging method as it utilizes both 
              bagging and feature randomness to create an forest of decision tress.
              The key differenc between a random forest and decision trees. While a decision tree cosniders 
              all possible splits, random forests only selects a subset of those split"
              )
    con4 <- p("For a classification task, a majority vote i.e. the most frequent categorical
              variable -  will yield the predicted class.", style = 'text-align:center; color: coral')
    img = tags$img(src = 'rf.png', style =  'display: block;
                                             margin-left: auto;
                                             margin-right: auto;
                                             width: 20%' )
    con2 <- p("Advantages & Challenges of Random Forest", style = 'font-weight:bold;')
    con3 <- p("* Reduces risk of Overfitting: Decision trees run the risk of overfitting as they tend to tightly fit all the 
                samples within training data",
              br(), br(),
              "* Provide Flexibility: Since random forests can handle both regressiona and 
              classsification problems with a very high accuracy, they are very popular among the 
              data scienctist.", br(), br(),
              "* Easy to determine feature Importance: Random Forest make it easy to
              evaluate variable importance. Gini importance and mean decrease in impurity (MDI) are usually used to measure how much 
              the model’s accuracy decreases when a given variable is excluded", br(), br(),
              
              "Key Challenges:", br(),
              "* Time Consuming: As Random forest has to compute data for individual decision tree they are
              expensive process in terms of the time and
              computational power. ",
              br(), br(),
              "*More Complex: The prediction of a single decision tree is easier to interpret when compared to a forest of them."
              )
    
    h4(con1, con4, img, con2, con3, style = 'font-family: Times New Roman;')
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
  
  # model performance on test set
  # predict response
  predict <- reactive({
    set.seed(42)
    # get the current data frame
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
    # get the test df
    testDf <- as.data.frame(df[-index,])
  })
  
  output$testDf <- renderPrint(
    if(input$model %in% c('Generalized Model', 'Classification Tree', 'Random Forest') && input$fit && input$test){
      model <- model()
      testDf <- predict()
      testDf$high.Quality <- as.factor(testDf$high.Quality)
      prediction <- stats::predict(model, newdata = testDf)
      prediction <- ifelse(prediction>0.5,1,0)
      cm <- confusionMatrix(testDf$high.Quality, as.factor(prediction))
      print(cm)
    },
     width = 10000
  )
  
  # plot on the test data set
  output$testPlot <- renderPlot({
    model <- model()
    testDf <- predict()
    testDf$high.Quality <- as.factor(testDf$high.Quality)
    prediction <- stats::predict(model, newdata = testDf)
    prediction <- ifelse(prediction>0.5,1,0)
    # plot for GLM
    if(input$model %in% c('Generalized Model', 'Classification Tree', 'Random Forest') && input$roc){
      #plotROC::plot_interactive_roc(testDf$high.Quality, prediction)
      rocObj <- pROC::roc(testDf$high.Quality, prediction)
      auc <- round(auc(testDf$high.Quality, prediction),4)
      # create a roc plot
      ggroc(rocObj, color = 'steelblue', size = 2) +
        ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))
    }
  })
  
  
  # Model Prediction
  
  predVariable <- reactive(
    vars <- input$variables
  )
  
  output$note <- renderUI({
    text <- "Model is developed based on the following predictor variables in 'Fit Model'
              page. You will have to select same predictor variables to give a value"
  })
  output$selectedvar <- renderPrint({
    vars <- predVariable()
    print(vars)
  })
  
  #get all selected input from fit model tab
  # observe({
  #   #selected variables from fit
  #   selectedVar <- input$variables
  #   updatePickerInput(session,
  #                            inputId = 'predVar',
  #                            label = 'Selected Var',
  #                            choices = selectedVar,
  #                            selected = 'high.Qulaity'
  #                            )
  #   aa <- input$predVar
  #   if('fixed.acidity' %in% aa){
  #     updateNumericInput(session,
  #                        inputId = 'predVar',
  #                        label = "Fixed Acidity",
  #                        value = 0)
  #   } else if('volatile.acidity' %in% aa){
  #     updateNumericInput(session, 
  #                        label = "Volatile Acidity",
  #                        inputId = 'predVar',
  #                        value = 0)
  #     
  #   }
  #   
  #   
  # 
  # 
  # })
  
  # # generate a df from the selected input
  # fixed.acidity <- renderUI({
  #   #val <- input$fa
  # })

  # userInput <- reactive({
  #   cols <- c(input$variables)
  #   cols <- cols[-length(cols)]
  #   val <- c()
  #   # m <- matrix(data = 0, nrow = 1,  ncol = length(cols))
  #   # colnames(m) <- cols
  #   # df <- as.data.frame(m)
  #   if(input$varPred == ('fixed.acidity')){
  #     fixed.acidity = input$fa
  #     append(val, fixed.acidity)
  #   } 
  #   if(input$varPred == 'voaltile.acidity'){
  #     volatile.acidity = input$va
  #     append(val, volatile.acidity)
  #   }
  #     val
  # 
  # })
  # 
  # output$text33 <- renderPrint(
  #   print(userInput()),
  #   width = 10000
  # )
  
  
  

  
  
  
  
  
  
  # Data set Page
  getRawData <- reactive({
    # filter data based on wine radio button
    if (input$wineType == 'Red'){
      df <- df %>% dplyr::filter(Type == 'red')
    } else if(input$wineType == 'White') {
      df <- df %>% dplyr::filter(Type == 'white')
    } else {
      df <- df
    }
  })
  
  output$WineData <- DT::renderDataTable({
    df <- getRawData()
    DT::datatable(df,
    options = list(paging = T,
                   pageLength = 15,
                   scrollX = T,
                   scrollY = T,
                   autoWidth = T,
                   dom = 'Bfrtip',
                   style = 'bootstrap',
                   buttons = c('csv', 'excel'),
                   columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                     list(targets = c(0, 8, 9), visible = FALSE))
                   
                   ),
    extensions = 'Buttons',
    selection = 'single', ## enable selection of a single row
    filter = 'top',              ## include column filters at the bottom
    rownames = FALSE                ## don't show row numbers/names
    )
  })

  # About Page
  output$about <- renderUI({
    Intro <- p('Vinho Verede refers to Portuguese wine that originated
               in the historic Minho province. The wine names means
               "green wine " but translates as a "young wine" as 
               they are released after three to six months grapes
               harvested. Majority of wines classified as Vinho
               Verde are white , but the region is also popular for 
               producing red and rose wine', br())
    title <- p('Vinho Verde Location, Portugal', style = 'text-align:center; color:coral')
    img = tags$img(src = 'location.png', style =  'display: block;
                                             margin-left: auto;
                                             margin-right: auto;
                                             width: 20%' )
    main <- p("In this project I am trying to build a predictive model to
              estimate the quality of the wine. The estimated quality 
              will have a score either '0' or '1'. The predicted quality
              1 will have the quality index greater than 5 and 0 will have less 
              than 5", br())
    head1 <-  p("Data Exploration ", style = 'font-weight:bold; color:black; text-align:center')
    explore <- ("In Data exploration panel you can find all related visualization i.e box plot, histogram and 
                density plot. Along with the plot we can also estimate for a significance level of 0.05
                whetehr the predictor variable is significant or not. All options can be explored using a 
                side panel which will be visible once you click the 'Data Exploration' tab.")
    head2 <-  p("Data Modeling ", style = 'font-weight:bold; color:black; text-align:center')
    data <- p("Data Modeling tab is broadly divided into three tab panel, the modeling info, model fit and
              prediction tab. The modeling info tab describes some basic concepts regrarding difernt models
              that I have used to built this shiny application, i.e. classification tree, generalized linear model, and
              random forest.", br(),br(), 
              "Model fit is brain of the application. First it has flexibility to choose the desired 
              model a user want to run. It also has an option to select the desired test-train split ratio
              for training and testing of the model.", br(),
              "Based on the selected model, the application will generate option to tune the parameters. User can 
              also add or remove any predictor parameter from the model and the model will update automatically. 
              Ther user can also check how model performing in the training dataset based on the diagnostic generated in model fit tab", br(), br(),
              "Once you are satidfied with the model performance you can also check model's performance on the test data. 
              You can also generate a confusion matric and AUC curve based on the test diagnostics."
              )
    head3 <-  p("Model Prediction ", style = 'font-weight:bold; color:black; text-align:center')
    prediction <- p("In model predition tab user has option to select the predictor variables used for model building and simultaneiuly user
                    can insert numerical values to predict.")
    
    head4 <-  p("Data ", style = 'font-weight:bold; color:black; text-align:center')
    df <- p("In data table we can see dataframe, filter rows and can download it too.")
    h4(Intro, title,  img,  br(), br(), main, head1, explore, br(), br(), head2, data, head3, prediction, head4, df,   style = 'font-family:Times New Roman; text-align:justify')
  })
}
