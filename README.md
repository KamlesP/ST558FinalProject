# R-Shiny App: Wine Quality Prediction  

## Introduction  


## Packages Required  
Following packages are used for the R shiny web application, to download required libraries pleas use following R code.  
 
```{r}
packages <- c("shiny", 
            "shinydashboard", 
            "dplyr", "shinyalert",
            "mathjaxr", "shinythemes", "dashboardthemes", "shinyWidgets",
            "ggplot2", "DataExplorer", "viridis",
            "PerformanceAnalytics", "caret", "rpart.plo", 
            "pROC", "rattle" )
            
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)

```

## To Access my R Shiny Application  
Please use following code to access my R shiny web application!!
```{r}
library('shiny')

runGitHub("ST558FinalProject", "KamlesP", ref = 'main', subdir = "WineQualityPrediction")

```