library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(caret)
library(glmnet)

dfRed <- read.csv('winequality-red.csv', sep = ";")
dfWhite <- read.csv('winequality-white.csv', sep = ";")

dfRed$Type <- 'red'
dfWhite$Type <- 'white'

# final combined data frame
df <- rbind(dfRed, dfWhite)
df$high.Quality <- ifelse(df$quality>5,1,0)

# convert quality into a two-level factor
df$high.Quality <- as.factor(df$high.Quality)

