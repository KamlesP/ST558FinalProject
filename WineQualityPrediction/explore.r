library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(caret)
library(glmnet)

df <- read.csv('winequality-red.csv', sep = ";")

g <- ggplot(df, aes(quality)) + 
  geom_bar(stat = 'count')
plot(g)