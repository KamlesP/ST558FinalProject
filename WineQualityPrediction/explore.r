library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(caret)
library(glmnet)

df <- read.csv('winequality-red.csv', sep = ";")
g <- ggplot(df, aes(x = fixed.acidity)) + scale_fill_brewer(palette = "Spectral")
g + geom_histogram( binwidth = 1, aes(fill= factor(quality)))

g <- ggplot(df, aes(x= fixed.acidity))
g + geom_density(aes(fill = factor(quality)))
