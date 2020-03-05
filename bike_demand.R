library(tidyverse)
library(lubridate)
library(stringr)
library(caret)
library(readr)
library(gridExtra)
library(xgboost)
library(Metrics)

training_set <-read.csv("train.csv")
test_set <- read.csv("test.csv")

str(training_set)

str(test_set)




