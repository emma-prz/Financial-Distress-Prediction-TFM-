# MAIN FILE TO RUN THE PROJECT#
# The whole code is divided in 4 parts
# Scripts are numerated in order of execution
# Code for saving/loading subdatasets is commented in order to run the full analysis at once

options(scipen = 999)

# Fix the working directory to the code folder
#setwd(file.path(dirname(getwd()), "Codigo"))
#Run the scripts in order
library(readr)
library(pROC)
library(tidyverse)
library(openxlsx)
library(cowplot)
library(MatchIt)
library(cobalt)
library(caret)
library(mice)
library(DataExplorer)
library(lubridate)
library(corrplot)
library(xgboost)
library(glmnet)
library(randomForest)
library(fastDummies)
library(Ckmeans.1d.dp)
library(ggthemes)
library(writexl)
library(ggplot2)
library(patchwork)

# Part 1: Generating subsamples ----
# Creating main dataset 
# source("join cbi.R") Not necessary, the resulting data is provided

# Closing firms subsample
source("2.closing firms subsample.R")

# Non-closing firms subsample
source("3.non-closing firms subsample.R")

# Generate summary statistics to check samples composition
source("summary stats.R")

# Part 2: Distress analysis and definition ----

# Perform distress analysis in the two subsamples
source("4.distress analysis_subsamples.R")

# Create the combined plot and save it
source("4.1.combined plots.R")

# Additional checks for distress measures with different ratios
source("4.2.distress other ratios.R")


# Generate a full sample data with the distress variables included
source("5.full sample.R")

# Perform the distress analysis in the full data
source("6.distress analysis_fullsample.R")

# Part 3: Matching and imputation ----

# Matching - creating models sample
source("7.matching.R")

# Missing data imputation in models sample
source("8.imputation.R")

# Part 4: Models training ----

# Random Forest
source("9. random forest.R")

# XGB
source("10. xgb.R")

# Penalized logistic regression and performance metrics
source("11. lasso logistic.R")
