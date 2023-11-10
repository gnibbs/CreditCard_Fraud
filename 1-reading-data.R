####################################################################################################
###################################### Reading Data ################################################

# Load the necessary library
library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(corrplot)
library(tidymodels)
library(parsnip)


wd <- '.\\creditcard_fraud\\data\\'

# Read the dataset
credit_fraud.df <- read_csv(paste0(wd, 'creditcard_fraud.csv'))

# Selecting original columns.
credit_fraud.df <- 
  credit_fraud.df %>%
  select(TRANS_ID, DATETIME, CUSTOMER_ID, TERMINAL_ID, TRANS_AMOUNT, TRANS_FRAUD)

# Convert DATETIME from character to POSIXct.
credit_fraud.df <- credit_fraud.df %>%
  mutate(DATETIME = ymd_hms(DATETIME))