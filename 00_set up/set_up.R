library(rvest)
library(jsonlite)
library(httr)
library(stringr)
library(dplyr)

# Define the base URL for the Guardian API content endpoint
url <- "https://content.guardianapis.com/search"

# Setup query parameters
api_key_data <- read.csv("api_key.csv", header = FALSE)
api_key <- api_key_data$V1[1]


