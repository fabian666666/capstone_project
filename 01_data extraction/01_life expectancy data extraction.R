
library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(countrycode)

# Get the list with all the variables that are available (indicators)
response <- httr::GET(
  url = 'https://ghoapi.azureedge.net/api/Indicator',
  verbose()
)

# Make a data frame of the list
cnt <- content(response, as = "parsed")
dat <- tibble(
  code = map_chr(cnt$value, 1), 
  name = map_chr(cnt$value, 2), 
  language = map_chr(cnt$value, 3)
)

# Download data from one indicator (Life expectancy): 
dat[dat$code == "WHOSIS_000001",]
response <- httr::GET(
  url = 'https://ghoapi.azureedge.net/api/WHOSIS_000001',
  verbose()
)
cnt_life_exp <- content(response, as = "parsed")

# Function to extract elements and convert if necessary
extract_chr <- function(x, index) {
  val <- x[[index]]
  if (is.null(val)) return(NA_character_)
  as.character(val)
}

extract_dbl <- function(x, index) {
  val <- x[[index]]
  if (is.null(val)) return(NA_real_)
  as.numeric(val)
}

# Get the list of indicators
response <- httr::GET(url = 'https://ghoapi.azureedge.net/api/WHOSIS_000001', verbose())

# Parse the response as text
raw_content <- content(response, as = "text")

# Convert the JSON text to a list
json_data <- fromJSON(raw_content, flatten = TRUE)

# Inspect the structure of the data
str(json_data)

# Print a subset of the data to understand its structure
print(json_data$value[1:5])  # Print the first 5 entries to inspect

# Create the tibble with the extracted data
dat_lifeexp <- tibble(
  id = map_chr(cnt_life_exp$value, ~ extract_chr(.x, "Id")),         # Extracts the first element (`Id`) from all the lists inside the cnt$value list
  country = map_chr(cnt_life_exp$value, ~ extract_chr(.x, "SpatialDim")),    # Extracts the 4th element (`SpatialDim`, country acronym)
  year = map_dbl(cnt_life_exp$value, ~ extract_dbl(.x, "TimeDim")),       # Extracts the year by name
  sex = map_chr(cnt_life_exp$value, ~ extract_chr(.x, "Dim1")),  # Assuming 'Dim1' is the
  life_expectancy = map_dbl(cnt_life_exp$value, ~ extract_dbl(.x, "NumericValue"))
)

# Filter for "Both sexes"
dat_lifeexp <- dat_lifeexp %>%
  filter(sex == "SEX_BTSX") %>%
  select(-sex)  

# Convert country codes to country names using countrycode package
dat_lifeexp <- dat_lifeexp %>%
  mutate(country_name = countrycode(country, "iso3c", "country.name"))

# View the updated tibble
print(dat_lifeexp)

# Remove rows where country_name is NA
dat_lifeexp <- dat_lifeexp %>%
  filter(!is.na(country_name))
dat_lifeexp

# Save the data to a CSV file
write_csv(dat_lifeexp, "data/life_expectancy.csv")













# Load necessary packages
library(shiny)
library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(countrycode)

# Function to extract elements and convert if necessary
extract_chr <- function(x, index) {
  val <- x[[index]]
  if (is.null(val)) return(NA_character_)
  as.character(val)
}

extract_dbl <- function(x, index) {
  val <- x[[index]]
  if (is.null(val)) return(NA_real_)
  as.numeric(val)
}

# Get the list of indicators
response <- httr::GET(url = 'https://ghoapi.azureedge.net/api/WHOSIS_000001', verbose())

# Parse the response as text
raw_content <- content(response, as = "text")

# Convert the JSON text to a list
cnt_life_exp <- fromJSON(raw_content, flatten = TRUE)

# Create the tibble with the extracted data
dat_lifeexp <- tibble(
  id = map_chr(cnt_life_exp$value, ~ extract_chr(.x, "Id")),
  country = map_chr(cnt_life_exp$value, ~ extract_chr(.x, "SpatialDim")),
  year = map_dbl(cnt_life_exp$value, ~ extract_dbl(.x, "TimeDim")),
  life_expectancy = map_dbl(cnt_life_exp$value, ~ extract_dbl(.x, "NumericValue")),
  sex = map_chr(cnt_life_exp$value, ~ extract_chr(.x, "Dim1"))  # Assuming 'Dim1' is the column for sex
)

# Filter for "Both sexes"
dat_lifeexp <- dat_lifeexp %>%
  filter(Dim1 == "Both sexes")

# Ensure only unique values for each country per year
dat_lifeexp <- dat_lifeexp %>%
  distinct(country, year, .keep_all = TRUE)

# Convert country codes to country names using countrycode package
dat_lifeexp <- dat_lifeexp %>%
  mutate(country_name = countrycode(country, "iso3c", "country.name"))

# Remove rows where country_name is NA
dat_lifeexp <- dat_lifeexp %>%
  filter(!is.na(country_name))





