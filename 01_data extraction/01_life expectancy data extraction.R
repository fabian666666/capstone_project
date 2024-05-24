
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
  life_expectancy = map_dbl(cnt_life_exp$value, ~ extract_dbl(.x, "NumericValue"))
)

# Ensure only the first value for each country per year is taken
dat_lifeexp <- dat_lifeexp %>%
  group_by(country, year) %>%
  slice(1) %>%
  ungroup()

# Convert country codes to country names using countrycode package
dat_lifeexp <- dat_lifeexp %>%
  mutate(country_name = countrycode(country, "iso3c", "country.name"))

# View the updated tibble
print(dat_lifeexp)

# Remove rows where country_name is NA
dat_lifeexp <- dat_lifeexp %>%
  filter(!is.na(country_name))


# Plotting life expectancy over time:
ggplot(dat_lifeexp, aes(x = year, y = life_expectancy)) + 
  geom_smooth(aes(group = country), size = 0.1, se = FALSE, color = "black") + 
  geom_smooth(size = 2) +
  labs(title = "Life Expectancy Over Time", x = "Year", y = "Life Expectancy")


















