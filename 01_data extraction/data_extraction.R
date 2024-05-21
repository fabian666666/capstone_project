library(rvest)
library(jsonlite)
library(httr)
library(stringr)
library(dplyr)

## Guardian API

# Define the base URL for the Guardian API content endpoint
base_url <- "https://content.guardianapis.com/search"

# Set up API key
api_key_data <- read.csv("api_key.csv", header = FALSE)
api_key <- api_key_data$V1[1]

# filter the news articles related to single payer health insurance from 2024
query_params <- list(
  q = "single payer health insurance", 
  "order-by" = "newest",
  "page-size" = 200,
  "from-date" = "2024-01-01",
  "to-date" = "2024-05-20",
  "show-fields" = "body",
  "api-key" = api_key
)

# Construct URL and make a HTTP request
url <- modify_url(base_url, query = query_params)
response <- GET(url)

# Process the API response and extract article details
if (http_status(response)$category == "Success") {
  data <- content(response, "parsed")
  articles <- map(data$response$results, ~list(
    title = .x$webTitle,
    body = .x$fields$body,
    date = .x$webPublicationDate  
  ))
} else {
  stop("Failed to fetch data")
}

# Converting list to a data frame
articles_df <- map_df(data$response$results, ~data.frame(
  title = coalesce(.x$webTitle, ""),
  body = coalesce(.x$fields$body, ""),
  date = coalesce(.x$webPublicationDate, "")  
), .id = "article_id")

# Save to CSV
write.csv(articles_df, "single_payer_health_insurance_articles.csv", row.names = FALSE)



## serpAPI

library(httr)
library(jsonlite)

# Set up API key
api_key_google_scolar <- api_key_data$V1[2]

# Function to perform a Google Scholar search using SerpApi
google_scholar_search <- function(query, api_key_google_scolar) {
  base_url_SerpAPI <- "https://serpapi.com/search"
  
  params <- list(
    engine = "google_scholar",
    q = query,
    api_key = api_key_google_scolar
  )
  
  response <- GET(base_url_SerpAPI, query = params)
  
  if (status_code(response) == 200) {
    result <- content(response, "text")
    result <- fromJSON(result, flatten = TRUE)
    return(result$organic_results)
  } else {
    stop("Failed to fetch data: ", status_code(response))
  }
}


# filter for publications related to single payer health insurance 
query <- "single payer health insurance"
organic_results <- google_scholar_search(query, api_key_google_scolar)

# Print the results
print(organic_results)

# Access the link of the first publication
first_publication_link <- organic_results[[1]]$link

# Extract the links of the publications
publication_links <- sapply(organic_results, function(result) result$link)

# Print the links
print(publication_links)






