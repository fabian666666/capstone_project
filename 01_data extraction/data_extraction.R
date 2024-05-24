library(rvest)
library(jsonlite)
library(httr)
library(stringr)
library(dplyr)

# Define the base URL for the Guardian API content endpoint
base_url <- "https://content.guardianapis.com/search"

# Setup query parameters
api_key_data <- read.csv("api_key.csv", header = FALSE)
api_key <- api_key_data$V1[1]

# filter the news articles related to NHS
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



## serpAPI

library(httr)
library(jsonlite)
library(dplyr)

# Set up API key
api_key_google_scolar <- api_key_data$V1[2]

# Function to perform a Google Scholar search using SerpApi
google_scholar_search <- function(query, api_key_google_scolar) {
  base_url_SerpAPI <- "https://serpapi.com/search"
  
  params <- list(
    engine = "google_scholar",
    q = query,
    as_ylo = 2014,
    as_yhi = 2024,
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
query <- "single payer health insurance OR single payer health system OR single payer health care" 

organic_results <- google_scholar_search(query, api_key_google_scolar)

# Print the results
print(organic_results)

# Convert the list to a data frame
organic_results_df <- as.data.frame(organic_results)

# Access the fourth column of the data frame
links <- organic_results_df[, 4]

# Print the fourth column
print(links)

# Function to fetch and extract text from the first link
fetch_text_from_link <- function(url) {
  page <- read_html(url)
  paragraphs <- page %>% html_nodes("p") %>% html_text()
  return(paragraphs)
}

# Fetch the text from the first link
first_link <- links[1]
text_content <- fetch_text_from_link(first_link) # does not work (Fehler in open.connection(x, "rb") : HTTP error 403.)

# Fetch the content of the links
page <- read_html(links[1])
# HTTP error 403.

# Extract and print the text 
page_text <- page %>%
  html_nodes("p") %>% 
  html_text()

# Extract and print the text from the page
page_text <- page %>%
  html_nodes("p") %>% 
  html_text()

# Print the extracted text
print(page_text)

library(rvest)
library(syuzhet)

# Function to extract text from a link
extract_text_from_link <- function(link) {
  tryCatch({
    page <- read_html(link)
    text <- page %>% html_nodes("p") %>% html_text()
    return(text)
  }, error = function(e) {
    return(NA)
  })
}

# Extract text from all links
texts <- sapply(links, extract_text_from_link)




# Manually scrape the publication links from the search results

# Pubmed

# Load libraries
library(rentrez)
library(dplyr)
library(stringr)

# Set up PubMed query
search_term <- "single payer"
search_results <- entrez_search(db = "pubmed", term = search_term, retmax = 10)

# Fetch abstracts for each article
article_ids <- search_results$ids

fetch_abstract <- function(article_id) {
  abstract <- entrez_fetch(db = "pubmed", id = article_id, rettype = "abstract", retmode = "text")
  return(abstract)
}

abstracts <- sapply(article_ids, fetch_abstract)

# Clean and format data
articles <- data.frame(
  id = article_ids,
  abstract = abstracts,
  stringsAsFactors = FALSE
)

# Save data
write.csv(articles, "pubmed_abstracts_single_payer.csv", row.names = FALSE)





library(rentrez)
library(dplyr)
library(stringr)

# Set up PubMed query
search_term <- "single payer"
search_results <- entrez_search(db = "pubmed", term = search_term, retmax = 3)

# Fetch abstracts for each article
article_ids <- search_results$ids

fetch_abstract <- function(article_id) {
  abstract_text <- entrez_fetch(db = "pubmed", id = article_id, rettype = "abstract", retmode = "text")
  
  # Extract abstract text using regular expression
  abstract_lines <- unlist(strsplit(abstract_text, "\n"))
  abstract_start <- which(grepl("^\\s*\\[Abstract\\]", abstract_lines))
  if(length(abstract_start) > 0){
    abstract_lines <- abstract_lines[(abstract_start + 1):length(abstract_lines)]
  }
  
  abstract <- paste(abstract_lines, collapse = " ")
  return(abstract)
}

abstracts <- sapply(article_ids, fetch_abstract)

# Clean and format data
articles <- data.frame(
  id = article_ids,
  abstract = abstracts,
  stringsAsFactors = FALSE
)

# View the articles data frame
print(articles)

# Save data
write.csv(articles, "test_pubmed_abstracts_single_payer.csv", row.names = FALSE)







