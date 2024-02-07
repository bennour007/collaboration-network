library(httr)
library(jsonlite)


# Scopus search query URL
search_url <- "https://api.elsevier.com/content/search/scopus"

# Your search query
query <- 'AFFIL(("University of Veterinary Medicine Budapest" OR "Állatorvostudományi Egyetem") OR ("Corvinus University of Budapest" OR "Budapesti Corvinus Egyetem") OR ("Budapest Business School" OR "Budapesti Gazdasági Egyetem") OR ("Budapest University of Technology and Economics" OR "Budapesti Műszaki és Gazdaságtudományi Egyetem") OR ("Eötvös Loránd University" OR "Eötvös Loránd Tudományegyetem") OR ("Semmelweis University" OR "Semmelweis Egyetem") OR ("University of Debrecen" OR "Debreceni Egyetem") OR ("University of Szeged" OR "Szegedi Tudományegyetem") OR ("University of Pécs" OR "Pécsi Tudományegyetem")) AND PUBYEAR > 2018 AND PUBYEAR < 2023 AND (LIMIT-TO(SRCTYPE,"j")) AND (LIMIT-TO(DOCTYPE,"ar")) AND (LIMIT-TO(LANGUAGE,"English"))'

# Headers
headers <- add_headers(`X-ELS-APIKey` = api_key, Accept = "application/json")

# Initial request to get total results count
initial_params <- list(query = query, count = 1, field = 'eid')
initial_response <- GET(url = search_url, query = initial_params, config = headers)
initial_data <- fromJSON(content(initial_response, "text"), flatten = TRUE)
total_results <- as.numeric(initial_data$`search-results`$`opensearch:totalResults`)

# Calculate the number of pages, assuming 25 results per page (or adjust as needed)
results_per_page <- 25
total_pages <- ceiling(total_results / results_per_page)

# Initialize an empty list to store all results
all_results <- list()

# Loop through each page and collect results
for (page in 1:200) {
  start <- (page - 1) * results_per_page
  params <- list(query = query, start = start, count = results_per_page, field = 'eid,doi,pubmed-id,author-ids,abstract,authors,authkeywords,title,coverDate,source-title,issn,source-type,volume,issue,articlenumber,author-afids,afid,affilname,author-count,authname,citedby-count,aggregation-type,publisher,subjareas,description,citation-count,')
  response <- GET(url = search_url, query = params, config = headers)
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"), flatten = TRUE)
    all_results[[page]] <- data$search_results$entry
  } else {
    print(paste("Failed to retrieve data on page", page, ". Status code:", status_code(response)))
    break
  }
}







