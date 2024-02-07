# Install and load the rcrossref package
pacman::p_load(rcrossref)

# List of DOIs
dois <- data_raw %>% 
  filter(is.na(author_keywords) & is.na(index_keywords)) %>% 
  select(doi)


extract_keywords <- function(doi_list) {
  results <- lapply(doi_list, function(doi) {
    # Attempt to retrieve the metadata for the given DOI
    metadata <- cr_works(dois = doi)$data
    # Initialize keyword variables
    author_keywords <- NA

    # Attempt to extract author keywords (assuming they could be in 'subject' or similar fields)
    if (!is.null(metadata$subject)) {
      author_keywords <- paste(metadata$subject, collapse = ", ")
    }
    
    # Note: For index keywords, there isn't a specific, uniformly used field in Crossref metadata.
    # The code here assumes 'subject' might include relevant keywords, but this is an approximation.
    
    # Return a list of the extracted information
    list(DOI = doi, Author_Keywords = author_keywords)
  })
  return(results)
}

# Extract metadata
metadata_list <- extract_metadata(dois)

data_raw_imputed <- dois %>% 
  mutate(metadata = map(doi, extract_metadata))


data_raw_imputed %>% 
  mutate(metadata = map(metadata, function(x) x$Keywords)) %>% 
  pull(metadata)
# Print the results
print(metadata_list)





################################################################################
################################################################################
################################################################################

# Load the httr package
library(httr)

# Your Scopus API Key
api_key <- "YOUR_API_KEY_HERE"

# DOI for which you want to retrieve data
doi <- "10.1016/j.compstruct.2020.112233" # Replace with your DOI

# Base URL for Scopus API Abstract Retrieval
base_url <- "https://api.elsevier.com/content/article/doi/"

# Prepare the request URL
request_url <- paste0(base_url, doi, "?apiKey=", api_key, "&httpAccept=application/json")

# Make the GET request
response <- GET(request_url)

# Check if the request was successful
if (status_code(response) == 200) {
  content_data <- content(response, as = "parsed", type = "application/json")
  
  # Extracting Author Keywords and Index Keywords
  author_keywords <- content_data$`abstracts-retrieval-response`$`authkeywords`$keyword
  index_keywords <- NA # Scopus API doesn't directly provide "index keywords" in the same way as Web of Science
  
  # Print the keywords
  if (!is.null(author_keywords)) {
    print("Author Keywords:")
    print(sapply(author_keywords, function(k) k$`$`))
  } else {
    print("No Author Keywords found.")
  }
  
  # For index keywords, you might need to look into other parts of the response
  # or use specific fields that could be interpreted as index keywords.
} else {
  print(paste("Failed to retrieve data. Status code:", status_code(response)))
}

