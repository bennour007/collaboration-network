
### I designed this so that no one need to have an issue dealing with large data files.
### Instead you can simply run this on your machine (the last table is huge it can break your seesion)


pacman::p_load(tidyverse, here)




path <- here('data', 'WoS')

data <- read_tsv(here('data','WoS','savedrecs18001-19000.txt'))
# List all .txt files in the directory
txt_files <- list.files(path = path, pattern = "\\.txt$", full.names = TRUE)

# Read each .txt file into a list of data frames
list_of_data_frames <- map(txt_files, read_tsv)



# Define the selected variables and new names in independent vectors
selected_vars <- c("TI", "DT", "PT", "PY", "DI", "AU", "C1", "DE", "ID", "WC", "SC", "AB", "TC", "SO")
new_names <- c("DocumentTitle", "DocumentType", "PublicationType", "PublicationYear", "DOI",
               "Authors", "AuthorAffiliation", "AuthorKeywords", "KeywordsPlus",
               "WoSCategories", "SubjectCategory", "Abstract", "TimesCited", "SourcePublication")

# Define a function to filter and rename dataset columns
filter_and_rename <- function(dataset) {
  dataset %>%
    select(all_of(selected_vars)) %>%
    rename_with(~new_names, all_of(selected_vars)) %>%
    mutate(
      across(
        c(TimesCited, PublicationYear),
        as.numeric
      )
    ) %>%
    janitor::clean_names()
}


wos_data <- list_of_data_frames %>%
  map(filter_and_rename) %>%
  bind_rows()


# ################################################################################
# ################################################################################
# ################################################################################
# # CLEAN DATA
# 
# clean_wos <- read_csv(here('data', 'WoS', 'clean_wos.csv'))

clean_wos <- wos_data %>%
  distinct(doi, .keep_all = T) %>%
  filter(!is.na(authors) & !is.na(publication_year) & !is.na(author_keywords) & !is.na(keywords_plus) & !is.na(author_affiliation) )


# clean_wos %>% write_csv(here('data', 'WoS', 'clean_wos.csv'))

# clean_wos <- read_csv(here('data', 'WoS', 'clean_wos.csv'))

# adding id to the each paper

wos_ided <- clean_wos %>% 
  rowid_to_column()

################################################################################
################################################################################
################################################################################
# DIVIDE DATA 


################################################################################
# ## by authors and affiliation:
# THERE'S A POTENTIAL ISSUE WITH AUTHORS BEING AFFILIATED WITH MULTIPLE 
# ENTITIES SIMULTANEUOUSLY FOR A SINGLE PAPER
# NOT SURE HOW TO MITIGATE THIS.
################################################################################




# wos_ided %>% 
#   select(rowid, publication_year, author_affiliation, document_title, doi) %>% 
#   mutate(
#     authors = str_extract(author_affiliation, "\\[.*?\\]"), # Extract authors
#     affiliation = str_extract(author_affiliation, "(?<=\\]).+?(?=, [^,]+$)"), # Extract affiliation
#     country = str_extract(author_affiliation, "[^,]+$") # Extract country
#   ) %>%
#   mutate(
#     authors = str_remove(authors, "\\[|\\]"), # Remove brackets from authors
#     affiliation = str_trim(str_remove(affiliation, "\\[.*?\\]")) # Remove authors and trim spaces from affiliation
#   ) %>% View()

# Define a simplified and vectorized function
parse_authors_and_affiliations <- function(author_affiliation) {
  # Split the entire string by the pattern that indicates a new author-affiliation pair
  pairs <- str_split(author_affiliation, "; (?=\\[)", simplify = TRUE)
  
  # Initialize vectors to store extracted data
  authors <- character()
  affiliations <- character()
  countries <- character()
  
  for (pair in pairs) {
    # Extract authors (assuming they are within brackets)
    author <- str_extract(pair, "\\[.*?\\]") %>% str_remove_all("\\[|\\]")
    # Split affiliation and country by the last comma
    aff_country <- str_extract(pair, "(?<=\\]).*$") %>% 
      str_split(", (?=[^,]+$)", n = 2, simplify = TRUE)
    # Append to vectors
    authors <- c(authors, author)
    affiliations <- c(affiliations, aff_country[1])
    countries <- c(countries, aff_country[2])
  }
  
  # Return a tibble with the parsed data
  tibble(author = authors, affiliation = affiliations, country = countries)
}


# Apply the function to each row and unnest the result to create a long format dataframe

## by affiliation and country
wos_affiliations <- wos_ided %>%
  select(rowid, author_affiliation) %>% 
  rowwise() %>%
  mutate(parsed = list(parse_authors_and_affiliations(author_affiliation))) %>%
  unnest(parsed) 

wos_affiliations <- wos_affiliations%>% 
  select(-author_affiliation)



## by main paper characteristics


wos_paper <- wos_ided %>% 
  select(
    rowid, publication_year, document_title, document_type, publication_type, 
    source_publication, times_cited
  )


## by subject category

wos_sc <- wos_ided %>% 
  select(rowid, subject_category) %>% 
  separate_rows(subject_category, sep = ";\\s*")


## by keywords

wos_kw <- wos_ided %>% 
  select(rowid, author_keywords) %>% 
  separate_rows(author_keywords, sep = ";\\s*")


################################################################################
# ## JOINING THE DATA TOGETHER
# MANY TO MANY RELATIONSHIPS : POTENTIAL ISSUE?
################################################################################


wos_full <- wos_paper %>% 
  left_join(wos_sc) %>% 
  left_join(wos_kw) %>% 
  left_join(wos_affiliations)


# uncomment if needed
# wos_full %>%
#   write_csv(here('data', 'full_joined_wos_data.csv'))

# ns <- map( c('wos_paper', 'wos_affiliations', 'wos_kw', 'wos_sc'), function(x) paste0(here('data', 'clean_data'),'/', x,  '.csv'))
# 
# xs <- list(wos_paper, wos_affiliations, wos_kw, wos_sc)
#   
# 
# map2(xs, ns, function(x,y) x %>% write_csv(y))
