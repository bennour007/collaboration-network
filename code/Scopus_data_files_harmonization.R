pacman::p_load(tidyverse, here)


path <- here('data', 'Scopus')

# List all .txt files in the directory
txt_files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

# Read each .txt file into a list of data frames
list_of_data_frames <- map(txt_files, read_csv) %>% 
  bind_rows() %>% 
  janitor::clean_names()


cols_needed <- c(
  'title',
  'year',
  'authors',
  'cited_by',
  'author_keywords',
  'index_keywords',
  'authors_with_affiliations',
  'affiliations',
  'abstract',
  'doi'
)

data_ided_na <- list_of_data_frames %>% 
  # colnames()
  select(all_of(cols_needed)) %>% 
  rowid_to_column() 

scopus_doi <- data_ided_na %>% 
  distinct(doi, .keep_all = T) %>% 
  pull(doi)
  filter(!is.na(author_keywords) & !is.na(index_keywords))
