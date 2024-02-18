################################################################################
################################################################################
# IN THIS CODE I WILL CLEAN THE UNIVERSITIES NAMES AND HAVE A UNIFIED LIST OF 
# UNIS, THEIR REGIONS, AND THEIR COUNTRIES FOR AABROAD UNIS.
################################################################################
################################################################################

pacman::p_load(tidyverse, combinat, igraph, ggraph, Matrix, data.table, disk.frame)


# Define a function to clean the country strings
clean_countries <- function(country_string) {
  parts <- str_split(country_string, ";\\s*")[[1]]
  cleaned_parts <- sapply(parts, function(part) {
    if (grepl("USA", part)) "USA" else part
  })
  paste(cleaned_parts, collapse = "; ")
}
  
# pattern <- "(?<=,\\s)[^,;\\[]+(?=(; \\[|$))"

wos_ided_aff <- wos_ided %>%
  select(rowid, author_affiliation, authors, author_affiliation2)%>%
  mutate(
    countries = sapply(str_extract_all(author_affiliation, "(?<=,\\s)[^,;\\[]+(?=(; \\[|$))"), 
                       function(x) paste(x, collapse = "| ")),
    # countries = sapply(countries, clean_countries),
    author_groups = sapply(str_extract_all(author_affiliation, "\\[.*?\\]"), 
                     function(x) paste(x, collapse = "| "))
  ) %>% 
  mutate(
    institutions = gsub(";", "|", author_affiliation2)
  ) %>% 
  separate_rows(institutions, sep = "\\|") 



wos_ided_aff %>% 
  filter(rowid == 3) %>% 
  select(institutions, author_affiliation2)
  pull(author_affiliation)
  #  NO NEED TO INCLUDE AUTHOR GROUPS FOR NOW 
  #  SINCE THE FOCUS IS ON INSTITUTIONS MAINLY
  #  PROBLEM WITH SPERATING COUNTIES AS WELL, WILL BE QUITE DRAMTIC
  group_by(rowid) %>% 
  separate_rows(institutions, sep = "\\|") 
  
  
  