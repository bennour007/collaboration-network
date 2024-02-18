# Define the vector of names to match
universities <- c("University of Veterinary Medicine Budapest", "Allatorvostudomanyi Egyetem",
                  "Corvinus University of Budapest", "Budapesti Corvinus Egyetem",
                  "Budapest Business School", "Budapesti Gazdasagi Egyetem",
                  "Budapest University of Technology and Economics", "Budapesti Muszaki es Gazdasagtudomanyi Egyetem",
                  "Eotvos Lorand University", "Eotvos Lorand Tudomanyegyetem",
                  "Semmelweis University", "Semmelweis Egyetem",
                  "University of Debrecen", "Debreceni Egyetem",
                  "University of Szeged", "Szegedi Tudomanyegyetem",
                  "University of Pecs", "Pecsi Tudomanyegyetem")

keywords_mapping <- list(
  "University of Veterinary Medicine Budapest" = c("Veterinary", "Állatorvostudományi"),
  "Corvinus University of Budapest" = c("Corvinus", "Corvinus Egyetem"),
  "Budapest Business School" = c("Business School of Budapest", "Budapest Business School", "Gazdasági Egyetem"),
  "Budapest University of Technology and Economics" = c("University of Technology and Economics of Budapest", "Budapest University of Technology and Economics","Műszaki"),
  "Eötvös Loránd University" = c("Eötvös", "ELTE", "Loránd", "Eotvos Lorand", "Eotvos", "Lorand"),
  "Semmelweis University" = c("Semmelweis", "Semmelweis Egyetem"),
  "University of Debrecen" = c("Debrecen", "Debreceni Egyetem"),
  "University of Szeged" = c("Szeged", "Szegedi Tudományegyetem"),
  "University of Pécs" = c("Pécs", "Pecs", "Pécsi Tudományegyetem")
)

# Sample vector for demonstration (replace this with your actual vector)
sample_vector <- wos_aff %>% pull(author_affiliation2)


# Function to count occurrences based on direct keyword mapping
count_occurrences_direct <- function(vec, ref_unis, keywords_to_uni) {
  # Initialize a named vector for counts
  counts <- setNames(rep(0, length(ref_unis)), ref_unis)
  
  # Process each element in the vector
  for(item in vec) {
    # Find the first keyword match for the item
    matched_uni <- NA
    for(keyword in names(keywords_to_uni)) {
      if(grepl(keyword, item, ignore.case = TRUE)) {
        matched_uni <- keywords_to_uni[keyword]
        break
      }
    }
    
    # If a match was found, increment the count for the corresponding university
    if(!is.na(matched_uni)) {
      counts[matched_uni] <- counts[matched_uni] + 1
    }
  }
  
  # Convert to data frame
  df <- data.frame(University = names(counts), Occurrences = counts, stringsAsFactors = FALSE)
  
  # Filter out universities with 0 occurrences
  df <- df[df$Occurrences > 0, ]
  
  return(df)
}


# Use the function
occurrence_table <- create_occurrence_table(sample_vector, universities)

# Print the result
print(count)
