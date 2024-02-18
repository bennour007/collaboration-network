pacman::p_load(tidyverse, EconGeo)


reference_institutions <- c(
  "Eotvos Lorand University",
  "Eotvos Lorand Research Network",
  "MTA-DE Biodiversity & Ecosystem Services Research Group",
  "MTA-BME Stochastics Research Group",
  "MTA-ELTE Comparative Ethology Research Group",
  "MTA-SZTE Lendulet Porous Nanocomposites Research Group",
  "Corvinus University Budapest",
  "Obuda University",
  "University of Debrecen",
  "Semmelweis University",
  "University of Veterinary Medicine Budapest",
  "Pazmany Peter Catholic University",
  "University of Pecs",
  "University of Miskolc",
  "University of Pannonia",
  "University of Nyiregyhaza",
  "Budapest Metropolitan University",
  "Budapest University of Technology & Economics",
  "Budapest Business University",
  "Moholy-Nagy University of Art & Design Budapest",
  "HUN-REN Institute of Earth Physics & Space Science",
  "HUN-REN Centre for Agricultural Research",
  "HUN-REN Veterinary Medical Research Institute",
  "HUN-REN Research Centre for Natural Sciences",
  "HUN-REN Wigner Research Centre for Physics",
  "HUN-REN Biological Research Center",
  "HUN-REN Institute for Nuclear Research",
  "HUN-REN Centre for Social Sciences",
  "HUN-REN Research Centre for Astronomy & Earth Sciences",
  "HUN-REN Centre for Ecological Research",
  "HUN-REN Centre for Energy Research",
  "HUN-REN Institute of Experimental Medicine",
  "HUN-REN Centre for Economic & Regional Studies",
  "HUN-REN Institute for Computer Science & Control",
  "HUN-REN Alfred Renyi Institute of Mathematics",
  "HUN-REN Research Centre for the Humanities",
  "HUN-REN Balaton Limnological Research Institute",
  "HUN-REN Research Centre for Linguistics"
)


joined_wos <- wos_aff %>% 
  select(rowid, author_affiliation2) %>% 
  filter(author_affiliation2 %in% reference_institutions) %>% 
  right_join(wos_paper) %>% 
  left_join(wos_sc) %>% 
  # group_by(rowid) %>% 
  ## 30 missing values from the join clause
  ## ASK DANIEL, MAYBE THERE'S SOMETHING TO DO ABOUT IT
  filter(!is.na(author_affiliation2))



joined_wos_years <- map(
    c(2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013),
    function(x){
      joined_wos %>% 
        filter(publication_year == x)
    }
  )

# # 
# cooccurance <- joined_wos_years[[3]] %>%
#   ungroup() %>%
#   select(author_affiliation2, subject_category) %>%
#   group_by(author_affiliation2, subject_category) %>%
#   summarise(count = n(), .groups = 'drop') %>%
#   spread(key = subject_category, value = count, fill = 0) %>%
#   column_to_rownames(var = "author_affiliation2")
# 

# # rn <- cooccurance %>%  pull(author_affiliation2)
# # cn <- colnames(cooccurance)[2:ncol(cooccurance)]
# 
# cooccurancemat <- cooccurance %>% as.matrix( )
#   
# rownames(cooccurancemat) <- rn
# 
# ## RCA
# 
# rcamat <- RCA(cooccurancemat, binary = T)
# cooc <- co.occurrence(t(rcamat))
# relmat <- relatedness(cooc)
# relmat[relmat < 1] <- 0
# relmat[relmat > 1] <- 1
# 
# 
# rel_density <- relatedness.density(rcamat, relmat)
# 
# rel_density %>% 
#   get.list()
# 
# 
# library(igraph)
# 
# g1 <- graph_from_adjacency_matrix(relmat, mode = "undirected")
# 
# 
# plot(g1)
# 
# tci <- TCI(relmat)
# 
# 
# names()
# 


################################################################################
################################################################################
## STREAMLINE THIS PROCESS FOR EACH PERIOD IN THE LIST
################################################################################
################################################################################


streamline_indicators <- function(x){
  ## PUT IN MATRIX FORMAT
  matrixx <- x %>% 
    select(author_affiliation2, subject_category) %>% 
    group_by(author_affiliation2, subject_category) %>%
    summarise(count = n(), .groups = 'drop') %>%
    spread(key = subject_category, value = count, fill = 0) %>% 
    column_to_rownames(var = "author_affiliation2") %>% 
    as.matrix()
  
  ## COMPUTE RELATIVE COMPARATIVE ADVANTAGE 
  rcamat <- RCA(matrixx, binary = T)
  cooc <- co.occurrence(t(rcamat))
  
  ## COMPUTE RELATEDNESS AND TRANSFORM AS BINARY
  relmat <- relatedness(cooc)
  relmat[relmat < 1] <- 0
  relmat[relmat > 1] <- 1
  
  ## COMPUTE RELATEDNESS DENSITY AND PUT AS LIST
  rel_density <- relatedness.density(rcamat, relmat) %>%
    get.list()
  
  ## COMPUTE KNOWLEDGE COMPLEXITY FOR EACH INSTITUTION IN A TIBBLE
  # KCI <- tibble(int = rownames(rcamat), kci = kci)
  list(rcamat = rcamat, relmat = relmat, rel_density = rel_density)

}

tmp <- joined_wos_years %>% 
  map(streamline_indicators)

















