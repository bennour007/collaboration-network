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
  "University of Istvan Szechenyi",
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


joined_wos_raw <- wos_aff %>% 
  select(rowid, author_affiliation2) %>% 
  filter(author_affiliation2 %in% reference_institutions) %>% 
  right_join(wos_paper) %>% 
  left_join(wos_sc) %>% 
  # group_by(rowid) %>% 
  ## 30 missing values from the join clause
  ## ASK DANIEL, MAYBE THERE'S SOMETHING TO DO ABOUT IT
  filter(!is.na(author_affiliation2))

joined_wos <- joined_wos_raw %>% 
  mutate(
    author_affiliation2 = case_when(
      author_affiliation2 == "MTA-DE Biodiversity & Ecosystem Services Research Group" ~ "University of Debrecen",
      author_affiliation2 == "MTA-BME Stochastics Research Group" ~ "Budapest University of Technology and Economics",
      author_affiliation2 == "MTA-ELTE Comparative Ethology Research Group" ~ "Eötvös Loránd University",
      author_affiliation2 == "MTA-SZTE Lendulet Porous Nanocomposites Research Group" ~ "University of Szeged",
      TRUE ~ author_affiliation2 # Default case to keep the original value if none of the above conditions are met
    ),
    Location = case_when(
      author_affiliation2 %in% c("Eotvos Lorand University",
                         "Eotvos Lorand Research Network",
                         "Corvinus University Budapest",
                         "Obuda University",
                         "Semmelweis University",
                         "University of Veterinary Medicine Budapest",
                         "Pazmany Peter Catholic University",
                         "Budapest Metropolitan University",
                         "Budapest University of Technology & Economics",
                         "Budapest Business University",
                         "Moholy-Nagy University of Art & Design Budapest",
                         "HUN-REN Veterinary Medical Research Institute",
                         "HUN-REN Research Centre for Natural Sciences",
                         "HUN-REN Wigner Research Centre for Physics",
                         "HUN-REN Centre for Social Sciences",
                         "HUN-REN Research Centre for Astronomy & Earth Sciences",
                         "HUN-REN Centre for Ecological Research",
                         "HUN-REN Centre for Energy Research",
                         "HUN-REN Institute of Experimental Medicine",
                         "HUN-REN Centre for Economic & Regional Studies",
                         "HUN-REN Institute for Computer Science & Control",
                         "HUN-REN Alfred Renyi Institute of Mathematics",
                         "HUN-REN Research Centre for the Humanities",
                         "HUN-REN Research Centre for Linguistics") ~ "Budapest",
      author_affiliation2 == "University of Debrecen" ~ "Debrecen",
      author_affiliation2 == "University of Pecs" ~ "Pecs",
      author_affiliation2 == "University of Miskolc" ~ "Miskolc",
      author_affiliation2 == "University of Pannonia" ~ "Veszprem",
      author_affiliation2 == "University of Nyiregyhaza" ~ "Nyiregyhaza",
      author_affiliation2 == "HUN-REN Institute of Earth Physics & Space Science" ~ "Sopron",
      author_affiliation2 == "HUN-REN Centre for Agricultural Research" ~ "Martonvásár",
      author_affiliation2 == "HUN-REN Biological Research Center" ~ "Szeged",
      author_affiliation2 == "HUN-REN Institute for Nuclear Research" ~ "Debrecen",
      author_affiliation2 == "HUN-REN Balaton Limnological Research Institute" ~ "Tihany",
      TRUE ~ NA_character_ # For any institution not listed, NA is assigned
    )
  )



years <- c(2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013)

joined_wos_years <- set_names(years) %>%
  map(~joined_wos %>% filter(publication_year == .x))


# joined_wos_periods <- list(
#   before_2018 = joined_wos %>% 
#     filter(publication_year < 2018),
#   after_2018 = joined_wos %>% 
#     filter(publication_year >= 2018)
# )


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
  
  ## COMPUTE ENTRY
  
  ## COMPUTE KNOWLEDGE COMPLEXITY FOR EACH INSTITUTION IN A TIBBLE
  KCI <- tibble(Region = rownames(rcamat), KCI = KCI(rcamat, RCA = F))
  TCI <- tibble(Industry = colnames(rcamat), TCI = TCI(rcamat, RCA = F))
  
  
  ## RETURN LIST ITEMS
  list(rcamat = rcamat, relmat = relmat, rel_density = rel_density, KCI = KCI, TCI = TCI)

}

tmp <- joined_wos_years %>% 
  map(streamline_indicators)
# 
# entry <- entry.list(tmp$before_2018$rcamat, tmp$after_2018$rcamat)
# 


# colnames(entry)[1:2] <- c("Region", "Industry")
# 
# 
# merged1 <- merge(entry, tmp$before_2018$rel_density, tmp$before_2018$KCI, by = c("Region", "Industry")) 
#   
# merged2 <-merge(merged, tmp$before_2018$KCI, by = "Region")
# 
# 
# summary(lm(merged2$entry ~ merged2$Count + merged2$KCI))


entry_2023_2024 <- entry.list(tmp$`2023`$rcamat, tmp$`2024`$rcamat) %>% 
  mutate(period = 2024) %>% 
  dplyr::rename(
    Region = region,
    Industry = industry
  )
entry_2022_2023 <- entry.list(tmp$`2022`$rcamat, tmp$`2023`$rcamat) %>% 
  mutate(period = 2023) %>% 
  dplyr::rename(
    Region = region,
    Industry = industry
  )
entry_2021_2022 <- entry.list(tmp$`2021`$rcamat, tmp$`2022`$rcamat) %>% 
  mutate(period = 2022) %>% 
  dplyr::rename(
    Region = region,
    Industry = industry
  )
entry_2020_2021 <- entry.list(tmp$`2020`$rcamat, tmp$`2021`$rcamat) %>% 
  mutate(period = 2021) %>% 
  dplyr::rename(
    Region = region,
    Industry = industry
  )
entry_2019_2020 <- entry.list(tmp$`2019`$rcamat, tmp$`2020`$rcamat) %>% 
  mutate(period = 2020) %>% 
  dplyr::rename(
    Region = region,
    Industry = industry
  )
entry_2018_2019 <- entry.list(tmp$`2018`$rcamat, tmp$`2019`$rcamat) %>% 
  mutate(period = 2019) %>% 
  dplyr::rename(
    Region = region,
    Industry = industry
  )
entry_2017_2018 <- entry.list(tmp$`2017`$rcamat, tmp$`2018`$rcamat) %>% 
  mutate(period = 2018) %>% 
  dplyr::rename(
    Region = region,
    Industry = industry
  )
entry_2016_2017 <- entry.list(tmp$`2016`$rcamat, tmp$`2017`$rcamat) %>% 
  mutate(period = 2017) %>% 
  dplyr::rename(
    Region = region,
    Industry = industry
  )
entry_2015_2016 <- entry.list(tmp$`2015`$rcamat, tmp$`2016`$rcamat) %>% 
  mutate(period = 2016) %>% 
  dplyr::rename(
    Region = region,
    Industry = industry
  )
entry_2014_2015 <- entry.list(tmp$`2014`$rcamat, tmp$`2015`$rcamat) %>% 
  mutate(period = 2015) %>% 
  dplyr::rename(
    Region = region,
    Industry = industry
  )
entry_2013_2014 <- entry.list(tmp$`2013`$rcamat, tmp$`2014`$rcamat) %>%
  mutate(period = 2014) %>% 
  dplyr::rename(
    Region = region,
    Industry = industry
  )

merged_2023_2024 <- merge(entry_2023_2024, tmp$`2024`$rel_density, by = c("Region", "Industry"))
merged_2023_2024 <- merge(merged_2023_2024, tmp$`2024`$KCI, by = "Region")
merged_2023_2024 <- merge(merged_2023_2024, tmp$`2024`$TCI, by = "Industry")

merged_2022_2023 <- merge(entry_2022_2023, tmp$`2023`$rel_density, by = c("Region", "Industry"))
merged_2022_2023 <- merge(merged_2022_2023, tmp$`2023`$KCI, by = "Region")
merged_2022_2023 <- merge(merged_2022_2023, tmp$`2023`$TCI, by = "Industry")

merged_2021_2022 <- merge(entry_2021_2022, tmp$`2022`$rel_density, by = c("Region", "Industry"))
merged_2021_2022 <- merge(merged_2021_2022, tmp$`2022`$KCI, by = "Region")
merged_2021_2022 <- merge(merged_2021_2022, tmp$`2022`$TCI, by = "Industry")

merged_2020_2021 <- merge(entry_2020_2021, tmp$`2021`$rel_density, by = c("Region", "Industry"))
merged_2020_2021 <- merge(merged_2020_2021, tmp$`2021`$KCI, by = "Region")
merged_2020_2021 <- merge(merged_2020_2021, tmp$`2021`$TCI, by = "Industry")

merged_2019_2020 <- merge(entry_2019_2020, tmp$`2020`$rel_density, by = c("Region", "Industry"))
merged_2019_2020 <- merge(merged_2019_2020, tmp$`2020`$KCI, by = "Region")
merged_2019_2020 <- merge(merged_2019_2020, tmp$`2020`$TCI, by = "Industry")

merged_2018_2019 <- merge(entry_2018_2019, tmp$`2019`$rel_density, by = c("Region", "Industry"))
merged_2018_2019 <- merge(merged_2018_2019, tmp$`2019`$KCI, by = "Region")
merged_2018_2019 <- merge(merged_2018_2019, tmp$`2019`$TCI, by = "Industry")

merged_2017_2018 <- merge(entry_2017_2018, tmp$`2018`$rel_density, by = c("Region", "Industry"))
merged_2017_2018 <- merge(merged_2017_2018, tmp$`2018`$KCI, by = "Region")
merged_2017_2018 <- merge(merged_2017_2018, tmp$`2018`$TCI, by = "Industry")

merged_2016_2017 <- merge(entry_2016_2017, tmp$`2017`$rel_density, by = c("Region", "Industry"))
merged_2016_2017 <- merge(merged_2016_2017, tmp$`2017`$KCI, by = "Region")
merged_2016_2017 <- merge(merged_2016_2017, tmp$`2017`$TCI, by = "Industry")

merged_2015_2016 <- merge(entry_2015_2016, tmp$`2016`$rel_density, by = c("Region", "Industry"))
merged_2015_2016 <- merge(merged_2015_2016, tmp$`2016`$KCI, by = "Region")
merged_2015_2016 <- merge(merged_2015_2016, tmp$`2016`$TCI, by = "Industry")

merged_2014_2015 <- merge(entry_2014_2015, tmp$`2015`$rel_density, by = c("Region", "Industry"))
merged_2014_2015 <- merge(merged_2014_2015, tmp$`2015`$KCI, by = "Region")
merged_2014_2015 <- merge(merged_2014_2015, tmp$`2015`$TCI, by = "Industry")

merged_2013_2014 <- merge(entry_2013_2014, tmp$`2014`$rel_density, by = c("Region", "Industry"))
merged_2013_2014 <- merge(merged_2013_2014, tmp$`2014`$KCI, by = "Region")
merged_2013_2014 <- merge(merged_2013_2014, tmp$`2014`$TCI, by = "Industry")




all_merged <- bind_rows(
  merged_2023_2024,
  merged_2022_2023,
  merged_2021_2022,
  merged_2020_2021,
  merged_2019_2020,
  merged_2018_2019,
  merged_2017_2018,
  merged_2016_2017,
  merged_2015_2016,
  merged_2014_2015,
  merged_2013_2014
) %>% 
  as_tibble()



