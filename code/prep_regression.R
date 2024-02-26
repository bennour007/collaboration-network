pacman::p_load(tidyverse)

all_merged_regions <- all_merged %>% 
  mutate(
    City = case_when(
      Region %in% c("Eotvos Lorand University", "Eotvos Lorand Research Network",
                        "Corvinus University Budapest", "Obuda University", "Semmelweis University",
                        "University of Veterinary Medicine Budapest", "Pazmany Peter Catholic University",
                        "Budapest Metropolitan University", "Budapest University of Technology & Economics",
                        "Budapest Business University", "Moholy-Nagy University of Art & Design Budapest",
                        "HUN-REN Veterinary Medical Research Institute", "HUN-REN Research Centre for Natural Sciences",
                        "HUN-REN Wigner Research Centre for Physics", "HUN-REN Centre for Social Sciences",
                        "HUN-REN Research Centre for Astronomy & Earth Sciences", "HUN-REN Centre for Ecological Research",
                        "HUN-REN Centre for Energy Research", "HUN-REN Institute of Experimental Medicine",
                        "HUN-REN Centre for Economic & Regional Studies", "HUN-REN Institute for Computer Science & Control",
                        "HUN-REN Alfred Renyi Institute of Mathematics", "HUN-REN Research Centre for the Humanities",
                        "HUN-REN Research Centre for Linguistics") ~ "Budapest",
      Region == "University of Istvan Szechenyi" ~ "Gyor",
      Region %in% c("University of Debrecen", "HUN-REN Institute for Nuclear Research") ~ "Debrecen",
      Region == "University of Pecs" ~ "Pecs",
      Region == "University of Miskolc" ~ "Miskolc",
      Region == "University of Pannonia" ~ "Veszprem",
      Region == "University of Nyiregyhaza" ~ "Nyiregyhaza",
      Region == "HUN-REN Institute of Earth Physics & Space Science" ~ "Sopron",
      Region == "HUN-REN Centre for Agricultural Research" ~ "Martonvasar",
      Region == "HUN-REN Biological Research Center" ~ "Szeged",
      Region == "HUN-REN Balaton Limnological Research Institute" ~ "Tihany",
      TRUE ~ "Unknown" # Default case if none of the above matches
    ),
    NUTS3 = case_when(
      City == "Budapest" ~ "HU110",
      City == "Gyor" ~ "HU221",
      City == "Debrecen" ~ "HU321",
      City == "Pecs" ~ "HU231",
      City == "Miskolc" ~ "HU312",
      City == "Veszprem" ~ "HU222",
      City == "Nyiregyhaza" ~ "HU323",
      City == "Sopron" ~ "HU221",
      City == "Martonvasar" ~ "HU212", # Assuming it falls under Fejér's NUTS3 code for example
      City == "Szeged" ~ "HU333",
      City == "Tihany" ~ "HU222", # Assuming it falls under Veszprém's NUTS3 code as an example
      TRUE ~ "Unknown" # Fallback option
    )
  )

publication_p_year <- wos_aff %>% 
  filter(author_affiliation2 %in% reference_institutions) %>% 
  left_join(wos_paper[,1:2]) %>%
  left_join(wos_sc) %>% 
  select(rowid, author_affiliation2, publication_year, subject_category) %>% 
  count(author_affiliation2, publication_year, subject_category, sort = T, name = "n_pub") %>% 
  mutate(
    author_affiliation2 = as_factor(author_affiliation2)
  ) %>% 
  dplyr::rename(
    Region = author_affiliation2,
    period = publication_year,
    Industry = subject_category
  )


#### read eurostat vars and join them with the data 



# Set the directory containing the CSV files
directory_path <- "data/eurostat"

# List all CSV files in the directory
csv_files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)
# Read all CSV files and store them in a list
eurostat_data_list <- map(csv_files, function(x){ read_csv(x) %>% select(- freq) %>% janitor::clean_names()}) 

eurostat_data_long <- eurostat_data_list %>% 
  map(
    function(x){
      x %>% 
        mutate(
          across(
            x2013:x2022,
            as.numeric
          )
        ) %>% 
        pivot_longer(x2013:x2022, names_to = 'year', values_to = 'es_value') %>% 
        mutate(year = str_remove(year, 'x'))
    }
  ) 

# Create a named vector where the names are the codes and the values are the city names
city_names <- c(
  HU001C = "Budapest",
  HU002C = "Miskolc",
  HU003C = "Nyiregyhaza",
  HU004C = "Pecs",
  HU005C = "Debrecen",
  HU006C = "Szeged",
  HU007C = "Gyor",
  HU008C = "Kecskemet",
  HU009C = "Szekesfehervar",
  HU010C = "Szombathely",
  HU011C = "Szolnok",
  HU012C = "Tatabanya",
  HU013C = "Veszprem",
  HU014C = "Bekescsaba",
  HU015C = "Kaposvar",
  HU016C = "Eger",
  HU017C = "Dunaujvaros",
  HU018C = "Zalaegerszeg",
  HU019C = "Sopron"
)

# Assume your data frame is named 'df' and the column with the city codes is named 'cities_time_period'
eurostat_data_long[[3]] <- eurostat_data_long[[3]] %>%
  mutate(City = city_names[cities_time_period]) %>% 
  select(-cities_time_period)

eurostat_data_long[[1]] <- eurostat_data_long[[1]] %>%
  mutate(City = city_names[cities_time_period]) %>% 
  select(-cities_time_period)


eurostat_data_long[[1]] <- eurostat_data_long[[1]]%>% 
  mutate(
    indic_ur = case_when(
      indic_ur == "TE1026V" ~ "num_students",
      indic_ur == "TE1026I" ~ "share_students",
      indic_ur == "DE1001V" ~ "population",
      TRUE ~ indic_ur # Keeps the original value if none of the conditions above are met
    )
  ) %>% 
  dplyr::rename(indc_students = indic_ur, value_students = es_value)

eurostat_data_long[[3]] <- eurostat_data_long[[3]] %>% 
  mutate(indic_ur = case_when(
    indic_ur == "DE1001V" ~ "population",
    TRUE ~ indic_ur # Keeps the original value if none of the conditions above are met
  ))%>% 
  dplyr::rename(indc_pop = indic_ur, value_pop = es_value)

eurostat_eco_wide <- eurostat_data_long[[2]] %>% 
  pivot_wider(names_from = 'unit', values_from = 'es_value' ) %>% 
  filter(geo_time_period %in% c(all_merged_regions %>% pull(NUTS3) %>%  unique())) %>% 
  dplyr::rename(NUTS3 = geo_time_period) %>% 
  mutate(year = as.numeric(year))



eurostat_merged <- eurostat_data_long[[1]] %>% 
  left_join(eurostat_data_long[[3]], by = c('City', 'year')) %>% 
  pivot_wider(names_from = indc_students, values_from = value_students) %>% 
  pivot_wider(names_from = indc_pop, values_from = value_pop) %>% 
  mutate(year = as.numeric(year))
  

data_full <- all_merged_regions %>% 
  group_by(City, period) %>% 
  left_join(eurostat_merged, by = c('City' = 'City', 'period' = 'year')) %>% 
  left_join(eurostat_eco_wide, by = c('NUTS3' = 'NUTS3', 'period' = 'year')) %>% 
  left_join(publication_p_year) %>% 
  dplyr::rename(
    pps = PPS_EU27_2020_HAB,
    gdp = MIO_EUR,
    gdp_c = EUR_HAB
  )

# 
# 
# all_merged_regions %>% 
#   ggplot() + 
#   geom_histogram(
#     aes(x = log(KCI))
#   )
