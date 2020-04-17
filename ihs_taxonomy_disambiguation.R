ihs <- read_csv('data/cleaned_data/ihs/ihs_data.csv')

# match taxa with gbif database names----------------
sp_check_ihs <-
  gnr_resolve(
    ihs$`Valid scientific name`,
    data_source_ids = c(11, 9, 1, 12),
    best_match_only = T,
    fields = "all",
    canonical = T
  )

sp_check_ihs %>% 
  group_by(match_value) %>% 
  summarise(n = n())

sp_check_ihs %>%
  filter(match_value == "Could only match genus") %>%
  select(user_supplied_name, matched_name2 , match_value) %>% 
  print(n = 100)

# sp_check_ihs %>% 
#   filter(duplicated(matched_name2))

distinct_ihs <- 
  sp_check_ihs %>% 
  distinct(matched_name2)

# match taxa with gbif database names----------------
gbif_taxo_ihs <- get_gbifid_(distinct_ihs$matched_name2, rows = 1)

gbif_taxo_ihs_df <-
  bind_rows(gbif_taxo_ihs, .id = "matched_name2") %>%
  write_csv('data/cleaned_data/gbif_taxonomy_ihs_data.csv')


# explore species taxonomy matching ---
gbif_taxo_ihs_df %>% 
  group_by(status) %>% 
  summarise(n = n())

gbif_taxo_ihs_df %>% 
  group_by(rank) %>% 
  summarise(n = n())

gbif_taxo_ihs_df %>% 
  group_by(matchtype) %>% 
  summarise(n = n())

gbif_taxo_ihs_df %>% 
  group_by(kingdom) %>% 
  summarise(n = n())


distinct_taxa_import <- 
  import_all_clean %>% 
  distinct(species) 

distinct_ihs_clean_taxa <- 
  gbif_taxo_ihs_df %>%
  mutate(
    taxa = if_else(is.na(species), paste0(genus, " sp."), species)
  ) %>%
  distinct(taxa)


distinct_ihs_taxa <- 
  ihs %>%
  mutate(taxa =  `Valid scientific name`) %>%
  distinct(taxa)


distinct_ornamental_taxa <- 
  read_csv('data/cleaned_data/ornamental_data.csv') %>%
  distinct(taxa)

mismatched_taxa <- 
  anti_join(distinct_ornamental_taxa, distinct_ihs_clean_taxa, by = 'taxa') %>% 
  write_csv('data/cleaned_data/ihs/mistmatched_import_ihs_taxa.csv')
