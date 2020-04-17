library(tidyverse)
library(readxl)
library(taxize)

# data read and cleaning --------
ihs <-
  bind_rows(
    Freshwater = read_excel('data/IHS_taxa.xlsx', 1) ,
    Marine = read_excel('data/IHS_taxa.xlsx', 2),
    Invertebrates = read_excel('data/IHS_taxa.xlsx', 3),
    .id = 'Group'
  ) %>%
  drop_na(`Valid scientific name`) %>%
  mutate(
    CITES = str_detect(`Valid scientific name`, '\\?'),
    S3 = str_detect(`Valid scientific name`, '\\*'),
    Num = str_detect(`Valid scientific name`, "\\d"),
    `Valid scientific name` = str_replace_all(`Valid scientific name`, "[[:punct:]]", ""),
    `Valid scientific name` = if_else(Num==TRUE, str_replace_all(`Valid scientific name`,"(?s) .*",""), `Valid scientific name`),
    `Valid scientific name` = str_trim(`Valid scientific name`),
    Duplicated = if_else(duplicated(`Valid scientific name`), "TRUE","FALSE")
  ) %>% 
  filter(Duplicated == "FALSE") %>% 
  select(-Num, -Duplicated) %>% 
  write_csv('data/cleaned_data/ihs/ihs_data.csv')
