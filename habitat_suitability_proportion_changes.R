library(tidyverse)

# Get habitat suitability -------
thres_current <- read_csv('tables/thresholds_current.csv')

# Read model predictions----
sdm_future_pred <- readRDS('outputs/sdm_future_pred.RDS')


# 1. MIROC-ESM--------------
prop_suit_mr85_2070 <- 
  sdm_future_pred %>% 
  dplyr::select(Taxa, nz_pred_df_mr85_2070) %>% 
  unnest(c(nz_pred_df_mr85_2070)) %>% 
  drop_na() %>% 
  left_join(thres_current) %>% 
  group_by(Taxa) %>% 
  mutate(Suitability = if_else(layer>V1, "Yes", "No")) %>% 
  count(Suitability) %>% 
  pivot_wider(values_from = n, names_from = Suitability) %>% 
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>% 
  transmute(prop = Yes / (Yes + No) * 100) %>% 
  arrange(desc(prop))

prop_suit_mr26_2050 <- 
  sdm_future_pred %>% 
  dplyr::select(Taxa, nz_pred_df_mr26_2050) %>% 
  unnest(c(nz_pred_df_mr26_2050)) %>% 
  drop_na() %>% 
  left_join(thres_current) %>% 
  group_by(Taxa) %>% 
  mutate(Suitability = if_else(layer>V1, "Yes", "No")) %>% 
  count(Suitability) %>% 
  pivot_wider(values_from = n, names_from = Suitability) %>% 
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>% 
  transmute(prop = Yes / (Yes + No) * 100) %>% 
  arrange(desc(prop))


prop_suit_mr26_2070 <- 
  sdm_future_pred %>% 
  dplyr::select(Taxa, nz_pred_df_mr26_2070) %>% 
  unnest(c(nz_pred_df_mr26_2070)) %>% 
  drop_na() %>% 
  left_join(thres_current) %>% 
  group_by(Taxa) %>% 
  mutate(Suitability = if_else(layer>V1, "Yes", "No")) %>% 
  count(Suitability) %>% 
  pivot_wider(values_from = n, names_from = Suitability) %>% 
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>% 
  transmute(prop = Yes / (Yes + No) * 100) %>% 
  arrange(desc(prop))

prop_suit_mr85_2050 <- 
  sdm_future_pred %>% 
  dplyr::select(Taxa, nz_pred_df_mr85_2050) %>% 
  unnest(c(nz_pred_df_mr85_2050)) %>% 
  drop_na() %>% 
  left_join(thres_current) %>% 
  group_by(Taxa) %>% 
  mutate(Suitability = if_else(layer>V1, "Yes", "No")) %>% 
  count(Suitability) %>% 
  pivot_wider(values_from = n, names_from = Suitability) %>% 
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>% 
  transmute(prop = Yes / (Yes + No) * 100) %>% 
  arrange(desc(prop))


# 2. HadGEM2-ES--------------
prop_suit_he85_2070 <- 
  sdm_future_pred %>% 
  dplyr::select(Taxa, nz_pred_df_he85_2070) %>% 
  unnest(c(nz_pred_df_he85_2070)) %>% 
  drop_na() %>% 
  left_join(thres_current) %>% 
  group_by(Taxa) %>% 
  mutate(Suitability = if_else(layer>V1, "Yes", "No")) %>% 
  count(Suitability) %>% 
  pivot_wider(values_from = n, names_from = Suitability) %>% 
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>% 
  transmute(prop = Yes / (Yes + No) * 100) %>% 
  arrange(desc(prop))

prop_suit_he26_2050 <- 
  sdm_future_pred %>% 
  dplyr::select(Taxa, nz_pred_df_he26_2050) %>% 
  unnest(c(nz_pred_df_he26_2050)) %>% 
  drop_na() %>% 
  left_join(thres_current) %>% 
  group_by(Taxa) %>% 
  mutate(Suitability = if_else(layer>V1, "Yes", "No")) %>% 
  count(Suitability) %>% 
  pivot_wider(values_from = n, names_from = Suitability) %>% 
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>% 
  transmute(prop = Yes / (Yes + No) * 100) %>% 
  arrange(desc(prop))


prop_suit_he26_2070 <- 
  sdm_future_pred %>% 
  dplyr::select(Taxa, nz_pred_df_he26_2070) %>% 
  unnest(c(nz_pred_df_he26_2070)) %>% 
  drop_na() %>% 
  left_join(thres_current) %>% 
  group_by(Taxa) %>% 
  mutate(Suitability = if_else(layer>V1, "Yes", "No")) %>% 
  count(Suitability) %>% 
  pivot_wider(values_from = n, names_from = Suitability) %>% 
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>% 
  transmute(prop = Yes / (Yes + No) * 100) %>% 
  arrange(desc(prop))

prop_suit_he85_2050 <- 
  sdm_future_pred %>% 
  dplyr::select(Taxa, nz_pred_df_he85_2050) %>% 
  unnest(c(nz_pred_df_he85_2050)) %>% 
  drop_na() %>% 
  left_join(thres_current) %>% 
  group_by(Taxa) %>% 
  mutate(Suitability = if_else(layer>V1, "Yes", "No")) %>% 
  count(Suitability) %>% 
  pivot_wider(values_from = n, names_from = Suitability) %>% 
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>% 
  transmute(prop = Yes / (Yes + No) * 100) %>% 
  arrange(desc(prop))

# 3. CCSM4-------------
prop_suit_cc85_2070 <- 
  sdm_future_pred %>% 
  dplyr::select(Taxa, nz_pred_df_cc85_2070) %>% 
  unnest(c(nz_pred_df_cc85_2070)) %>% 
  drop_na() %>% 
  left_join(thres_current) %>% 
  group_by(Taxa) %>% 
  mutate(Suitability = if_else(layer>V1, "Yes", "No")) %>% 
  count(Suitability) %>% 
  pivot_wider(values_from = n, names_from = Suitability) %>% 
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>% 
  transmute(prop = Yes / (Yes + No) * 100) %>% 
  arrange(desc(prop))

prop_suit_cc26_2050 <- 
  sdm_future_pred %>% 
  dplyr::select(Taxa, nz_pred_df_cc26_2050) %>% 
  unnest(c(nz_pred_df_cc26_2050)) %>% 
  drop_na() %>% 
  left_join(thres_current) %>% 
  group_by(Taxa) %>% 
  mutate(Suitability = if_else(layer>V1, "Yes", "No")) %>% 
  count(Suitability) %>% 
  pivot_wider(values_from = n, names_from = Suitability) %>% 
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>% 
  transmute(prop = Yes / (Yes + No) * 100) %>% 
  arrange(desc(prop))


prop_suit_cc26_2070 <- 
  sdm_future_pred %>% 
  dplyr::select(Taxa, nz_pred_df_cc26_2070) %>% 
  unnest(c(nz_pred_df_cc26_2070)) %>% 
  drop_na() %>% 
  left_join(thres_current) %>% 
  group_by(Taxa) %>% 
  mutate(Suitability = if_else(layer>V1, "Yes", "No")) %>% 
  count(Suitability) %>% 
  pivot_wider(values_from = n, names_from = Suitability) %>% 
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>% 
  transmute(prop = Yes / (Yes + No) * 100) %>% 
  arrange(desc(prop))

prop_suit_cc85_2050 <- 
  sdm_future_pred %>% 
  dplyr::select(Taxa, nz_pred_df_cc85_2050) %>% 
  unnest(c(nz_pred_df_cc85_2050)) %>% 
  drop_na() %>% 
  left_join(thres_current) %>% 
  group_by(Taxa) %>% 
  mutate(Suitability = if_else(layer>V1, "Yes", "No")) %>% 
  count(Suitability) %>% 
  pivot_wider(values_from = n, names_from = Suitability) %>% 
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>% 
  transmute(prop = Yes / (Yes + No) * 100) %>% 
  arrange(desc(prop))


# read curret suitability data -----
suit_current <- 
  read_csv('tables/prop_suitability_current.csv') %>% 
  dplyr::select(Taxa, prop) %>% 
  rename(prop_current = prop)

suit_all <-
  bind_rows(
    mr26_2050 = prop_suit_mr26_2050,
    mr85_2050 = prop_suit_mr85_2050,
    mr26_2070 = prop_suit_mr26_2070,
    mr85_2070 = prop_suit_mr85_2070,
    he26_2050 = prop_suit_he26_2050,
    he85_2050 = prop_suit_he85_2050,
    he26_2070 = prop_suit_he26_2070,
    he85_2070 = prop_suit_he85_2070,
    cc26_2050 = prop_suit_cc26_2050,
    cc85_2050 = prop_suit_cc85_2050,
    cc26_2070 = prop_suit_cc26_2070,
    cc85_2070 = prop_suit_cc85_2070,
    .id = 'key'
  ) %>%
  mutate(
    year = str_sub(key, -4, -1),
    rcp = str_sub(key, 3, 4),
    model = str_sub(key, 1, 2),
    rcp = fct_recode(rcp, RCP2.6 = "26", RCP8.5 = "85"),
    model = fct_recode(
      model,
      CCSM4 = "cc",
      "HadGEM2-ES" = 'he',
      "MIROC-ESM" = "mr"
    )
  ) %>%
  left_join(suit_current, by = "Taxa") %>%
  mutate(diff = prop - prop_current) %>% 
  write_csv('tables/suitability_changes_predictions.csv')
