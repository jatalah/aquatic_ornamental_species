library(tidyverse)
library(ggpubr)
source('theme_javier.R')

# read models and results ---
sdms_current <- readRDS('outputs/sdm_current_res.RDS')

# read top20 taxa order -----
top20 <- 
  read_csv('data/cleaned_data/top_20_taxa.csv') %>% 
  mutate(rank = 1:20)


# Model results results ----
res_sdm_current <- 
  sdms_current %>% 
  dplyr::select(Taxa, res) %>%
  unnest(cols = res) %>% 
  write_csv('tables/sdm_results_current.csv')


# Habitat suitability threshold-------
thres_current <- 
  res_sdm_current %>% 
  filter(variable =="Maximum.training.sensitivity.plus.specificity.Cloglog.threshold") %>% 
  dplyr::select(Taxa, V1) %>% 
  write_csv('tables/thresholds_current.csv')

# Calculate habitat suitability for each species-----------
prop_suit <- 
  sdms_current %>% 
  dplyr::select(Taxa, nz_pred_df) %>% 
  unnest(c(nz_pred_df)) %>% 
  drop_na() %>% 
  left_join(thres_current) %>% 
  group_by(Taxa) %>% 
  mutate(Suitability = if_else(layer>V1, "Yes", "No")) %>% 
  count(Suitability) %>% 
  pivot_wider(values_from = n, names_from = Suitability) %>% 
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>% 
  mutate(prop = Yes / (Yes + No) * 100) %>% 
  ungroup() %>% 
  left_join(., top20) %>% 
  arrange(rank) %>% 
  write_csv('tables/prop_suitability_current.csv')

prop_suit <- read_csv('tables/prop_suitability_current.csv')


# Variable importance ---
var_import_current <-
  sdms_current %>%
  dplyr::select(Taxa, var_imp) %>%
  unnest(cols = var_imp) %>%
  mutate(
    variable = fct_recode(
      variable,
      `Max. temp.` = "WC_bio5",
      `Min. temp.` = "WC_bio6",
      Precipitation = "WC_bio12",
      Altitute = "WC_alt"
    )
  ) %>%
  write_csv('tables/var_imp_current.csv')

# var. imp. summaries---
var_import_current %>% 
 group_by(variable) %>% 
  summarise_at(vars(permutation.importance), mean)

var_import_current %>%
  group_by(variable, Taxa) %>%
  summarise_at(vars(permutation.importance), mean) %>%
  pivot_wider(names_from = variable, values_from = permutation.importance) %>% 
  View()


# plot variable importance ----
var_imp_plot <- 
  ggplot(var_import_current,
         aes(variable, permutation.importance, fill = variable)) +
  geom_col(
    position = position_dodge(),
    width = .9,
    colour = "black",
    alpha = 0.5
  ) +
  coord_flip() +
  facet_wrap(~Taxa) +
  theme_javier() +
  theme(strip.text = element_text(face = 'italic')) +
  labs(x = "", y = "Percent contribution (%)") +
  scale_fill_discrete(name = NULL, guide = F)


var_imp_plot2 <- 
ggplot(
  var_import_current,
  aes(
    fct_rev(fct_relevel(Taxa, top20$Taxa)),
    permutation.importance,
    color = variable,
    label = scales::percent(permutation.importance/100, accuracy = NULL)
  )
) +
  geom_point(aes(size = permutation.importance),alpha = 0.6) +
  geom_segment(aes(
    xend = Taxa,
    x = Taxa,
    y = 0,
    yend = permutation.importance
  )) +
  # geom_text(color = 1, size = 3) +
  coord_flip() +
  facet_wrap( ~ variable, nrow = 1) +
  labs(x = "", y = "Permutation importance (%)") +
  scale_fill_discrete(name = NULL, guide = F) +
  scale_size(guide = F) +
  scale_color_discrete(guide = F) +
  theme_javier() + 
  theme(axis.text.y = element_text(face = "italic")) 


ggsave(var_imp_plot2,
       filename = 'figures/var_imp_plot.png',
       device = 'png',
       width = 9,
       height = 6)


var_import_current %>% 
  group_by(variable) %>% 
  summarise_at(vars(percent.contribution, permutation.importance), mean)



# NZ prediction plots -------------
nz_pred_maps <-
  ggarrange(
    plotlist = sdms_current$nz_pred_map,
    nrow = 4,
    ncol = 5,
    common.legend = T, 
    legend = 'right', 
    labels = scales::percent(prop_suit$prop/100,accuracy = NULL),
    label.x = 0.25, 
    label.y = 0.7,
    font.label = list(face = 'plain', size = 12)
  )
nz_pred_maps

# Save prediction maps------
ggsave(nz_pred_maps,
       filename = 'figures/nz_pred_maps.png',
       device = 'png',
       width = 12.5,
       height = 10)




