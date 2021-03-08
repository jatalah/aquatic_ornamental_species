library(tidyverse)
source('theme_javier.R')


# Changes in suitability for future climate-------------
suit_all <- read_csv('tables/suitability_changes_predictions.csv')
# read top20 taxa order -----
top20 <- 
  read_csv('data/cleaned_data/top_20_taxa.csv') %>% 
  mutate(rank = 1:20)


# Summarise suitability changes-----------
mean_suit_changes <- 
  suit_all %>% 
  group_by(Taxa, year, rcp) %>% 
  summarise_at(vars(diff, prop), mean) %>% 
  # filter(abs(diff)<2) %>% 
  # distinct(Taxa)
  View()


mean_suit_changes %>% 
  ungroup() %>% 
  filter(abs(diff)<1 & diff!=0) %>% 
  distinct(Taxa)


mean_suit_changes %>% 
  ungroup() %>% 
  filter(diff == 0) %>% 
  distinct(Taxa)

# Changes in habitat suitability plot-----
suit_plot <-    
  ggplot(suit_all, aes(x= fct_rev(fct_relevel(Taxa, top20$Taxa)))) +
  geom_col(aes(y = diff, fill = model),
           position = position_dodge(),
           width = .7,
           alpha = 0.5,
           color = 'gray30'
  ) +
  coord_flip() +
  theme_javier() +
  theme(axis.text.y = element_text(face = 'italic'),
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  labs(y = 'Change in habitat suitability (%)') +
  facet_grid(rcp~year) +
  scale_fill_discrete(name = element_blank())
suit_plot

ggsave(suit_plot,
       filename = 'figures/suit_plot.png',
       device = 'png',
       width = 8,
       height = 6)
