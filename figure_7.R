library(tidyverse)
source('theme_javier.R')


# Changes in suitability for future climate-------------
suit_all <- read_csv('tables/suitability_changes_predictions.csv')

# read top-20 species order -----
top20 <- 
  read_csv('outputs/top_20_taxa.csv') %>% 
  mutate(rank = 1:20)


# Changes in habitat suitability plot-----
figure_7 <-    
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
        # axis.title.y = element_blank(),
        legend.position = "bottom") +
  labs(y = 'Change in habitat suitability (%)', x = "20 prioritised species") +
  facet_grid(rcp~year) +
  scale_fill_discrete(name = element_blank())
figure_7

ggsave(figure_7,
       filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_7.png',
       device = 'png',
       width = 8,
       height = 6)

ggsave(figure_7,
       filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_7.svg',
       width = 8,
       height = 6)

ggsave(figure_7,
       filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_7.tiff',
       width = 8,
       dpi = 900,
       compression = 'lzw',
       height = 6)

