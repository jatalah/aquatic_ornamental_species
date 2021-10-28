library(tidyverse)

var_import_current <- read_csv('tables/var_imp_current.csv')


figure_6 <- 
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
  labs(y = "Permutation importance (%)", x = '20 prioritised species') +
  scale_fill_discrete(name = NULL, guide = F) +
  scale_size(guide = F) +
  scale_color_discrete(guide = F) +
  theme_javier() + 
  theme(axis.text.y = element_text(face = "italic")) 

figure_6

ggsave(figure_6,
       filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_6.png',
       device = 'png',
       width = 9,
       height = 6)

ggsave(figure_6,
       filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_6.svg',
       device = 'svg',
       width = 9,
       height = 6)

ggsave(figure_6,
       filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_6.tiff',
       device = 'tiff',
       compression = 'lzw',
       width = 9,
       height = 6)
