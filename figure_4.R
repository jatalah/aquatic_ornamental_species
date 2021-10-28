library(tidyverse)
source('scripts/theme_javier.R')
d <- read_csv('data/cleaned_data/temp_tolerance_data.csv') %>% 
  mutate(n = if_else(is.na(n) & source =="Lit" ,0,n))
top20 <- 
  read_csv('outputs/top_20_taxa.csv') %>% 
  mutate(rank = 1:20) 

fig_4 <- 
  ggplot(d, aes(fct_rev(fct_relevel(Species, top20$Taxa)), color = source)) +
  geom_linerange(
    aes(min = low, ymax = high),
    size = 3,
    position = position_dodge(width = 1),
    alpha = .8
  ) +
  annotate(
    "rect",
    xmin = 0,
    xmax = 21,
    ymin = 7.8,
    ymax = 18.2,
    alpha = 0.4,
    fill = "yellow"
  ) +
  annotate(
    "rect",
    xmin = 0,
    xmax = 21,
    ymin = 2,
    ymax = 24,
    alpha = 0.2,
    fill = "green"
  ) +
  geom_hline(yintercept = c(7.8, 18.2), lty = 2) +
  geom_hline(yintercept = c(2, 24), lty = 2) +
  coord_flip() +
  theme_javier() +
  theme(axis.text.y = element_text(face = 'italic')) +
  geom_text(aes(y = 45, label =n),
            color = 1,
            size = 3) +
  labs(y = "Temperature (°C)", x = "20 prioritised species") +
  geom_linerange(
    aes(min = low, ymax = high),
    size = 3,
    position = position_dodge(width = 1),
    alpha = .8
  ) +
  scale_color_discrete(guide = F)

fig_4

ggsave(fig_4, filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_4.png',
       device = 'png',
       width = 6,
       height = 6
)

ggsave(fig_4, filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_4.svg',
       device = 'svg',
       width = 6,
       height = 6
)

ggsave(fig_4, filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_4.tiff',
       dpi = 900,
       device = 'tiff',
       compression = 'lzw',
       width = 6,
       height = 6
)
