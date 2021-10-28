library(tidyverse)
ranking_ornamental <- read_csv('outputs/ornamental_ranking.csv')

# Lollipop plot ------------
figure_3 <- 
  ranking_ornamental %>% 
  top_n(n = 20,wt = desc(Overall_ranking)) %>% 
  ggplot(aes(
    x = fct_reorder(Taxa,desc(Overall_ranking)),
    y = Quantity_sum,
    label = scales::comma(round(Quantity_sum, 0)),
    color = Frequency_sum
  )) +
  geom_segment(aes(
    x = fct_reorder(Taxa,desc(Overall_ranking)),
    xend = fct_reorder(Taxa,desc(Overall_ranking)),
    y = 0,
    yend = Quantity_sum
  )) +
  geom_point(aes(size = Quantity_sum), alpha = 0.6) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(.85,.2),
    axis.text.y = element_text(face = "italic")
  ) +
  xlab("") +
  scale_color_viridis_c(trans = 'log10', name = "Import frequency", end = .95) +
  scale_size_area(guide = 'none', max_size = 50, trans = 'identity') +
  coord_flip() +
  labs(x = '20 prioritised species', y = 'Total import quantity') +
  scale_y_log10(labels = scales::comma)  +
  geom_text(color = 1, size = 3)

figure_3

# Save plot -----
ggsave(
  figure_3,
  filename = "C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_3.png",
  width = 7,
  height = 7
)

ggsave(
  figure_3,
  filename = "C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_3.svg",
  width = 7,
  height = 7
)

ggsave(
  figure_3,
  filename = "C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_3.tiff",
  width = 7,
  compression = 'lzw',
  dpi = 900,
  height = 7
)
