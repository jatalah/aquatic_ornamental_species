library(tidyverse)
library(ggpubr)
library(svglite)
ornamental_hab <- read_csv('data/cleaned_data/ornamental_habitat_data.csv')
source('scripts/theme_javier.R')

orn_hab_long <-
  ornamental_hab %>%
  mutate(habitat = fct_collapse(habitat, Freshwater = "Anodromus")) %>% 
  gather(key, value, Quantity:Frequency) 

p1 <- 
  orn_hab_long %>%
  mutate(key = fct_relevel(key, "Quantity")) %>%
  filter(key == "Frequency") %>% 
  ggplot() +
  geom_boxplot(aes(Phylum, value, fill = habitat), alpha = 0.7) +
  # geom_text(data = S_labs, aes(x = Phylum, y = Inf, label = S, hjust = 3)) +
  scale_y_log10(breaks = c(1, 10, 1e2, 1e3, 1e4),
                labels = scales::comma_format(accuracy = 1)) +
  coord_flip() +
  scale_fill_manual(name = NULL, values = c('gray60','transparent')) +
  theme_javier() +
  labs(y = "Frequency (no. of consignments)") +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())


p2 <- 
  orn_hab_long %>%
  mutate(key = fct_relevel(key, "Quantity")) %>%
  filter(key == "Quantity") %>% 
  ggplot() +
  geom_boxplot(aes(Phylum, value, fill = habitat), alpha = 0.7) +
  # geom_text(data = S_labs, aes(x = Phylum, y = Inf, label = S, hjust = 3)) +
  scale_y_log10(breaks = c(1, 10, 1e2, 1e3, 1e4),
                labels = scales::comma_format(accuracy = 1)) +
  coord_flip() +
  scale_fill_manual(name = NULL, values = c('gray60','transparent')) +
  theme_javier() +
  labs(y = "Quantitity (no. of individuals)", x = "Phylum")


p3 <- 
  orn_hab_long %>% 
  distinct(Taxa, habitat, Phylum) %>% 
  count(Phylum, habitat) %>% 
  ggplot(aes(Phylum, n, fill  = habitat)) +
  geom_col(position = position_dodge(width = 1.03), color = 1,  alpha = 0.7) +
  coord_flip() +
  theme_javier() +
  scale_fill_manual(name = NULL, values = c('gray60','transparent')) +
  labs(y = "No. of taxa") +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())


fig_2 <-
  ggarrange(
    p2,
    p1,
    p3,
    common.legend = T,
    legend = 'bottom',
    ncol = 3,
    widths = c(.4, .3, .3),
    labels = "AUTO",
    font.label = list(size = 12, face = "plain"),
    label.x = c(.85,.82,.82),
    label.y = .99
  )

fig_2


ggsave(fig_2, filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_2.png',
       device = 'png',
       width = 10*.8,
       height = 5*.8
       )

ggsave(fig_2, filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_2.svg',
       device = 'svg',
       width = 10*.8,
       height = 5*.8
)

ggsave(
  fig_2,
  filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_2.tiff',
  device = 'tiff',
  compression = 'lzw',
  dpi = 900,
  width = 10 * .8,
  height = 5 * .8
)

