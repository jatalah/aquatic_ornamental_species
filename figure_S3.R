library(tidyverse)
cv_res <- read_csv('tables/cv_results.csv')
source('scripts/theme_javier.R')


figure_S3_dat <- 
  cv_res %>% 
  filter(variable == "Test.AUC" | variable =="Training.AUC") %>% 
  mutate(variable = fct_recode(variable, Test = "Test.AUC", Training = "Training.AUC"))

figure_S3_dat %>% 
  group_by(variable) %>% 
  summarise(max= max(`species (average)`),
            min = min(`species (average)`),
            mean = mean(`species (average)`))
  

figure_S3 <-
  ggplot(figure_S3_dat, aes(Taxa, `species (average)`, fill = variable)) +
  geom_col(
    position = position_dodge(width = .7),
    alpha = 0.7,
    color = 'gray30',
    width = .5
  ) +
  coord_flip() +
  scale_fill_discrete(name = NULL) +
  theme_javier() +
  labs(y = "Area under the curve (AUC)", x = '20 prioritised species') +
  theme(axis.text.y = element_text(face = 'italic'))

figure_S3

ggsave(
  figure_S3,
  filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_S3.png',
  device = 'png',
  width = 6,
  height = 6
)


ggsave(
  figure_S3,
  filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_S3.svg',
  width = 6,
  height = 6
)

ggsave(
  figure_S3,
  filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_S3.tiff',
  dpi = 900,
  compression = 'lzw',
  width = 6,
  height = 6
)

