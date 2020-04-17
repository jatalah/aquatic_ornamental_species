library(tidyverse)
library(knitr)
source('theme_javier.R')

# read data -----------
ornamental <- 
  read_csv('data/cleaned_data/ornamental_data.csv')


# Summary tables taxonomy ---------------
ornamental %>% 
  summarise_at(vars(Kingdom:Genus, Species, Taxa), n_distinct) %>% 
  kable(caption = "Number of disctinct taxonomic levels in the dataset")


ornamental %>% 
  group_by(Phylum) %>% 
  summarise_at(vars(Taxa), n_distinct) %>% 
  arrange(desc(Taxa)) %>% 
  kable(caption = "Number of Taxa per Phylum")

genus_level <- 
  ornamental %>% 
  filter(str_detect(Taxa, '\\bsp\\b|\\bspp\\b')) %>% 
  distinct(Taxa) %>% 
  nrow()

unique_taxa <- length(unique(ornamental$Taxa))

orn_long <-
  ornamental %>%
  gather(key, value, Quantity:Frequency) 

# Histograms of Frequency and Quantity --------------
ggplot(orn_long) +
  geom_histogram(aes(value, fill = Phylum), alpha = .8) +
  labs(x = '') +
  scale_x_log10(breaks = c(1, 10, 1e2, 1e3, 1e4, 1e5),
                labels = scales::comma) +
  scale_fill_viridis_d() +
  theme_minimal() +
  facet_wrap( ~ key, scale = 'free')



# Boxplots by Phylum--------------
ggplot(ornamental) +
  geom_boxplot(aes(Order, Quantity, fill = Class)) +
  scale_y_log10(breaks = c(1, 10, 1e2, 1e3, 1e4),
                labels = scales::comma) +
  facet_wrap( ~ Phylum, scales = 'free') +
  coord_flip() +
  scale_fill_viridis_d() +
  theme_javier() 


ggplot(ornamental) +
  geom_boxplot(aes(Order, Frequency, fill = Class)) +
  scale_y_log10() +
  facet_wrap( ~ Phylum, scales = 'free') +
  coord_flip() +
  scale_fill_viridis_d() +
  theme_javier() 


# ornamental %>%
#   ggplot(aes(Order, Quantity, color = Phylum) ) +
#   stat_summary(fun = mean, 
#                geom = "point") +
#   stat_summary(fun.data = "mean_se", geom = "errorbar",
#                width = 0.2) +
#   scale_y_log10(breaks = c(1, 10, 1e2, 1e3, 1e4),
#                 labels = scales::comma) +
#   facet_wrap( ~ Phylum, scales = 'free') +
#   labs(x = 'Mean imports per yr', title = 'Imports Frequency by Order per Phylum') +
#   coord_flip() +
#   scale_color_viridis_d(guide =F) +
#   theme_minimal()


# Boxplots by Year --------
ornamental %>%
  drop_na(Year) %>% 
  ggplot() +
  geom_boxplot(aes(factor(Year), Quantity, fill= factor(Phylum))) +
  scale_y_log10(breaks = c(1, 10,  1e2, 1e3, 1e4),
                labels = scales::comma) +
  facet_wrap( ~ Phylum, scales = 'free') +
  scale_fill_viridis_d(guide = F) +
  theme_javier() +
  theme(axis.title.x = element_blank())

ornamental %>%
  drop_na(Year) %>% 
  ggplot() +
  geom_boxplot(aes(factor(Year), Frequency, fill= Phylum)) +
  scale_y_sqrt() +
  facet_wrap( ~ Phylum, scales = 'free') +
  scale_fill_viridis_d(guide = F) +
  theme_javier() +
  theme(axis.title.x = element_blank())

# top 15 species per Phylum -------
ornamental %>%
  group_by(Taxa, Phylum) %>% 
  summarise_at("Quantity", list(mean = mean, sd = sd, se = ~ sd / sqrt(n()))) %>%
  group_by(Phylum) %>%
  top_n(wt = mean, n = 20) %>% 
  ggplot(aes(x = fct_reorder(Taxa,.x =  mean), y = mean, fill = Phylum)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3,
           width = .8) +      # Thinner lines
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  scale_y_log10(breaks = c(1, 10, 1e2, 1e3, 1e4),
                labels = scales::comma) +
  coord_flip() +
  facet_wrap( ~ Phylum, scales = 'free') +
  theme_minimal() +
  theme(axis.text.y = element_text(face = 'italic')) +
  labs(x = "Taxa", y = "Mean number of individulas per Year (2015 - 2019)") +
  scale_fill_viridis_d(name = "Phylum")
  
  
ornamental %>%
  group_by(Taxa, Phylum) %>% 
  summarise_at("Frequency", list(mean = mean, sd = sd, se = ~ sd / sqrt(n()))) %>%
  group_by(Phylum) %>%
  top_n(wt = mean, n = 20) %>% 
  ggplot(aes(x = fct_reorder(Taxa,.x =  mean), y = mean, fill = Phylum)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3,
           width = .8) +      # Thinner lines
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  scale_y_log10(breaks = c(1, 10, 1e2, 1e3, 1e4),
                labels = scales::comma) +
  coord_flip() +
  facet_wrap( ~ Phylum, scales = 'free') +
  theme_minimal() +
  theme(axis.text.y = element_text(face = 'italic')) +
  labs(x = "Taxa", y = "Mean number of individulas per Year (2015 - 2019)") +
  scale_fill_viridis_d(name = "Phylum")


ornamental %>%
  ggplot(aes(Frequency, Quantity, color = Phylum)) +
  geom_point() +
  facet_wrap(~Phylum, scales = 'free') +
  # scale_color_viridis_d() +
  labs(name = "Phylum") +
  theme_javier() +
  scale_y_log10(breaks = c(1, 10, 1e2, 1e3, 1e4),
                labels = scales::comma) 



