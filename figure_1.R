library(tidyverse)
source('scripts/theme_javier.R')
wos_dat <-
  read_delim(
    "data/cleaned_data/WoS ornamental pubs by year.txt",
    "\t",
    escape_double = FALSE,
    col_types = cols(`Publication Years` = col_double(),
                     `% of 745` = col_skip()),
    trim_ws = TRUE
  )

fig_1 <- 
wos_dat %>%
  filter(`Publication Years` < 2021) %>%
  ggplot(aes(`Publication Years`, records)) + geom_line() +
  theme_bw() +
  labs(x = "Year", y = "Number of publications") +
  theme_javier()

ggsave(fig_1,
       filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_1.png',
       width = 4,
       height= 3)

ggsave(fig_1,
       filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_1.svg',
       width = 4,
       height= 3)

ggsave(fig_1,
       filename = 'C:/Users/javiera/Cawthron/Ornamental fish - what are the risks - General/MS imports and SDMs/figures/figure_1.tiff',
       dpi = 900,
       compression = 'lzw',
       width = 4,
       height= 3)
