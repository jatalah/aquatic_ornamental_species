# Read model predictions----
sdm_future_pred_df <- 
  readRDS('outputs/sdm_future_pred.RDS') %>% 
  dplyr::select(Taxa, contains("df"))

# Sketch all maps -----
future_maps <-
  sdm_future_pred_df %>%
  mutate(
    mr26_2050_map = map2(
      .x = Taxa,
      .y = nz_pred_df_mr26_2050,
      ~ nz_plot_func(data = .y) + ggtitle(.x)
    ),
    mr85_2050_map = map2(
      .x = Taxa,
      .y = nz_pred_df_mr85_2050,
      ~ nz_plot_func(data = .y) + ggtitle(.x)
    ),
    mr26_2070_map = map2(
      .x = Taxa,
      .y = nz_pred_df_mr26_2070,
      ~ nz_plot_func(data = .y) + ggtitle(.x)
    ),
    mr85_2070_map = map2(
      .x = Taxa,
      .y = nz_pred_df_mr85_2070,
      ~ nz_plot_func(data = .y) + ggtitle(.x)
    ),
    he26_2050_map = map2(
      .x = Taxa,
      .y = nz_pred_df_he26_2050,
      ~ nz_plot_func(data = .y) + ggtitle(.x)
    ),
    he85_2050_map = map2(
      .x = Taxa,
      .y = nz_pred_df_he85_2050,
      ~ nz_plot_func(data = .y) + ggtitle(.x)
    ),
    he26_2070_map = map2(
      .x = Taxa,
      .y = nz_pred_df_he26_2070,
      ~ nz_plot_func(data = .y) + ggtitle(.x)
    ),
    he85_2070_map = map2(
      .x = Taxa,
      .y = nz_pred_df_he85_2070,
      ~ nz_plot_func(data = .y) + ggtitle(.x)
    ),
    cc26_2050_map = map2(
      .x = Taxa,
      .y = nz_pred_df_cc26_2050,
      ~ nz_plot_func(data = .y) + ggtitle(.x)
    ),
    cc85_2050_map = map2(
      .x = Taxa,
      .y = nz_pred_df_cc85_2050,
      ~ nz_plot_func(data = .y) + ggtitle(.x)
    ),
    cc26_2070_map = map2(
      .x = Taxa,
      .y = nz_pred_df_cc26_2070,
      ~ nz_plot_func(data = .y) + ggtitle(.x)
    ),
    cc85_2070_map = map2(
      .x = Taxa,
      .y = nz_pred_df_cc85_2070,
      ~ nz_plot_func(data = .y) + ggtitle(.x)
    )
  )

# 2. Create individual maps-------
mr26_2050_map <-
  ggarrange(
    plotlist = future_maps$mr26_2050_map,
    nrow = 4,
    ncol = 5,
    common.legend = T,legend = 'right'
  )

mr85_2050_map <-
  ggarrange(
    plotlist = future_maps$mr85_2050_map,
    nrow = 4,
    ncol = 5,
    common.legend = T,legend = 'right'
  )

mr26_2070_map <-
  ggarrange(
    plotlist = future_maps$mr26_2070_map,
    nrow = 4,
    ncol = 5,
    common.legend = T,legend = 'right'
  )
mr85_2070_map <-
  ggarrange(
    plotlist = future_maps$mr85_2070_map,
    nrow = 4,
    ncol = 5,
    common.legend = T,legend = 'right'
  )

he26_2050_map <-
  ggarrange(
    plotlist = future_maps$he26_2050_map,
    nrow = 4,
    ncol = 5,
    common.legend = T,legend = 'right'
  )

he85_2050_map <-
  ggarrange(
    plotlist = future_maps$he85_2050_map,
    nrow = 4,
    ncol = 5,
    common.legend = T,legend = 'right'
  )

he26_2070_map <-
  ggarrange(
    plotlist = future_maps$he26_2070_map,
    nrow = 4,
    ncol = 5,
    common.legend = T,legend = 'right'
  )
he85_2070_map <-
  ggarrange(
    plotlist = future_maps$he85_2070_map,
    nrow = 4,
    ncol = 5,
    common.legend = T,legend = 'right'
  )

cc26_2050_map <-
  ggarrange(
    plotlist = future_maps$cc26_2050_map,
    nrow = 4,
    ncol = 5,
    common.legend = T,legend = 'right'
  )

cc85_2050_map <-
  ggarrange(
    plotlist = future_maps$cc85_2050_map,
    nrow = 4,
    ncol = 5,
    common.legend = T,legend = 'right'
  )

cc26_2070_map <-
  ggarrange(
    plotlist = future_maps$cc26_2070_map,
    nrow = 4,
    ncol = 5,
    common.legend = T,legend = 'right'
  )
cc85_2070_map <-
  ggarrange(
    plotlist = future_maps$cc85_2070_map,
    nrow = 4,
    ncol = 5,
    common.legend = T,legend = 'right'
  )

# 2. Save individual maps-------
ggsave(mr26_2050_map,
       filename = 'figures/mr26_2050_map.png',
       device = 'png',
       width = 12,
       height = 10)

ggsave(mr85_2050_map,
       filename = 'figures/mr85_2050_map.png',
       device = 'png',
       width = 12,
       height = 10)

ggsave(mr26_2070_map,
       filename = 'figures/mr26_2070_map.png',
       device = 'png',
       width = 12,
       height = 10)

ggsave(mr85_2050_map,
       filename = 'figures/mr85_2070_map.png',
       device = 'png',
       width = 12,
       height = 10)


ggsave(he26_2050_map,
       filename = 'figures/he26_2050_map.png',
       device = 'png',
       width = 12,
       height = 10)

ggsave(he85_2050_map,
       filename = 'figures/he85_2050_map.png',
       device = 'png',
       width = 12,
       height = 10)

ggsave(he26_2070_map,
       filename = 'figures/he26_2070_map.png',
       device = 'png',
       width = 12,
       height = 10)

ggsave(he85_2050_map,
       filename = 'figures/he85_2070_map.png',
       device = 'png',
       width = 12,
       height = 10)

ggsave(cc26_2050_map,
       filename = 'figures/cc26_2050_map.png',
       device = 'png',
       width = 12,
       height = 10)

ggsave(cc85_2050_map,
       filename = 'figures/cc85_2050_map.png',
       device = 'png',
       width = 12,
       height = 10)

ggsave(cc26_2070_map,
       filename = 'figures/cc26_2070_map.png',
       device = 'png',
       width = 12,
       height = 10)

ggsave(cc85_2050_map,
       filename = 'figures/cc85_2070_map.png',
       device = 'png',
       width = 12,
       height = 10)
