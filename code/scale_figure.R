library(data.table)
library(ggplot2)

scale <- fread('data/scale_figure_data.csv')
scale$label <- gsub(' n ', '\n', scale$label)
scale <- scale[1:5]
scale[grepl('Fishing fleets', label), 'label'] <- 'Migration,\npop. dynamics'

scale_fig <-
  ggplot() +
  geom_rect(data = scale[!grepl('pop.', label)],
            aes(xmin = space_min/(1000^2), xmax = space_max/(1000^2),
                ymin = time_min, ymax = time_max, color = label),
            fill = NA, linewidth = 2,
            show.legend = F) +
  geom_label(data = scale[!grepl('pop.', label)],
             aes(x = space_min/(1000^2), y = time_min,
                 label = label, color = label),
             show.legend = F,
             hjust = 0,
             vjust = 0) +
  geom_rect(data = scale[grepl('pop.', label)],
            aes(xmin = space_min/(1000^2), xmax = space_max/(1000^2),
                ymin = time_min, ymax = time_max),
            color = 'black', 
            fill = NA, linewidth = 3,
            show.legend = F) +
  geom_label(data = scale[grepl('pop.', label)],
             aes(x = space_min/(1000^2), y = time_min,
                 label = label),
             fontface = 'bold',
             show.legend = F,
             hjust = 0,
             vjust = 0) +
  scale_y_log10(breaks = c(60, 60*60, 60*60*24, 60*60*24*7, 60*60*24*7*4.5,
                           60*60*24*7*52, 60*60*24*7*52*10,
                           60*60*24*7*52*10*10),
                labels = c('min', 'hour', 'day', 'week', 'month', 'year',
                           'decade', 'century')) +
  scale_x_log10(breaks = 10 ^ seq(1, 13, 2)) +
  labs(x = bquote(m^2), y = NULL) +
  scale_color_brewer(type = 'qual',
                     palette = 'Dark2') +
  scale_linetype_manual(values = c('dashed', 'solid')) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), 'mm'))

library(ragg)
# get dpi, starting with mm
# https://www.tidyverse.org/blog/2020/08/taking-control-of-plot-scaling/
scale_dpi <- function(w, h, r){
  list(
    width = ceiling(w / 25.4 * r),
    height = ceiling(h / 25.4 * r),
    res = r
  )
}
scales <- scale_dpi(w = 85, h = 90, r = 300)


agg_png("ms_figures/v3_figure1.png",
        width = scales$width,
        height = scales$height,
        units = 'px',
        res = scales$res,
        scaling = 0.65)

scale_fig

dev.off()
