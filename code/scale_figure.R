library(data.table)
library(ggplot2)

scale <- fread('data/scale_figure_data.csv')
scale$label <- gsub(' n ', '\n', scale$label)

label_loc <- copy(scale)
label_loc[grepl('Pop', label), 'space_min'] <- 
  label_loc[grepl('Pop', label), 'space_max']
label_loc[grepl('EMF', label), 'time_min'] <- 
  label_loc[grepl('EMF', label), 'time_max']


scale_fig <-
  ggplot() +
  geom_rect(data = scale,
            aes(xmin = space_min/(1000^2), xmax = space_max/(1000^2),
                ymin = time_min, ymax = time_max, color = label,
                linetype = type),
            fill = NA, linewidth = 2,
            show.legend = F) +
  geom_label(data = label_loc,
             aes(x = space_min/(1000^2), y = time_min,
                 label = label, color = label),
             show.legend = F,
             hjust = c(rep(0, 2), 1, rep(0, 4)),
             vjust = c(rep(0, 5), 1, 0)) +
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
agg_png("figures/figure1.png",
        width = 170,
        height = 180,
        units = 'mm',
        res = 500,
        scaling = 1.3)

scale_fig

dev.off()
