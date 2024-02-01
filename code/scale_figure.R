library(data.table)
library(ggplot2)

scale <- fread('data/scale_figure_data.csv')
scale$label <- gsub(' n ', '\n', scale$label)


ggplot(data = scale) +
  geom_rect(aes(xmin = space_min, xmax = space_max,
                ymin = time_min, ymax = time_max, color = label),
            fill = NA, linewidth = 2,
            show.legend = F) +
  geom_label(aes(x = space_min,y = time_min, label = label, color = label),
             show.legend = F, hjust = 0, vjust = 0) +
  scale_y_log10(breaks = c(60, 60*60, 60*60*24, 60*60*24*7, 60*60*24*7*4.5,
                           60*60*24*7*52, 60*60*24*7*52*10,
                           60*60*24*7*52*10*10),
                labels = c('min', 'hour', 'day', 'week', 'month', 'year',
                           'decade', 'century')) +
  scale_x_log10(breaks = 10 ^ seq(2, 18, 2)) +
  labs(x = bquote(mm^2), y = NULL) +
  theme_minimal()
