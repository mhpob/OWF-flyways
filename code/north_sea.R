library(sf)
library(dplyr)
library(ggplot2)
library(geomtextpath)

herring <- read_sf('data/spatial/herring_corten.gpkg')
mackerel <- read_sf('data/spatial/mackerel_daan.gpkg') |> 
  mutate(loop = ifelse(loop == 'west', 'south', loop),
         species = 'Atl. mackerel')
whiting <- read_sf('data/spatial/whiting_gonzalez-irusta.gpkg')

wind_europe <- read.csv('data/spatial/OWF Names_data.csv') |> 
  st_as_sf(coords = c('Avg..Gis.Longitude', 'Gis.Latitude'),
           crs = 4326)

land <- '/vsizip/data/spatial/ne_10m_land.zip/ne_10m_land.shp' |> 
  read_sf() |> 
  st_intersection(st_bbox(c(xmin = -6, xmax = 11, ymin = 48, ymax = 63),
                          crs = st_crs(4326)) |> 
                    st_as_sfc())




plot_dat <- dplyr::bind_rows(herring, mackerel, whiting)

plot_nolegend <-
  ggplot() +
  geom_sf(data = land) +
  geom_sf(data = wind_europe,
          aes(size = Total.Project.Capacity),
          pch = 21, alpha = 0.7,
          show.legend = F) +
  geom_sf(data = whiting, aes(color = species),
          linewidth = 1,
          show.legend = F) +
  geom_sf(data = dplyr::bind_rows(herring, mackerel),
              aes(color = species, lty = loop),
          linewidth = 1,
          show.legend = F) +
  geom_textsf(data = dplyr::bind_rows(herring, mackerel),
          aes(color = species, label = season),
          vjust = -0.1, linecolor = NA,
          show.legend = F) +
  scale_color_brewer(palette = 'Accent',
                     guide = guide_legend(order = 1)) +
  scale_linetype(guide = 'none', na.value = 'solid') +
  scale_size(breaks = c(100, 500, 1000),
             guide = guide_legend(order = 2)) +
  labs(color = 'Species', size = 'OW Capacity (MW)',
       fill = 'OW Status') +
  coord_sf(xlim = c(-5, 10), ylim = c(49, 62.5)) +
  theme_minimal() +
    theme(
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      plot.margin = unit(c(0,0,0,0), 'mm')
    ) +
  guides(fill = guide_legend(override.aes = list(size = 5)))


library(gridExtra)
library(cowplot)
plot_legend <-
  ggplot() +
  geom_sf(data = land) +
  geom_sf(data = wind_europe,
          aes(size = Total.Project.Capacity),
          pch = 21, alpha = 0.7) +
  geom_sf(data = whiting, aes(color = species),
          linewidth = 1) +
  geom_sf(data = dplyr::bind_rows(herring, mackerel),
          aes(color = species, lty = loop),
          linewidth = 1) +
  geom_textsf(data = dplyr::bind_rows(herring, mackerel),
              aes(color = species, label = season),
              vjust = -0.1, linecolor = NA) +
  scale_color_brewer(palette = 'Accent',
                     guide = guide_legend(order = 1)) +
  scale_linetype(guide = 'none', na.value = 'solid') +
  scale_size(breaks = c(100, 500, 1000),
             guide = guide_legend(order = 2)) +
  labs(color = 'Species', size = 'OW Capacity (MW)',
       fill = 'OW Status') +
  coord_sf(xlim = c(-5, 10), ylim = c(49, 62.5)) +
  theme_minimal() +
  theme(
    plot.margin = unit(c(0,0,0,0), 'mm'),
    legend.position = c(0.8, 0.1)
  ) +
  guides(fill = guide_legend(override.aes = list(size = 5)))

plot_legend <- get_legend(plot_legend)

library(ragg)

##630w*789h  0.66
agg_png('fig4.png',
        width = 800,
        height = 800/.95,
        scaling = 1.5)

the_plot

dev.off()


# ggsave('file.eps', device = 'eps')

library(svglite)
svglite(
  'ms_figures/f2_north-sea.svg',
  width = 85 / 25.4,
  height = 130.2233 / 25.4 
)
plot_nolegend
dev.off()

svglite(
  'ms_figures/f2_2_north-sea_legend.svg',
  width = 85 / 25.4,
  height = 129.8932 / 25.4 
)
plot_legend
dev.off()
