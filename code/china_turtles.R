library(dplyr)
library(basemaps)
library(tidyterra)
library(terra)
library(sf)
library(ggplot2)

land <- '/vsizip/data/spatial/ne_10m_land.zip/ne_10m_land.shp' |>
  read_sf()

base <- basemap_terra(
  ext = st_bbox(
    c(xmin = 102, xmax = 140, ymin = 10, ymax = 40),
    crs = 4326
  ),
  map_service = 'esri',
  map_type = 'world_terrain_base'
) |> 
  project('EPSG:4326')

taiwan <- land |> 
  st_crop(
    st_bbox(c(
      xmin = 119.36,
      xmax = 122.3,
      ymin = 21.85,
      ymax = 25.5
    ))
  ) |> 
  _[-c(1,2), ] |> 
  st_combine() |> 
  st_sf(geometry = _) |>
  mutate(Capacity = 2200,
         Province = 'Taiwan')

ch_wind_capacity <- read.csv('data/ch_wind_capacity.csv')

provinces <- '/vsizip/data/spatial/chn_adm_ocha_2020_shp.zip/chn_admbnda_adm1_ocha_2020.shp' |> 
  read_sf() |> 
  mutate(Province = gsub(' .*', '', ADM1_EN)) |> 
  right_join(ch_wind_capacity) |> 
  bind_rows(taiwan)

turtle_sites <- st_read('data/spatial/turtle_flyway.gpkg',
                        layer = 'turtle_flyway') |> 
  mutate(species = ifelse(species == 'hawkbill', 'hawksbill',species))
turtle_paths <- st_read('data/spatial/turtle_loops.gpkg',
                        layer = 'migration_loops')
turtle_paths[c(1, 5), 'geom'] <- st_reverse(turtle_paths[c(5, 1), 'geom'])

turtle_paths <- turtle_paths |> 
  mutate(type = rep(c('foraging', 'nesting'), each = 3))

map <- ggplot() +
  geom_spatraster_rgb(data = base) +
  geom_sf(data = provinces,
          aes(fill = Capacity), alpha = 0.7) +
  geom_sf_label(data = provinces,
          aes(label = Province),
          nudge_x = c(rep(-2, times = 9), 1),
          nudge_y = c(0,0,-1,0,0,0,0,0,0,0)) +
  geom_sf(data = turtle_sites,
          aes(color = species, shape = type), size = 4) +
  scale_color_brewer(type = 'qual', palette = 'Set1',
                     guide =  guide_legend(order = 1)) +
  geom_sf(data = turtle_paths, arrow = arrow(length = unit(0.1, "inches")),
          aes(linetype = type)) +
  scale_fill_viridis_c(option = 'B', trans = 'log',
                       breaks = c(50, 500, 3000)) +
  coord_sf(xlim = c(107, 135), ylim = c(15, 35)) +
  labs(x = NULL, y = NULL, color = 'Species', linetype = NULL, shape = NULL,
       fill = 'Capacity (MW)') +
  theme_minimal()+
  theme(legend.key.width = unit(2, 'line'),
        legend.position =  c(0.898, 0.4),
        # legend.text = element_text(size =12),
        # legend.title = element_text(size = 14),
        # axis.text = element_text(size = 12),
        plot.margin = unit(rep(0,4), 'mm'),
        panel.grid = element_blank()
        )+
  guides(linetype = guide_legend(order = 2,
                                 override.aes = list(alpha = 0.7)),
         shape = guide_legend(order  = 2))


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
# ratio = 1.294425
scales <- scale_dpi(w = 85, h = 66, r = 500)

agg_png("ms_figures/figure3.png",
        width = scales$width,
        height = scales$height,
        units = 'px',
        res = scales$res,
        scaling = 0.5)

map

dev.off()

library(svglite)
svglite("ms_figures/figure3.svg",
        width = 85/25.4,
        height = 66/25.4,
        scaling = 0.5)
map
dev.off()
