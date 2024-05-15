library(sf)
library(ggplot2)
library(dplyr)

land <- '/vsizip/data/spatial/ne_10m_land.zip/ne_10m_land.shp' |> 
  read_sf()

ch_wind_capacity <- read.csv('data/ch_wind_capacity.csv')

provinces <- '/vsizip/data/spatial/chn_adm_ocha_2020_shp.zip/chn_admbnda_adm1_ocha_2020.shp' |> 
  read_sf() |> 
  mutate(Province = gsub(' .*', '', ADM1_EN)) |> 
  right_join(ch_wind_capacity)

turtle_sites <- st_read('data/spatial/turtle_flyway.gpkg',
                        layer = 'turtle_flyway') |> 
  mutate(species = ifelse(species == 'hawkbill', 'hawksbill',species))
turtle_paths <- st_read('data/spatial/turtle_loops.gpkg',
                        layer = 'migration_loops')
turtle_paths[c(1, 5), 'geom'] <- st_reverse(turtle_paths[c(5, 1), 'geom'])

turtle_paths <- turtle_paths |> 
  mutate(type = rep(c('foraging', 'nesting'), each = 3))

ggplot() +
  geom_sf(data = land) +
  geom_sf(data = provinces,
          aes(fill = Capacity)) +
  geom_sf_label(data = provinces,
          aes(label = Province),
          nudge_x = -2, nudge_y = c(0,0,-1,0,0,0,0,0,0)) +
  geom_sf(data = turtle_sites,
          aes(color = species, shape = type), size = 4) +
  scale_color_brewer(type = 'qual', palette = 'Set1',
                     guide =  guide_legend(order = 1)) +
  geom_sf(data = turtle_paths, arrow = arrow(length = unit(0.1, "inches")),
          aes(linetype = type)) +
  scale_fill_viridis_c(option = 'B', trans = 'log', breaks = c(50, 500, 3000)) +
  coord_sf(xlim = c(107, 135), ylim = c(15, 35)) +
  labs(x = NULL, y = NULL, color = 'Species', linetype = NULL, shape = NULL,
       fill = 'Capacity (MW)') +
  theme_minimal()+
  theme(legend.key.width = unit(2, 'line'),
        legend.position =  c(0.9, 0.33),
        legend.text = element_text(size =12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12))+
  guides(linetype = guide_legend(order = 2,
                                 override.aes = list(alpha = 0.7)),
         shape = guide_legend(order  = 2))
