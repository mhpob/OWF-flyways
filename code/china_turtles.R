library(sf)
library(ggplot2)
library(dplyr)

land <- 'c:/users/darpa2/analysis/rtwb-flyway/data/spatial/coastline.gpkg' |> 
  read_sf()

ch_wind_capacity <- read.csv('data/ch_wind_capacity.csv')

provinces <- '/vsizip/data/spatial/chn_adm_ocha_2020_shp.zip/chn_admbnda_adm1_ocha_2020.shp' |> 
  read_sf() |> 
  mutate(Province = gsub(' .*', '', ADM1_EN)) |> 
  right_join(ch_wind_capacity)

turtle_sites <- st_read('data/spatial/turtle_flyway.gpkg',
                        layer = 'turtle_flyway')
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
          nudge_x = -2,
          nudge_y = -0.5) +
  geom_sf(data = turtle_sites,
          aes(color = species, shape = type)) +
  geom_sf(data = turtle_paths, arrow = arrow(length = unit(0.1, "inches")),
          aes(color = type)) +
  scale_fill_viridis_c(option = 'B', trans = 'log') +
  coord_sf(xlim = c(107, 135), ylim = c(15, 35)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()
