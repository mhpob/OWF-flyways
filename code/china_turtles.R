library(sf)
library(ggplot2)
library(dplyr)

land <- '/vsizip/vsicurl/https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_land.zip' |> 
  read_sf()

ch_wind_capacity <- read.csv('data/ch_wind_capacity.csv')

provinces <- '/vsizip/c:/users/darpa2/downloads/chn_adm_ocha_2020_shp.zip/chn_admbnda_adm1_ocha_2020.shp' |> 
  read_sf() |> 
  mutate(Province = gsub(' .*', '', ADM1_EN)) |> 
  right_join(ch_wind_capacity)

turtle_sites <- st_read('data/spatial/turtle_flyway.gpkg',
                        layer = 'turtle_flyway')
turtle_paths <- st_read('data/spatial/turtle_flyway.gpkg',
                        layer = 'migration_loops')

ggplot() +
  geom_sf(data = land) +
  geom_sf(data = provinces,
          aes(fill = Capacity)) +
  geom_sf(data = turtle_sites,
          aes(color = species, shape = type)) +
  geom_sf(data = turtle_paths) +
  scale_fill_viridis_c(option = 'B', trans = 'log') +
  coord_sf(xlim = c(107, 135), ylim = c(15, 35)) +
  theme_minimal()
