library(sf)
library(ggplot2)
library(geomtextpath)

herring <- read_sf('data/spatial/herring_corten.gpkg')
mackerel <- read_sf('data/spatial/mackerel_daan.gpkg')
whiting <- read_sf('data/spatial/whiting_gonzalez-irusta.gpkg')

wind_europe <- read.csv('data/spatial/OWF Names_data.csv') |> 
  st_as_sf(coords = c('GIS.Longitude', 'GIS.Latitude'),
           crs = 4326)

land <- '/vsizip/vsicurl/https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_land.zip' |> 
  read_sf()

plot_dat <- dplyr::bind_rows(herring, mackerel, whiting)

ggplot() +
  geom_sf(data = land) +
  geom_sf(data = wind_europe,
          aes(fill = Status,
              size = Total.Project.Capacity), pch = 21) +
  scale_size(guide = 'none') +
  geom_sf(data = dplyr::bind_rows(herring, mackerel),
              aes(color = species, lty = loop)) +
  geom_textsf(data = dplyr::bind_rows(herring, mackerel),
          aes(color = species, label = season),
          linecolor = NA, vjust = -0.1, size = 3) +
  geom_sf(data = whiting, aes(color = species)) +
  scale_linetype(na.value = 'solid') +
  coord_sf(xlim = c(-5, 10), ylim = c(49, 62.5)) +
  theme_minimal()
