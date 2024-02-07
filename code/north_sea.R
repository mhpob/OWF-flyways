library(sf)
library(ggplot2)

herring <- read_sf('herring_corten.gpkg')
mackerel <- read_sf('mackerel_daan.gpkg')
whiting <- read_sf('whiting_gonzalez-irusta.gpkg')

wind_europe <- read.csv('OWF Names_data.csv') |> 
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
geom_sf(data = plot_dat, aes(color = species, lty = season)) +
  scale_linetype(na.value = 'solid') +
  coord_sf(xlim = c(-5, 10), ylim = c(49, 62.5)) +
  theme_minimal()

plot(x = c(-5, 10),type = 'n', y = c(49, 62.5), asp=1)
plot(st_geometry(land),
     add = T)
north_sea <- land |> 
  st_transform(32631) 

ggplot(north_sea) +
  geom_sf() +
  coord_sf(xlim = c(-2e5, 9e5), ylim = c(5.45e6, 7e6))

?north_sea |> 
  st_crop(c(xmin = -2e5, xmax = 9e5, ymin = 5.45e6, ymax= 8.2e6)) |> 
  st_geometry() |> 
  plot()
