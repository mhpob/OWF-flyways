library(sf)
library(ggplot2)

leases <- read_sf('https://services7.arcgis.com/G5Ma95RzqJRPKsWL/ArcGIS/rest/services/Wind_Lease_Boundaries__BOEM_/FeatureServer/0/query?where=1=1&f=pjson&token=') |> 
  st_transform("+proj=longlat")

lease_plan <- read_sf('https://services7.arcgis.com/G5Ma95RzqJRPKsWL/ArcGIS/rest/services/Wind_Planning_Area_Boundaries__BOEM_/FeatureServer/0/query?where=1%3D1&f=pjson&token=')


ggplot() +
  geom_sf(data = leases) +
  geom_sf(data  = lease_plan) +
  coord_sf(xlim = c(-77, -65),
           ylim = c(35, 45),
           crs = 4326)

library(OpenStreetMap)

esri <- openmap(c(43, -77), c(35, -68), type = 'bing') |> 
  openproj()



leases_df <- st_coordinates(leases) |> 
  as.data.frame()
lease_plan_df <- lease_plan |> 
  st_transform(4326) |> 
  _[-35,] |> 
  st_coordinates() |> 
  as.data.frame()

mab <- '/vsizip/vsicurl/https://cmgds.marine.usgs.gov/data/whcmsc/data-release/doi-P999PY84/MiddleAtlanticBight/data/MAB_95th_perc.zip/MAB_95th_perc/MAB_95th_perc.shp' |> 
  st_read() |> 
  st_union() |>
  st_geometry() |> plot()
  st_coordinates() |> 
  as.data.frame()


autoplot.OpenStreetMap(esri) +
  geom_path(data = leases_df, aes(x = X, y = Y, group = L3)) +
  geom_path(data = lease_plan_df, aes(x = X, y = Y, group = L3)) +
  # geom_path(data = mab,  aes(x = X, y = Y, group = L2)) +
  coord_cartesian(x = c(-76.5, -68), y = c(35, 42.6), expand = F) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )

