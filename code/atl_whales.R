library(sf)
library(basemaps)
library(tidyterra)
library(terra)
library(ggplot2)

base <- basemap_terra(
  ext = st_bbox(
    c(xmin = -86, xmax = -57, ymin = 20, ymax = 55),
    crs = 4326
  ),
  map_service = 'esri',
  map_type = 'world_terrain_base'
) |> 
  project('EPSG:4326')


land <- '/vsizip/data/spatial/ne_10m_land.zip/ne_10m_land.shp' |> 
  read_sf()

narw_usa <- st_read(
  'data/spatial/whalenorthatlanticright_20160127/whalenorthatlanticright_20160127.shp'
)

# narw_can <- st_read('https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/DFO_SARA_CriticalHabitat/MapServer/0/query?where=Common_Name_EN+like+%27%25North+Atlantic+Right+Whale%25%27&outFields=*&f=geojson')
# st_write(narw_can, 'narw_can_crithab.gpkg')

narw_can <- st_read('data/spatial/narw_can_crithab.gpkg')

# ggplot() +
#   geom_spatraster_rgb(data = base) +
#   geom_sf(data = narw_usa, fill = 'pink') +
#   geom_sf(data = narw_can, fill = 'pink') +
#   coord_sf(xlim = c(-85, -60),
#            ylim = c(23, 53)) +
#   theme_minimal() +
#   theme(
#     panel.grid = element_blank()
#   )
# 
# land_cropped <- st_intersection(
#   land,
#   st_bbox(c(xmin = -90, xmax = -50,
#             ymin = 20, ymax = 60), crs = 4326) |> 
#     st_as_sfc()
# )
# 
# lc_omerc <- land_cropped |> 
#   st_transform('+proj=omerc +lat_0=40 +lonc=-74 +gamma=-40')
# 
# narw_usa_omerc <- narw_usa |> 
#   st_transform('+proj=omerc +lat_0=40 +lonc=-74 +gamma=-40')
# 
# narw_can_omerc <- narw_can |> 
#   st_transform('+proj=omerc +lat_0=40 +lonc=-74 +gamma=-40')
# 
# ggplot() +
#   geom_sf(data = mab_omerc, fill = 'lightblue', color = 'blue',
#           linewidth = 0.5) +
#   geom_sf(data = stopovers, fill = 'gold') +
#   geom_sf(data = lc_omerc) +
#   
#   geom_sf(data = boem, fill ='red', color = 'red') +
#   geom_sf(data = narw_usa_omerc, fill = 'pink') +
#   geom_sf(data = narw_can_omerc, fill = 'pink') +
#   geom_sf(data = whales, aes(color = species),
#           linewidth = 1, arrow = arrow(ends = 'both', length = unit(0.2, 'inches')),
#           show.legend = F) +
#   coord_sf(xlim = c(-2e5, 6.5e5),
#            ylim = c(-17e5, 9e5)) +
#   theme_minimal()

mab <- '/vsizip/vsicurl/https://cmgds.marine.usgs.gov/data/whcmsc/data-release/doi-P999PY84/MiddleAtlanticBight/data/MAB_95th_perc.zip/MAB_95th_perc/MAB_95th_perc.shp' |> 
  st_read() |> 
  st_union()
# mab_omerc <- st_transform(mab, '+proj=omerc +lat_0=40 +lonc=-74 +gamma=-40')
# mab_omerc <- st_simplify(mab_omerc, dTolerance = 10000)

boem <-
  '/vsizip/data/spatial/BOEM_Renewable_Energy_Geodatabase_5.zip/BOEMWindLayers_4Download.gdb' |> 
  st_read(layer = 'Wind_Lease_Outlines_11_16_2023') 

whales <- st_read('data/spatial/whale_movements.gpkg',
                  layer = 'whale_movements') 
st_geometry(whales[2,]) <- st_geometry(whales[2, ]) + c(0.5, 0)
st_crs(whales) <- st_crs(4326)
# |> 
# st_transform('+proj=omerc +lat_0=40 +lonc=-74 +gamma=-40')
stopovers <- st_read('data/spatial/whale_movements.gpkg',
                     layer = 'stopovers') |> 
  # st_transform('+proj=omerc +lat_0=40 +lonc=-74 +gamma=-40') |> 
  st_buffer(1e5) 
stopovers <- stopovers |> 
  st_difference(
    land |> 
      st_crop(stopovers) |> 
      st_union()
  )



main <- ggplot() +
  geom_spatraster_rgb(data = base) +
  geom_sf(data = stopovers, fill = 'gold', alpha = 0.7) +
  geom_sf(data = narw_usa, fill = 'pink', alpha = 0.7) +
  geom_sf(data = narw_can, fill = 'pink', alpha = 0.7) +
  geom_sf(data = whales, aes(color = species),
          linewidth = 1,
          arrow = arrow(ends = 'both', length = unit(0.2, 'inches')),
          show.legend = F) +
  scale_color_manual(values = c('red', 'blue')) +
  coord_sf(xlim = c(-80.5, -58),
           ylim = c(26, 45)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.margin = unit(rep(0, 4), 'mm')
  )


leases <- read_sf('https://services7.arcgis.com/G5Ma95RzqJRPKsWL/ArcGIS/rest/services/Wind_Lease_Boundaries__BOEM_/FeatureServer/8/query?where=1=1&f=pjson&token=') |> 
  st_transform("+proj=longlat")

lease_plan <- read_sf('https://services7.arcgis.com/G5Ma95RzqJRPKsWL/ArcGIS/rest/services/Wind_Planning_Area_Boundaries__BOEM_/FeatureServer/0/query?where=1%3D1&f=pjson&token=') |> 
  st_transform("+proj=longlat")

base_inset <- basemap_terra(
  ext = st_bbox(
    c(xmin = -77, xmax = -67, ymin = 35, ymax = 43),
    crs = 4326
  ),
  map_service = 'esri',
  map_type = 'world_terrain_base'
) |> 
  project('EPSG:4326')

inset <-
  ggplot() +
  geom_spatraster_rgb(data = base_inset) +
  geom_sf(data = stopovers, fill = 'gold', alpha = 0.7) +
  geom_sf(data = narw_usa, fill = 'pink', alpha = 0.7) + 
  geom_sf(data = leases, fill = NA, color = 'black',
          linewidth = 0.5) +
  geom_sf(data = lease_plan, fill = NA, color = 'black',
          linewidth = 0.5) +
  geom_sf(data = whales, aes(color = species),
          linewidth = 1, arrow = arrow(ends = 'both', length = unit(0.2, 'inches')),
          show.legend = F) +
  scale_color_manual(values = c('red', 'blue')) +
  coord_sf(xlim = c(-76.5, -69.5),
           ylim = c(36, 42)) +
  theme_void() +
  theme(
    plot.margin = unit(rep(0, 4), 'mm')
  )


library(patchwork)
library(svglite)

svglite(
  'ms_figures/figure4_main.svg',
  width = 85 / 25.4,
  height = 85 / 25.4 / 0.9937888
)

main

dev.off()

svglite(
  'ms_figures/figure4_inset.svg',
  width = 85 / 25.4,
  height = 85 / 25.4 / 0.9051919
)

inset

dev.off()
