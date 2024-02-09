library(sf)
library(ggplot2)

land <- '/vsizip/vsicurl/https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_land.zip' |> 
  read_sf()

narw_usa <- st_read(
  'data/spatial/whalenorthatlanticright_20160127/whalenorthatlanticright_20160127.shp'
)

# narw_can <- st_read('https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/DFO_SARA_CriticalHabitat/MapServer/0/query?where=Common_Name_EN+like+%27%25North+Atlantic+Right+Whale%25%27&outFields=*&f=geojson')
# st_write(narw_can, 'narw_can_crithab.gpkg')

narw_can <- st_read('narw_can_crithab.gpkg')

ggplot() +
  geom_sf(data = land) +
  geom_sf(data = narw_usa, fill = 'pink') +
  geom_sf(data = narw_can, fill = 'green') +
  coord_sf(xlim = c(-85, -60),
           ylim = c(23, 53))

land_cropped <- st_intersection(
  land,
  st_bbox(c(xmin = -90, xmax = -50,
            ymin = 20, ymax = 60), crs = 4326) |> 
    st_as_sfc()
)

lc_omerc <- land_cropped |> 
  st_transform('+proj=omerc +lat_0=40 +lonc=-74 +gamma=-40')

narw_usa_omerc <- narw_usa |> 
  st_transform('+proj=omerc +lat_0=40 +lonc=-74 +gamma=-40')

narw_can_omerc <- narw_can |> 
  st_transform('+proj=omerc +lat_0=40 +lonc=-74 +gamma=-40')

ggplot() +
  geom_sf(data = mab_omerc, fill = 'lightblue', color = 'blue',
          linewidth = 0.5) +
  geom_sf(data = stopovers, fill = 'gold') +
  geom_sf(data = lc_omerc) +

  geom_sf(data = boem, fill ='red', color = 'red') +
  geom_sf(data = narw_usa_omerc, fill = 'pink') +
  geom_sf(data = narw_can_omerc, fill = 'green') +
  geom_sf(data = whales, aes(color = species),
          linewidth = 1, arrow = arrow(ends = 'both', length = unit(0.2, 'inches')),
          show.legend = F) +
  coord_sf(xlim = c(-2e5, 6.5e5),
           ylim = c(-17e5, 9e5)) +
  theme_minimal()

mab <- '/vsizip/vsicurl/https://cmgds.marine.usgs.gov/data/whcmsc/data-release/doi-P999PY84/MiddleAtlanticBight/data/MAB_95th_perc.zip/MAB_95th_perc/MAB_95th_perc.shp' |> 
  st_read() |> 
  st_union()
mab_omerc <- st_transform(mab, '+proj=omerc +lat_0=40 +lonc=-74 +gamma=-40')
mab_omerc <- st_simplify(mab_omerc, dTolerance = 10000)

boem <-
  '/vsizip/data/spatial/BOEM_Renewable_Energy_Geodatabase_5.zip/BOEMWindLayers_4Download.gdb' |> 
  st_read(layer = 'Wind_Lease_Outlines_11_16_2023') |> 
  st_transform('+proj=omerc +lat_0=40 +lonc=-74 +gamma=-40')

whales <- st_read('data/spatial/whale_movements.gpkg',
                  layer = 'whale_movements') |> 
  st_transform('+proj=omerc +lat_0=40 +lonc=-74 +gamma=-40')
stopovers <- st_read('data/spatial/whale_movements.gpkg',
                     layer = 'stopovers') |> 
  st_transform('+proj=omerc +lat_0=40 +lonc=-74 +gamma=-40') |> 
  st_buffer(1e5)
