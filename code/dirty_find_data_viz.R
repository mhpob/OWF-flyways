owf <- st_read(
  'data/spatial/Global offshore wind turbines dataset_v1.3/GOWF_V1.3.shp'
  )

land <- '/vsizip/vsicurl/https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_land.zip' |> 
  read_sf()
plot(land |> st_transform(st_crs(ire)) |> st_geometry(), add= T)
plot(st_geometry(owf), add = T, col = 'red')

ire <- '/vsizip/c:/users/darpa2/downloads/_ags_wind_turbines.zip' |> 
  read_sf() 
plot(st_geometry(ire), col = 'red')
plot(land |> st_transform(st_crs(ire)) |> st_geometry(), add= T)

dutch <- '/vsizip/c:/users/darpa2/downloads/vergunde_windparken.zip' |> 
  read_sf()
plot(st_geometry(dutch), col = 'red')
plot(land |> st_transform(st_crs(scot)) |> st_geometry(), add= T)


scot <- '/vsizip/c:/users/darpa2/downloads/WINDFARM_SCOTLAND_GPKG_27700.zip/WINDFARM_SCOTLAND.gpkg' |> 
  read_sf()
plot(st_geometry(scot), col = 'red')
