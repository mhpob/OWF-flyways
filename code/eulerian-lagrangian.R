library(sf)
library(ggplot2)

leases <- read_sf('https://services7.arcgis.com/G5Ma95RzqJRPKsWL/ArcGIS/rest/services/Wind_Lease_Boundaries__BOEM_/FeatureServer/8/query?where=1=1&f=pjson&token=') |> 
  st_transform("+proj=longlat")

lease_plan <- read_sf('https://services7.arcgis.com/G5Ma95RzqJRPKsWL/ArcGIS/rest/services/Wind_Planning_Area_Boundaries__BOEM_/FeatureServer/0/query?where=1%3D1&f=pjson&token=')


ggplot() +
  geom_sf(data = leases) +
  geom_sf(data  = lease_plan) +
  coord_sf(xlim = c(-77, -65),
           ylim = c(35, 45),
           crs = 4326)

library(OpenStreetMap)

esri <- openmap(c(45, -78), c(32, -68), type = 'esri-terrain') |> 
  openproj()



leases_df <- st_coordinates(leases) |> 
  as.data.frame()
idx <- st_bbox(c(xmin = -77.3787, xmax = -68.509, ymin = 35.748, ymax = 43.186),
        crs = 4326) |> 
  st_as_sfc() |> 
  st_intersects(lease_plan |> 
                  st_transform(4326) |> 
                  st_make_valid())
lease_plan_df <- lease_plan |> 
  st_transform(4326) |> 
  _[unlist(idx), ] |> 
  st_coordinates() |> 
  as.data.frame()

mab <- '/vsizip/vsicurl/https://cmgds.marine.usgs.gov/data/whcmsc/data-release/doi-P999PY84/MiddleAtlanticBight/data/MAB_95th_perc.zip/MAB_95th_perc/MAB_95th_perc.shp' |> 
  st_read() |> 
  st_union() |>
  st_geometry() |>
  st_coordinates() |> 
  as.data.frame()


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
scales <- scale_dpi(w = 85, h = 95, r = 500)

agg_png("figures/figure2.png",
        width = scales$width,
        height = scales$height,
        units = 'px',
        res = scales$res,
        scaling = 1)

autoplot.OpenStreetMap(esri) +
  geom_path(data = leases_df, aes(x = X, y = Y, group = L3)) +
  geom_path(data = lease_plan_df, aes(x = X, y = Y, group = L3)) +
  # geom_path(data = mab,  aes(x = X, y = Y, group = L2)) +
  coord_cartesian(x = c(-76.5, -68), y = c(35, 42.6), expand = F) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.margin = unit(c(0,0,0,0), 'mm')
  )

dev.off()
