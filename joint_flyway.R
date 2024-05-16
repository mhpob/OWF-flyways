library(data.table)
library(ggplot2)
library(qs)

# Striped bass
hud_sb <- qread('data/hud_detects.qs')

setDT(hud_sb)

hud_sb[, yr := year(date.local)]
hud_sb[, week := format(date.local, "%V")]

sb_agg <- hud_sb[, .(mean = mean(lat, na.rm = T),
                     lsd = mean(lat, na.rm = T) - sd(lat, na.rm = T),
                     usd = mean(lat, na.rm = T) + sd(lat, na.rm = T),
                     species = 'striped bass'),
                 by = c('week', 'yr')] 

# pick 2017
# ggplot(data= sb_agg) +
#   geom_errorbar(aes(ymin = lsd, ymax = usd, x =week)) +
#   facet_wrap(~yr) 

sb_agg <- sb_agg[yr == 2017, -'yr']


# Gannet
gannet <- fread('extracted_figures/gannet_extracted.csv',
                col.names = c('date', 'latitude'))
## Last point had no confidence intervals
gannet <- rbind(gannet, gannet[.N], gannet[.N])
gannet[, date := rep(
  seq.Date(as.Date('2012-09-01'), by = '5 days', length.out = 74),
  each = 3)]

setorder(gannet, date, latitude)

gannet[, value := rep(c('lsd', 'mean', 'usd'), times = 74)]

gannet_agg <- gannet[, week := format(date, "%V")] |>
  dcast(week ~ value, value.var = 'latitude', fun.aggregate = mean) |> 
  _[, species := 'gannet']


## NARW
narw <- fread('data/davis-et-al-2017_narw-daily-detection-data.csv')
narw <- narw[NARW_PRESENCE == 1]
narw[, week := format(ANALYSIS_END_ISO, '%V')]
narw[, yr := year(ANALYSIS_END_ISO)]


narw_agg <- narw[, .(mean = mean(RECORDER_LATITUDE, na.rm = T),
                     lsd = mean(RECORDER_LATITUDE, na.rm = T) -
                       sd(RECORDER_LATITUDE, na.rm = T),
                     usd = mean(RECORDER_LATITUDE, na.rm = T) +
                       sd(RECORDER_LATITUDE, na.rm = T),
                     species = 'NARW'),
                 by = c('week', 'yr')] 


# pick 2013
# ggplot(data= narw_agg, aes(x =week)) +
#   geom_errorbar(aes(ymin = lsd, ymax = usd)) +
#   geom_point(aes(y = mean), size = 0.5) +
#   facet_wrap(~yr)

narw_agg <- narw_agg[yr == 2013, -'yr']
#correct missing values
narw_agg <- narw_agg[data.table(week = c(paste0('0', 1:9), 10:52),
                                species = 'NARW'), on = c('week', 'species')]

# Merge
agg <- rbind(sb_agg, gannet_agg, narw_agg)
agg[, week := as.Date('2024-01-01') + as.numeric(week) * 7]

# Same plot, different colors
ggplot(data = agg, aes(x = week, color = species)) +
  geom_errorbar(aes(ymin = lsd, ymax = usd)) +
  geom_point(aes(y = mean)) +
  scale_x_date(date_labels = '%b') +
  labs(x = '', y = 'Latitude') +
  theme_minimal()

# Facet by species
ggplot(data = agg, aes(x = week)) +
  geom_errorbar(aes(ymin = lsd, ymax = usd)) +
  geom_point(aes(y = mean)) +
  facet_wrap(~species, ncol = 1, scales = 'free_y') +
  scale_x_date(date_labels = '%b') +
  labs(x = '', y = 'Latitude') +
  theme_minimal()


# Speeds
setorder(agg, species, week)

# Pointwise lat/week
agg[, spd := (shift(mean, -1) - mean) / 7, by = species]
ggplot(agg) +
  geom_line(aes(x = week, y = spd)) +
  facet_wrap(~species, ncol = 1, scale = 'free_y') +
  scale_x_date(date_labels = '%b') +
  labs(x = NULL, y = 'Latitudinal velocity') +
  theme_minimal()

spd_mo <- copy(agg)
spd_mo[, mon := month(week)]
spd_mo <- spd_mo[, .(mean = mean(mean, na.rm = T)),
                 by = c('mon', 'species')]

spd_mo[, spd := (shift(mean, -1) - mean), by = species]



agg_na <- copy(agg)
agg_na[,lat_before := nafill(mean, "locf")]
# Bring back the next non-missing dist
agg_na[,lat_after := nafill(mean, "nocb")]
# rleid will create groups based on run-lengths of values within the data.
# This means 4 NA's in a row will be grouped together, for example.
# We then count the missings and add 1, because we want the 
# last NA before the next non-missing to be less than the non-missing value.
agg_na[, rle := rleid(mean)][,missings := max(.N +  1 , 2), by = rle] 
agg_na[is.na(mean), ':='(mean = lat_before + .SD[,.I] *
     (lat_after - lat_before)/(missings)), by = rle]
agg_na[,':='(lat_before = NULL,
             lat_after = NULL,
             rle = NULL,
             missings = NULL,
             type = 'interp')]
agg[,type := '']

agg <- rbind(agg, agg_na)
setorder(agg, species, week)

agg[, spd := (shift(mean, -1) - mean) / 7, by = c('species', 'type')]

ggplot() +
  geom_line(data = agg[type == 'interp'],
            aes(x = week, y = spd),
            linetype = 'dashed') +
  geom_line(data = agg[type == ''],
            aes(x = week, y = spd)) +
  facet_wrap(~species, ncol = 1, scale = 'free_y') +
  scale_x_date(date_labels = '%b') +
  labs(x = NULL, y = 'Latitudinal velocity') +
  theme_minimal()


library(sf)

land <- 'data/spatial/simple_coast.gpkg' |> 
  sf::read_sf() 
|> 
  st_cast('LINESTRING')

l <- land |> 
  st_make_valid() |> 
  st_crop(ymin = 30, ymax = 60, xmin = -80, xmax = -50)

## Convert mean lat to linestring spanning the coast (-90 to -50?)
## intersect with land

agg_sf <- copy(agg)

agg_sf[, geom :=
         list(
           list(
             st_linestring(
               matrix(
                 c(-90, -50,rep(as.numeric(.SD[,'mean']), 2)),
                 ncol = 2
               )
             )
           )
         ),
       by = .I]  
agg_sf[, geom := st_sfc(geom, crs = 4326)]

agg_sf <- agg_sf |> 
  st_as_sf()

k <- st_intersection(
  agg_sf,
  l[l$min_zoom == 0,]
)
|> 
  st_nearest_points(
    st_linestring(matrix(c( -45, -45, 0, 90), ncol=2)) |> 
      st_sfc(crs = 4326)
  )

plot(k)

st_nearest_points(agg_sf, l) |>
  plot()

|> 
  st_transform('+proj=omerc +lat_0=40 +lonc=-68 +alpha=-40')

crd <- st_coordinates(agg_sf) |> 
  data.frame()
setDT(crd)

crd <- cbind(agg, crd)

crd[, spd := (Y - shift(Y, 1)) / 7 / 1000, by = species]

ggplot(crd) +
  geom_line(aes(x = week, y = spd, color = species))


library(mgcv)
agg[, species := as.factor(species)]
agg[, wk := week(week)]
m1 <- gam(mean ~  
            0 + species +
            s(wk, by = species, k = 13, bs = 'cc'),
          knots = list(wk = c(1,53)),
          data = agg, method = 'REML')

pred_dat <- data.table(
  wk = rep(1:53, times = 3),
  species = rep(c('gannet', 'striped bass', 'NARW'),
                each = 53)
)
pd <- predict(m1, newdata = pred_dat, se.fit = TRUE)

pred_dat[, ':='(pred = pd$fit,
         lci = pd$fit - 2*pd$se.fit,
         uci = pd$fit + 2*pd$se.fit)]



smooth_plot <- ggplot(data = pred_dat) +
  geom_ribbon(aes(x = wk, ymin = lci, ymax = uci),
              fill = 'lightgray') +
  geom_line(aes(x = wk, y = pred)) +
  geom_errorbar(data = agg, aes(x = wk, ymin = lsd, ymax = usd),
                color = 'orange') +
  geom_point(data = agg, aes(x = wk, y = mean),
             color = 'orange') +
  facet_wrap(~species, ncol = 1, scales = 'free_y')+
  labs(x = 'Week of year', y = 'Latitude') +
  theme_minimal()




library(gratia)

deriv_plot <- derivatives(m1, data = pred_dat, unconditional = T) |> 
  ggplot() +
  geom_ribbon(aes(x = wk, ymin = .lower_ci, ymax = .upper_ci),
              fill = 'lightgray') +
  geom_line(aes(x = wk, y = .derivative)) +
  facet_wrap(~species, ncol = 1, scales = 'free_y')+
  labs(x = 'Week of year', y = 'Latitudinal velocity (degrees/week)') +
  theme_minimal()
  
  
library(patchwork)
smooth_plot + deriv_plot
