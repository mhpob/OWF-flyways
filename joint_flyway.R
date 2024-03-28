library(data.table)
library(ggplot2)
library(qs)

# Striped bass
hud_sb <- qread('data/hud_detects.qs')

setDT(hud_sb)

hud_sb[, week := format(date.local, "%V")]

sb_agg <- hud_sb[, .(mean = mean(lat, na.rm = T),
           lsd = mean(lat, na.rm = T) - sd(lat, na.rm = T),
           usd = mean(lat, na.rm = T) + sd(lat, na.rm = T),
           species = 'striped bass'),
       by = week] 

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


# Merge
agg <- rbind(sb_agg, gannet_agg)
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
agg[, spd := (mean - shift(mean, 1)) / 7, by = species]
ggplot(agg) +
  geom_line(aes(x = week, y = spd, color = species))

library(sf)
agg[, lon := -68]

agg_sf <- agg |> 
  st_as_sf(coords = c('lon', 'mean'),
           crs = 4326) |> 
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
agg[, wk := as.numeric(week)]
m1 <- gam(mean ~ species + s(wk, by = species, k = 15), data = agg, method = 'REML')

library(gratia)

derivatives(m1) |> 
  dplyr::mutate(wk = as.Date(wk)) |> 
  draw(ncol=1) + scale_x_date(date_labels = '%b')
  