library(data.table)

gannet <- fread('extracted_figures/gannet_extracted.csv',
                col.names = c('date', 'latitude'))
# Last point had no confidence intervals
gannet <- rbind(gannet, gannet[.N], gannet[.N])
gannet[, date := rep(
  seq.Date(as.Date('2012-09-01'), by = '5 days', length.out = 74),
  each = 3)]

setorder(gannet, date, latitude)

gannet[, value := rep(c('lsd', 'mean', 'usd'), times = 74)]

library(ggplot2)
dcast(gannet, date ~ value, value.var = 'latitude') |> 
ggplot() +
  geom_errorbar(aes(x = date, ymin = lse, ymax = use)) +
  geom_point(aes(x = date, y = mean)) +
  labs(y='lat')
