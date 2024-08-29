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


# colonies from PDF
library(pdftools)

k <- pdf_text('C:\\Users\\darpa2\\Zotero\\storage\\FJ24QNKH\\Spiegel et al. - 2017 - Determining Fine-scale Use and Movement Patterns o.pdf')

wdths <- c(10,22,9,6,10,10,7,7,10,15,9,15,13,18)

t1 <- k[[88]] |> 
  strsplit('\n')  |> 
  unlist() |> 
  _[7:32]|> 
  I() |> 
  readr::read_fwf(readr::fwf_widths(wdths))

t2 <- k[[89]] |> 
  strsplit('\n')  |> 
  unlist() |> 
  _[1:31]|> 
  I() |> 
  readr::read_fwf(readr::fwf_widths(wdths))

t3 <- k[[90]] |> 
  strsplit('\n')  |> 
  unlist() |> 
  _[1:18]|> 
  I() |> 
  readr::read_fwf(readr::fwf_widths(wdths))

tab_a1 <- fread('data/spiegel_A-1.csv',
                col.names = function(.) gsub('[ \\(\\)/]', '_', .))
tab_a1[, let(capture_area = gsub("\x92", '', capture_area,useBytes = TRUE),
             associated_colony = gsub("\x92", '', associated_colony, useBytes = TRUE))]

# info on Table 2.5
tab_a1[, region := fcase(grepl('Cape|Funk|Bacc', associated_colony), 'Newfoundland',
                         grepl('Bona|Bird', associated_colony), 'St. Lawrence',
                         associated_colony == 'unknown', 'unknown')]

fwrite(tab_a1, 'data/spiegel_A-1.csv')
