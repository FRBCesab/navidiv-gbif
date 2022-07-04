
library(tidyverse)
library(sf)

# Adding a status to species as plotted out
plotted <- read.csv("../data/derived-data/gbif_sp_in_boxes.csv")
status <- read.csv("../data/derived-data/GBIF_SPECIES_STATUS.csv")

str(plotted)
str(status)

status <- dplyr::select(status, -X)
plotted2 <- left_join(plotted, status, by = c("species" = "species"))
View(plotted2)

plotted2 <- plotted2 %>% 
  drop_na(status)



plotted_sf <- st_read("../data/derived-data/gbif_fish_data_on_buffers_sf.gpkg")
plotted_sf2 <- left_join(plotted_sf, status, by = c("species" = "species"))
plotted_sf2 <- plotted_sf2 %>% 
  drop_na(status)

plot(st_geometry(plotted_sf2), col = sf.colors(3, categorical = TRUE), axes = TRUE)

ggplot() + 
  geom_sf(data = plotted_sf2, aes(fill = status))





