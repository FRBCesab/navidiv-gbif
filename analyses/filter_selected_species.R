## Import filtered GBIF data ----

dat    <- read.csv(here::here("data", "derived-data", "gbif_fish_data_filtered.csv"))


## Import filtered GBIF spatial data ----

dat_sf <- sf::st_read(here::here("data", "derived-data", "gbif_fish_data_filtered_sf.gpkg"))


## Import species to keep ----

sp_list <- read.csv(here::here("data", "derived-data", "gbif_fish_species_list_KEEP.csv"))


## Select species ----

dat    <- dat[dat$"species" %in% unique(sp_list$"species"), ]
dat_sf <- dat_sf[dat_sf$"species" %in% unique(sp_list$"species"), ]


## Export objects ----

write.csv(dat, here::here("data", "derived-data", "gbif_fish_data_filtered_selected.csv"),
          row.names = FALSE)

sf::st_write(dat_sf, here::here("data", "derived-data", "gbif_fish_data_filtered_selected_sf.gpkg"),
             delete_dsn = TRUE)
