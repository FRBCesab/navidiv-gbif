## Import study area ----

study_area_bbox <- sf::st_read(here::here("data", "raw-data", "study_area", 
                                          "study_area_bbox.gpkg"))


## Import rivers buffers sections ----

buffers <- sf::st_read(here::here("data", "raw-data", "river_buffers.gpkg"))
buffers <- sf::st_make_valid(buffers)


## Import GBIF filtered data ----

gbif_data <- sf::st_read(here::here("data", "derived-data", 
                                    "gbif_fish_data_filtered_sf.gpkg"))


## Crop layers (if study area has changed) ----

buffers   <- sf::st_crop(buffers, study_area_bbox)
gbif_data <- sf::st_crop(gbif_data, study_area_bbox)


## Intersect layers (select GBIF records) ----

pos <- sf::st_intersects(gbif_data, buffers, sparse = FALSE)
ppos <- lapply(seq_len(ncol(pos)), function(x) which(pos[ , x]))

names(ppos) <- 1:length(ppos)


## Subset GBIF records per cells ----

gbif <- lapply(seq_len(length(ppos)), function(i) {
  dat <- gbif_data[ppos[[i]], ]
  if (nrow(dat)) dat$"buffer_id" <- i else dat <- NULL
  dat
})


## Remove empty cells (no GBIF records) ----

is_not_null <- unlist(lapply(gbif, function(x) if (is.null(x)) FALSE else TRUE))

gbif <- gbif[which(is_not_null)]


## Convert list to data.frame (spatial object) -----

gbif <- do.call(rbind.data.frame, gbif)


## Convert spatial to data.frame ----

gbif_data <- sf::st_drop_geometry(gbif)


## Create species table ----

species <- gbif_data[ , c("class", "order", "family", "genus", "species")]
species <- species[-which(duplicated(species$"species")), ]
species <- species[with(species, order(class, order, family, genus, species)), ]
rownames(species) <- NULL


## Remove useless columns ----

gbif <- gbif[ , c("buffer_id", "year", "eventDate", "species")]
gbif <- gbif[with(gbif, order(buffer_id, eventDate, species)), ]
rownames(gbif) <- NULL

gbif_data <- gbif_data[ , c("buffer_id", "year", "eventDate", "species")]
gbif_data <- gbif_data[with(gbif_data, order(buffer_id, eventDate, species)), ]
rownames(gbif_data) <- NULL


## Compute richness (unique species per buffer) ----

richness <- tapply(gbif_data$species, gbif_data$buffer_id, function(x) length(unique(x)))

richness <- data.frame("buffer_id" = names(richness), richness)


## Add richness to buffers layer ----

buffers <- merge(buffers, richness, by.x = "id", by.y = "buffer_id", all = TRUE)


## Richness map ----

europe <- sf::st_read(here::here("data", "raw-data", "study_area", 
                                 "study_area_basemap.gpkg"))

ggplot2::ggplot() + 
  ggplot2::geom_sf(data = europe, color = "white", fill = "black", size = 0.1) +
  ggplot2::geom_sf(data = buffers, ggplot2::aes(fill = richness, color = richness)) +
  ggplot2::scale_fill_gradient("GBIF Fish richness", low = "yellow", high = "red", na.value = "darkgreen") +
  ggplot2::scale_colour_gradient(low = "yellow", high = "red", na.value = "darkgreen", guide = "none") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom", legend.key.width = ggplot2::unit(1, 'cm'))

ggplot2::ggsave(here::here("figures", "gbif_fish_species_richness.png"), bg = "white")


## Export layers ----

sf::st_write(gbif, delete_dsn = TRUE, 
             here::here("data", "derived-data", 
                        "gbif_fish_data_on_buffers_sf.gpkg"))

write.csv(gbif_data, here::here("data", "derived-data", 
                                "gbif_fish_data_on_buffers.csv"), row.names = FALSE)

write.csv(species, here::here("data", "derived-data", 
                              "gbif_fish_species_list.csv"), row.names = FALSE)

sf::st_write(buffers, delete_dsn = TRUE, 
             here::here("outputs", "gbif_fish_richness_on_buffers_sf.gpkg"))
