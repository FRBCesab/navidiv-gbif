## Define MIN latitude ----

greece <- rnaturalearth::ne_states(country = "greece", returnclass = "sf")
ymin <- sf::st_bbox(greece)$"ymin"


## Define MIN longitude ----

ireland <- rnaturalearth::ne_states(country = "ireland", returnclass = "sf")
xmin <- sf::st_bbox(ireland)$"xmin"


## Define MAX latitude ----

norway <- rnaturalearth::ne_states(country = "norway", returnclass = "sf")
norway <- norway[norway$type_en == "County", ]
ymax <- sf::st_bbox(norway)$"ymax"


## Define MAX longitude ----

ukraine <- rnaturalearth::ne_states(country = "ukraine", returnclass = "sf")
xmax <- sf::st_bbox(ukraine)$"xmax"


## Add space around box ----

xmin <- xmin - 0.50
xmax <- xmax + 1.50
ymin <- ymin - 0.25
ymax <- ymax + 0.35


## Create area box ----

study_bbox <- sf::st_polygon(list(cbind(c(xmin, xmax, xmax, xmin, xmin), 
                                        c(ymin, ymin, ymax, ymax, ymin))))


## Extract WKT expression (for GBIF) ----

study_wkt <- sf::st_as_text(study_bbox)


## Convert to real spatial object ----

study_bbox <- sf::st_sfc(study_bbox)
study_bbox <- sf::st_as_sf(study_bbox, crs = 4326)


## Get Europe layer ----

europe <- rnaturalearth::ne_countries(scale = "large", continent = "europe", 
                                      returnclass = "sf")

## Rewrite CRS ----

sf::st_crs(europe) <- 4326


## Crop Europe layer ----

europe <- sf::st_crop(europe, study_bbox)


## Map study area ----

ggplot2::ggplot() + 
  ggplot2::geom_sf(data = europe, ggplot2::aes(fill = sovereignt), 
                   col = "white", size = 0.1) + 
  ggplot2::theme_light() +
  ggplot2::theme(legend.position = "none")


## Export layers ----

sf::st_write(study_bbox, delete_dsn = TRUE, 
             here::here("data", "raw-data", "study_area", 
                        "study_area_bbox.gpkg"))

sf::st_write(europe, delete_dsn = TRUE, 
             here::here("data", "raw-data", "study_area", 
                        "study_area_basemap.gpkg"))

cat(paste0(study_wkt, "\n"), 
    file = here::here("data", "raw-data", "study_area", "study_area_wkt.txt"))
