## Filters ----

basis_of_record <- c("HUMAN_OBSERVATION", "OCCURRENCE", "OBSERVATION", 
                     "MACHINE_OBSERVATION")

occurrence_status <- "PRESENT"


## Columns to keep ----

col_names <- c("year", "eventDate", "class", "order", "family", "genus", 
               "species", "decimalLongitude", "decimalLatitude")


## Read GBIF downloads info ----

downloads_info <- read.csv(here::here("data", "raw-data", "gbif", 
                                      "gbif_requests_keys.csv"), header = TRUE)


gbif_data <- data.frame()

for (i in seq_len(nrow(downloads_info))) {

  cat("Processing file", i, "on", nrow(downloads_info), "\n")
  
  
  ## Import GBIF raw data ----
  
  tab <- rgbif::occ_download_import(key   = downloads_info[i, "download_key"], 
                                    path  = here::here("data", "raw-data", "gbif"), 
                                    quote = "")
  
  tab <- as.data.frame(tab)
  
  
  ## Filter BASIS OF RECORDS ----
  
  tab <- tab[which(tab$"basisOfRecord" %in% basis_of_record), ]
  
  
  ## Filter OCCURRENCE STATUS ----
  
  tab <- tab[which(tab$"occurrenceStatus" %in% occurrence_status), ]
  
  
  ## Keep only NON NA SPECIES ----
  
  tab <- tab[which(!is.na(tab$"species")), ]
  tab <- tab[which(tab$"species" != ""), ]
  
  
  ## Select columns ----
  
  tab <- tab[ , col_names]
  
  
  ## Append to final data frame ----
  
  gbif_data <- rbind(gbif_data, tab)
}


## Convert dates ----

gbif_data$"eventDate" <- as.character(as.Date(gbif_data$"eventDate"))


## Remove duplicates ----

keys <- paste(gsub("\\s", "_", gbif_data$"species"), gbif_data$"eventDate", 
              gbif_data$"decimalLongitude", gbif_data$"decimalLatitude", 
              sep = "__")

pos <- which(duplicated(keys))
if (length(pos)) gbif_data <- gbif_data[-pos, ]


## Reorder rows ----

gbif_data <- gbif_data[with(gbif_data, order(species, eventDate)), ]
rownames(gbif_data) <- NULL


## Convert to spatial object ----

gbif_data_sf <- sf::st_as_sf(gbif_data, coords = c("decimalLongitude", "decimalLatitude"),
                             crs = 4326)

## Export layers ----

sf::st_write(gbif_data_sf, delete_dsn = TRUE, 
             here::here("data", "derived-data", 
                        "gbif_fish_data_filtered_sf.gpkg"))

write.csv(gbif_data, here::here("data", "derived-data", 
                                "gbif_fish_data_filtered.csv"), row.names = FALSE)
