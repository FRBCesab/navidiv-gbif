## Get GBIF taxonKey for Actinopterygii ----

taxon_key  <- rgbif::name_suggest(taxon_name, rank = "class")$"data"$"key"


## Get the spatial extent ----

study_area_wkt <- readLines(here::here("data", "raw-data", "study_area", 
                                       "study_area_wkt.txt"))


## Set temporal extent ----

periods <- list()
periods[[1]] <- c("1992", "2001")
periods[[2]] <- c("2002", "2011")
periods[[3]] <- c("2012", "2021")


## Define objects ----

downloads_info <- data.frame()
info_requests  <- list()
k              <- 1


for (i in 1:length(periods)) {
  
  
  cat("Requesting chunk", i, "on", length(periods), "\n")
  
  if (k <= 3) {
  
  
    ## Prepare ZIP file on GBIF server ----
  
    info_requests[[k]] <- rgbif::occ_download(
      rgbif::pred("taxonKey", taxon_key),
      rgbif::pred("hasCoordinate", TRUE),
      rgbif::pred("hasGeospatialIssue", FALSE),
      rgbif::pred("geometry", study_area_wkt),
      rgbif::pred_gte("year", min(periods[[i]])),
      rgbif::pred_lte("year", max(periods[[i]])),
      format = "SIMPLE_CSV",
      user   = Sys.getenv("GBIF_USER"),
      pwd    = Sys.getenv("GBIF_PWD"),
      email  = Sys.getenv("GBIF_EMAIL"))
    
    
    ## Save request info ----
    
    info_requests[[k]] <- data.frame("download_key" = info_requests[[k]][1], 
                                     do.call(cbind.data.frame, 
                                             attributes(info_requests[[k]])))
    
    downloads_info <- rbind(downloads_info, info_requests[[k]])
  
    k <- k + 1
  }

  if (k > 3) {
  
    
    ## Wait until ZIP files are done on GBIF servers ----
    
    for (n in seq_len(k - 1)) {
      rgbif::occ_download_wait(info_requests[[n]]$"download_key", 
                               status_ping = 60) 
    }
    
    
    ## Downloads ZIP files ----
    
    for (n in seq_len(k - 1)) {
      rgbif::occ_download_get(key       = info_requests[[n]]$"download_key",
                              path      = here::here("data", "raw-data", "gbif"),
                              overwrite = TRUE)
    }
    
  
    ## Reset objects ----
    
    k             <- 1
    info_requests <- list()
  }
}


## Export ZIP files info ----

write.csv(downloads_info, here::here("data", "raw-data", "gbif", 
                                     "gbif_requests_keys.csv"), 
          row.names = FALSE)
