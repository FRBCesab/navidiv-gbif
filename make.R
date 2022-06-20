#' Get GBIF Data for All European Fish Species
#' 
#' @description 
#' This compendium is dedicated to download and clean GBIF data for all 
#' European fish species between 1992 and 2021.
#' 
#' @author Nicolas Casajus \email{nicolas.casajus@fondationbiodiversite.fr}
#' 
#' @date 2022/06/19



## Install dependencies (listed in DESCRIPTION) ----

remotes::install_deps(upgrade = "never")


## Load project addins ----

pkgload::load_all(here::here())


## Global Variables ----

taxon_name <- "Actinopterygii"


## Run Project ----

source(here::here("analyses", "create_study_area_extent.R"))
source(here::here("analyses", "get_gbif_data.R"))
