
## Generating model data frames


# Model data frame 1
#     Grid | Species | P/A | Ship | Infra | ...


# Model data frame 2
#     Grid | % Invasive | Ship | Infra | ...



## Outline
#  Plotting species to match them with their Grid
#  Matching the species with their status (Native / Exotic)
#  Calculating Infrastructure Density in each grid
#  Calculating Ship density in each grid
#  Modelframe 1
#  Calculating proportion invasive in each grid
#  Modelframe 2

library(sf)

# Plotting species into their grids ----
grids <- st_read(here::here("data", "raw-data", "river_buffers.gpkg"))
species <- st_read(here::here("data", "derived-data", "gbif_fish_data_on_buffers_sf.gpkg"))



# Lets just take a look to make sure they look good
plot(st_geometry(grids))
plot(st_geometry(species), add = T)
# Beautiful


# Now lets match it so that each grid says which species they have in them
intersection <- st_intersects(grids, species)
# Not working so I'm just going to do it in QGIS. Sorry.

# Upload from QGIS work
intersection <- read.csv(here::here("data", "derived-data", "gbif_sp_in_grid.csv"))
head(intersection)

# Clean it up
intersection <- dplyr::select(intersection, -fid_2, -id, -eventDate)





# Adding Species Status ----

# Read in the file
status <- read.csv(here::here("data", "derived-data", "GBIF_SPECIES_STATUS.csv"))
head(status)
status <- dplyr::select(status, -X)

# Connect status with intersection file
intersection2 <- dplyr::left_join(intersection, status, 
                                  by = c("species" = "species"))
head(intersection2)

# See how many are in each category
intersection2 %>% 
  count(status)

# NAs are the marine species, 
# Lets remove them
intersection3 <- intersection2[!is.na(intersection2$status), ]




# Calculating species richness and count of natives and invasives in each grid
# Group by grid
intersection4 <- dplyr::group_by(intersection3, buffer_id)

# Count number of observations in each grid
observations <- dplyr::summarise(intersection4, obs = length(species))
head(observations)

# Species richness in each grid
sp_rich <- dplyr::summarise(intersection4, sr = length(unique(species)))
head(sp_rich)

# Make an intersection df with only natives and one with only exotics
intersection_nat <- dplyr::filter(intersection4, status == "N")
intersection_inv <- dplyr::filter(intersection4, status == "E")

# Count and sr of natives
intersection_nat2 <- dplyr::group_by(intersection_nat, buffer_id)
nat_obs <- dplyr::summarise(intersection_nat2, nat_obs = length(species))
nat_sr <- dplyr::summarise(intersection_nat2, nat_sr = length(unique(species)))

# Count and sr of exotics
intersection_inv2 <- dplyr::group_by(intersection_inv, buffer_id)
inv_obs <- dplyr::summarise(intersection_inv2, inv_obs = length(species))
inv_sr <- dplyr::summarise(intersection_inv2, inv_sr = length(unique(species)))

# Now combine all these dfs
grid_counts <- dplyr::left_join(observations, sp_rich, by = c("buffer_id" = "buffer_id"))
head(grid_counts)
grid_counts <- dplyr::left_join(grid_counts, nat_obs, by = c("buffer_id" = "buffer_id"))
grid_counts <- dplyr::left_join(grid_counts, nat_sr, by = c("buffer_id" = "buffer_id"))
grid_counts <- dplyr::left_join(grid_counts, inv_obs, by = c("buffer_id" = "buffer_id"))
grid_counts <- dplyr::left_join(grid_counts, inv_sr, by = c("buffer_id" = "buffer_id"))

# Now calculate proportion invasive
grid_counts <- dplyr::mutate(grid_counts, prop_inv_obs = inv_obs / obs)
grid_counts <- dplyr::mutate(grid_counts, prop_inv_sr = inv_sr / sr)
hist(grid_counts$prop_inv_sr)
# Interestiiiiinnngg..

# Now lets add in the unknown species status species
intersection_mixed <- dplyr::filter(intersection4, status == "U")
intersection_mixed2 <- dplyr::group_by(intersection_mixed, buffer_id)
mix_obs <- dplyr::summarise(intersection_mixed2, mix_obs = length(species))
mix_sr <- dplyr::summarise(intersection_mixed2, mix_sr = length(unique(species)))
grid_counts <- dplyr::left_join(grid_counts, mix_obs, by = c("buffer_id" = "buffer_id"))
grid_counts <- dplyr::left_join(grid_counts, mix_sr, by = c("buffer_id" = "buffer_id"))
grid_counts <- dplyr::mutate(grid_counts, prop_mix_sr = mix_sr / sr)
grid_counts <- dplyr::mutate(grid_counts, prop_mix_obs= mix_obs / obs)
grid_counts <- dplyr::mutate(grid_counts, prop_mix_inv_sr = prop_mix_sr + prop_inv_sr)
grid_counts <- dplyr::mutate(grid_counts, prop_mix_inv_obs = prop_mix_obs + prop_inv_obs)
hist(grid_counts$prop_mix_inv_obs)

# Plotting ----
# Now lets add it to the gpkg of the grids so that we can plot and visualize
head(grids)

grids2 <- dplyr::left_join(grids, grid_counts, by = c("id" = "buffer_id"))
head(grids2)


mapview::mapview(grids2, zcol = "prop_mix_inv_obs")
# Look at that!


# ggplot it
ggplot2::ggplot() +
  ggplot2::geom_sf(data = grids2, mapping = aes(fill = prop_mix_inv_obs)) +
  scale_fill_viridis_c(option = "plasma") 

# Read in a basemap
basemap <- st_read(here::here("data", "raw-data", "study_area", "study_area_basemap.gpkg"))

# ggplot it
ggplot2::ggplot() + geom_sf(data = basemap) +
  ggplot2::geom_sf(data = grids2, mapping = aes(fill = prop_mix_inv_obs)) +
  scale_fill_viridis_c(option = "magma") +
  coord_sf(xlim = c(-4, 35), ylim = c(43, 56)) 

ggplot2::ggplot() + geom_sf(data = basemap) +
  ggplot2::geom_sf(data = grids2, mapping = aes(fill = prop_mix_inv_obs)) +
  scale_fill_distiller(palette = "Spectral")+
  coord_sf(xlim = c(-4, 35), ylim = c(43, 56)) 




# Infrastructure Metrics ----












