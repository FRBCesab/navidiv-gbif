
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
hist(grid_counts$obs)


# Plotting ----
# Now lets add it to the gpkg of the grids so that we can plot and visualize
head(grids)

grids2 <- dplyr::left_join(grids, grid_counts, by = c("id" = "buffer_id"))
head(grids2)


mapview::mapview(grids2, zcol = "prop_mix_inv_obs")
# Look at that!

library(ggplot2)
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




#
#





# Infrastructure ----
ports <- read.csv(here::here("data", "derived-data", "port_count_ingrids.csv"))
locks <- read.csv(here::here("data", "derived-data", "lock_count_ingrids.csv"))
canals <- st_read(here::here("data", "derived-data", "canals_in_buffers.gpkg"))

str(ports)
str(locks)
str(canals)


# Clean up the files
ports <- dplyr::select(ports, BB_PORT_CO, CARGOHACA, id)
ports <- dplyr::rename(ports, port_id = BB_PORT_CO, port_size = CARGOHACA)
ports %>% 
  count(id)

locks <- dplyr::select(locks, -WATERWAY, -fid)
locks <- dplyr::rename(locks, lock_id = NOM, lock_size = NAVIGATION)
locks %>% 
  count(id)

canals <- dplyr::select(canals, id, proportion_canal2, geom)
canals <- dplyr::rename(canals, proportion_canalized = proportion_canal2)
hist(canals$proportion_canalized)



## Match these with the grid df built above

# Canals
str(canals)
st_as
canals_for_joining <- sf::st_drop_geometry(canals)
grids3 <- left_join(grids2, canals_for_joining, by = c("id" = "id"))
plot(grids3$prop_inv_obs ~ grids3$proportion_canalized)

grids3$proportion_canalized[is.na(grids3$proportion_canalized)] <- 0

# Plot out canal density real quick
ggplot2::ggplot() + geom_sf(data = basemap) +
  ggplot2::geom_sf(data = grids3, mapping = aes(fill = proportion_canalized)) +
  scale_fill_distiller(palette = "Spectral")+
  coord_sf(xlim = c(-4, 35), ylim = c(43, 56))


# Ports
portcount <- ports %>% 
  count(id)
portcount <- dplyr::rename(portcount, ports = n)
grids3 <- dplyr::left_join(grids3, portcount, by = c("id" = "id"))
grids3$ports[is.na(grids3$ports)] <- 0
hist(grids3$ports)
hist(sqrt(grids3$ports))
plot(grids3$prop_mix_inv_obs ~ grids3$ports)
ggplot2::ggplot() + geom_sf(data = basemap) +
  ggplot2::geom_sf(data = grids3, mapping = aes(fill = ports)) +
  scale_fill_distiller(palette = "Spectral")+
  coord_sf(xlim = c(-4, 35), ylim = c(43, 56))


# Locks
lockcount <- locks %>% 
  count(id)
lockcount <- dplyr::rename(lockcount, locks = n)
grids3 <- dplyr::left_join(grids3, lockcount, by = c("id" = "id"))
grids3$locks[is.na(grids3$locks)] <- 0
hist(grids3$locks)
hist(sqrt(grids3$locks))
plot(grids3$prop_mix_inv_obs ~ sqrt(grids3$locks))


# lol quick and dirty models
summary(lm(grids3$prop_mix_inv_obs ~ grids3$ports))
try1 <- glm(prop_mix_inv_obs ~ ports + locks + proportion_canalized +
              ports:locks, data = grids3)
summary(try1)

















