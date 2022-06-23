
# Trimming Species List

# European Wide freshwater species list obtained
# Now trying to match it with our GBIF dataset to trim off any marine or others


# Files
paneuro <- read.csv(here::here("data", "derived-data", "PanEuro_spec_list.csv"))
head(paneuro)

gbiflist <- read.csv(here::here("data", "derived-data", "gbif_fish_species_list.csv"))
head(gbiflist)

iucn <- read.csv(here::here("data", "derived-data", "iucn_sp_list_trim.csv"))
head(iucn)

frewtr <- read.csv(here::here("data", "derived-data", "Freshwaterecology_specieslist.csv"))
head(frewtr)

gbifsp <- read.csv(here::here("data", "derived-data", "gbif_fish_species_list_KEEP.csv"))
head(gbifsp)






#   1. PanEuro----
# the status column in the paneuro column is based of the fish species status as
# N = native, E = Exotic and U = uncertain (native in some areas exotic in others)


#Trimming both to just have one column with the genus and species in it
paneuro <- dplyr::select(paneuro, full, Status)

gbiflist <- dplyr::select(gbiflist, species)

head(paneuro)
head(gbiflist)

# Joining now
joinedsp <- dplyr::left_join(gbiflist, paneuro, by = c("species" = "full"))

head(joinedsp)
str(joinedsp)
length(joinedsp$Status)
length(unique(joinedsp$Status))

# Great so there are four categories for status now (instead of 3)
# The 4th is NA which will stand for species that were in GBIF but not the 
# pan-european database. 

joinedsp %>% 
  count(Status)

paneuro %>% 
  count(Status)

# 240 of our 359 species were marked as NA
# 52 native, 52 unknown and 15 exotic
# It doesn't seem to be a weird spelling issue


# Save only the NA species
is.na(joinedsp$Status)
na_sp <- joinedsp[is.na(joinedsp$Status),]
head(na_sp)





#      2. IUCN species list----
# iucn <- read.csv("../navidiv-gbif/data/derived-data/iucn_sp_list_trim.csv")
head(iucn)
length(iucn$Species)
#534 species

joined2 <- left_join(joinedsp, iucn, by = c("species" = "Species"))
head(joined2)

joined2 %>% 
  count(IUCN_status)

# Combine the two status types to see how many NANAs we will have
joined2$status2 <- paste(joined2$Status, joined2$IUCN_status)
joined2 %>% 
  count(status2)

# Now have 204 species that are not on either list

stillunknown <- dplyr::filter(joined2, status2 == "NA NA")
head(stillunknown)
View(stillunknown)





#     3. Freshwater Ecology database ----
# https://www.freshwaterecology.info/

# frewtr <- read.csv("../navidiv-gbif/data/derived-data/Freshwaterecology_specieslist.csv")
head(frewtr)

# will add a status column to id if it joined or not
frewtr$status <- "FR"

# now joining
joined3 <- dplyr::left_join(joined2, frewtr, by = c("species" = "species"))
head(joined3)

# create a column combining statuses to find continuous NAs
joined3$status4 <- paste(joined3$status2, joined3$status)

joined3 %>% 
  count(status4)

# 192 species as NA NA NA
stillunknown <- dplyr::filter(joined3, status4 == "NA NA NA")
head(stillunknown)
View(stillunknown)
stillunknown <- dplyr::rename(stillunknown, peter = Status, iucn = IUCN_status,
                              fre = status)
stillunknown <- dplyr::select(stillunknown, species, peter, iucn, fre)


# That would give us 167 species
known <- dplyr::filter(joined3, status4 != "NA NA NA")
known <- dplyr::rename(known, peter_iucn_fre = status4)
known <- dplyr::select(known, species, peter_iucn_fre)
head(known)






# 4. Googling ----

# Now just looking through the unknown list and googling them

# write.csv(stillunknown, "../navidiv-gbif/data/derived-data/unknown_splist.csv")

# Googled every single species on this list and determined if they were freshwater
# or marine and where they originated from.

determined <- read.csv(here::here("data", "derived-data", "unknown_splist.csv"))
View(determined)

determined %>% 
  count(status)

# Drop 171 species (because they're marine)
# Keep 21 - 16 exotic 5 native to Europe

keep <- dplyr::filter(determined, status != "drop")

drop <- dplyr::filter(determined, status == "drop")

# Add them back to the known species list
keep <- dplyr::select(keep, species, status)
head(keep)
keep <- dplyr::rename(keep, google = status)


known2 <- dplyr::full_join(known, keep, by = c("species" = "species"))
View(known2)

known2 <- dplyr::select(known2, species)

# SO
# Drop 171 species, keep 188 species

# write.csv(known2, "../navidiv-gbif/data/derived-data/gbif_fish_species_list_KEEP.csv")
# write.csv(drop, "../navidiv-gbif/data/derived-data/gbif_fish_species_list_DROP.csv")






#     5. Determining species status (Native vs. Exotic) ----

head(gbifsp)

head(paneuro)

# Clean both
gbifsp <- dplyr::select(gbifsp, species)
paneuro <- dplyr::select(paneuro, full, Status)


# Join them
joined4 <- dplyr::left_join(gbifsp, paneuro, by = c("species" = "full"))
head(joined4)
joined4 %>% 
  count(Status)

# This now gives us 
#  E  15
#  N  52
#  U  52
#  NA 69

# So still need to determine the status of 121 species
View(joined4)

# Pulling out just those species
nat_status_unknown <- dplyr::filter(joined4, Status == "U")
View(nat_status_unknown)

nas <- joined4[is.na(joined4$Status), ]

nat_status_unknown <- dplyr::full_join(nat_status_unknown, nas, 
                                       by = c("species" = "species"))

nat_status_unknown <- dplyr::select(nat_status_unknown, species)

# write.csv(nat_status_unknown, "../GBIF_sp_needstatus.csv")

