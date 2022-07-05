
# Functional Traits

# Functional trait database - Freshwater ecology -
# Maintained by a NAVIDIV collaborator - Astrid Schmidt-Kloiber


# read file with species list and traits from this database
sptraits <- read.csv("../navidiv-gbif/data/derived-data/freshwatereco_traits.csv")
head(sptraits)
View(sptraits)
# no

sptraits2 <- read.csv("../navidiv-gbif/data/derived-data/freshwatereco_traits2.csv", sep = ";")
View(sptraits2)
# No

# Manually uploaded it with a seperater of ; and changed the quotes to ""
View(freshwatereco_traits2)
write.csv(freshwatereco_traits2, "../navidiv-gbif/data/derived-data/freshwatereco_traits3.csv")
