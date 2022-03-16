##############

# Database editions 
# author: "Florencia Grattarola"
# date: "10 September 2019"

# Initial number of records 69380
# Initial number of species 673


##############

library(tidyverse)

Biodiversidata <- read_csv('Biodiversidata_1.0.0.csv', 
                           col_types = cols(occurrenceID = col_character(),
                                            scientificName = col_character(),
                                            scientificNameAuthorship = col_character(),
                                            vernacularName = col_character(),
                                            kingdom = col_character(),
                                            phylum = col_character(),
                                            class = col_character(),
                                            order = col_character(),
                                            family = col_character(),
                                            genus = col_character(),
                                            specificEpithet = col_character(),
                                            infraspecificEpithet = col_character(),
                                            countryCode = col_character(),
                                            stateProvince = col_character(),
                                            verbatimLocality = col_character(),
                                            decimalLatitude = col_number(),
                                            decimalLongitude = col_number(),
                                            georeferenceSources = col_character(),
                                            georeferencedBy = col_character(),
                                            eventDate = col_date(format = ""),
                                            year = col_number(),
                                            month = col_number(),
                                            day = col_number(),
                                            basisOfRecord = col_character(),
                                            institutionCode = col_character(),
                                            collectionCode = col_character(),
                                            catalogNumber = col_character(),
                                            recordedBy = col_character(),
                                            recordNumber = col_character(),
                                            identifiedBy = col_character(),
                                            dynamicProperties = col_character(),
                                            associatedReferences  = col_character()))


################################

# DELETE

# occurrenceID: Biodiversidata:UY:Amphibia:2422
# number or records 1
# reason: Melanophryniscus montevidensis (Philippi, 1902) poorly identified

Biodiversidata <- Biodiversidata %>% 
  filter(occurrenceID!='Biodiversidata:UY:Amphibia:2422')

# occurrenceID: Biodiversidata:UY:Aves:50490
# number or records 1
# reason: record from Synallaxis ruficapilla (Vieillot, 1819) deleted from eBird

Biodiversidata <- Biodiversidata %>% 
  filter(occurrenceID!='Biodiversidata:UY:Aves:50490')

# Biodiversidata:UY:Aves:52526; Biodiversidata:UY:Aves:57753
# number or records 2
# reason: record from Poospiza cinerea (Bonaparte, 1851) deleted from eBird

Biodiversidata <- Biodiversidata %>% 
  filter(scientificName!='Poospiza cinerea (Bonaparte, 1851)')

# scientificName: Passer domesticus (Linnaeus, 1758) 
# number or records 460
# reason: non-native species

Biodiversidata <- Biodiversidata %>% 
  filter(scientificName!='Passer domesticus (Linnaeus, 1758)')

# scientificName: Chloris chloris (Linnaeus, 1758)
# number or records 43
# reason: non-native species

Biodiversidata <- Biodiversidata %>% 
  filter(scientificName!='Chloris chloris (Linnaeus, 1758)')

# scientificName: Anas platyrhynchos (Linnaeus, 1758)
# number or records 4
# reason: non-native species

Biodiversidata <- Biodiversidata %>% 
  filter(scientificName!='Anas platyrhynchos (Linnaeus, 1758)')

# Total records deleted: 511
# Total species deleted: 5

################################

# UPDATE iucnStatus

# scientificName: Boana pulchella (Duméril and Bibron, 1841)
# dynamicProperties: {"iucnStatus":"least concern"}
# Reason: Listed in IUCN Red List under synonym species Hypsiboas pulchellus

Biodiversidata <- Biodiversidata %>% 
  mutate(dynamicProperties=(ifelse(scientificName=='Boana pulchella (Duméril and Bibron, 1841)',
                                   '{"iucnStatus":"least concern"}',
                                   dynamicProperties))) 

# scientificName: Ololygon aromothyella (Faivovich, 2005)
# dynamicProperties: {"iucnStatus":"data deficient"}
# Reason: Listed in IUCN Red List under synonym species Scinax aromothyella
Biodiversidata <- Biodiversidata %>% 
  mutate(dynamicProperties=(ifelse(scientificName=='Ololygon aromothyella (Faivovich, 2005)',
                                   '{"iucnStatus":"data deficient"}',
                                   dynamicProperties)))

# scientificName: Ololygon berthae (Barrio, 1962)
# dynamicProperties: {"iucnStatus":"least concern"}
# Reason: Listed in IUCN Red List under synonym species Scinax berthae

Biodiversidata <- Biodiversidata %>% 
  mutate(dynamicProperties=(ifelse(scientificName=='Ololygon berthae (Barrio, 1962)',
                                   '{"iucnStatus":"least concern"}',
                                   dynamicProperties)))


################################

# UPDATE scientificName

# 1) Melanophryniscus orejasmirandai (Prigioni and Langone, 1987)
# scientificName: Melanophryniscus pachyrhynus (Miranda-Ribeiro, 1920)
# scientificNameAuthorship  Miranda-Ribeiro, 1920
# vernacularName sapito de nariz gorda
# specificEpithet pachyrhynus
# dynamicProperties {"iucnStatus":"data deficient"}
# occurrenceID: Biodiversidata:UY:Amphibia:1051; Biodiversidata:UY:Amphibia:1052; Biodiversidata:UY:Amphibia:2417


Biodiversidata <- Biodiversidata %>%
  mutate(scientificName=(ifelse(scientificName=='Melanophryniscus orejasmirandai (Prigioni and Langone, 1987)',
                                'Melanophryniscus pachyrhynus (Miranda-Ribeiro, 1920)', scientificName))) %>%
  mutate(scientificNameAuthorship=(ifelse(scientificName=='Melanophryniscus pachyrhynus (Miranda-Ribeiro, 1920)',
                                          'Miranda-Ribeiro, 1920', scientificNameAuthorship))) %>%
  mutate(vernacularName=(ifelse(scientificName=='Melanophryniscus pachyrhynus (Miranda-Ribeiro, 1920)',
                                'sapito de nariz gorda', vernacularName))) %>%
  mutate(specificEpithet=(ifelse(scientificName=='Melanophryniscus pachyrhynus (Miranda-Ribeiro, 1920)',
                                 'pachyrhynus', specificEpithet))) %>%
  mutate(dynamicProperties=(ifelse(scientificName=='Melanophryniscus pachyrhynus (Miranda-Ribeiro, 1920)',
                                   '{"iucnStatus":"data deficient"}', dynamicProperties)))


# 2) Notiochelidon cyanoleuca (Vieillot, 1817)
# scientificName Pygochelidon cyanoleuca (Vieillot, 1817)
# genus Pygochelidon

Biodiversidata <- Biodiversidata %>%
  mutate(scientificName=(ifelse(scientificName=='Notiochelidon cyanoleuca (Vieillot, 1817)',
                                'Pygochelidon cyanoleuca (Vieillot, 1817)', scientificName))) %>%
  mutate(genus=(ifelse(scientificName=='Pygochelidon cyanoleuca (Vieillot, 1817)',
                                          'Pygochelidon', genus))) 

# 3) Leucocarbo atriceps (King, 1828)
# scientificName Phalacrocorax atriceps (King, 1828)
# genus Phalacrocorax

Biodiversidata <- Biodiversidata %>%
  mutate(scientificName=(ifelse(scientificName=='Leucocarbo atriceps (King, 1828)',
                                'Phalacrocorax atriceps (King, 1828)', scientificName))) %>%
  mutate(genus=(ifelse(scientificName=='Phalacrocorax atriceps (King, 1828)',
                       'Phalacrocorax', genus))) 

# 4) Heteroxolmis dominicana (Vieillot, 1823)
# scientificName Xolmis dominicanus (Vieillot, 1823)
# genus Xolmis
# specificEpithet dominicanus

Biodiversidata <- Biodiversidata %>%
  mutate(scientificName=(ifelse(scientificName=='Heteroxolmis dominicana (Vieillot, 1823)',
                                'Xolmis dominicanus (Vieillot, 1823)', scientificName))) %>%
  mutate(genus=(ifelse(scientificName=='Xolmis dominicanus (Vieillot, 1823)',
                       'Xolmis', genus))) %>% 
  mutate(specificEpithet=(ifelse(scientificName=='Xolmis dominicanus (Vieillot, 1823)',
                                 'dominicanus', specificEpithet))) 
 
# 5) Geothlypis velata (Vieillot, 1809)
# scientificName Geothlypis aequinoctialis (Gmelin, 1789)
# scientificNameAuthorship  Gmelin, 1789
# specificEpithet aequinoctialis
# dynamicProperties {"iucnStatus":"least concern"}


Biodiversidata <- Biodiversidata %>%
  mutate(scientificName=(ifelse(scientificName=='Geothlypis velata (Vieillot, 1809)',
                                'Geothlypis aequinoctialis (Gmelin, 1789)', scientificName))) %>%
  mutate(scientificNameAuthorship=(ifelse(scientificName=='Geothlypis aequinoctialis (Gmelin, 1789)', 
                       'Gmelin, 1789', scientificNameAuthorship))) %>% 
  mutate(specificEpithet=(ifelse(scientificName=='Geothlypis aequinoctialis (Gmelin, 1789)',
                                 'aequinoctialis', specificEpithet))) %>% 
  mutate(dynamicProperties=(ifelse(scientificName=='Geothlypis aequinoctialis (Gmelin, 1789)',
                                 '{"iucnStatus":"least concern"}', dynamicProperties)))

# 6) Buteo albicaudatus (Vieillot, 1816)
# scientificName Geranoaetus albicaudatus (Vieillot, 1816)
# genus Geranoaetus

Biodiversidata <- Biodiversidata %>%
  mutate(scientificName=(ifelse(scientificName=='Buteo albicaudatus (Vieillot, 1816)',
                                'Geranoaetus albicaudatus (Vieillot, 1816)', scientificName))) %>%
  mutate(genus=(ifelse(scientificName=='Geranoaetus albicaudatus (Vieillot, 1816)',
                       'Geranoaetus', genus))) 

# 7) Species Dipsas turgidus (Cope, 1868)
# scientificName Dipsas turgida (Cope, 1868)
# specificEpithet turgida

Biodiversidata <- Biodiversidata %>%
  mutate(scientificName=(ifelse(scientificName=='Dipsas turgidus (Cope, 1868)',
                                'Dipsas turgida (Cope, 1868)', scientificName))) %>%
  mutate(specificEpithet=(ifelse(scientificName=='Dipsas turgida (Cope, 1868)',
                       'turgida', specificEpithet))) 

# 8) Species Tropidurus torquatus (Wied-Neuwied, 1820)
# scientificName Tropidurus catalanensis (Gudynas and Skuk, 1893)
# scientificNameAuthorship  Gudynas and Skuk, 1893
# specificEpithet catalanensis

Biodiversidata <- Biodiversidata %>%
  mutate(scientificName=(ifelse(scientificName=='Tropidurus torquatus (Wied-Neuwied, 1820)',
                                'Tropidurus catalanensis (Gudynas and Skuk, 1893)', scientificName))) %>%
  mutate(scientificNameAuthorship=(ifelse(scientificName=='Tropidurus catalanensis (Gudynas and Skuk, 1893)', 
                                          'Gudynas and Skuk, 1893', scientificNameAuthorship))) %>% 
  mutate(specificEpithet=(ifelse(scientificName=='Tropidurus catalanensis (Gudynas and Skuk, 1893)',
                                 'catalanensis', specificEpithet))) %>% 
  mutate(dynamicProperties=(ifelse(scientificName=='Tropidurus catalanensis (Gudynas and Skuk, 1893)',
                                   '{"iucnStatus":"not evaluated"}', dynamicProperties)))

# 9) Poospiza lateralis (Nordmann, 1835)
# scientificName: Microspingus lateralis (Nordmann, 1835)
# genus: Microspingus

Biodiversidata <- Biodiversidata %>%
  mutate(scientificName=(ifelse(scientificName=='Poospiza lateralis (Nordmann, 1835)',
                                'Microspingus lateralis (Nordmann, 1835)', scientificName))) %>%
  mutate(genus=(ifelse(scientificName=='Microspingus lateralis (Nordmann, 1835)', 
                                          'Microspingus', genus)))


# 10) Rhinella diptycha (Cope, 1862)
# vernacularName sapo cururú

Biodiversidata <- Biodiversidata %>%
  mutate(vernacularName=(ifelse(scientificName=='Rhinella diptycha (Cope, 1862)',
                                'sapo cururú', vernacularName))) 

# 11) Rhinella fernandezae (Gallardo, 1957)
# vernacularName sapito de jardín de Fernández

Biodiversidata <- Biodiversidata %>%
  mutate(vernacularName=(ifelse(scientificName=='Rhinella fernandezae (Gallardo, 1957)',
                                'sapito de jardín de Fernández', vernacularName))) 

# 12) Rhinella dorbignyi (Duméril and Bibron, 1841)
# vernacularName sapito de jardín de d’Orbigny

Biodiversidata <- Biodiversidata %>%
  mutate(vernacularName=(ifelse(scientificName=='Rhinella dorbignyi (Duméril and Bibron, 1841)',
                                'sapito de jardín de d’Orbigny', vernacularName))) 
# 
# 13) Trachemys dorbigni (Duméril & Bibron, 1835)
# vernacularName morrocoyo

Biodiversidata <- Biodiversidata %>%
  mutate(vernacularName=(ifelse(scientificName=='Trachemys dorbigni (Duméril & Bibron, 1835)',
                                'morrocoyo', vernacularName))) 

# 14) Hydrochoerus hydrochaeris (Linnaeus, 1766)
# vernacularName capibara | carpincho

Biodiversidata <- Biodiversidata %>%
  mutate(vernacularName=(ifelse(scientificName=='Hydrochoerus hydrochaeris (Linnaeus, 1766)',
                                'carpincho', vernacularName))) 

# 15) Coendou prehensilis (Linnaeus, 1758)
# scientificName Coendou spinosus (F. Cuvier, 1823)
# scientificNameAuthorship  F. Cuvier, 1823
# specificEpithet spinosus

Biodiversidata <- Biodiversidata %>%
  mutate(scientificName=(ifelse(scientificName=='Coendou prehensilis (Linnaeus, 1758)',
                                'Coendou spinosus (F. Cuvier, 1823)', scientificName))) %>%
  mutate(scientificNameAuthorship=(ifelse(scientificName=='Coendou spinosus (F. Cuvier, 1823)',
                                'F. Cuvier, 1823', scientificNameAuthorship))) %>% 
  mutate(specificEpithet=(ifelse(scientificName=='Coendou spinosus (F. Cuvier, 1823)',
                                          'spinosus', specificEpithet)))
  
# Final number of records 68869 (-511)
# Initial number of species 665  (-8)


####################################

Biodiversidata_UPDATED <- Biodiversidata

####################################

### ADD NEW RECORDS FROM 'NEW_10-09-2019.CSV'

####################################

Biodiversidata_UPDATED %>% count()
Biodiversidata_UPDATED %>% 
  distinct(scientificName) %>% count()

Biodiversidata_UPDATED %>% 
  distinct(genus, specificEpithet, class) %>% 
  group_by(class) %>% 
  count()

# # A tibble: 4 x 2
# # Groups:   class [4]
# class          n
# <chr>      <int>
# 1 Amphibia  2546
# 2 Aves     60524
# 3 Mammalia  3951
# 4 Reptilia  2343

# # A tibble: 4 x 2
# # Groups:   class [4]
# class          n
# <chr>      <int>
# 1 Amphibia    50
# 2 Aves       430
# 3 Mammalia   117
# 4 Reptilia    68

# Final number of records 69364 (69380-69364=+16)
# Final number of species 665  (673-665=-8)


###############################


write_excel_csv(Biodiversidata_UPDATED, 'Biodiversidata_1.0.1.csv')


