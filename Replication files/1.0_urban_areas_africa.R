setwd("E:/nd17631/Chapter IV/Data")

library(dplyr)
library(sf)
library(tidyr)
library(stringr)
library(countrycode)
library(fuzzyjoin)

#Africa boundary data 
africa_gadm = africa = st_read("gadm36_levels_shp/gadm36_0.shp") %>%
  dplyr::filter(GID_0 %in% c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "DJI", "DZA", "EGY", "ERI", "ESH", "ETH",
                             "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LBY", "LSO", "MAR", "MDG", "MLI", "MOZ", "MRT", "MWI",
                             "NAM", "NER", "NGA", "RWA", "SDN", "SEN", "SLE", "SOM", "SSD", "SWZ", "TCD", "TGO", "TUN", "TZA", "UGA", "ZAF",
                             "ZMB", "ZWE")) %>% #read African countries excluding small islands
  rmapshaper::ms_simplify(., keep = 0.1, keep_shapes = TRUE)

ssa_gadm = dplyr::filter(africa_gadm, !(NAME_0 %in% c("Algeria", "Egypt", "Western Sahara", "Sudan", 
                                                      "Libya", "Mauritania", "Morocco", "Tunisia")))

ssa_africa_extent = st_union(ssa_gadm)

#read smod, filter unnamed, join with gadm, calculate area
smod_africa = st_read("GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_labelHDC_V2_0/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_labelHDC_V2_0.gpkg") %>%
  st_transform(., crs = st_crs(ssa_africa_extent)) %>%
  .[ssa_africa_extent, ] %>%
  dplyr::filter(NAME_MAIN != "UNNAMED") %>% #filter 168 unnamed settlements - 2647
  st_join(., ssa_gadm, st_intersects, largest = TRUE) %>% # join using max intersection
  dplyr::filter(GID_0 %in% ssa_gadm$GID_0) %>%
  dplyr::mutate(city_area = st_area(.)/1000000) %>% #calculate area and convert ot km^2
  units::drop_units()  

#count cities per country 
city_per_country = smod_africa %>%
  dplyr::group_by(GID_0) %>%
  summarise(count = n())


#capital and major cities
capital_cities = read.csv("capital_cities.csv") %>%
  dplyr::rename(NAME_0 = ï..NAME_0) %>%
  dplyr::mutate(GID_0 = countrycode::countryname(NAME_0, destination = "iso3c", warn = FALSE)) %>%
  tidyr::unite("match_key", c(GID_0, NAME_MAIN), sep = "-", remove = F) %>%
  dplyr::filter(GID_0 %in% ssa_gadm$GID_0)

#cities taken from https://worldpopulationreview.com/continents/capitals/africa
capital_match = stringdist_join(capital_cities, smod_africa %>% st_drop_geometry() %>%
                                  dplyr::select(NAME_MAIN),
                                by = "NAME_MAIN", #change to common name in both datasets
                                mode = "left",
                                ignore_case = FALSE, 
                                method = "lv", 
                                max_dist = 5, 
                                distance_col = "dist") %>%
  group_by(NAME_MAIN.x) %>%
  slice_min(order_by = dist, n = 1) %>%
  dplyr::rename(cities = NAME_MAIN.x, smod = NAME_MAIN.y) %>%
  .[-c(9, 30:39), ] 

smod_africa = smod_africa %>% 
  dplyr::mutate(capital_or_major_city = ifelse(NAME_MAIN %in% capital_match$smod, 1, 0))

#read smod africa as R object 
saveRDS(smod_africa, file = "smod_africa.rds")



# 2) Africapolis ----------------------------------------------------------

africapolis = st_read("AFRICAPOLIS2020/AFRICAPOLIS2020.shp") %>%
  dplyr::filter(ISO3 %in% ssa_gadm$GID_0) %>%
  dplyr::filter(Pop2015 >= 100000) %>%
  dplyr::mutate(city_area = st_area(.)/1000000, 
                agglosName = ifelse(grepl("Lome", agglosName), "Lome", agglosName)) %>% #calculate area and convert ot km^2
  units::drop_units() %>%
  tidyr::unite("match_key", c(ISO3, agglosName), sep = "-", remove = F)
  
  
#cities taken from https://worldpopulationreview.com/continents/capitals/africa
capital_match_polis = stringdist_join(capital_cities, africapolis %>% st_drop_geometry() %>%
                                        dplyr::select(match_key),
                                by = "match_key", #change to common name in both datasets
                                mode = "left",
                                ignore_case = FALSE, 
                                method = "lv", 
                                max_dist = 10, 
                                distance_col = "dist") %>%
  group_by(match_key.x) %>%
  slice_min(order_by = dist, n = 1) %>%
  dplyr::rename(cities = match_key.x, polis = match_key.y) %>%
  .[-c(18:19, 28:33, 44:47, 54), ] 

africapolis = africapolis %>% 
  dplyr::mutate(capital_or_major_city = ifelse(match_key %in% capital_match_polis$polis, 1, 0))

saveRDS(africapolis, file = "africapolis.rds")

#plots for Figure 1. 

smod_nga = ggplot(africa_gadm %>% filter(GID_0 == "NGA")) +
  geom_sf(fill = "white", size = 0.05) +
  geom_sf(smod_africa %>% filter(GID_0 == "NGA"), fill = "black", size = 0.001, mapping = aes())
ggsave(smod_nga, filename = "figure1a.png", dpi = 600)

africapolis_nga = ggplot(africa_gadm %>% filter(GID_0 == "NGA")) +
  geom_sf(fill = "white", size = 0.05) +
  geom_sf(africapolis %>% filter(GID_0 == "NGA"), fill = "black", 
          size = 0.001, mapping = aes())
ggsave(africapolis_nga, filename = "figure1b.png", dpi = 600)


