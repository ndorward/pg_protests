setwd("E:/nd17631/Chapter IV/Data")

library(readxl)
library(dplyr)
library(GADMTools)
library(sf)
library(tidyr)
library(stringr)
library(raster)
library(exactextractr)
library(ncdf4)
library(countrycode)
library(ggplot2)
library(DescTools)
library(modeest)

# 1) preliminaries --------------------------------------------------------

#ISO codes for countries in Africa - used in multiple operations below
africa_iso = readxl::read_excel('iso-codes.xlsx') %>%
  dplyr::filter(region == "Africa")

#COW country codes
cow_africa = read.csv("COW_country_codes.csv") %>%
  dplyr::filter(StateNme %in% africa_iso$name)

FIPS_africa = readxl::read_excel("FIPS-ISO.xlsx") %>%
  dplyr::filter(`ISO 3166` %in% africa_iso$`alpha-2`)

fips_iso = africa_iso %>%
  dplyr::left_join(., readxl::read_excel("FIPS-ISO.xlsx") %>%
                     dplyr::filter(`ISO 3166` %in% africa_iso$`alpha-2`), 
                   by = c("alpha-2" = "ISO 3166"))

#read africapolis polygons 
africapolis = readRDS("africapolis.rds") %>%
  dplyr::rename(GID_0 = ISO3)

# 2) create four time point panel 2000, 2005, 2010, 2015, 2020
africapolis_panel = matrix(0, ncol = 24, nrow = nrow(africapolis)) %>% 
  as.data.frame() %>%
  `colnames<-`(c(1997:2020))%>%
  cbind(africapolis, .) %>%
  reshape2::melt(measure.vars = 20:43, variable.name = "year") %>%
  dplyr::mutate(year = as.character(year),
                year = stringr::str_remove(year, "X"), 
                agglosID = as.character(agglosID)) %>%
  dplyr::select(-value) %>%
  dplyr::arrange(agglosID, year) 

# 3) main IV to panel ------------------------------------------------

#population size and growth - WorldPop
rastlist = list.files(path = "WP_Yearly_pop_Est", pattern='.tif$', all.files=TRUE, full.names=FALSE)
setwd("E:/nd17631/Chapter IV/Data/WP_Yearly_pop_Est") 
allrasters = lapply(rastlist, raster) 

results <- list()
for(i in 1:length(allrasters)) {
  rc = exactextractr::exact_extract(allrasters[[i]], africapolis, fun = "sum")
  results[[i]] = rc 
}

setwd("E:/nd17631/Chapter IV/Data")

world_pop = data.frame(matrix(unlist(results), ncol = length(results), byrow=FALSE)) %>%
  dplyr::mutate(agglosID = as.character(africapolis$agglosID)) %>%
  `colnames<-`(c(2000:2020, "agglosID")) %>%
  reshape2::melt(., measure.vars = 1:21, variable.name = "year") %>%
  dplyr::rename(world_pop = value)

#join to panel
africapolis_panel = dplyr::left_join(africapolis_panel, world_pop, by = c("agglosID", "year")) %>%
  dplyr::arrange(GID_0, agglosID, year) %>%
  dplyr::group_by(agglosID) %>% #group by id
  #calculate growth, absolute growth, and population density
  dplyr::mutate(world_pop_growth = (world_pop - lag(world_pop))/lag(world_pop),
                world_pop_growth_abs = log(world_pop-lag(world_pop)),
                world_pop_dens = world_pop/city_area) %>%
  ungroup()

#we see within country variation for pop growth?
ggplot(africapolis_panel, aes(x=world_pop_growth)) + 
  geom_histogram() + 
  facet_wrap(~GID_0)

#population growth rate - national world bank
pop_growth = read.csv("WB_Pop_Growth/WB_pop_growth.csv", skip = 4) %>%
  dplyr::mutate(GID_0 = countrycode::countryname(Country.Name, destination = "iso3c",
                                                 warn = FALSE)) %>%
  dplyr::select(-66) %>%
  pivot_longer(cols = starts_with("x"), names_to = "year") %>%
  drop_na() %>%
  dplyr::mutate(year = as.numeric(stringr::str_remove(year, "X"))) %>%
  dplyr::filter(year %in% 1997:2020 & GID_0 %in% africa_iso$`alpha-3`) %>%
  dplyr::arrange(GID_0, year) 

ssd = rbind(pop_growth %>% dplyr::filter(GID_0 == "SDN" & year %in% 1997:2010),
            pop_growth %>% dplyr::filter(GID_0 == "SSD" & year %in% 2011:2020)) %>%
  dplyr::mutate(GID_0 = "SSD")

pop_growth = rbind(pop_growth %>% dplyr::filter(!(GID_0 == "SSD")), ssd) %>%
  dplyr::filter(GID_0 %in% africapolis$GID_0) %>%
  dplyr::rename(wb_pop_growth = value) %>%
  dplyr::mutate(year = as.character(year)) %>%
  dplyr::select(GID_0, year, wb_pop_growth)

africapolis_panel = dplyr::left_join(africapolis_panel, pop_growth, by = c("GID_0", "year")) 

# VDEM - liberal democracy, freedom of expression, and civil socitey 
vdem = read.csv("Country_Year_V-Dem_Core_CSV_v10/V-Dem-CY-Core-v10.csv") %>%
  dplyr::select(country_text_id, year, v2x_libdem, v2xcs_ccsi, v2x_freexp, v2csprtcpt) %>%
  dplyr::filter(year %in% 1997:2020) %>%
  dplyr::mutate(year = as.character(year)) 

ssd = rbind(vdem %>% dplyr::filter(country_text_id == "SDN" & year %in% 1997:2010),
            vdem %>% dplyr::filter(country_text_id == "SSD" & year %in% 2011:2020)) %>%
  dplyr::mutate(country_text_id = "SSD")

vdem = rbind(vdem %>% dplyr::filter(!(country_text_id == "SSD")),
             ssd) %>%
  dplyr::filter(country_text_id %in% africapolis$GID_0) %>%
  dplyr::rename(GID_0 = country_text_id) 

africapolis_panel = dplyr::left_join(africapolis_panel, vdem, by = c("year", "GID_0"))

#Polity regime type 
polity = readxl::read_xls("p5v2018.xls") %>%
  dplyr::mutate(GID_0 = countrycode::countryname(country, destination = "iso3c", warn = FALSE), 
                year = as.character(year)) %>%
  dplyr::filter(year %in% 1997:2020)

ssd = rbind(polity %>% dplyr::filter(GID_0 == "SDN" & year %in% 1997:2010),
            polity %>% dplyr::filter(GID_0 == "SSD" & year %in% 2011:2020)) %>%
  dplyr::mutate(GID_0 = "SSD")

polity = rbind(polity %>% dplyr::filter(!(GID_0 == "SSD")),
               ssd) %>%
  dplyr::filter(GID_0 %in% africapolis$GID_0) %>%
  dplyr::select(GID_0, year, polity2, parcomp, exrec, democ, autoc) %>%
  dplyr::mutate(polity2 = tidyr::replace_na(polity2, 0), #replace Somalia 2011 na with zero
                regime_type = dplyr::case_when(exrec %in% 1:5 & parcomp %in% 0:1 ~ 1,
                                               exrec %in% 1:5 & parcomp %in% 2:5 ~ 2,
                                               exrec %in% 6:8 & parcomp %in% 0:1 ~ 2,
                                               exrec %in% 6:8 & parcomp %in% 2:4 ~ 3,
                                               exrec %in% 6:8 & parcomp == 3 ~ 4,
                                               exrec == 8 & parcomp %in% c(2, 4) ~3,
                                               exrec == 8 & parcomp == 5 ~ 3,
                                               exrec ==8 & parcomp == 8 ~ 5, 
                                               exrec %in% c(-88, -77, -66) ~ 6, 
                                               exrec %in% c(-88, -77, -66) ~ 6)) 

africapolis_panel = dplyr::left_join(africapolis_panel, polity, by = c("year", "GID_0")) #523 na - investigate

# 3) controls to panel ------------------------------------------------

#Age strucutres - youth bulge
#rastlist = list.files(path = "Africa_1km_Age_structures", pattern='.tif$', all.files=TRUE, full.names=FALSE)
#setwd("E:/nd17631/Chapter IV/Data/Africa_1km_Age_structures") 
#allrasters = lapply(rastlist, raster) 
#rasterstack = raster::stack(allrasters)
#proj4string(rasterstack) = crs(africapolis)
#age_structures = exactextractr::exact_extract(rasterstack, africapolis, fun = "sum") %>%
#  dplyr::mutate(agglosID = africapolis$agglosID)
#setwd("E:/nd17631/Chapter IV/Data")

#demog = cbind(age_structures[c(2:6, 22)] %>%
#                `colnames<-`(c(2000, 2005, 2010, 2015, 2020, "agglosID")) %>%
#                reshape2::melt(., measure.vars = 1:5, variable.name = "year") %>%
#                dplyr::rename(female_1519 = value), 
#              age_structures[7:11] %>%
#                `colnames<-`(c(2000, 2005, 2010, 2015, 2020)) %>%
#                reshape2::melt(., measure.vars = 1:5, variable.name = "year") %>%
#                dplyr::rename(male_1519 = value) %>% dplyr::select(-year),
#              age_structures[12:16] %>%
#                `colnames<-`(c(2000, 2005, 2010, 2015, 2020)) %>%
#                reshape2::melt(., measure.vars = 1:5, variable.name = "year") %>%
#                dplyr::rename(female_2024 = value) %>% dplyr::select(-year), 
#              age_structures[17:21] %>%
#                `colnames<-`(c(2000, 2005, 2010, 2015, 2020)) %>%
#                reshape2::melt(., measure.vars = 1:5, variable.name = "year") %>%
#                dplyr::rename(male_2024 = value) %>% dplyr::select(-year))

#africapolis_panel = dplyr::left_join(africapolis_panel, demog, by = c("agglosID", "year")) %>%
#  dplyr::mutate(young_men_15_24 = male_1519 + male_2024/world_pop, 
#                young_women_15_24 = female_1519 + female_2024/world_pop,
#                youth_bulge = young_men_15_24 + young_women_15_24/world_pop)

#Poverty/living standards - under 5 Infant Mortality rate and neonatal death rate 
#http://ghdx.healthdata.org/record/ihme-data/africa-under-5-and-neonatal-mortality-geospatial-estimates-2000-2015

rastlist = list.files(path = "IHME_IMR", pattern=c('MEAN', '.TIF$'), all.files=TRUE, full.names=FALSE)
setwd("E:/nd17631/Chapter IV/Data/IHME_IMR") 
allrasters = lapply(rastlist, raster) 
rasterstack = raster::stack(allrasters)
proj4string(rasterstack) = crs(africapolis)
imr = exactextractr::exact_extract(rasterstack, africapolis, fun = "mean") %>%
  dplyr::mutate(agglosID = as.character(africapolis$agglosID))
setwd("E:/nd17631/Chapter IV/Data")

imr = cbind(imr %>%
              dplyr::select(c(1:4, 9)) %>%
              `colnames<-`(c(2000, 2005, 2010, 2015, "agglosID")) %>%
              reshape2::melt(., measure.vars = 1:4, variable.name = "year") %>%
              dplyr::rename(neonatalMean = value), 
            imr %>%
              dplyr::select(c(5:8)) %>%
              `colnames<-`(c(2000, 2005, 2010, 2015)) %>%
              reshape2::melt(., measure.vars = 1:4, variable.name = "year") %>%
              dplyr::rename(imrMean = value)) %>%
  dplyr::select(-4)

africapolis_panel = dplyr::left_join(africapolis_panel, imr, by = c("agglosID", "year"))

#extract observed years and interpolate linearly
interpolation = africapolis_panel %>%
  dplyr::select(agglosID, year, imrMean) %>%
  tidyr::spread(year, imrMean) %>% dplyr::select(-agglosID)

linear = as.data.frame(t(imputeTS::na_interpolation(t(interpolation), option = "linear"))) %>%
  dplyr::mutate(agglosID = as.character(africapolis$agglosID)) %>%
  reshape2::melt(., measure.vars = 1:24, variable.name = "year", value.name = "imrMean") %>%
  dplyr::arrange(agglosID, year) #arrange to id~year

africapolis_panel = africapolis_panel %>% 
  dplyr::select(-imrMean) %>%
  dplyr::left_join(., linear, by = c("agglosID", "year"))

#local economy - (Night Lights)
rastlist = list.files(path = "Harmonised_NTL", 
                      pattern='.NTL', all.files=TRUE, full.names=FALSE)

#split up as images have different extents
rastlist_a = rastlist[c(1:12, 16:22)] 
rastlist_b = rastlist[13:15]

setwd("E:/nd17631/Chapter IV/Data/Harmonised_NTL") 
rasterstack_a = lapply(rastlist_a, raster) %>% 
  raster::stack(allrasters)
ntl_a = as.data.frame(exactextractr::exact_extract(rasterstack_a, africapolis, fun = "mean"))
colnames(ntl_a) = c(1997:2008, 2012:2018)

rasterstack_b = lapply(rastlist_b, raster) %>% 
  raster::stack(allrasters)
ntl_b = as.data.frame(exactextractr::exact_extract(rasterstack_b, africapolis, fun = "mean"))
colnames(ntl_b) = c(2009:2011)

setwd("E:/nd17631/Chapter IV/Data")

ntl = cbind(ntl_a, ntl_b) %>%
  dplyr::mutate(agglosID = as.character(africapolis$agglosID)) %>%
  reshape2::melt(., measure.vars = 1:22, variable.name = "year") %>%
  dplyr::rename(ntlMean = value)

africapolis_panel = dplyr::left_join(africapolis_panel, ntl, by = c("agglosID", "year"))

#urban education - for 2000-2014 think what to do? Maybe just replace front and end years
rastlist <- list.files(path = "IHME_AFRICA_EDU_2000_2015_YEARS_FEMALE_15_49_MEAN", 
                       pattern='.TIF$', all.files=TRUE, full.names=FALSE)
setwd("E:/nd17631/Chapter IV/Data/IHME_AFRICA_EDU_2000_2015_YEARS_FEMALE_15_49_MEAN") 
rasterstack <- lapply(rastlist, raster) %>% 
  raster::stack(allrasters)
setwd("E:/nd17631/Chapter IV/Data")

female = as.data.frame(exactextractr::exact_extract(rasterstack, africapolis, fun = "mean")) %>%
  `colnames<-`(c(2000:2014)) %>% 
  dplyr::mutate(agglosID = as.character(africapolis$agglosID)) %>%
  reshape2::melt(., measure.vars = 1:15, variable.name = "year") %>%
  dplyr::rename(female_education = value)

rastlist <- list.files(path = "IHME_AFRICA_EDU_2000_2015_YEARS_MALE_15_49_MEAN", 
                       pattern='.TIF$', all.files=TRUE, full.names=FALSE)
setwd("E:/nd17631/Chapter IV/Data/IHME_AFRICA_EDU_2000_2015_YEARS_MALE_15_49_MEAN") 
rasterstack <- lapply(rastlist, raster) %>% 
  raster::stack(allrasters)
setwd("E:/nd17631/Chapter IV/Data")

male = as.data.frame(exactextractr::exact_extract(rasterstack, africapolis, fun = "mean")) %>%
  `colnames<-`(c(2000:2015)) %>% 
  dplyr::mutate(agglosID = as.character(africapolis$agglosID)) %>%
  reshape2::melt(., measure.vars = 1:16, variable.name = "year") %>%
  dplyr::rename(male_education = value) %>%
  dplyr::filter(!(year == 2015))

africapolis_panel = africapolis_panel %>%
  dplyr::left_join(., female, by = c("agglosID", "year")) %>%
  dplyr::left_join(., male, by = c("agglosID", "year")) %>%
  group_by(agglosID) %>%
  tidyr::fill(female_education, .direction = "down") %>%
  tidyr::fill(female_education, .direction = "up") %>%
  tidyr::fill(male_education, .direction = "down") %>%
  tidyr::fill(male_education, .direction = "up") %>%
  ungroup()

#WB GDP 
wb_gdp = read.csv("WB_GDP_PPP_2017/WB_GDP_PPP_2017.csv", skip = 4) %>%
  dplyr::mutate(GID_0 = countrycode::countryname(Country.Name, destination = "iso3c",
                                                 warn = FALSE)) %>%
  dplyr::select(-66) %>%
  pivot_longer(cols = starts_with("x"), names_to = "year") %>%
  drop_na() %>%
  dplyr::mutate(year = as.numeric(stringr::str_remove(year, "X"))) %>%
  dplyr::filter(year %in% 1997:2020 & GID_0 %in% africa_iso$`alpha-3`) %>%
  dplyr::arrange(GID_0, year)

ssd = rbind(wb_gdp %>% dplyr::filter(GID_0 == "SDN" & year %in% 1997:2010),
            wb_gdp %>% dplyr::filter(GID_0 == "SSD" & year %in% 2011:2020)) %>%
  dplyr::mutate(GID_0 = "SSD")

wb_gdp = rbind(wb_gdp %>% dplyr::filter(!(GID_0 == "SSD")), ssd) %>%
  dplyr::filter(GID_0 %in% africapolis$GID_0) %>%
  dplyr::rename(wb_gdp = value) %>%
  dplyr::select(GID_0, year, wb_gdp) %>%
  dplyr::mutate(year = as.character(year)) %>%
  dplyr::group_by(GID_0) %>% #group by id
  dplyr::mutate(wb_gdp_growth = (wb_gdp - lag(wb_gdp))/lag(wb_gdp)) %>%
  dplyr::ungroup()

africapolis_panel = dplyr::left_join(africapolis_panel, wb_gdp, by = c("GID_0", "year")) %>%
  group_by(agglosID) %>%
  dplyr::mutate(wb_gdp_growth = (wb_gdp - lag(wb_gdp))/lag(wb_gdp))


#ongoing conflict in country
acd = read.csv("ucdp-prio-acd-201-csv/ucdp-prio-acd-201.csv") %>%
  dplyr::filter(region == 4) %>%
  dplyr::mutate(ISO_a = countrycode::countrycode(gwno_a, origin = 'gwn', 
                                                 destination = "iso3c", warn = FALSE),
                ISO_b = countrycode::countrycode(gwno_b, origin = 'gwn', 
                                                 destination = "iso3c", warn = FALSE))

africapolis_panel = africapolis_panel %>%
  left_join(., acd %>%
              dplyr::group_by(ISO_a, year) %>%
              dplyr::summarise(n_conflict_a = n(),
                               conflict_year_a = ifelse(n_conflict_a > 0, 1, 0)) %>%
              dplyr::mutate(year = as.character(year)) %>%
              dplyr::rename(GID_0 = ISO_a) %>%
              dplyr::select(-n_conflict_a), by = c("year", "GID_0")) %>%
  dplyr::mutate(., conflict_year_a = ifelse(is.na(conflict_year_a), 0, conflict_year_a)) %>% 
  left_join(., acd %>%
              dplyr::group_by(ISO_b, year) %>%
              dplyr::summarise(n_conflict_b = n(),
                               conflict_year_b = ifelse(n_conflict_b > 0, 1, 0)) %>%
              dplyr::mutate(year = as.character(year)) %>%
              dplyr::rename(GID_0 = ISO_b) %>%
              dplyr::select(-n_conflict_b), by = c("year", "GID_0")) %>%
  dplyr::mutate(conflict_year_b = ifelse(is.na(conflict_year_b), 0, conflict_year_b)) %>%
  dplyr::mutate(ACD_conflict_year = conflict_year_a + conflict_year_b,
                ACD_conflict_year = ifelse(ACD_conflict_year > 0, 1, 0)) %>%
  dplyr::select(-c(conflict_year_a, conflict_year_b))

#crude birth nation - national world bank
#https://data.worldbank.org/indicator/SP.DYN.CBRT.IN

births = read.csv("WB_Crude_Birth_Rate/WB_crude_birth_rate.csv", skip = 4) %>%
  dplyr::mutate(GID_0 = countrycode::countryname(Country.Name, destination = "iso3c",
                                                 warn = FALSE)) %>%
  dplyr::select(-66) %>%
  pivot_longer(cols = starts_with("x"), names_to = "year") %>%
  drop_na() %>%
  dplyr::mutate(year = stringr::str_remove(year, "X")) %>%
  dplyr::filter(year %in% 1997:2019 & GID_0 %in% africa_iso$`alpha-3`) %>%
  dplyr::arrange(GID_0, year)

ssd = rbind(births %>% dplyr::filter(GID_0 == "SDN" & year %in% 1997:2010),
            births %>% dplyr::filter(GID_0 == "SSD" & year %in% 2011:2019)) %>%
  dplyr::mutate(GID_0 = "SSD")

births = rbind(births %>% dplyr::filter(!(GID_0 == "SSD")), ssd) %>%
  dplyr::filter(GID_0 %in% africapolis$GID_0) %>%
  dplyr::rename(wb_births = value) %>%
  dplyr::select(GID_0, year, wb_births)

africapolis_panel = dplyr::left_join(africapolis_panel, births, by = c("GID_0", "year"))

#crude death rate - national world bank
#https://data.worldbank.org/indicator/SP.DYN.CDRT.IN

deaths = read.csv("WB_Crude_Death_Rate/WB_crude_death_rate.csv", skip = 4) %>%
  dplyr::mutate(GID_0 = countrycode::countryname(Country.Name, destination = "iso3c",
                                                 warn = FALSE)) %>%
  dplyr::select(-66) %>%
  pivot_longer(cols = starts_with("x"), names_to = "year") %>%
  drop_na() %>%
  dplyr::mutate(year = stringr::str_remove(year, "X")) %>%
  dplyr::filter(year %in% 1997:2019 & GID_0 %in% africa_iso$`alpha-3`) %>%
  dplyr::arrange(GID_0, year)

ssd = rbind(deaths %>% dplyr::filter(GID_0 == "SDN" & year %in% 1997:2010),
            deaths %>% dplyr::filter(GID_0 == "SSD" & year %in% 2011:2019)) %>%
  dplyr::mutate(GID_0 = "SSD")

deaths = rbind(deaths %>% dplyr::filter(!(GID_0 == "SSD")), ssd) %>%
  dplyr::filter(GID_0 %in% africapolis$GID_0) %>%
  dplyr::rename(wb_deaths = value) %>%
  dplyr::select(GID_0, year, wb_deaths)

africapolis_panel = dplyr::left_join(africapolis_panel, deaths, by = c("GID_0", "year")) %>%
  dplyr::mutate(natural_increase = wb_births - wb_deaths)

#Executive and legislative elections
nelda = read_xls("NELDA 5.0/NELDA.xls") %>%
  dplyr::select(1:8) %>%
  dplyr::mutate(GID_0 = countrycode::countryname(country, destination = "iso3c", warn = FALSE),
                year = as.character(year)) %>%
  dplyr::filter(GID_0 %in% africapolis$GID_0) %>%
  dplyr::group_by(GID_0, year) %>%
  dplyr::summarise(n_elections = n()) %>%
  dplyr::mutate(election_event = ifelse(n_elections > 0, 1, 0)) %>%
  drop_na()

africapolis_panel = dplyr::left_join(africapolis_panel, nelda, by = c("GID_0", "year")) %>% 
  dplyr::mutate(n_elections = ifelse(is.na(n_elections), 0, n_elections),
                election_event = ifelse(is.na(election_event), 0, election_event))

# 4) Protest data ---------------------------------------------------------

#ACLED Protest
acled = read.csv("1997-01-01-2019-12-31.csv", sep = ";") %>%
  st_as_sf(., coords = c('longitude', 'latitude'), crs= crs(africapolis)) %>%
  st_join(., africapolis, st_within, left = FALSE) %>% # assign settlement attributes to acled events
  st_drop_geometry()

protest = acled %>%
  dplyr::filter(event_type == "Protests") %>%
  dplyr::group_by(year, agglosID) %>%
  dplyr::summarise(acled_protest_count = n()) %>%
  dplyr::mutate(year = as.character(year),
                agglosID = as.character(agglosID)) %>%
  drop_na()

riots = acled %>%
  dplyr::filter(event_type == "Riots") %>%
  dplyr::group_by(year, agglosID) %>%
  dplyr::summarise(acled_riot_count = n()) %>%
  dplyr::mutate(year = as.character(year),
                agglosID = as.character(agglosID)) %>%
  drop_na()

africapolis_panel = dplyr::left_join(africapolis_panel, protest, by = c("year", "agglosID")) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(acled_protest_event = ifelse(acled_protest_count < 1, 0, 1)) %>%
  dplyr::left_join(., riots, by = c("year", "agglosID")) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(acled_riot_event = ifelse(acled_riot_count < 1, 0, 1),
                acled_count = acled_protest_count + acled_riot_count, 
                acled_event = ifelse(acled_count < 1, 0, 1))

save.image("africapolis_panel_build.RData")

# final data cleaning  ----------------------------------------------------

#cities per country 
country_city = africapolis %>%
  dplyr::group_by(GID_0) %>%
  summarise(country_city_count = n()) %>%
  arrange(country_city_count)

#create final data including education, neonatal, and imr - just three bins. 
final_africapolis_panel = africapolis_panel %>% 
  dplyr::filter(Pop2015 >= 50000 & year %in% 2001:2015) %>%
  #remove obsolete columns & education (save for subset)
  #dplyr::select(-c(female_1519, male_1519, female_2024, male_2024)) %>%
  #remove 5 cities with zero values on imr, educ, and neotatal
  dplyr::filter(!(imrMean == 0)) %>%
  #variable transformations
  dplyr::mutate(log_world_pop = log(1+world_pop),
                log_world_pop_dens = log(world_pop_dens),
                log_ntlMean = log(1+ntlMean),
                log_wb_gdp = log(1+wb_gdp), 
                log_imr = log(imrMean)) %>%
  group_by(agglosID, year) %>%
  #calculate city rank variables
  dplyr::mutate(wp_city_rank = dense_rank(desc(world_pop))) %>%
  ungroup() %>%
  group_by(agglosID) %>%
  dplyr::mutate(acled_count_lead_1 = dplyr::lead(acled_count, n = 1),
                acled_count_lead_2 = dplyr::lead(acled_count, n = 2),
                acled_count_lead_3 = dplyr::lead(acled_count, n = 3),
                acled_count_lead_4 = dplyr::lead(acled_count, n = 4),
                acled_count_lead_5 = dplyr::lead(acled_count, n = 5),
                acled_count_lead_6 = dplyr::lead(acled_count, n = 6),
                acled_count_lead_7 = dplyr::lead(acled_count, n = 7),
                acled_count_lead_8 = dplyr::lead(acled_count, n = 8),
                acled_count_lead_9 = dplyr::lead(acled_count, n = 9),
                acled_count_lead_10 = dplyr::lead(acled_count, n = 10)) %>%
  ungroup()
  
# get values for cutoff from un agglos data
un_agglos_growth = read_xls("WUP2018-F14-Growth_Rate_Cities.xls", skip = 16) %>%
  dplyr::mutate(GID_0 = countrycode::countryname(`Country or area`, destination = "iso3c", warn = FALSE)) %>%
  dplyr::filter(GID_0 %in% africapolis_panel$GID_0) %>%
  dplyr::select(-c("Index", "Country Code", "Country or area", "City Code", 
                   "Note", "Latitude", "Longitude")) %>%
  pivot_longer(cols = 2:18, names_to = "year", values_to = "pop_growth")

plot(un_agglos_growth$pop_growth)
#crop outliers
un_agglos_growth = un_agglos_growth %>% filter(pop_growth < 20 & pop_growth > -10)
plot(un_agglos_growth$pop_growth) 
min(un_agglos_growth$pop_growth) #-2.59139
max(un_agglos_growth$pop_growth) #17.31634

#remove extreme population growth values - we don't appeat to lose any data? 
final_africapolis_panel_wp = final_africapolis_panel %>% filter(world_pop_growth < 17 & world_pop_growth > -3)
nrow(final_africapolis_panel_wp) - nrow(final_africapolis_panel)

ggplot(final_africapolis_panel, aes(x=world_pop_growth)) + 
  geom_histogram()

#check how lost obs are dist - e.g., we don't want places with 1 or 2 years
year_city = final_africapolis_panel_wp %>% #years per city 
  dplyr::group_by(agglosID) %>%
  summarise(year_city_count = n()) %>%
  arrange(year_city_count) %>%
  filter(year_city_count > 2)

#we then cut every city with less than 3 year observations 
#final_africapolis_panel_wp = final_africapolis_panel_wp %>% filter(agglosID %in% year_city$agglosID) %>%
# we lose 55 cities - remaining sample 953 length(unique(final_africapolis_panel$agglosID)) - length(unique(final_africapolis_panel_wp$agglosID))
# now calculate migrant residual
#  dplyr::mutate(wp_migrant_residual = wb_pop_growth - world_pop_growth)

#test = final_africapolis_panel %>% filter(!(agglosID %in% final_africapolis_panel_wp$agglosID))
#mostly cutting off places without events
#ggplot(test, aes(x=acled_count)) + 
#  geom_histogram()
#
#ggplot(final_africapolis_panel_wp, aes(x=acled_count)) + 
#  geom_histogram()


#still have 38 countries 
length(unique(final_africapolis_panel_wp$GID_0))
#cities per country - all with enough cities (just)
country_city = final_africapolis_panel_wp %>%
  dplyr::group_by(GID_0) %>%
  summarise(country_city_count = n()) %>%
  arrange(country_city_count)

#save final data
saveRDS(final_africapolis_panel_wp, file = "africapolis_panel.rds")





