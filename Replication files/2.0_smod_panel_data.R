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
                     dplyr::filter(`ISO 3166` %in% africa_iso$`alpha-2`)
                   , by = c("alpha-2" = "ISO 3166"))

#read smod polygons 
smod_africa = readRDS("smod_africa.rds")

# 2) create four time point panel 2000, 2005, 2010, 2015, 2020
smod_panel = matrix(0, ncol = 31, nrow = nrow(smod_africa)) %>% 
  as.data.frame() %>%
  `colnames<-`(c(1990:2020))%>%
  cbind(smod_africa, .) %>%
  reshape2::melt(measure.vars = 10:40, variable.name = "year") %>%
  dplyr::mutate(year = as.character(year),
                year = stringr::str_remove(year, "X"),
                ID_HDC_G0 = as.character(ID_HDC_G0)) %>%
  dplyr::select(-value) %>%
  dplyr::arrange(ID_HDC_G0, year)

# 3) main IV to panel ------------------------------------------------

#population size and growth - GHS-POP
rastlist = list.files(path = "GHS_POP_1990-2000", pattern='.tif$', all.files=TRUE, full.names=FALSE)
setwd("E:/nd17631/Chapter IV/Data/GHS_POP_1990-2000") 
allrasters = lapply(rastlist, raster) 
rasterstack = raster::stack(allrasters) 
smod_moll = smod_africa %>%
  st_transform(crs = crs(rasterstack))
ghs_pop = exactextractr::exact_extract(rasterstack, smod_moll, fun = "sum") %>%
  dplyr::mutate(ID_HDC_G0 = as.character(smod_africa$ID_HDC_G0)) %>%
  dplyr::rename("1990" = sum.GHS_POP_E1990_GLOBE_R2019A_54009_250_V1_0,
                "2000" = sum.GHS_POP_E2000_GLOBE_R2019A_54009_250_V1_0,
                "2015" = sum.GHS_POP_E2015_GLOBE_R2019A_54009_250_V1_0) %>%
  reshape2::melt(., measure.vars = 1:3, variable.name = "year") %>%
  dplyr::rename(ghs_pop = value)
setwd("E:/nd17631/Chapter IV/Data")

smod_panel = dplyr::left_join(smod_panel, ghs_pop, by = c("year", "ID_HDC_G0")) 

interpolation = smod_panel %>%
  dplyr::select(ID_HDC_G0, year, ghs_pop) %>%
  tidyr::spread(year, ghs_pop) %>% dplyr::select(-ID_HDC_G0) 

linear = as.data.frame(t(imputeTS::na_interpolation(t(interpolation), option = "linear"))) %>%
  dplyr::mutate(ID_HDC_G0 = as.character(smod_africa$ID_HDC_G0)) %>%
  reshape2::melt(., measure.vars = 1:31, variable.name = "year", value.name = "ghs_pop_linear") %>%
  dplyr::arrange(ID_HDC_G0, year) %>% #arrange to id~year
  dplyr::group_by(ID_HDC_G0) 

smod_panel = smod_panel %>%
  dplyr::filter(year %in% 1997:2020) %>%
  dplyr::left_join(., linear, by = c("ID_HDC_G0", "year")) %>%
  dplyr::group_by(ID_HDC_G0) %>% #group by id
  dplyr::mutate(ghs_pop_growth = (ghs_pop_linear - lag(ghs_pop_linear))/lag(ghs_pop_linear),
                ghs_pop_growth_abs = log(ghs_pop_linear-lag(ghs_pop_linear)),
                ghs_pop_dens = ghs_pop_linear/city_area)

#we see within country variation for pop growth?
ggplot(smod_panel, aes(x=ghs_pop_growth)) + 
  geom_histogram() + 
  facet_wrap(~GID_0)

#population size and growth - WorldPop
rastlist = list.files(path = "WP_Yearly_pop_Est", pattern='.tif$', all.files=TRUE, full.names=FALSE)
setwd("E:/nd17631/Chapter IV/Data/WP_Yearly_pop_Est") 
allrasters = lapply(rastlist, raster) 

results <- list()
for(i in 1:length(allrasters)) {
  rc = exactextractr::exact_extract(allrasters[[i]], smod_africa, fun = "sum")
  results[[i]] = rc 
}

setwd("E:/nd17631/Chapter IV/Data")

world_pop = data.frame(matrix(unlist(results), ncol = length(results), byrow=FALSE)) %>%
  dplyr::mutate(ID_HDC_G0 = as.character(smod_africa$ID_HDC_G0)) %>%
  `colnames<-`(c(2000:2020, "ID_HDC_G0")) %>%
  reshape2::melt(., measure.vars = 1:21, variable.name = "year") %>%
  dplyr::rename(world_pop = value)

#join to panel
smod_panel = dplyr::left_join(smod_panel, world_pop, by = c("ID_HDC_G0", "year")) %>%
  dplyr::arrange(GID_0, ID_HDC_G0, year) %>%
  dplyr::group_by(ID_HDC_G0) %>% #group by id
  #calculate growth, absolute growth, and population density
  dplyr::mutate(world_pop_growth = (world_pop - lag(world_pop))/lag(world_pop),
                world_pop_growth_abs = log(world_pop-lag(world_pop)),
                world_pop_dens = world_pop/city_area) %>%
  ungroup()

#we see within country variation for pop growth?
ggplot(smod_panel, aes(x=world_pop_growth)) + 
  geom_histogram() + 
  facet_wrap(~GID_0)

#what do those pop growth figures look like? - we will need to decide where to cut off. 
#see un prospects growth rates 

#population size and growth - GPW
rastlist = list.files(path = "GPW", pattern='.tif$', all.files=TRUE, full.names=FALSE)
setwd("E:/nd17631/Chapter IV/Data/GPW") 
allrasters = lapply(rastlist, raster) 
rasterstack = raster::stack(allrasters)
proj4string(rasterstack) = crs(smod_africa)
gpw = exactextractr::exact_extract(rasterstack, smod_africa, fun = "sum") %>%
  dplyr::mutate(ID_HDC_G0 = smod_africa$ID_HDC_G0)
setwd("E:/nd17631/Chapter IV/Data")

gpw = gpw %>%
  `colnames<-`(c(2000, 2005, 2010, 2015, 2020, "ID_HDC_G0")) %>%
  reshape2::melt(., measure.vars = 1:5, variable.name = "year") %>%
  dplyr::rename(gpw_pop = value) %>%
  dplyr::mutate(ID_HDC_G0 = as.character(ID_HDC_G0))

smod_panel = dplyr::left_join(smod_panel, gpw, by = c("ID_HDC_G0", "year"))

#extract observed years and interpolate linearly
interpolation = smod_panel %>%
  dplyr::select(ID_HDC_G0, year, gpw_pop) %>%
  tidyr::spread(year, gpw_pop) %>% dplyr::select(-ID_HDC_G0)

linear = as.data.frame(t(imputeTS::na_interpolation(t(interpolation), option = "linear"))) %>%
  dplyr::mutate(ID_HDC_G0 = as.character(smod_africa$ID_HDC_G0)) %>%
  reshape2::melt(., measure.vars = 1:24, variable.name = "year", value.name = "gpw_pop_linear") %>%
  dplyr::arrange(ID_HDC_G0, year) #arrange to id~year

smod_panel = dplyr::left_join(smod_panel, linear, by = c("ID_HDC_G0", "year")) %>%
  dplyr::group_by(ID_HDC_G0) %>% #group by id
  #calculate growth, absolute growth, and population density
  dplyr::mutate(gpw_pop_growth = (gpw_pop_linear - lag(gpw_pop_linear))/lag(gpw_pop_linear),
                gpw_pop_growth_abs = log(gpw_pop_linear-lag(gpw_pop_linear)),
                gpw_pop_dens = gpw_pop_linear/city_area) %>%
  ungroup()

#not an enormous amount of within country variation
ggplot(smod_panel, aes(x=gpw_pop_growth)) + 
  geom_histogram() + 
  facet_wrap(~GID_0)

#check correlation with United Nation's World Population Prospects?

#land scan - https://landscan.ornl.gov/landscan-datasets 
rastlist = list.files(path = "Land_Scan_Pop", recursive = T, pattern='hdr.adf', all.files=TRUE, full.names=FALSE)
setwd("E:/nd17631/Chapter IV/Data/Land_Scan_Pop") 
allrasters = lapply(rastlist, raster)

results <- list() #do in loop to handle multiple extents problem
for(i in 1:length(allrasters)){ #assign crs to all rasters 
  proj4string(allrasters[[i]]) = crs(smod_africa)
  rc = exactextractr::exact_extract(allrasters[[i]], smod_africa, fun = "sum")
  results[[i]] = rc 
}

setwd("E:/nd17631/Chapter IV/Data")

land_scan = data.frame(matrix(unlist(results), ncol = length(results), byrow=FALSE)) %>%
  `colnames<-`(c(2000:2019)) %>%
  dplyr::mutate(ID_HDC_G0 = as.character(smod_africa$ID_HDC_G0)) %>%
  reshape2::melt(., measure.vars = 1:20, variable.name = "year") %>%
  dplyr::rename(ls_pop = value) 

smod_panel = dplyr::left_join(smod_panel, land_scan, by = c("ID_HDC_G0", "year")) %>%
  dplyr::arrange(ID_HDC_G0, year) %>% #arrange to id~year
  dplyr::group_by(ID_HDC_G0) %>% #group by city id 
  dplyr::mutate(ls_pop_growth = (ls_pop - lag(ls_pop))/lag(ls_pop),
                ls_growth_abs = log(ls_pop-lag(ls_pop)),
                ls_pop_dens = ls_pop/city_area) %>%
  ungroup()

#we get clear within country variation ... 
ggplot(smod_panel, aes(x=ls_pop_growth)) + 
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
  dplyr::filter(GID_0 %in% smod_africa$GID_0) %>%
  dplyr::rename(wb_pop_growth = value) %>%
  dplyr::mutate(year = as.character(year)) %>%
  dplyr::select(GID_0, year, wb_pop_growth)

smod_panel = dplyr::left_join(smod_panel, pop_growth, by = c("GID_0", "year")) 

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
  dplyr::filter(country_text_id %in% smod_africa$GID_0) %>%
  dplyr::rename(GID_0 = country_text_id) 

smod_panel = dplyr::left_join(smod_panel, vdem, by = c("year", "GID_0"))

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
  dplyr::filter(GID_0 %in% smod_africa$GID_0) %>%
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

smod_panel = dplyr::left_join(smod_panel, polity, by = c("year", "GID_0")) #523 na - investigate

# 3) controls to panel ------------------------------------------------

#Age strucutres - youth bulge
#rastlist = list.files(path = "Africa_1km_Age_structures", pattern='.tif$', all.files=TRUE, full.names=FALSE)
#setwd("E:/nd17631/Chapter IV/Data/Africa_1km_Age_structures") 
#allrasters = lapply(rastlist, raster) 
#rasterstack = raster::stack(allrasters)
#proj4string(rasterstack) = crs(smod_africa)
#age_structures = exactextractr::exact_extract(rasterstack, smod_africa, fun = "sum") %>%
#  dplyr::mutate(ID_HDC_G0 = smod_africa$ID_HDC_G0)
#setwd("E:/nd17631/Chapter IV/Data")

#demog = cbind(age_structures[c(2:6, 22)] %>%
#                `colnames<-`(c(2000, 2005, 2010, 2015, 2020, "ID_HDC_G0")) %>%
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

#smod_panel = dplyr::left_join(smod_panel, demog, by = c("ID_HDC_G0", "year")) %>%
#  dplyr::mutate(young_men_15_24 = male_1519 + male_2024/world_pop, 
#                young_women_15_24 = female_1519 + female_2024/world_pop,
#                youth_bulge = young_men_15_24 + young_women_15_24/world_pop)

#Poverty/living standards - under 5 Infant Mortality rate and neonatal death rate 
#http://ghdx.healthdata.org/record/ihme-data/africa-under-5-and-neonatal-mortality-geospatial-estimates-2000-2015

rastlist = list.files(path = "IHME_IMR", pattern=c('MEAN', '.TIF$'), all.files=TRUE, full.names=FALSE)
setwd("E:/nd17631/Chapter IV/Data/IHME_IMR") 
allrasters = lapply(rastlist, raster) 
rasterstack = raster::stack(allrasters)
proj4string(rasterstack) = crs(smod_africa)
imr = exactextractr::exact_extract(rasterstack, smod_africa, fun = "mean") %>%
  dplyr::mutate(ID_HDC_G0 = as.character(smod_africa$ID_HDC_G0))
setwd("E:/nd17631/Chapter IV/Data")

imr = cbind(imr %>%
              dplyr::select(c(1:4, 9)) %>%
              `colnames<-`(c(2000, 2005, 2010, 2015, "ID_HDC_G0")) %>%
              reshape2::melt(., measure.vars = 1:4, variable.name = "year") %>%
              dplyr::rename(neonatalMean = value), 
            imr %>%
              dplyr::select(c(5:8)) %>%
              `colnames<-`(c(2000, 2005, 2010, 2015)) %>%
              reshape2::melt(., measure.vars = 1:4, variable.name = "year") %>%
              dplyr::rename(imrMean = value)) %>%
  dplyr::select(-4)

smod_panel = dplyr::left_join(smod_panel, imr, by = c("ID_HDC_G0", "year"))

#extract observed years and interpolate linearly
interpolation = smod_panel %>%
  dplyr::select(ID_HDC_G0, year, imrMean) %>%
  tidyr::spread(year, imrMean) %>% dplyr::select(-ID_HDC_G0)

linear = as.data.frame(t(imputeTS::na_interpolation(t(interpolation), option = "linear"))) %>%
  dplyr::mutate(ID_HDC_G0 = as.character(smod_africa$ID_HDC_G0)) %>%
  reshape2::melt(., measure.vars = 1:24, variable.name = "year", value.name = "imrMean") %>%
  dplyr::arrange(ID_HDC_G0, year) #arrange to id~year

smod_panel = smod_panel %>% 
  dplyr::select(-imrMean, -neonatalMean) %>%
  dplyr::left_join(., linear, by = c("ID_HDC_G0", "year"))

#local economy - (Night Lights)
rastlist = list.files(path = "Harmonised_NTL", 
                      pattern='.NTL', all.files=TRUE, full.names=FALSE)

#split up as images have different extents
rastlist_a = rastlist[c(1:12, 16:22)] 
rastlist_b = rastlist[13:15]

setwd("E:/nd17631/Chapter IV/Data/Harmonised_NTL") 
rasterstack_a = lapply(rastlist_a, raster) %>% 
  raster::stack(allrasters)
ntl_a = as.data.frame(exactextractr::exact_extract(rasterstack_a, smod_africa, fun = "mean"))
colnames(ntl_a) = c(1997:2008, 2012:2018)

rasterstack_b = lapply(rastlist_b, raster) %>% 
  raster::stack(allrasters)
ntl_b = as.data.frame(exactextractr::exact_extract(rasterstack_b, smod_africa, fun = "mean"))
colnames(ntl_b) = c(2009:2011)

setwd("E:/nd17631/Chapter IV/Data")

ntl = cbind(ntl_a, ntl_b) %>%
  dplyr::mutate(ID_HDC_G0 = as.character(smod_africa$ID_HDC_G0)) %>%
  reshape2::melt(., measure.vars = 1:22, variable.name = "year") %>%
  dplyr::rename(ntlMean = value)

smod_panel = dplyr::left_join(smod_panel, ntl, by = c("ID_HDC_G0", "year"))

#urban education - for 2000-2014 think what to do? Maybe just replace front and end years
rastlist <- list.files(path = "IHME_AFRICA_EDU_2000_2015_YEARS_FEMALE_15_49_MEAN", 
                       pattern='.TIF$', all.files=TRUE, full.names=FALSE)
setwd("E:/nd17631/Chapter IV/Data/IHME_AFRICA_EDU_2000_2015_YEARS_FEMALE_15_49_MEAN") 
rasterstack <- lapply(rastlist, raster) %>% 
  raster::stack(allrasters)
setwd("E:/nd17631/Chapter IV/Data")

female = as.data.frame(exactextractr::exact_extract(rasterstack, smod_africa, fun = "mean")) %>%
  `colnames<-`(c(2000:2014)) %>% 
  dplyr::mutate(ID_HDC_G0 = as.character(smod_africa$ID_HDC_G0)) %>%
  reshape2::melt(., measure.vars = 1:15, variable.name = "year") %>%
  dplyr::rename(female_education = value)

rastlist <- list.files(path = "IHME_AFRICA_EDU_2000_2015_YEARS_MALE_15_49_MEAN", 
                       pattern='.TIF$', all.files=TRUE, full.names=FALSE)
setwd("E:/nd17631/Chapter IV/Data/IHME_AFRICA_EDU_2000_2015_YEARS_MALE_15_49_MEAN") 
rasterstack <- lapply(rastlist, raster) %>% 
  raster::stack(allrasters)
setwd("E:/nd17631/Chapter IV/Data")

male = as.data.frame(exactextractr::exact_extract(rasterstack, smod_africa, fun = "mean")) %>%
  `colnames<-`(c(2000:2015)) %>% 
  dplyr::mutate(ID_HDC_G0 = as.character(smod_africa$ID_HDC_G0)) %>%
  reshape2::melt(., measure.vars = 1:16, variable.name = "year") %>%
  dplyr::rename(male_education = value) %>%
  dplyr::filter(!(year == 2015))

smod_panel = smod_panel %>%
  dplyr::left_join(., female, by = c("ID_HDC_G0", "year")) %>%
  dplyr::left_join(., male, by = c("ID_HDC_G0", "year")) %>%
  group_by(ID_HDC_G0) %>%
  tidyr::fill(female_education, .direction = "down") %>%
  tidyr::fill(female_education, .direction = "up") %>%
  tidyr::fill(male_education, .direction = "down") %>%
  tidyr::fill(male_education, .direction = "up") %>%
  dplyr::mutate(Meaneducation = (male_education + female_education)/2)
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
            wb_gdp %>% dplyr::filter(GID_0 == "SSD" & year %in% 2011:2018)) %>%
  dplyr::mutate(GID_0 = "SSD")

wb_gdp = rbind(wb_gdp %>% dplyr::filter(!(GID_0 == "SSD")), ssd) %>%
  dplyr::filter(GID_0 %in% smod_africa$GID_0) %>%
  dplyr::rename(wb_gdp = value) %>%
  dplyr::select(GID_0, year, wb_gdp) %>%
  dplyr::mutate(year = as.character(year)) %>%
  dplyr::group_by(GID_0) %>% #group by id
  dplyr::mutate(wb_gdp_growth = (wb_gdp - lag(wb_gdp))/lag(wb_gdp)) %>%
  dplyr::ungroup()
  
smod_panel = dplyr::left_join(smod_panel, wb_gdp, by = c("GID_0", "year")) %>%
    group_by(ID_HDC_G0) %>%
    dplyr::mutate(wb_gdp_growth = (wb_gdp - lag(wb_gdp))/lag(wb_gdp))
    
#ongoing conflict in country
acd = read.csv("ucdp-prio-acd-201-csv/ucdp-prio-acd-201.csv") %>%
  dplyr::filter(region == 4) %>%
  dplyr::mutate(ISO_a = countrycode::countrycode(gwno_a, origin = 'gwn', 
                                                 destination = "iso3c", warn = FALSE),
                ISO_b = countrycode::countrycode(gwno_b, origin = 'gwn', 
                                                 destination = "iso3c", warn = FALSE))

smod_panel = smod_panel %>%
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
  dplyr::filter(GID_0 %in% smod_africa$GID_0) %>%
  dplyr::rename(wb_births = value) %>%
  dplyr::select(GID_0, year, wb_births)

smod_panel = dplyr::left_join(smod_panel, births, by = c("GID_0", "year"))

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
  dplyr::filter(GID_0 %in% smod_africa$GID_0) %>%
  dplyr::rename(wb_deaths = value) %>%
  dplyr::select(GID_0, year, wb_deaths)

smod_panel = dplyr::left_join(smod_panel, deaths, by = c("GID_0", "year")) %>%
  dplyr::mutate(natural_increase = wb_births - wb_deaths)

#Executive and legislative elections
nelda = read_xls("NELDA 5.0/NELDA.xls") %>%
  dplyr::select(1:8) %>%
  dplyr::mutate(GID_0 = countrycode::countryname(country, destination = "iso3c", warn = FALSE),
                year = as.character(year)) %>%
  dplyr::filter(GID_0 %in% smod_africa$GID_0) %>%
  dplyr::group_by(GID_0, year) %>%
  dplyr::summarise(n_elections = n()) %>%
  dplyr::mutate(election_event = ifelse(n_elections > 0, 1, 0)) %>%
  drop_na()

smod_panel = dplyr::left_join(smod_panel, nelda, by = c("GID_0", "year")) %>% 
  dplyr::mutate(n_elections = ifelse(is.na(n_elections), 0, n_elections),
                election_event = ifelse(is.na(election_event), 0, election_event))

# 4) Protest data ---------------------------------------------------------

#ACLED Protest
acled = read.csv("1997-01-01-2019-12-31.csv", sep = ";") %>%
  st_as_sf(., coords = c('longitude', 'latitude'), crs= crs(smod_africa)) %>%
  st_join(., smod_africa, st_within, left = FALSE) %>% # assign settlement attributes to acled events
  st_drop_geometry()

protest = acled %>%
  dplyr::filter(event_type == "Protests") %>%
  dplyr::group_by(year, ID_HDC_G0) %>%
  dplyr::summarise(acled_protest_count = n()) %>%
  dplyr::mutate(year = as.character(year),
                ID_HDC_G0 = as.character(ID_HDC_G0)) %>%
  drop_na()

riots = acled %>%
  dplyr::filter(event_type == "Riots") %>%
  dplyr::group_by(year, ID_HDC_G0) %>%
  dplyr::summarise(acled_riot_count = n()) %>%
  dplyr::mutate(year = as.character(year),
                ID_HDC_G0 = as.character(ID_HDC_G0)) %>%
  drop_na()

smod_panel = dplyr::left_join(smod_panel, protest, by = c("year", "ID_HDC_G0")) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(acled_protest_event = ifelse(acled_protest_count < 1, 0, 1)) %>%
  dplyr::left_join(., riots, by = c("year", "ID_HDC_G0")) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(acled_riot_event = ifelse(acled_riot_count < 1, 0, 1),
                acled_count = acled_protest_count + acled_riot_count, 
                acled_event = ifelse(acled_count < 1, 0, 1))

#ACLED - Organised violence
#battles = acled %>%
#  dplyr::filter(event_type == "Battles") %>%
#  dplyr::group_by(year, ID_HDC_G0) %>%
#  dplyr::summarise(acled_battle_count = n()) %>%
#  dplyr::mutate(year = as.character(year), 
#                ID_HDC_G0 = as.character(ID_HDC_G0)) %>%
#  drop_na()

#remote = acled %>%
#  dplyr::filter(event_type == "Explosions/Remote violence") %>%
#  dplyr::group_by(year, ID_HDC_G0) %>%
#  dplyr::summarise(acled_remote_count = n()) %>%
#  dplyr::mutate(year = as.character(year), 
#                ID_HDC_G0 = as.character(ID_HDC_G0)) %>%
#  drop_na()

#vac = acled %>%
#  dplyr::filter(event_type == "Violence against civilians") %>%
#  dplyr::group_by(year, ID_HDC_G0) %>%
#  dplyr::summarise(acled_vac_count = n()) %>%
#  dplyr::mutate(year = as.character(year),
#                ID_HDC_G0 = as.character(ID_HDC_G0)) %>%
#  drop_na()

#smod_panel = dplyr::left_join(smod_panel, battles, by = c("year", "ID_HDC_G0")) %>%
#  replace(is.na(.), 0) %>%
#  dplyr::mutate(acled_battle_event = ifelse(acled_battle_count < 1, 0, 1)) %>%
#  dplyr::left_join(., remote, by = c("year", "ID_HDC_G0")) %>%
#  replace(is.na(.), 0) %>%
#  dplyr::mutate(acled_remote_event = ifelse(acled_remote_count < 1, 0, 1)) %>%
#  dplyr::left_join(., vac, by = c("year", "ID_HDC_G0")) %>%
#  replace(is.na(.), 0) %>%
#  dplyr::mutate(acled_vac_event = ifelse(acled_vac_count < 1, 0, 1),
#                acled_OV_count = acled_battle_count + acled_remote_count + acled_vac_count, 
#                acled_OV_event = ifelse(acled_OV_count < 1, 0, 1))

#SCAD Protests 
scad = read.csv("SCAD2018Africa_Final/SCAD2018Africa_Final.csv") %>%
  st_as_sf(., coords = c('longitude', 'latitude'), crs= crs(smod_africa)) %>%
  st_join(., smod_africa, st_within, left = FALSE) %>% # assign settlement attributes to scad events
  st_drop_geometry() %>%
  dplyr::mutate(ID_HDC_G0 = as.character(ID_HDC_G0))

demon = scad %>%
  dplyr::filter(etype %in% 1:2) %>%
  dplyr::group_by(eyr, ID_HDC_G0) %>%
  dplyr::summarise(scad_demon_count = n()) %>%
  dplyr::mutate(year = as.character(eyr)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-eyr)

riot = scad %>%
  dplyr::filter(etype %in% 3:4) %>%
  dplyr::group_by(eyr, ID_HDC_G0) %>%
  dplyr::summarise(scad_riot_count = n()) %>%
  dplyr::mutate(year = as.character(eyr)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-eyr)

strike = scad %>%
  dplyr::filter(etype %in% 5:6) %>%
  dplyr::group_by(eyr, ID_HDC_G0) %>%
  dplyr::summarise(scad_strike_count = n()) %>%
  dplyr::mutate(year = as.character(eyr)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-eyr)

smod_panel = dplyr::left_join(smod_panel, demon, by = c("year", "ID_HDC_G0")) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(scad_demon_event = ifelse(scad_demon_count < 1, 0, 1)) %>%
  dplyr::left_join(., riot, by = c("year", "ID_HDC_G0")) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(scad_riot_event = ifelse(scad_riot_count < 1, 0, 1)) %>%
  dplyr::left_join(., strike, by = c("year", "ID_HDC_G0")) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(scad_strike_event = ifelse(scad_strike_count < 1, 0, 1),
                scad_count = (scad_demon_count + scad_riot_count + scad_strike_count), 
                scad_event = ifelse(scad_count < 1, 0, 1))

# 5) Conflict data ---------------------------------------------------------
#ged = read.csv("ged211.csv") %>%
#  st_as_sf(., coords = c('longitude', 'latitude'), crs= crs(smod_africa)) %>%
#  st_join(., smod_africa, st_within, left = FALSE) %>% # assign settlement attributes to ged events
#  st_drop_geometry() %>%
#  dplyr::mutate(ID_HDC_G0 = as.character(ID_HDC_G0))

#state_based = ged %>%
#  dplyr::filter(type_of_violence == 1) %>%
#  dplyr::group_by(year, ID_HDC_G0) %>%
#  dplyr::summarise(state_based_count = n()) %>%
#  dplyr::mutate(year = as.character(year)) %>%
#  dplyr::ungroup()

#non_state = ged %>%
#  dplyr::filter(type_of_violence == 2) %>%
#  dplyr::group_by(year, ID_HDC_G0) %>%
#  dplyr::summarise(non_state_count = n()) %>%
#  dplyr::mutate(year = as.character(year)) %>%
#  dplyr::ungroup()

#one_sided = ged %>%
#  dplyr::filter(type_of_violence == 3) %>%
#  dplyr::group_by(year, ID_HDC_G0) %>%
#  dplyr::summarise(one_sided_count = n()) %>%
#  dplyr::mutate(year = as.character(year)) %>%
#  dplyr::ungroup()

#smod_panel = dplyr::left_join(smod_panel, state_based, by = c("year", "ID_HDC_G0")) %>%
#  replace(is.na(.), 0) %>%
#  dplyr::mutate(state_based_event = ifelse(state_based_count < 1, 0, 1)) %>%
#  dplyr::left_join(., non_state, by = c("year", "ID_HDC_G0")) %>%
#  replace(is.na(.), 0) %>%
#  dplyr::mutate(non_state_event = ifelse(non_state_count < 1, 0, 1)) %>%
#  dplyr::left_join(., one_sided, by = c("year", "ID_HDC_G0")) %>%
#  replace(is.na(.), 0) %>%
#  dplyr::mutate(one_sided_event = ifelse(one_sided_count < 1, 0, 1),
#                ged_count = (state_based_count + non_state_count + one_sided_count), 
#                ged_event = ifelse(ged_count < 1, 0, 1))

# 6) Ethnic-Political exclusion -------------------------------------------
#number of excluded groups (thanks prio-grid for providing base code)

#this makes sense in a yearly model when it is the primary variable of interest ... 
#add to script once I've updated that script ... 
#epr = read.csv("EPR-2021.csv") %>%
#  dplyr::mutate(epr_excluded = ifelse(status == "POWERLESS" | status == "DISCRIMINATED",
#                                      yes = 1, no = 0), year = purrr::map2(from, to, seq)) %>%
#  tidyr::unnest(year) %>%
#  dplyr::select(-from, -to) %>%
#  dplyr::filter(year >= 1996)

#geo_epr = st_read("GeoEPR-2021/GeoEPR-2021.shp") %>%
#  st_transform(st_crs(smod_africa)) %>% 
#  dplyr::filter(type == "Regionally based" | type == "Regional & urban" | type == "Aggregate") %>%
#  dplyr::mutate(year = purrr::map2(from, to, seq)) %>% 
#  tidyr::unnest(year) %>% dplyr::select(-from, -to) %>% 
#  subset(year >= 1996) %>%  
#  dplyr::left_join(epr) %>%
#  dplyr::mutate(status_cat = ifelse(status == "MONOPOLY" | status == "DOMINANT", 1,
#                                    ifelse(status == "SENIOR PARTNER" | status == "JUNIOR PARTNER", 2,
#                                           ifelse(status == "POWERLESS" | status == "DISCRIMINATED", 3,
#                                                  NA)))) %>% # Removing "irrelevant", "self-exclusion", and "state collapse"
#  dplyr::arrange(statename, group, year) %>%
#  dplyr::group_by(gwgroupid) %>%
#  dplyr::mutate(lag_status = dplyr::lag(status, n = 1),
#                lag_status_cat = dplyr::lag(status_cat, n = 1)) %>%
#  dplyr::ungroup() %>%
#  dplyr::mutate(change = ifelse(status_cat != lag_status_cat, 1, 0)) %>%
#  dplyr::mutate(epr_promoted = ifelse(as.numeric(lag_status_cat) > as.numeric(status_cat), 1, 0),
#                epr_demoted = ifelse(as.numeric(lag_status_cat) < as.numeric(status_cat), 1, 0)) %>%
#  dplyr::mutate(epr_promoted = tidyr::replace_na(epr_promoted, 0),
#                epr_demoted = tidyr::replace_na(epr_demoted, 0)) %>%
#  dplyr::mutate(epr_regional = ifelse(type == "Regionally based", yes = 1, no = 0)) %>%
#  dplyr::select(statename, gwid, gwgroupid, group, year, epr_promoted, epr_demoted, epr_excluded, epr_regional, geometry) %>%
#  dplyr::filter(!sf::st_is_empty(geometry))

#geo_epr_smod = st_join(smod_africa, geo_epr, st_intersects, left = T) %>%
#  st_drop_geometry() %>%
#  dplyr::mutate(epr_excluded_groups = ifelse(epr_excluded > 0, paste0(group), NA)) %>%
#  dplyr::group_by(year, ID_HDC_G0) %>%
#  dplyr::summarise(epr_excluded = sum(epr_excluded), #should return the number of excluded groups in each unit - ranges 0-9, maybe too high? 
#                   epr_groups = paste0(sort(unique(group)), collapse = ", "),
#                   epr_excluded_groups = ifelse(epr_excluded > 0, 
#                                                (paste0(sort(unique(epr_excluded_groups)), 
#                                                        collapse = ", ")),NA),
#                   epr_promoted = ifelse(sum(epr_promoted) > 0, 1, 0),
#                   epr_demoted = ifelse(sum(epr_demoted) > 0, 1, 0),
#                   epr_regional = ifelse(sum(epr_regional) > 0, 1, 0)) %>%
#  dplyr::mutate(epr_n_excluded = epr_excluded,
#                epr_excluded = ifelse(epr_n_excluded > 0, 1, 0),
#                year = as.character(year)) 

#smod_panel = dplyr::left_join(smod_panel, geo_epr_smod, by = c("GID", "year")) %>%
#  dplyr:: mutate(epr_excluded = tidyr::replace_na(epr_excluded, 0),
#                 epr_promoted= tidyr::replace_na(epr_promoted, 0),
#                 epr_demoted= tidyr::replace_na(epr_demoted, 0),
#                 epr_regional= tidyr::replace_na(epr_regional, 0),
#                 epr_n_excluded= tidyr::replace_na(epr_n_excluded, 0),
#                 epr_excluded_groups = tidyr::replace_na(epr_excluded_groups, "Null"),
#                 epr_groups = tidyr::replace_na(epr_groups, "Null"))


save.image("smod_panel_build.RData")

# final data cleaning  ----------------------------------------------------

#cities per country 
country_city = smod_africa %>%
  dplyr::group_by(GID_0) %>%
  summarise(country_city_count = n()) %>%
  arrange(country_city_count)

#filter out countries with <3 cities for ML. Leaves 2163 cities in 38 countries. 
greater_than_3 = country_city %>% 
  dplyr::filter(country_city_count > 2)

#create final data including education, neonatal, and imr - just three bins. 
final_smod_panel = smod_panel %>% 
  dplyr::filter(GID_0 %in% greater_than_3$GID_0 & POP_2015 >= 100000 & year %in% 2001:2015) %>%
  #remove obsolete columns & education (save for subset)
  #dplyr::select(-c(female_1519, male_1519, female_2024, male_2024)) %>%
  #remove 5 cities with zero values on imr, educ, and neotatal
  dplyr::filter(!(imrMean == 0)) %>%
  #variable transformations
  dplyr::mutate(log_world_pop = log(world_pop),
                log_world_pop_dens = log(world_pop_dens),
                log_gpw_pop = log(1+gpw_pop), 
                log_gpw_pop_dens = log(gpw_pop_dens),
                log_ls_pop = log(1+ls_pop), 
                log_ls_pop_dens = log(ls_pop_dens),
                log_ghs_pop = log(ghs_pop_linear),
                log_ghs_pop_dens = log(ghs_pop_dens),
                #log_young_men = log(young_men_15_24), 
                #log_young_women = log(young_women_15_24), 
                #log_youth_bulge = log(youth_bulge), 
                log_ntlMean = log(1+ntlMean),
                log_wb_gdp = log(1+wb_gdp), 
                log_imr = log(imrMean),
                #log_neonatal = log(neonatalMean),
                #calculate Winsorized growth variables
                wind_wp_growth = DescTools::Winsorize(world_pop_growth, probs = c(0.1, 0.9)), 
                wind_gpw_growth = DescTools::Winsorize(gpw_pop_growth, probs = c(0.1, 0.9)),
                wind_ls_growth = DescTools::Winsorize(ls_pop_growth, probs = c(0.1, 0.9)),
                wind_ghs_growth = DescTools::Winsorize(ghs_pop_growth, probs = c(0.1, 0.9))) %>%
  group_by(ID_HDC_G0, year) %>%
  #calculate city rank variables
  dplyr::mutate(ghs_city_rank = dense_rank(desc(ghs_pop_linear)), 
                wp_city_rank = dense_rank(desc(world_pop)),
                gpw_city_rank = dense_rank(desc(gpw_pop)),
                ls_city_rank = dense_rank(desc(ls_pop)),
                #convert gpw to same percentage
                gpw_pop_growth = gpw_pop_growth*10) %>%
  ungroup() %>%
  group_by(ID_HDC_G0) %>%
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
  
# the windorisation appears to work - we can probably achieve this by trimming extreme values
ggplot(final_smod_panel, aes(x=wind_wp_growth)) + 
  geom_histogram()
ggplot(final_smod_panel, aes(x=wind_gpw_growth)) + 
  geom_histogram()
ggplot(final_smod_panel, aes(x=wind_ls_growth)) + 
  geom_histogram()
ggplot(final_smod_panel, aes(x=wind_ghs_growth)) + 
  geom_histogram()

#save these, then crease histos for the finished dataset. 
ghs_pop_growth_pre = ggplot(final_smod_panel, aes(x=ghs_pop_growth)) + 
  geom_histogram() + xlab("Population growth rate (%)") + ylab("Frequency")
ggsave(world_pop_growth_pre, filename = "figure2_ghs_pre.png", dpi = 400)

world_pop_growth_pre = ggplot(final_smod_panel, aes(x=world_pop_growth)) + 
  geom_histogram() + xlab("Population growth rate (%)") + ylab("Frequency")
ggsave(world_pop_growth_pre, filename = "figure2_wp_pre.png", dpi = 400)

gpw_pop_growth_pre = ggplot(final_smod_panel, aes(x=gpw_pop_growth)) + 
  geom_histogram() + xlab("Population growth rate (%)") + ylab("Frequency")
ggsave(gpw_pop_growth_pre, filename = "figure2_gpw_pre.png", dpi = 400)

ls_pop_growth_pre = ggplot(final_smod_panel, aes(x=ls_pop_growth)) + 
  geom_histogram() + xlab("Population growth rate (%)") + ylab("Frequency")
ggsave(ls_pop_growth_pre, filename = "figure2_ls_pre.png", dpi = 400)

# get values for cutoff from un agglos data
un_agglos_growth = read_xls("WUP2018-F14-Growth_Rate_Cities.xls", skip = 16) %>%
  dplyr::mutate(GID_0 = countrycode::countryname(`Country or area`, destination = "iso3c", warn = FALSE)) %>%
  dplyr::filter(GID_0 %in% smod_panel$GID_0) %>%
  dplyr::select(-c("Index", "Country Code", "Country or area", "City Code", 
                   "Note", "Latitude", "Longitude")) %>%
  pivot_longer(cols = 2:18, names_to = "year", values_to = "pop_growth")

plot(un_agglos_growth$pop_growth)
#crop outliers
un_agglos_growth = un_agglos_growth %>% filter(pop_growth < 20 & pop_growth > -10)
plot(un_agglos_growth$pop_growth) 
min(un_agglos_growth$pop_growth) #-2.59139
max(un_agglos_growth$pop_growth) #17.31634

#remove extreme population growth values 
final_smod_panel_wp = final_smod_panel %>% filter(world_pop_growth < 17 & world_pop_growth > -3)
#we don't lose any data
nrow(final_smod_panel_wp) - nrow(final_smod_panel)

ggplot(final_smod_panel, aes(x=world_pop_growth)) + 
  geom_histogram()

world_pop_growth_post = ggplot(final_smod_panel_wp, aes(x=world_pop_growth)) + 
  geom_histogram() + xlab("Population growth rate (%)") + ylab("Frequency")
ggsave(world_pop_growth_post, filename = "figure2_wp_post.png", dpi = 400)

polityV = ggplot(final_smod_panel_wp, aes(x=polity2)) + 
  geom_histogram() + xlab("Polity V Score") + ylab("Frequency")
ggsave(polityV, filename = "figur3.png", dpi = 400)

civilsoc = ggplot(final_smod_panel_wp, aes(x=v2csprtcpt)) + 
  geom_histogram() + xlab("Civil Society Participatory Environment") + ylab("Frequency")
ggsave(polityV, filename = "figur3.png", dpi = 400)

#save final data
saveRDS(final_smod_panel_wp, file = "smod_panel.rds")


#map for pres 

country_city = smod_africa %>%
  dplyr::filter(ID_HDC_G0 %in% final_smod_panel_wp$ID_HDC_G0) %>%
  dplyr::group_by(GID_0) %>%
  summarise(country_city_count = n()) %>%
  arrange(country_city_count) %>%
  st_drop_geometry()

africa = st_read("gadm36_levels_shp/gadm36_0.shp") %>%
  dplyr::filter(GID_0 %in% africa_iso$`alpha-3`) %>%
  dplyr::filter(!(NAME_0 %in% c("French Southern Territories","Comoros", "Cape Verde", 
                                "British Indian Ocean Territory", "Mauritius", "Mayotte",
                                "Reunion", "Saint Helena", "São Tomé and Príncipe", 
                                "Seychelles"))) %>%
  dplyr::left_join(., country_city, by = "GID_0")

ggplot(africa) + 
  geom_sf(aes(fill = country_city_count)) + 
  theme(legend.position="bottom")


nga_100k_centers = ggplot(africa) +
  geom_sf(fill = "white", size = 0.05) +
  geom_sf(ucdb_africa, fill = "black", size = 0.05, mapping = aes())
ggsave(filename = "nga_100k_centers.jpg", plot = nga_100k_centers, width = 12, height = 10, dpi = 400)


  
  
  
  #read out gwp data 
#remove extreme population growth values
ggplot(final_smod_panel, aes(x=gpw_pop_growth)) + 
  geom_histogram()

final_smod_panel_gpw = final_smod_panel %>% filter(gpw_pop_growth < 17 & gpw_pop_growth >-3) %>%
  dplyr::mutate(gpw_migrant_residual = gpw_pop_growth - world_pop_growth)

#we lose zero 
nrow(final_smod_panel) - nrow(final_smod_panel_gpw) 

gpw_pop_growth_post = ggplot(final_smod_panel_gpw, aes(x=gpw_pop_growth)) + 
  geom_histogram() + xlab("Population growth rate (%)") + ylab("Frequency")
ggsave(gpw_pop_growth_post, filename = "figure2_gpw_post.png", dpi = 400)

#save final data
saveRDS(final_smod_panel_gpw, file = "smod_panel_gpw.rds")

#read out landScan data
ggplot(final_smod_panel, aes(x=ls_pop_growth)) + 
  geom_histogram()
#remove extreme population growth values - we lose observations
final_smod_panel_ls = final_smod_panel %>% filter(ls_pop_growth < 17 & ls_pop_growth >-3)

#check distribution
ggplot(final_smod_panel_ls, aes(x=ls_pop_growth)) + 
  geom_histogram()

#we lose 79
nrow(final_smod_panel) - nrow(final_smod_panel_ls) 

#we don't lose any cities just a few years which does not matter because mlm. 
year_city = final_smod_panel_ls %>% #years per city 
  dplyr::group_by(ID_HDC_G0) %>%
  summarise(year_city_count = n()) %>%
  arrange(year_city_count) %>%
  filter(year_city_count > 2)

#have 1008 cities
length(unique(final_smod_panel_ls$ID_HDC_G0))

#lose no countries
length(unique(final_smod_panel_ls$GID_0))

ls_pop_growth_post = ggplot(final_smod_panel_ls, aes(x=ls_pop_growth)) + 
  geom_histogram() + xlab("Population growth rate (%)") + ylab("Frequency")
ggsave(ls_pop_growth_post, filename = "figure2_ls_post.png", dpi = 400)

#save final data
saveRDS(final_smod_panel_ls, file = "smod_panel_ls.rds")


#read out GHS-POP data
ggplot(final_smod_panel, aes(x=ghs_pop_growth)) + 
  geom_histogram()
#remove extreme population growth values - we lose observations
final_smod_panel_ghs = final_smod_panel %>% filter(ghs_pop_growth < 17 & ghs_pop_growth >-3)
#check distribution
ggplot(final_smod_panel_ghs, aes(x=ghs_pop_growth)) + 
  geom_histogram()
#convert to percentage
final_smod_panel_ghs = final_smod_panel_ghs %>%
  dplyr::mutate(ghs_pop_growth = ghs_pop_growth*10)
ggplot(final_smod_panel_ghs, aes(x=ghs_pop_growth)) + 
  geom_histogram()

#we lose 11
nrow(final_smod_panel) - nrow(final_smod_panel_ghs) 

#we don't lose any cities just a few years which does not matter. 
year_city = final_smod_panel_ghs %>% #years per city 
  dplyr::group_by(ID_HDC_G0) %>%
  summarise(year_city_count = n()) %>%
  arrange(year_city_count) %>%
  filter(year_city_count > 2)

#have 1008 cities
length(unique(final_smod_panel_ghs$ID_HDC_G0))

ghs_pop_growth_post = ggplot(final_smod_panel_ghs, aes(x=ghs_pop_growth)) + 
  geom_histogram() + xlab("Population growth rate (%)") + ylab("Frequency")
ggsave(ghs_pop_growth_post, filename = "figure2_ghs_post.png", dpi = 400)

#save final data
saveRDS(final_smod_panel_ghs, file = "smod_panel_ghs.rds")



# Population growth histograms for subnational variation -----------------------------

appendix_a_wp = ggplot(final_smod_panel_wp, aes(x=world_pop_growth)) + 
  geom_histogram() + 
  facet_wrap(~GID_0) + xlab("Population Growth (%)") + ylab("Frequency")
ggsave(appendix_a_wp, filename = "appendix_a_wp.png", dpi = 400)

appendix_a_gpw = ggplot(final_smod_panel_gpw, aes(x=gpw_pop_growth)) + 
  geom_histogram() + 
  facet_wrap(~GID_0) + xlab("Population Growth (%)") + ylab("Frequency")
ggsave(appendix_a_gpw, filename = "appendix_a_gpw.png", dpi = 400)

appendix_a_ls = ggplot(final_smod_panel_ls, aes(x=ls_pop_growth)) + 
  geom_histogram() + 
  facet_wrap(~GID_0) + xlab("Population Growth (%)") + ylab("Frequency")
ggsave(appendix_a_ls, filename = "appendix_a_ls.png", dpi = 400)

appendix_a_ghs = ggplot(final_smod_panel_ghs, aes(x=ghs_pop_growth)) + 
  geom_histogram() + 
  facet_wrap(~GID_0) + xlab("Population Growth (%)") + ylab("Frequency")
ggsave(appendix_a_ghs, filename = "appendix_a_ghs.png", dpi = 400)
