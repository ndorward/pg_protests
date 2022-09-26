setwd("~/OneDrive - University of Bristol/PhD/Chapter IV - Urban Protest/Data")

library(dplyr)
library(ggplot2)
library(ggmap)
library(sf)
library(raster)
library(terra)

register_google(key = "AIzaSyCfof_SFJ1IR5nr4TEBzgtOhBqt0TXmFX8")

#read SSA boundaries
africa_gadm = st_read("gadm36_levels_shp/gadm36_0.shp") %>%
  dplyr::filter(GID_0 %in% c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "DJI", "DZA", "EGY", "ERI", "ESH", "ETH",
                             "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LBY", "LSO", "MAR", "MDG", "MLI", "MOZ", "MRT", "MWI",
                             "NAM", "NER", "NGA", "RWA", "SDN", "SEN", "SLE", "SOM", "SSD", "SWZ", "TCD", "TGO", "TUN", "TZA", "UGA", "ZAF",
                             "ZMB", "ZWE")) %>% #read African countries excluding small islands
  rmapshaper::ms_simplify(., keep = 0.1, keep_shapes = TRUE)

#subset north africa countries not in analysis 
na_gadm = africa_gadm %>%
  dplyr::filter(NAME_0 %in% c("Algeria", "Egypt", "Western Sahara", "Sudan", "Libya", "Mauritania", 
                              "Morocco", "Tunisia"))

#read smod africa
smod_africa = readRDS("smod_africa.rds")

smod_africa_100k = smod_africa %>%
  dplyr::filter(POP_2015 > 100000)

smod_nga = smod_africa_100k %>%
  dplyr::filter(GID_0 == "NGA")

smod_uga = smod_africa_100k %>%
  dplyr::filter(GID_0 == "UGA")

# 1) All africa
all_centers = ggplot(africa_gadm) +
  geom_sf(fill = "white", size = 0.05) +
  geom_sf(na_gadm, fill = "grey", size =0.05, mapping = aes()) +
  geom_sf(smod_africa, fill = "black", size = .1, mapping = aes())

# 2) All Africa with 100k population threshold
all_centers_100k = ggplot(africa_gadm) +
  geom_sf(fill = "white", size = 0.05) +
  geom_sf(na_gadm, fill = "grey", size =0.05, mapping = aes()) +
  geom_sf(smod_africa, fill = "black", size = 0.1, mapping = aes())

# 3) Nigeria 100k 
nga_centers_100k = ggplot(africa_gadm %>% dplyr::filter(GID_0 == "NGA") ) +
  geom_sf(fill = "white", size = 0.05) +
  geom_sf(smod_nga, fill = "black", color = "red", size = 0.2, mapping = aes())
ggsave(filename = "nga_centers_100k.jpg", plot = nga_centers_100k, width = 12, height = 10, dpi = 400)

# 4) Lagos centers 100k pop
p = ggmap(get_googlemap(center = c(lon = 3.311414, lat = 6.631663), zoom = 10, scale = 2, 
                        maptype ='satellite',color = 'color'))

#filter centers over 100k in Lagos region
lagos = smod_nga %>%
  dplyr::filter(NAME_MAIN %in% c("Lagos","Ikorodu", "Iguan")) #Iguan should actually be Sefu

#bb = st_bbox(lagos)

#define plot window 
disp_win = st_sfc(st_point(c(2.9, 6.3)), st_point(c(3.8, 7)),
                  crs = 4326)

#extract coordinates of plot window
disp_win_coord = st_coordinates(disp_win)

#lagos plot

#p + geom_sf(smod_nga, fill = "black", color = "red", size = 0.5, mapping = aes())

lagos_centers_100k = ggplot(africa_gadm %>% dplyr::filter(GID_0 == "NGA") ) +
  geom_sf(fill = "white", size = 0.05) +
  geom_sf(smod_nga, fill = "black", color = "red", size = 0.5, mapping = aes()) + 
  coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
           datum = 4326, expand = FALSE) 
ggsave(filename = "lagos_centers_100k.jpg", plot = lagos_centers_100k, width = 12, height = 10, dpi = 400)

# 4) Nigeria - polygon over population over population grid
#read nga pop raster
nga_pop = terra::rast("nga_ppp_2015_1km_Aggregated.tif")

#define extent for lagos region
e = extent(2.7619302273, 4.5259122657, 6.2431533337, 7.4308369866)

#create raster subset for lagos and polygonise
lagos_pop = nga_pop %>% 
  crop(., e)
lagos_pop = terra::as.polygons(lagos_pop) %>% #polygonise 
  sf::st_as_sf() %>% #convert to sf
  dplyr::rename(ppp_2015 = nga_ppp_2015_1km_Aggregated) %>%
  dplyr::mutate(log_ppp_2015 = log(ppp_2015)) #create logged variable

#create nigeria polygons  
nga_pop = terra::as.polygons(nga_pop) %>% #polygonise 
  sf::st_as_sf() %>% #convert to sf
  dplyr::rename(ppp_2015 = nga_ppp_2015_1km_Aggregated) %>%
  dplyr::mutate(log_ppp_2015 = log(ppp_2015)) #create logged variable

plot(nga_pop['ppp_2015'])

#plot for Nigeria
nga_pop_plot = ggplot(nga_pop) +
  geom_sf(aes(fill = log_ppp_2015), size = .1) +
  scale_fill_gradient(name = "Population count (log)", low = "white", high = "black") +
  geom_sf(smod_nga, fill = "red", color = "red", size = .2, alpha = 0.05,mapping = aes(), 
          inherit.aes = FALSE) + 
  theme(legend.position="bottom", text = element_text(size = 20))
ggsave(filename = "nga_pop_plot.jpg", plot = nga_pop_plot, width = 12, height = 10, dpi = 400)

# 5) Lagos - polygon over population grid -------------------------------
lagos_pop_plot = ggplot(lagos_pop) +
  geom_sf(aes(fill = log_ppp_2015), size = .2) +
  scale_fill_gradient(name = "Population count (log)", low = "white", high = "black") +
  geom_sf(lagos, fill = "red", color = "red", size = .5, alpha = 0.05,mapping = aes(), 
          inherit.aes = FALSE) +
  coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
           datum = 4326, expand = FALSE) + 
  theme(legend.position="bottom", text = element_text(size = 20))
ggsave(filename = "lagos_pop_plot.jpg", plot = lagos_pop_plot, width = 12, height = 10, dpi = 400)


# 4) uganda - polygon over population over population grid
#read nga pop raster
uga_pop = terra::rast("uga_ppp_2015_1km_Aggregated.tif")

kampala = ggmap(get_googlemap(center = c(lon = 32.582520, lat = 0.347596), zoom = 10, scale = 2, 
                        maptype ='satellite',color = 'color'))

kampala_bb = bbox(kampala)

e = extent(32.328254, 32.875167, 0.071334, 0.57698)

#create raster subset for lagos and polygonise
kampala_pop = uga_pop %>% 
  crop(., e)
kampala_pop = terra::as.polygons(kampala_pop) %>% #polygonise 
  sf::st_as_sf() %>% #convert to sf
  dplyr::rename(ppp_2015 = uga_ppp_2015_1km_Aggregated) %>%
  dplyr::mutate(log_ppp_2015 = log(ppp_2015)) #create logged variable

kampala = smod_uga %>%
  dplyr::filter(NAME_MAIN %in% c("Kampala","Matuga")) #Iguan should actually be Sefu


#create nigeria polygons  
uga_pop = terra::as.polygons(uga_pop) %>% #polygonise 
  sf::st_as_sf() %>% #convert to sf
  dplyr::rename(ppp_2015 = uga_ppp_2015_1km_Aggregated) %>%
  dplyr::mutate(log_ppp_2015 = log(ppp_2015)) #create logged variable

plot(uga_pop['ppp_2015'])

#plot for Uganda
uga_pop_plot = ggplot(uga_pop) +
  geom_sf(aes(fill = log_ppp_2015), size = .1) +
  scale_fill_gradient(name = "Population count (log)", low = "white", high = "black") +
  geom_sf(smod_uga, fill = "red", color = "red", size = .2, alpha = 0.05,mapping = aes(), 
          inherit.aes = FALSE) + 
  theme(legend.position="bottom", text = element_text(size = 20))
ggsave(filename = "uga_pop_plot.jpg", plot = uga_pop_plot, width = 12, height = 10, dpi = 400)

# 5) Lagos - polygon over population grid -------------------------------

disp_win = st_sfc(st_point(c(32.328254, 0.11334)), st_point(c(32.875167, 0.55698)),
                  crs = 4326)

#extract coordinates of plot window
disp_win_coord = st_coordinates(disp_win)

kampala_pop_plot = ggplot(kampala_pop) +
  geom_sf(aes(fill = log_ppp_2015), size = .2) +
  scale_fill_gradient(name = "Population count (log)", low = "white", high = "black") +
  geom_sf(kampala, fill = "red", color = "red", size = .5, alpha = 0.05,mapping = aes(), 
          inherit.aes = FALSE) +
  coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
           datum = 4326, expand = FALSE) + 
  theme(legend.position="bottom", text = element_text(size = 20))
ggsave(filename = "kampala_pop_plot.jpg", plot = kampala_pop_plot, width = 12, height = 10, dpi = 400)








