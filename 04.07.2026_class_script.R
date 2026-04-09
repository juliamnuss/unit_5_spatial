# 04.07.2026
# JMN
# Class Script
# Unit 5
# 5.2 Vector data

# install.packages("sf")
library(sf)
library(tidyverse)
library(mapdata)
library(marmap)

# read in whale carcass location data
carcass = read.csv("data/RW_carcasses_2017.csv")
# read in US critical habitat shapefiles
USA_crit_hab = st_read(dsn = 'data/North_Atlantic_Right_Whale_Critical_Habitat/', layer = 'North_Atlantic_Right_Whale_Critical_Habitat')
USA_crit_hab
st_crs(USA_crit_hab)$epsg # find EPSG code
USA_crit_hab_sf = st_transform(USA_crit_hab, crs = 4326)

# load in Canadian RW critical habitat coordinates
CAN_crit_hab = read.csv('data/NARW_canadian_critical_habitat_2017.csv')
head(CAN_crit_hab)

# turn data frame into sf points, then sf polygon
CAN_crit_hab_sf = CAN_crit_hab %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  dplyr::group_by(habitat, country) %>%
  dplyr::summarize(do_union = FALSE) %>%
  st_cast(to = "POLYGON")
head(CAN_crit_hab_sf)

# Simplify USA_crit_hab data rame to match CAN_crit_hab
plot(USA_crit_hab_sf$geometry[1], axes = TRUE) # GOM habitat
plot(USA_crit_hab_sf$geometry[2], axes = TRUE) # FL / GA habitat
USA_crit_hab_sf$habitat = c("GOM", "SEUS")
USA_crit_hab_sf$country = "USA"
USA_crit_hab_sf = USA_crit_hab_sf %>%
  dplyr::select(country, habitat, geometry) # drop all other variables

# join the USA and Canada critical habitat sf objects
crit_hab = rbind(USA_crit_hab_sf, CAN_crit_hab_sf)

# Making Maps

# set GOM + GSL map limits
lon_bounds = c(-72, -54)
lat_bounds = c(39, 53)

# coastline data
world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)

# plot critical habitats and carcass locations
crit_map = ggplot() + 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black") +
  geom_sf(data = crit_hab, alpha = 0.5, aes(fill = country)) + 
  geom_point(data = carcass, aes(x = Longitude, y = Latitude, color = Carcass_position), size = 2) +
  coord_sf(xlim = lon_bounds, ylim = lat_bounds) + 
  labs(y = "Latitude", x = "Longitude") + 
  theme_bw()
crit_map

# install.packages("ggnewscale")
library(ggnewscale)

bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1]-2,
                                    lon2 = lon_bounds[2]+2,
                                    lat1 = lat_bounds[1]-2,
                                    lat2 = lat_bounds[2]+2,
                                    resolution = 4)

# convert bathymetry to data frame
bath_m_fortify = marmap::fortify.bathy(bath_m_raw)
bath_m = bath_m_fortify %>%
  mutate(depth_m = ifelse(z>0, NA, z)) %>%
  dplyr::select(-z)

# plot critical habitats and carcass locations over bathymetry
rw_map = ggplot() +
  geom_raster(data = bath_m, aes(x = x, y = y, fill = depth_m), alpha = 0.75) +
  scale_fill_gradientn(colors = c("black", "navy", "blue4", "lightblue"),
                        values = scales::rescale(c(-5000, -3000, -300, 0)),
                        name = "Depth (m)") + 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) +
  geom_sf(data = crit_hab, alpha = 0.5, fill = "yellow") + 
  geom_point(data = carcass, aes(x = Longitude, y = Latitude, color = Carcass_position), size = 2) +
  coord_sf(xlim = lon_bounds, ylim = lat_bounds) +
  labs(y = "Latitude", x = "Longitude") +
  theme_bw()
rw_map
ggsave(rw_map, filename = 'figures/rw_habitats.png', device = "png", height = 5, width = 7)
