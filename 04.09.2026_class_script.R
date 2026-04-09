# 04.09.2026
# JMN
# Class Script
# Unit 5
# 5.3 Intersections and distance calculations

library(tidyverse)
library(sf)
library(mapdata)
library(lubridate)

# AIS data downloaded fro january 25, 2017
ais_day = read.csv('data/processed_ais/ais_2017-01-25.csv')
head(ais_day)

# coastline data
lat_bounds = c(25, 34)
lon_bounds = c(-82, -76)
world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)
dim(world_map)

# read in US critical habitat shapefiles
USA_crit_hab = st_read("data/North_Atlantic_Right_Whale_Critical_Habitat", "North_Atlantic_Right_Whale_Critical_Habitat")

# plot critical habitats and carcass locations
ais_map_pts = ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_sf(data = USA_crit_hab, alpha = 0.5, color = NA, fill = "yellow") + 
  geom_point(data = ais_day, aes(x = longitude, y = latitude, color = call_sign)) +
  coord_sf(xlim = lon_bounds, ylim = lat_bounds) + 
  guides(color = "none") + 
  labs( y = "Latitude", x = "Longitude") + 
  theme_bw()
ais_map_pts

# find AIS pings that intersect with RW habitat
Sys.time() # tells you the time on your computer when you run it
ships_RW_intersect = ais_day %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269) %>%
st_intersection(USA_crit_hab %>% dplyr::select(geometry))
Sys.time()

law_breakers = ships_RW_intersect %>%
  filter(length > 20, sog > 10)

dim(law_breakers)
summary(law_breakers)

length(unique(law_breakers$call_sign)) # how many law breakers?
unique(law_breakers$vessel_name) # what are their names?

illegal_paths = law_breakers %>%
  mutate(date_time = lubridate::ymd_hms(base_date_time)) %>% # calendar/time
  arrange(date_time) %>% # ensure ship track points are in chronological order
  group_by(call_sign) %>% 
  summarise(do_union = FALSE) %>% # collapses data into multipoint; do_union = FALSE prevents reordering points
  st_cast("LINESTRING") %>% 
  st_make_valid() # gets rid of lines with < 2 points

# plot ship tracks
# install.packages("mapview")
library(mapview)

law_breaking_map = ggplot() + 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # plotting the coastlines
  geom_sf(data = USA_crit_hab, alpha = 0.5, color = NA, fill = "yellow") + 
  geom_sf(data = illegal_paths, aes(color = call_sign)) + 
  coord_sf(xlim = lon_bounds, ylim = lat_bounds) +
  geom_point(aes(x = -81.09, y = 32.08), size = 2, color = "red", shape = 8) +
  labs( y = "Latitude", x = "Longitude") + 
  theme_bw()
law_breaking_map

# what are the lengths of the ship tracks that intersect the RW critical habitat?
illegal_path_lengths = illegal_paths %>%
  mutate(track_length_m = st_length(geometry))
class(illegal_path_lengths$track_length_m)

tot_illegal_path = sum(illegal_path_lengths$track_length_m)

# or convert the lengths form units class to numeric class and calculate total track length using dplyr
tot_illegal_path = illegal_paths %>%
  mutate(track_length_m = as.numeric(st_length(geometry))) %>%
  summarise(tot_track_length_m = sum(track_length_m)) %>%
  mutate(tot_track_length_km = tot_track_length_m/1000) %>% 
  st_drop_geometry()

tot_illegal_path
