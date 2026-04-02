# 04.02.2026
# JMN
# Class Script
# Unit 5
# 5.1 Rasters

# install.packages("raster")
# install.packages("mapdata")
# install.packages("marmap")
# install.packages("ncdf4")

library(tidyverse)
library(raster)
library(mapdata) # map_data worldHires coastline
library(marmap) # getNOAA.bathy()
# library(ncdf4) # also good for reading netCDF files

chl_raster = raster('data/AQUA_MODIS.20020701_20250731.L3m.MC.CHL.chlor_a.9km.nc')

class(chl_raster)
chl_raster

# rename raster layer
names(chl_raster) = "chl_a"
# convert to dataframe
chl_pts = raster::rasterToPoints(chl_raster, spatial = TRUE)
chl_df = data.frame(chl_pts) # explicityly converts spatial data (lat adn long) into columns
head(chl_df)

# see range of data to set good limits for color palette
hist(chl_df$chl_a) # weird data distribution under normal conditions
max(chl_df$chl_a)
hist(log10(chl_df$chl_a)) # viewing under log scale helps use to see variation in data bc the numnbers are sooo small
cols = rainbow(7, rev = TRUE)[-1] # reverse rainbow color hex codes, drops the first color (purple)

# plot global chl
global_chl_map = ggplot() +
  geom_raster(data = chl_df, aes(x = x, y = y, fill = log10(chl_a))) +
  scale_fill_gradientn(colors = cols, 
                        limits = c(-1.5, 0.75), 
                        oob = scales::squish, 
                        name = "log_10(chl_a)") +
  ggtitle("Global chl_a July Climatology") +
  theme_bw()
global_chl_map # be careful about printing this type of figure on a slow computer, you can just save and view
ggsave(global_chl_map, filename = 'figures/global_chl_July.pdf', device = "pdf", height = 5, width = 9)

# set GOM map limits (gulf of main bc of erin's whales)
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

# crop GOM
chl_GOM_raster = raster::crop(chl_raster, extent(c(lon_bounds, lat_bounds))) # ?extent

# convert GOM raster to points and then to a data frame
chl_GOM_df = data.frame(rasterToPoints(chl_GOM_raster, spatial = TRUE))
head(chl_GOM_df)
chl_GOMdf = chl_GOM_df %>% dplyr::select(-optional) # drop optional column

# grab coastline data from R's worldHired data in the mapdata package
world_map = map_data("worldHires")
head(world_map)

GOM_chl_map = ggplot()+
  geom_raster(data = chl_GOM_df, aes(x = x, y = y, fill = log10(chl_a))) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "darkgrey", data = world_map) +
  coord_fixed(ratio = 1.3, xlim = lon_bounds, ylim = lat_bounds, expand = FALSE) +
  scale_fill_gradientn(colors = cols, limits = c(-1, 1.75)) + 
  ggtitle("GOM chl_a July Climatology") + 
  theme_bw() + 
  xlab("Longitude") + ylab("Latitude")

GOM_chl_map
ggsave(GOM_chl_map, filename = 'figures/GOM_chl_July.png', device = "png", height = 5, width = 7)

# for the Pacific ocean, use "world2" from mapdata which shifts to a [0, 360] longitude system
pacific_map = map_data("world2")
chl_df_pacific_shift = chl_df %>%
  mutate(x = ifesle(x<0, x+360, x)) # manually adjust longitudes in satelite raster to [0, 360]
pacific_chl_map = ggplot() + 
  geom_raster(data = chl_df_pacific_shift, aes(x = x, y = y, fill = log10(chl_a))) +
  scale_fill_gradientn(colors = cols, limits = c(-1.5, 0.75), oob = scales::squish, name = "log10(chl_a)") +
  geom_polygon(data = pacific_map, aes(x = long, y = lat, group = group)) + 
  theme_bw()
pacific_chl_map

####################################################################
# Bathymetry

# install.packages("rgdal")
bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1],
                                    lon2 = lon_bounds[2],
                                    lat1 = lat_bounds[1],
                                    lat2 = lat_bounds[2],
                                    resolution = 4)
class(bath_m_raw)
# convert bathymetry to dat frame
bath_m_df = marmap::fortify.bathy(bath_m_raw)
bath_m = bath_m_df %>%
  mutate(depth_m = ifelse(z>20, NA, z)) %>%
  dplyr::select(-z)
head(bath_m)
summary(bath_m)

# plot raster data
GOM_bath_map = ggplot() +
  geom_raster(data = bath_m, aes(x = x, y = y, fill = depth_m)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "darkgrey", color = "black") +
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand = FALSE) + 
  scale_fill_gradientn(colors = c("black", "darkblue", "lightblue"),
                        values = scales::rescale(c(-6000, -300, 0)),
                        name = "Depth (m)") + 
  labs(x = "Longitude", y = "Latitude", title = "Bathymetry Map") + 
  theme_bw()
GOM_bath_map
ggsave(GOM_bath_map, filename = 'figures/GOM_bath_raster.png', device = "png", height = 5, width = 7)
