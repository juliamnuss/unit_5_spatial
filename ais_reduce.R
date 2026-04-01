# Reduce AIS data to manageable size for student use
# Load in .csv file of Jan. 25,. 2017 AIS points
# https://hub.marinecadastre.gov/pages/vesseltraffic
# access daily .csv.zst here:
# https://noaaocm.blob.core.windows.net/ais/csv2/csv2017/index.html

# Crop to a bounding box around FL and GA
# Remove duplicate rows and rows with no call_sign
# Keep only ships with status == 0 "under way using engine"
# Write out as .csv

library(tidyverse)
library(sf)
library(mapdata)
library(marmap)
library(lubridate) # ymd_hms

ais <- read.csv(pipe(
  "zstd -dc data_too_big_for_students/ais-2017-01-25.csv.zst"
))


## AIS data; Downloaded for January 2017, UTM Zone 17
# https://marinecadastre.gov/AIS/

# ais = read.csv("data_too_big_for_students/raw/AIS_UTM17_Jan2017_4GB/2017_v2/AIS_2017_01_Zone17.csv") #data/AIS_ASCII_by_UTM_Month/2017_v2/AIS_2017_01_Zone17.csv")
head(ais)
summary(ais)
dim(ais) # 7,868,542 rows (woah!)

lat_bounds = c(25, 34)
lon_bounds = c(-82, -76)

####################################################
#  Filter original Jan 2017 AIS data to just
#  one day (2021-1-25), ships underway and
#  ships with a call sign
####################################################

# https://support.marinetraffic.com/en/articles/9552867-what-do-ais-navigational-status-values-mean
# status == 0 is "under way using engine"

ais_filter = ais %>%
  distinct(.keep_all = TRUE) %>%
  filter(
    latitude <= 34,
    latitude >= 25,
    longitude >= -82,
    longitude <= -76,
    status == 0,
    call_sign != ""
  ) %>%
  mutate(date_time = lubridate::ymd_hms(base_date_time)) # %>%
# filter(date_time > ymd_hms("2017-01-25 00:00:00"),
#        date_time < ymd_hms("2017-01-25 11:59:59"))

head(ais_filter)
dim(ais_filter) # 271,292 rows

write_csv(
  ais_filter %>% dplyr::select(-date_time),
  file = 'data/processed_ais/ais_2017-01-25.csv'
)
