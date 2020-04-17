library(tidyverse)
library(sf)

options(stringsAsFactors = FALSE)

# get 2019 ward boundaries from the LTSB
#   https://data-ltsb.opendata.arcgis.com/datasets/wi-municipal-wards-january-2019
wards2019 <- st_read("https://opendata.arcgis.com/datasets/eb26ba98aef94b818c721a4c7068a4ae_0.geojson")

# just keep what we need
wards.clean <- wards2019 %>%
  select(WARD_FIPS, CNTY_NAME, MCD_NAME, CTV, WARDID)
wards.clean
saveRDS(wards.clean, "data/WardsSpring2019.rds")
