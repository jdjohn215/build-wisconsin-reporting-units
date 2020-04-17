library(tidyverse)

# this script matches wards and reporting units from the WEC to wards from the LTSB

wec.wards <- read_rds("data/WardsToReportingUnits_WEC_Spring2019.rds") %>%
  mutate_if(is.character, str_to_upper)
ltsb.wards <- read_rds("data/WardsSpring2019.rds") %>%
  mutate_if(is.character, str_to_upper)

ltsb <- ltsb.wards %>%
  st_set_geometry(NULL) %>%
  mutate(MCD_NAME = str_remove_all(MCD_NAME, "[.]"))

# check that every municipality string begins with CITY TOWN OR VILLAGE
unique(word(wec.wards$municipality, 1))

# clean up wec.wards so it has matching fields
wec <- wec.wards %>%
  mutate(CTV = str_sub(municipality, 1, 1),
         MCD_NAME = word(municipality, 3, -1),
         WARDID = str_pad(ward, width = 4, side = "left", pad = "0"),
         MCD_NAME = str_remove_all(MCD_NAME, "[.]")) %>%
  select(reporting_unit_name, CNTY_NAME = county, MCD_NAME, CTV, WARDID)

# troubleshoot
not.joined.from.wec <- anti_join(wec, ltsb)
not.joined.from.ltsb <- anti_join(ltsb, wec)

# edit wec.v2 to fix mismatches
wec.v2 <- wec %>%
  mutate(MCD_NAME = str_replace(MCD_NAME, "GRAND VIEW", "GRANDVIEW"),
         MCD_NAME = str_replace(MCD_NAME, "MT STERLING", "MOUNT STERLING"),
         MCD_NAME = str_replace(MCD_NAME, "FONTANA", "FONTANA-ON-GENEVA LAKE"),
         MCD_NAME = str_replace(MCD_NAME, "SAINT LAWRENCE", "ST LAWRENCE"))

ltsb.v2 <- ltsb %>%
  mutate(MCD_NAME = replace(MCD_NAME, MCD_NAME == "COUNTY SUBDIVISIONS NOT DEFINED", "YORKVILLE"),
         CTV = replace(CTV, MCD_NAME == "YORKVILLE", "V"))


# troubleshoot
not.joined.from.wecv2 <- anti_join(wec.v2, ltsb.v2)
not.joined.from.ltsbv2 <- anti_join(ltsb.v2, wec.v2)
