library(tidyverse)

spring2019 <- read_rds("data/Spring2019_SCOWIS.rds") %>%
  # generate a unique reporting unit id
  mutate(reporting_unit_id = 1:length(ward_names))

ltsb.wards <- read_rds("data/WardsSpring2019.rds") %>%
  sf::st_set_geometry(NULL)

# Reporting units are wards or combinations of wards
# sometimes the combinations are written as a range indicated with a hyphen
# other times the wards are listed, separated with a comma
# ranges and lists of wards can be combined

# What is the longest sequential list of wards or ward ranges?
ward.list.length <- max(str_count(spring2019$ward_names, ",")) + 1

# This next code chunk creates a clean list of each ward in each reporting unit

# extract the wards in each reporting unit
d <- spring2019 %>%
  # split ward_names into municipality and list of wards
  separate(col = ward_names,
           sep = " Ward",
           into = c("municipality", "wards"),
           remove = FALSE) %>%
  select(reporting_unit_id, county, ward_names, municipality, wards) %>%
  # clean up the ward list
  mutate(wards = str_remove(wards, "s "),
         wards = str_squish(wards)) %>%
  # separate the lists of wards separated by commas
  separate(col = wards,
           into = paste("ward_group", 1:ward.list.length, sep = "_"),
           sep = ",") %>%
  # pivot to long format
  pivot_longer(cols = starts_with("ward_group"),
               names_to = "ward_group",
               values_to = "ward_range") %>%
  # remove unneeded rows and columns
  filter(!is.na(ward_range)) %>%
  select(-ward_names, -ward_group) %>%
  # create numeric ward ranges
  separate(col = ward_range,
           into = c("ward_min", "ward_max"),
           sep = "-") %>%
  # if ward_max is NA, then replace it with ward_min
  mutate(ward_max = replace(ward_max, is.na(ward_max), ward_min[is.na(ward_max)]))


###############################################################################
###############################################################################
# numeric wards
#   some wards have a letter in their name, e.g. 1A. these don't
d.numeric <- d %>%
  mutate(ward_min_num = as.numeric(ward_min),
         ward_max_num = as.numeric(ward_max)) %>%
  filter(!is.na(ward_min_num),
         !is.na(ward_max_num)) %>%
  mutate(wards_in_range = mapply(":", ward_min, ward_max)) %>%
  unnest(cols = wards_in_range) %>%
  select(reporting_unit_id, county, municipality, ward = wards_in_range) %>%
  mutate(ward = as.character(ward))

# alpha wards
#   these wards have a letter, and aren't part of a range
d.alpha <- d %>%
  filter(str_detect(ward_min, "[[:alpha:]]"),
         ward_min == ward_max) %>%
  select(reporting_unit_id, county, municipality, ward = ward_min)

###############################################################################
###############################################################################
# alpha wards that are also part of a range
# these are super annoying (e.g. 1A-5)
#   manual fixes are required. Compare them to the LTSB ward list to
#   figure out what they mean
d.alpha.range <- d %>%
  filter(str_detect(paste(ward_min, ward_max), "[[:alpha:]]"),
         ward_min != ward_max)

d.alpha.range.hand.fixes <- d.alpha.range %>%
  mutate(wards_in_range = NA,
         wards_in_range = replace(wards_in_range, row_number() == 1, paste("1", "1S")),
         wards_in_range = replace(wards_in_range, row_number() == 2, paste("3", "3S")),
         wards_in_range = replace(wards_in_range, row_number() == 3, paste("8", "8S")),
         wards_in_range = replace(wards_in_range, row_number() == 4, paste("11", "11S")),
         wards_in_range = replace(wards_in_range, row_number() == 5, paste("5", "6", "7A", "7B")),
         wards_in_range = replace(wards_in_range, row_number() == 6, paste("1A", "1B", "2", "3", "4", "5"))) %>%
  separate(col = wards_in_range,
           into = c("ward1", "ward2", "ward3", "ward4", "ward5", "ward6"),
           sep = " ") %>%
  pivot_longer(cols = c(ward1, ward2, ward3, ward4, ward5, ward6)) %>%
  filter(!is.na(value)) %>%
  select(reporting_unit_id, county, municipality, ward = value)

###############################################################################
###############################################################################

all.wards.to.wardIDs <- bind_rows(d.numeric, d.alpha, d.alpha.range.hand.fixes) %>%
  # add full reporting unit name from WEC
  inner_join(spring2019 %>% select(reporting_unit_name = ward_names, reporting_unit_id))
saveRDS(all.wards.to.wardIDs, "data/WardsToReportingUnits_WEC_Spring2019.rds")
write_csv(all.wards.to.wardIDs, "data/WardsToReportingUnits_WEC_Spring2019.csv")
