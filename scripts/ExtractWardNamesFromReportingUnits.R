library(tidyverse)

spring2019 <- read_rds("data/Spring2019_SCOWIS.rds")

# Reporting units are wards or combinations of wards
# sometimes the combinations are written as a range indicated with a hyphen
# other times the wards are listed, separated with a comma
# ranges and lists of wards can be combined

# What is the longest sequential list of wards or ward ranges?
ward.list.length <- max(str_count(spring2019$ward_names, ",")) + 1

# This next code chunk creates a clean list of each ward in each reporting unit

# extract the wards in each reporting unit
d <- spring2019 %>%
  # generate a unique reporting unit id
  mutate(reporting_unit_id = 1:length(ward_names)) %>%
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

# numeric wards
#   some wards have a letter in their name, e.g. 1A. these don't
d.numeric <- d %>%
  mutate(ward_min_num = as.numeric(ward_min),
         ward_max_num = as.numeric(ward_max)) %>%
  filter(!is.na(ward_min_num),
         !is.na(ward_max_num)) %>%
  mutate(wards_in_range = mapply(":", ward_min, ward_max)) %>%
  unnest(cols = wards_in_range) %>%
  select(reporting_unit_id, county, municipality, ward = wards_in_range)

# alpha wards
#   these wards have a letter, and aren't part of a range
d.alpha <- d %>%
  filter(str_detect(ward_min, "[[:alpha:]]"),
         ward_min == ward_max) %>%
  select(reporting_unit_id, county, municipality, ward = ward_min)

# alpha wards that are also part of a range
# these are super annoying (e.g. 1A-5)
d.alpha.range <- d %>%
  filter(str_detect(ward_max, "[[:alpha:]]") |
           str_detect(ward_max, "[[:alpha:]]"),
         ward_min != ward_max)

d2 <- d %>%
  mutate(ward_min_num = as.numeric(ward_min),
         ward_max_num = as.numeric(ward_max))

d %>%
  mutate(wards_in_range = mapply(":", ward_min, ward_max))

