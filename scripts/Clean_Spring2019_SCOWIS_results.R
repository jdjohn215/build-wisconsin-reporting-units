library(tidyverse)

# download the spring 2019 ward-by-ward election results from:
#   https://elections.wi.gov/index.php/elections-voting/results/2019/spring-election

spring19.file <- tempfile()
download.file("https://elections.wi.gov/sites/elections.wi.gov/files/Spring%20Election%204.2.19-WxW%20Report-Supreme%20Court.xlsx",
              spring19.file)
spring19 <- readxl::read_excel(spring19.file, 
                               sheet = 2,
                               skip = 11,
                               col_names = c("county", "ward_names", "ballots_cast",
                                             "brian_hagedorn", "lisa_neubauer", "scatter")) %>%
  filter(!is.na(ward_names),
         ward_names != "County Totals:") %>%
  mutate(county = zoo::na.locf(county))

spring19

saveRDS(spring19, "data/Spring2019_SCOWIS.rds")
write_csv(spring19, "data/Spring2019_SCOWIS.csv")
