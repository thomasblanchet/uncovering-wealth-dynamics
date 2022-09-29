# ---------------------------------------------------------------------------- #
# Import the IPUMS census data
# ---------------------------------------------------------------------------- #

library(janitor)
library(ipumsr)
library(here)
library(readr)

census_ddi <- read_ipums_ddi(here("raw-data", "ipums", "usa-census.xml"))
census_data <- read_ipums_micro(
    census_ddi,
    data_file = here("raw-data", "ipums", "usa-census.dat"),
    var_attrs = NULL
)

census_data <- clean_names(census_data)

dir.create(here("work", "02-import-census"), showWarnings = FALSE)
write_rds(census_data, here("work", "02-import-census", "census_data.rds"))
