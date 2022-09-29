# ---------------------------------------------------------------------------- #
# Import the Forbes 400 panel data
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)

forbes400_data <- read_csv(here("raw-data", "forbes400", "forbes400.csv"), col_types = cols(
    year = col_double(),
    rank = col_double(),
    name = col_character(),
    familyid = col_double(),
    networth = col_double(),
    dropoff_reason = col_character(),
    dropoff_networth = col_double(),
    sample = col_double()
))

forbes400_data <- forbes400_data %>% select(year, name, familyid, networth, dropoff_reason)

forbes400_data <- forbes400_data %>%
    group_by(name, familyid) %>%
    complete(year = full_seq(year, 1))

dir.create(here("work", "02-import-forbes400"), showWarnings = FALSE)
write_rds(forbes400_data, here("work", "02-import-forbes400", "forbes400_data.rds"))
