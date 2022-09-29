# ---------------------------------------------------------------------------- #
# Import the SCF+ data from Kuhn, Schularick & Steins (JPE, 2020)
# ---------------------------------------------------------------------------- #

library(here)
library(tidyverse)
library(haven)

scf_plus <- read_dta(here("raw-data", "scf-plus", "SCF_plus.dta"))

# Try to give more or less the same format as the 'scf' dataset
scf_plus <- scf_plus %>% transmute(
    year = yearmerge,
    hid = id,
    implicate = impnum,
    weight = wgtI95W95,
    wealth = ffanw,
    income = tinc,
    couple = if_else(adults > 1, 1, 0),
    age = ageh
)

# Equal-split distribution
scf_plus <- scf_plus %>% mutate(
    nadults = if_else(as.logical(couple), 2, 1),
    weight = weight/nadults,
    wealth = wealth/nadults,
    income = income/nadults,
)

dir.create(here("work", "02-import-scf-plus"))
write_rds(scf_plus, here("work", "02-import-scf-plus", "scf_plus.rds"))
