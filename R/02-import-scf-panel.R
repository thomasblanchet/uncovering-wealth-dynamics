# ---------------------------------------------------------------------------- #
# Import the SCF panel (2007-2009)
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(readr)
library(progressr)
library(haven)

handlers(handler_progress(
    format = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta"
))

# ---------------------------------------------------------------------------- #
# Full data
# ---------------------------------------------------------------------------- #

if (!file.exists(here("raw-data", "scf", "p09pi6.dta"))) {
    download.file(
        url = "https://www.federalreserve.gov/econres/files/scf2009ps.zip",
        destfile = here("raw-data", "scf", "scf2009ps.zip")
    )
    unzip(
        zipfile = here("raw-data", "scf", "scf2009ps.zip"),
        exdir = here("raw-data", "scf")
    )
    unlink(here("raw-data", "scf", "scf2009ps.zip"))
}
scf_panel_full <- read_dta(here("raw-data", "scf", "p09pi6.dta"))
scf_panel_full <- scf_panel_full  %>% transmute(
    id = YY1,
    implicate = Y1 %% 10,
    age_head = X8022,
    age_spouse = X104,
    inheritance = if_else(X5725 == 12, X5724, 0)
)

# ---------------------------------------------------------------------------- #
# Summary data
# ---------------------------------------------------------------------------- #

if (!file.exists(here("raw-data", "scf", "rscfp2009panel.dta"))) {
    download.file(
        url = "https://www.federalreserve.gov/econres/files/rscfp2009panels.zip",
        destfile = here("raw-data", "scf", "rscfp2009panels.zip")
    )
    unzip(
        zipfile = here("raw-data", "scf", "rscfp2009panels.zip"),
        exdir = here("raw-data", "scf")
    )
    unlink(here("raw-data", "scf", "rscfp2009panels.zip"))
}
scf_panel <- read_dta(here("raw-data", "scf", "rscfp2009panel.dta"))
scf_panel <- scf_panel %>% transmute(
    id = YY1,
    implicate = Y1 %% 10,
    weight = WGT09,
    married2007 = MARRIED07,
    married2009 = MARRIED09,
    income2007 = INCOME07,
    income2009 = INCOME09,
    wealth2007 = NETWORTH07,
    wealth2009 = NETWORTH09
)

# ---------------------------------------------------------------------------- #
# Combine and save data
# ---------------------------------------------------------------------------- #

scf_panel <- scf_panel %>% left_join(scf_panel_full)

dir.create(here("work", "02-import-scf-panel"), showWarnings = FALSE)
write_rds(scf_panel, here("work", "02-import-scf-panel", "scf_panel.rds"))
