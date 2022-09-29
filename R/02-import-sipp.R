# ---------------------------------------------------------------------------- #
# Import SIPP data
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(glue)
library(here)
library(readr)
library(janitor)

options(dplyr.summarise.inform = FALSE)

# ---------------------------------------------------------------------------- #
# Download data if necessary
# ---------------------------------------------------------------------------- #

dir.create(here("raw-data", "sipp"), showWarnings = FALSE)

# Wave 1-4
for (i in 1:4) {
    if (!file.exists(here("raw-data", "sipp", glue("pu2014w{i}.dat.gz")))) {
        download.file(
            url = glue("https://www2.census.gov/programs-surveys/sipp/data/datasets/2014/w{i}/pu2014w{i}.dat.gz"),
            destfile = here("raw-data", "sipp", glue("pu2014w{i}.dat.gz"))
        )
    }
}

for (i in 1:4) {
    # Codes dictionary
    if (!file.exists(here("raw-data", "sipp", glue("pu2014w{i}.sas")))) {
        download.file(
            url = glue("https://www2.census.gov/programs-surveys/sipp/data/datasets/2014/w{i}/pu2014w{i}.sas"),
            destfile = here("raw-data", "sipp", glue("pu2014w{i}.sas"))
        )
    }
}

# ---------------------------------------------------------------------------- #
# Import the data
# ---------------------------------------------------------------------------- #

sipp_var_desc <- read_csv(here("raw-data", "sipp", "sippdict.csv"), col_types = cols(
    Variable = col_character(),
    Topic = col_character(),
    Subtopic = col_character(),
    `Survey Years` = col_character(),
    `Response Code` = col_character(),
    Description = col_character(),
    Question = col_character(),
    `Data Type` = col_character(),
    Universe = col_character(),
    `User Notes` = col_character(),
    `Record Level` = col_character()
))
sipp_var_desc <- clean_names(sipp_var_desc)

sipp_var_selection <- sipp_var_desc %>%
    filter(topic %in% c("ID Variables", "Demographics", "Marital History", "Assets", "Poverty and Income") & str_detect(survey_years, "2014")) %>%
    pull(variable)

sipp_data <- NULL
for (i in 1:4) {
    sipp_dict <- read_lines(here("raw-data", "sipp", glue("pu2014w{i}.sas")))
    # Parse the dictionary file (it's SAS code)
    sipp_dict <- str_match_all(sipp_dict, "^ *([A-Z0-9_]+) *(\\$?) *([0-9]+) *- *([0-9]+) *$")
    sipp_dict <- map_dfr(sipp_dict, function(entry) {
        if (length(entry) == 0) {
            return(NULL)
        } else {
            return(data.frame(
                var = entry[2],
                start = as.integer(entry[4]),
                end = as.integer(entry[5]),
                type = ifelse(entry[3] == "$", "c", "n")
            ))
        }
    })

    # Only keep relevant variables
    sipp_dict <- sipp_dict %>% filter(var %in% sipp_var_selection)

    data_year <- read_fwf(
        file = here("raw-data", "sipp", glue("pu2014w{i}.dat.gz")),
        col_positions = fwf_positions(sipp_dict$start, sipp_dict$end, sipp_dict$var),
        col_types = paste(sipp_dict$type, collapse = ""),
        na = "."
    )

    sipp_data <- bind_rows(sipp_data, data_year)
}
sipp_data <- clean_names(sipp_data)

# ---------------------------------------------------------------------------- #
# Only keep relevant variables
# ---------------------------------------------------------------------------- #

sipp_data <- sipp_data %>%
    group_by(ssuid, shhadid, spanel, swave, pnum) %>%
    summarise(
        tage = mean(tage),
        esex = mean(esex),
        ems = first(ems),
        epnspouse = first(epnspouse),
        wpfinwgt = mean(wpfinwgt),
        tnetworth = mean(tnetworth),
        tptotinc = sum(tptotinc)
    )

# ---------------------------------------------------------------------------- #
# Save data
# ---------------------------------------------------------------------------- #

dir.create(here("work", "02-import-sipp"), showWarnings = FALSE)
write_rds(sipp_data, here("work", "02-import-sipp", "sipp_data.rds"))
