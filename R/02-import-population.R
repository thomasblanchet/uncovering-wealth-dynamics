# ---------------------------------------------------------------------------- #
# Combine population data from the different sources
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(zoo)
library(glue)
library(scales)

options(dplyr.summarise.inform = FALSE)

census_data <- read_rds(here("work", "02-import-census", "census_data.rds"))

# ---------------------------------------------------------------------------- #
# Census micro data
# ---------------------------------------------------------------------------- #

population_ipums <- census_data %>%
    mutate(
        age = as.integer(pmin(age, 90)),
        sex = if_else(sex == 1, "male", "female")
    ) %>%
    group_by(year, sex, age) %>%
    summarise(population = sum(perwt, na.rm = TRUE)) %>%
    ungroup() %>%
    complete(age = 0:90, year, sex) %>%
    group_by(sex, year) %>%
    arrange(sex, year, age) %>%
    mutate(population = rollapply(population, width = 5, FUN = mean, align = "center", partial = TRUE)) %>%
    ungroup() %>%
    mutate(age = pmin(age, 75)) %>%
    group_by(sex, age, year) %>%
    summarise(population = sum(population)) %>%
    ungroup() %>%
    complete(age, year = full_seq(year, 1), sex) %>%
    group_by(sex, age) %>%
    mutate(population = exp(splinefun(year, log(population), method = "monoH.FC")(year)))

# ---------------------------------------------------------------------------- #
# Census Bureau
# ---------------------------------------------------------------------------- #

dir.create(here("raw-data", "uscb"), showWarnings = FALSE)

population_uscb <- NULL
census_url_ftp <- "https://www2.census.gov/programs-surveys/popest/tables/1900-1980/national/asrh/"
for (year in 1900:1979) {
    file <- here("raw-data", "uscb", glue("{year}.csv"))
    if (!file.exists(file)) {
        download.file(
            url = glue("{census_url_ftp}/pe-11-{year}.csv"),
            destfile = file
        )
    }

    population_year <- read_csv(
        file = file,
        skip = 7,
        col_names = FALSE
    )
    population_year <- population_year %>%
        drop_na() %>%
        filter(X1 != "All ages")

    population_year <- population_year %>% transmute(
        year = year,
        age = as.integer(as.numeric(gsub("[-+]", "", X1))),
        male = X3,
        female = X4,
        total = male + female
    )

    population_uscb <- bind_rows(population_uscb, population_year)
}

population_uscb <- population_uscb %>%
    select(-total) %>%
    pivot_longer(c(male, female), names_to = "sex", values_to = "population") %>%
    arrange(year, age)

# ---------------------------------------------------------------------------- #
# Human Mortality Database
# ---------------------------------------------------------------------------- #

population_hmd <- read_fwf(
    file = here("raw-data", "hmd", "population.txt"),
    skip = 3,
    guess_max = 1e4,
    col_positions = fwf_widths(
        widths = c(10, 9, 21, 16, 15),
        col_names = c("year", "age", "female", "male", "total")
    ),
    col_types = cols(
        year = col_character(),
        age = col_character(),
        female = col_double(),
        male = col_double(),
        total = col_double()
    )
)

population_hmd <- population_hmd %>% mutate(
    age = as.integer(as.numeric(gsub("[-+]", "", age))),
    year = as.integer(as.numeric(gsub("[-+]", "", year)))
)
population_hmd <- population_hmd %>%
    group_by(year, age) %>%
    summarise(female = mean(female), male = mean(male)) %>%
    pivot_longer(c(male, female), names_to = "sex", values_to = "population")

# ---------------------------------------------------------------------------- #
# World Population Prospects
# ---------------------------------------------------------------------------- #

dir.create(here("raw-data", "wpp"), showWarnings = FALSE)

wpp_url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES"

# Medium variant
if (!file.exists(here("raw-data", "wpp", "WPP2019_PopulationByAgeSex_Medium.csv"))) {
    download.file(
        url = glue("{wpp_url}/WPP2019_PopulationByAgeSex_Medium.csv"),
        destfile = here("raw-data", "wpp", "WPP2019_PopulationByAgeSex_Medium.csv")
    )
}
# Other variants
if (!file.exists(here("raw-data", "wpp", "WPP2019_PopulationByAgeSex_OtherVariants.csv"))) {
    download.file(
        url = glue("{wpp_url}/WPP2019_PopulationByAgeSex_OtherVariants.csv"),
        destfile = here("raw-data", "wpp", "WPP2019_PopulationByAgeSex_OtherVariants.csv")
    )
}

population_wpp <- bind_rows(
    read_csv(
        file = here("raw-data", "wpp", "WPP2019_PopulationByAgeSex_Medium.csv"),
        col_types = cols(
            LocID = col_double(),
            Location = col_character(),
            VarID = col_double(),
            Variant = col_character(),
            Time = col_double(),
            MidPeriod = col_double(),
            AgeGrp = col_character(),
            AgeGrpStart = col_double(),
            AgeGrpSpan = col_double(),
            PopMale = col_double(),
            PopFemale = col_double(),
            PopTotal = col_double()
        )
    ),
    read_csv(
        file = here("raw-data", "wpp", "WPP2019_PopulationByAgeSex_OtherVariants.csv"),
        col_types = cols(
            LocID = col_double(),
            Location = col_character(),
            VarID = col_double(),
            Variant = col_character(),
            Time = col_double(),
            MidPeriod = col_double(),
            AgeGrp = col_character(),
            AgeGrpStart = col_double(),
            AgeGrpSpan = col_double(),
            PopMale = col_double(),
            PopFemale = col_double(),
            PopTotal = col_double()
        )
    )
)
population_wpp <- population_wpp %>% filter(LocID == 840 & Time %% 5 == 0)
population_wpp <- population_wpp %>% transmute(
    year = Time,
    variant = str_replace(str_to_title(Variant), fixed("Pi"), "PI"),
    age = AgeGrpStart,
    male = round(1e3*PopMale),
    female = round(1e3*PopFemale)
)
population_wpp <- population_wpp %>% pivot_longer(-c(variant, year, age), names_to = "sex", values_to = "population")

# ---------------------------------------------------------------------------- #
# Interpolate population forecasts
# ---------------------------------------------------------------------------- #

# The UN published interpolated population figures by age and year, but
# only for the "medium" variant of their forecast. So I do my own interpolation
# for all variants, using monotonic spline interpolation.

population_wpp <- population_wpp %>%
    ungroup() %>%
    # Interpolate by year
    complete(year = 1950:2100, variant, age, sex) %>%
    filter(year >= 2020 | variant == "Medium") %>%
    # Interpolate medium first (to anchor the interpolation of the other
    # scenarios pre-2020)
    group_by(variant, age, sex) %>%
    mutate(population = if_else(
        variant == "Medium",
        exp(splinefun(x = year, y = log(population), method = "monoH.FC")(year)),
        population
    )) %>%
    # Copy the medium variant into each variant for 2020  to anchor
    # interpolation afterwards
    group_by(year, age, sex) %>%
    mutate(population = if_else(year == 2020, first(na.omit(population)), population)) %>%
    group_by(variant, age, sex) %>%
    mutate(population = exp(splinefun(x = year, y = log(population), method = "monoH.FC")(year))) %>%
    mutate(variant = if_else(year < 2020, "Past", variant)) %>%
    # Interpolate by age
    group_by(variant, sex, year) %>%
    arrange(variant, sex, year, age) %>%
    mutate(age = if_else(age < 100, age + 4, 110), population_cumul = cumsum(population)) %>%
    ungroup() %>%
    complete(age = -1:110, nesting(variant, year, sex)) %>%
    mutate(population_cumul = if_else(age == -1, 0, population_cumul)) %>%
    group_by(variant, sex, year) %>%
    mutate(population_cumul_interp = splinefun(x = age, y = population_cumul, method = "monoH.FC")(age)) %>%
    mutate(population_interp = round(diff(c(0, population_cumul_interp)))) %>%
    filter(age >= 0) %>%
    ungroup() %>%
    transmute(variant, sex, year, age, population = population_interp) %>%
    arrange(variant, sex, year, age)

# ---------------------------------------------------------------------------- #
# Combine the different sources
# ---------------------------------------------------------------------------- #

population_all <- bind_rows(
    population_ipums %>% mutate(source = "IPUMS USA microdata", variant = "Past"),
    population_uscb %>% mutate(source = "US Census Bureau", variant = "Past"),
    population_hmd %>% mutate(source = "Human Mortality Database", variant = "Past"),
    population_wpp %>% mutate(source = "UN World Population Prospects")
)

population <- bind_rows(
    population_ipums %>% mutate(source = "IPUMS USA microdata", variant = "Past") %>% filter(year < 1900),
    population_uscb %>% mutate(source = "US Census Bureau", variant = "Past") %>% filter(year >= 1900 & year < 1933),
    population_hmd %>% mutate(source = "Human Mortality Database", variant = "Past") %>% filter(year >= 1933 & year < 2020),
    population_wpp %>% mutate(source = "UN World Population Prospects") %>% filter(year >= 2020)
)

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

dir.create(here("work", "02-import-population"), showWarnings = FALSE)
write_rds(population,     here("work", "02-import-population", "population.rds"))
write_rds(population_all, here("work", "02-import-population", "population_all.rds"))

# ---------------------------------------------------------------------------- #
# Graphs
# ---------------------------------------------------------------------------- #

dir.create(here("graphs", "02-import-population"), showWarnings = FALSE)

pdf(file = here("graphs", "02-import-population", "population-selected-sources.pdf"), height = 5, width = 6)
print(population %>%
    filter(variant == "Medium" | variant == "Past") %>%
    group_by(year, source, variant) %>%
    summarise(total = sum(population), female = sum(population*(sex == "female"))) %>%
    ggplot() +
    geom_line(aes(x = year, y = total, color = source), size = 0.7) +
    geom_line(aes(x = year, y = female, color = source), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_x_continuous(breaks = seq(1850, 2100, 25), name = "") +
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "vertical"))
dev.off()

pdf(file = here("graphs", "02-import-population", "population-sources.pdf"), height = 5, width = 6)
print(population_all %>%
    filter(variant == "Medium" | variant == "Past") %>%
    group_by(year, source, variant) %>%
    summarise(total = sum(population), female = sum(population*(sex == "female"))) %>%
    ggplot() +
    geom_line(aes(x = year, y = total, color = source), size = 0.7) +
    geom_line(aes(x = year, y = female, color = source), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_x_continuous(breaks = seq(1850, 2100, 25), name = "") +
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "vertical"))
dev.off()

pdf(file = here("graphs", "02-import-population", "population-sources-male.pdf"), height = 5, width = 6)
print(population_all %>%
    filter(variant == "Medium" | variant == "Past") %>%
    group_by(year, source, variant) %>%
    summarise(population = sum(population*(sex == "male"))) %>%
    ggplot() +
    geom_line(aes(x = year, y = population, group = source, color = source), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_x_continuous(breaks = seq(1850, 2100, 25), name = "") +
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "vertical"))
dev.off()

pdf(file = here("graphs", "02-import-population", "population-sources-female.pdf"), height = 5, width = 6)
print(population_all %>%
    filter(variant == "Medium" | variant == "Past") %>%
    group_by(year, source, variant) %>%
    summarise(population = sum(population*(sex == "female"))) %>%
    ggplot() +
    geom_line(aes(x = year, y = population, group = source, color = source), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_x_continuous(breaks = seq(1850, 2100, 25), name = "") +
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "vertical"))
dev.off()

pdf(file = here("graphs", "02-import-population", "population-sources-0-19.pdf"), height = 5, width = 6)
print(population_all %>%
    filter(variant == "Medium" | variant == "Past") %>%
    filter(age < 20) %>%
    group_by(year, source, variant) %>%
    summarise(population = sum(population)) %>%
    ggplot() +
    geom_line(aes(x = year, y = population, group = source, color = source), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_x_continuous(breaks = seq(1850, 2100, 25), name = "") +
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "vertical"))
dev.off()

pdf(file = here("graphs", "02-import-population", "population-sources-20-39.pdf"), height = 5, width = 6)
print(population_all %>%
    filter(variant == "Medium" | variant == "Past") %>%
    filter(age >= 20 & age < 40) %>%
    group_by(year, source, variant) %>%
    summarise(population = sum(population)) %>%
    ggplot() +
    geom_line(aes(x = year, y = population, group = source, color = source), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_x_continuous(breaks = seq(1850, 2100, 25), name = "") +
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "vertical"))
dev.off()

pdf(file = here("graphs", "02-import-population", "population-sources-40-64.pdf"), height = 5, width = 6)
print(population_all %>%
    filter(variant == "Medium" | variant == "Past") %>%
    filter(age >= 40 & age < 65) %>%
    group_by(year, source, variant) %>%
    summarise(population = sum(population)) %>%
    ggplot() +
    geom_line(aes(x = year, y = population, group = source, color = source), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_x_continuous(breaks = seq(1850, 2100, 25), name = "") +
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "vertical"))
dev.off()

pdf(file = here("graphs", "02-import-population", "population-sources-65.pdf"), height = 5, width = 6)
print(population_all %>%
    filter(variant == "Medium" | variant == "Past") %>%
    filter(age >= 65) %>%
    group_by(year, source, variant) %>%
    summarise(population = sum(population)) %>%
    ggplot() +
    geom_line(aes(x = year, y = population, group = source, color = source), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_x_continuous(breaks = seq(1850, 2100, 25), name = "") +
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "vertical"))
dev.off()

pdf(file = here("graphs", "02-import-population", "population-forecasts.pdf"), height = 8, width = 12)
print(population_all %>%
    filter(source == "UN World Population Prospects") %>%
    group_by(year, variant) %>%
    summarise(total = sum(population)) %>%
    ungroup() %>%
    pivot_wider(year, names_from = variant, values_from = total) %>%
    mutate(`Median PI` = if_else(is.na(`Median PI`), `Past`, `Median PI`)) %>%
    ggplot() +
    geom_ribbon(aes(x = year, ymin = `Lower 95 PI`, ymax = `Upper 95 PI`), fill = "#9ecae1", alpha = 0.5) +
    geom_ribbon(aes(x = year, ymin = `Lower 80 PI`, ymax = `Upper 80 PI`), fill = "#9ecae1", alpha = 0.5) +
    geom_line(aes(x = year, y = `Median PI`), color = "#3182bd", size = 0.7) +
    scale_x_continuous(breaks = seq(1950, 2100, 10), name = "") +
    scale_y_continuous(labels = comma, limits = c(0, NA), breaks = seq(0, 6e8, 5e7)) +
    theme_bw() +
    theme(legend.position = "right", legend.title = element_blank(), legend.direction = "vertical"))

print(population_all %>%
    filter(source == "UN World Population Prospects") %>%
    group_by(year, variant) %>%
    summarise(total = sum(population), female = sum(population*(sex == "female"))) %>%
    ungroup() %>%
    complete(year = 1950:2100, variant) %>%
    group_by(year) %>%
    fill(female, total, .direction = "updown") %>%
    mutate(forecast = if_else(year > 2019, "forecast", "data")) %>%
    filter(variant != "Past") %>%
    filter(!(variant %in% c("Lower 95 PI", "Lower 80 PI", 'Median PI', "Upper 80 PI", "Upper 95 PI"))) %>%
    ggplot() +
    facet_wrap(vars(variant)) +
    geom_line(aes(x = year, y = total, color = "total", linetype = forecast), size = 0.7) +
    geom_line(aes(x = year, y = female, color = "female", linetype = forecast), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_linetype_manual(values = c("forecast" = "dashed", "data" = "solid"), guide = "none") +
    scale_x_continuous(breaks = seq(1950, 2100, 25), name = "") +
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_bw() +
    theme(legend.position = "right", legend.title = element_blank(), legend.direction = "vertical"))

dev.off()
