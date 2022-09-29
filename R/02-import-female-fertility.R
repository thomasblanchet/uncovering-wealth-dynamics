# ---------------------------------------------------------------------------- #
# Import fertility data
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(readxl)
library(here)

options(dplyr.summarise.inform = FALSE)

population <- read_rds(here("work", "02-import-population", "population.rds"))

# ---------------------------------------------------------------------------- #
# Data for 1933-2019 (Human Fertility Database)
# ---------------------------------------------------------------------------- #

fertility_hfd <- read_fwf(
    file = here("raw-data", "hfd", "USAbirthsRRbo.txt"),
    skip = 3,
    na = ".",
    col_positions = fwf_widths(
        widths = c(8, 4, 12, 12, 12, 12, 12, 12),
        col_names = c("year", "age", "bc", "bc1", "bc2", "bc3", "bc4", "bc5")
    ),
    col_types = cols(
        year = col_double(),
        age = col_character(),
        bc = col_double(),
        bc1 = col_double(),
        bc2 = col_double(),
        bc3 = col_double(),
        bc4 = col_double(),
        bc5 = col_double()
    )
)
fertility_hfd <- fertility_hfd %>% mutate(age = as.integer(as.numeric(gsub("[+-]", "", age))))

fertility_hfd <- fertility_hfd %>% left_join(population %>% filter(sex == "female")) %>% transmute(
    year, age, bc, bc1, bc2, bc3, bc4, bc5,
    asfr = bc/population,
    asfr1 = bc1/population,
    asfr2 = bc2/population,
    asfr3 = bc3/population,
    asfr4 = bc4/population,
    asfr5 = bc5/population
)

# ---------------------------------------------------------------------------- #
# Data for 1917-1932 (Human Fertility Collection)
# ---------------------------------------------------------------------------- #

fertility_hfc <- read_csv(
    file = here("raw-data", "hfc", "USA_ASFRstand_BO.txt"),
    na = ".",
    col_types = cols(
        .default = col_double(),
        Country = col_character(),
        Region = col_logical(),
        Urban = col_logical(),
        Origin = col_logical(),
        AgeDef = col_character(),
        Collection = col_character(),
        SourceType = col_character(),
        RefCode = col_character(),
        Note = col_integer()
    )
)

fertility_hfc <- fertility_hfc %>% filter(Year1 == Year2 & Vitality == 1 & AgeDef == "ACY")

fertility_hfc <- fertility_hfc %>% transmute(
    year = Year1,
    age = Age,
    asfr1 = ASFR1,
    asfr2 = ASFR2,
    asfr3 = ASFR3,
    asfr4 = ASFR4,
    asfr5 = ASFR5P
)

fertility_hfc <- fertility_hfc %>% group_by(year, age) %>% summarise(
    asfr1 = mean(asfr1, na.rm = TRUE),
    asfr2 = mean(asfr2, na.rm = TRUE),
    asfr3 = mean(asfr3, na.rm = TRUE),
    asfr4 = mean(asfr4, na.rm = TRUE),
    asfr5 = mean(asfr5, na.rm = TRUE)
) %>% mutate(asfr = asfr1 + asfr2 + asfr3 + asfr4 + asfr5) %>% ungroup()

# ---------------------------------------------------------------------------- #
# Data for 1897-1917 (Human Fertility Collection, no birth order)
# ---------------------------------------------------------------------------- #

fertility_hfc_early <- read_csv(
    file = here("raw-data", "hfc", "USA_ASFRstand_TOT.txt"),
    na = ".",
    col_types = cols(
        Country = col_character(),
        Region = col_logical(),
        Urban = col_logical(),
        Origin = col_logical(),
        Year1 = col_double(),
        Year2 = col_double(),
        Age = col_double(),
        AgeInt = col_double(),
        AgeDef = col_character(),
        Vitality = col_double(),
        ASFR = col_double(),
        CPFR = col_double(),
        Collection = col_character(),
        SourceType = col_character(),
        RefCode = col_character(),
        Note = col_double(),
        Split = col_double()
    )
)
fertility_hfc_early <- fertility_hfc_early %>%
    filter((Year1 == 1895 & Year2 == 1899) | (Year1 == 1905 & Year2 == 1910)) %>%
    transmute(
        year = floor((Year1 + Year2)/2),
        age = Age,
        asfr = if_else(is.na(ASFR), 0, ASFR)
    )

# Combine with the rest of the HFC data
fertility_hfc <- bind_rows(fertility_hfc, fertility_hfc_early)

# Interpolate between old and new HFC data and estimate ASFR by birth order
# by rescaling the birth order composition of 1917
fertility_hfc <- fertility_hfc %>%
    complete(year = full_seq(year, 1), age) %>%
    group_by(age) %>%
    mutate(asfr = splinefun(x = year, y = asfr, method = "monoH.FC")(year)) %>%
    arrange(age, year) %>%
    fill(asfr1, asfr2, asfr3, asfr4, asfr5, .direction = "up") %>%
    mutate(asfr_sum = rowSums(cbind(asfr1, asfr2, asfr3, asfr4, asfr5))) %>%
    mutate(across(c(asfr1, asfr2, asfr3, asfr4, asfr5), ~ .x/asfr_sum*asfr)) %>%
    select(-asfr_sum)

# Estimate birth counts
fertility_hfc <- fertility_hfc %>% left_join(population %>% filter(sex == "female")) %>% transmute(
    year, age, asfr, asfr1, asfr2, asfr3, asfr4, asfr5,
    bc = asfr*population,
    bc1 = asfr1*population,
    bc2 = asfr2*population,
    bc3 = asfr3*population,
    bc4 = asfr4*population,
    bc5 = asfr5*population
)

# ---------------------------------------------------------------------------- #
# Data before 1897 (Gapminder, total fertility rate only)
# ---------------------------------------------------------------------------- #

tfr_gapminder <- read_excel(
    path = here("raw-data", "gapminder", "tfr-by-gapminder.xlsx"),
    sheet = "countries_and_territories"
)
tfr_gapminder <- tfr_gapminder %>%
    filter(geo == "usa") %>%
    select(-c(geo.name, indicator.name, indicator)) %>%
    pivot_longer(-geo, values_to = "tfr", names_to = "year") %>%
    select(-geo) %>%
    mutate(year = as.integer(as.character(year))) %>%
    filter(year >= 1850 & year < 2015) # After 2014 it uses WPP 2015 projection

# Rescale 1897 ASFR on the TFR
fertility_gapminder <- expand_grid(year = 1850:1897, age = 14:50) %>%
    left_join(tfr_gapminder) %>%
    left_join(fertility_hfc %>% filter(year == 1897)) %>%
    group_by(age) %>%
    arrange(age, year) %>%
    fill(starts_with("asfr"), .direction = "up") %>%
    group_by(year) %>%
    mutate(tfr_sum = sum(asfr)) %>%
    mutate(across(c(asfr, asfr1, asfr2, asfr3, asfr4, asfr5), ~ .x/tfr_sum*tfr)) %>%
    select(-tfr, -tfr_sum) %>%
    ungroup() %>%
    arrange(year, age) %>%
    filter(year < 1897)

# Estimate birth counts
fertility_gapminder <- fertility_gapminder %>% left_join(population %>% filter(sex == "female")) %>% transmute(
    year, age, asfr, asfr1, asfr2, asfr3, asfr4, asfr5,
    bc = asfr*population,
    bc1 = asfr1*population,
    bc2 = asfr2*population,
    bc3 = asfr3*population,
    bc4 = asfr4*population,
    bc5 = asfr5*population
)

# ---------------------------------------------------------------------------- #
# Forecasts from the World Population Prospects
# ---------------------------------------------------------------------------- #

wpp_url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES"
if (!file.exists(here("raw-data", "wpp", "WPP2019_Fertility_by_Age.csv"))) {
    download.file(
        url = glue("{wpp_url}/WPP2019_Fertility_by_Age.csv"),
        destfile = here("raw-data", "wpp", "WPP2019_Fertility_by_Age.csv")
    )
}

fertility_wpp <- read_csv(
    file = here("raw-data", "wpp", "WPP2019_Fertility_by_Age.csv"),
    col_types = cols(
        LocID = col_double(),
        Location = col_character(),
        VarID = col_double(),
        Variant = col_character(),
        Time = col_character(),
        MidPeriod = col_double(),
        AgeGrp = col_character(),
        AgeGrpStart = col_double(),
        AgeGrpSpan = col_double(),
        ASFR = col_double(),
        PASFR = col_double(),
        Births = col_double()
    )
)
fertility_wpp <- fertility_wpp %>% filter(LocID == 840)
fertility_wpp <- fertility_wpp %>% transmute(
    variant = str_replace(str_to_title(Variant), fixed("Pi"), "PI"),
    year = MidPeriod,
    age_start = AgeGrpStart,
    age_span = AgeGrpSpan,
    asfr = ASFR/1000
)

# Interpolate by age
fertility_wpp <- fertility_wpp %>%
    group_by(variant, year) %>%
    arrange(variant, year, age_start) %>%
    mutate(cumul_asfr = cumsum(5*asfr)) %>%
    mutate(age = age_start + age_span) %>%
    complete(age = 14:50) %>%
    mutate(cumul_asfr = if_else(age == 14, 0, cumul_asfr)) %>%
    mutate(cumul_asfr = splinefun(x = age, y = cumul_asfr, method = "monoH.FC")(age)) %>%
    arrange(variant, year, age) %>%
    mutate(asfr = c(0, diff(cumul_asfr))) %>%
    ungroup() %>%
    select(variant, year, age, asfr)

# Interpolate by year
fertility_wpp <- fertility_wpp %>%
    complete(variant, age, year = 1950:2100) %>%
    # Interpolate medium first (to anchor the interpolation of the other
    # scenarios pre-2023)
    group_by(variant, age) %>%
    arrange(variant, age, year) %>%
    mutate(asfr = if_else(
        variant == "Medium",
        splinefun(year, asfr, method = "monoH.FC")(year),
        asfr
    )) %>%
    # Copy the medium variant into each variant for 2015-2020 to anchor
    # interpolation afterwards
    group_by(age, year) %>%
    mutate(asfr = if_else(year == 2020, first(na.omit(asfr)), asfr)) %>%
    ungroup() %>%
    filter(year >= 2020 | variant == "Medium") %>%
    mutate(variant = if_else(year < 2020, "Past", variant)) %>%
    group_by(variant, age) %>%
    arrange(variant, age, year) %>%
    mutate(asfr = splinefun(year, asfr, method = "monoH.FC")(year)) %>%
    ungroup() %>%
    arrange(variant, year, age)

# Extrapolate birth order decomposition from 2019
fertility_wpp <- fertility_wpp %>%
    left_join(fertility_hfd %>%
        filter(year == 2019) %>%
        mutate(year = 2020) %>%
        select(year, age, num_range("asfr", 1:5))
    ) %>%
    group_by(variant, age) %>%
    arrange(variant, age, year) %>%
    fill(num_range("asfr", 1:5), .direction = "down") %>%
    mutate(asfr_sum = rowSums(cbind(asfr1, asfr2, asfr3, asfr4, asfr5))) %>%
    mutate(across(c(asfr1, asfr2, asfr3, asfr4, asfr5), ~ .x/asfr_sum*asfr)) %>%
    select(-asfr_sum) %>%
    ungroup() %>%
    arrange(variant, year, age)

# Estimate birth counts
fertility_wpp <- fertility_wpp %>% left_join(population %>% filter(sex == "female")) %>% transmute(
    year, age, variant, asfr, asfr1, asfr2, asfr3, asfr4, asfr5,
    bc = asfr*population,
    bc1 = asfr1*population,
    bc2 = asfr2*population,
    bc3 = asfr3*population,
    bc4 = asfr4*population,
    bc5 = asfr5*population
)

# ---------------------------------------------------------------------------- #
# Combine the data sources & save
# ---------------------------------------------------------------------------- #

tfr_all <- bind_rows(
    tfr_gapminder %>% mutate(source = "Gapminder (2017)", variant = "Past"),
    fertility_wpp %>%
        group_by(variant, year) %>%
        summarise(tfr = sum(asfr)) %>%
        mutate(source = "UN World Population Prospects"),
    fertility_hfc %>%
        group_by(year) %>%
        summarise(tfr = sum(asfr)) %>%
        mutate(source = "Human Fertility Collection", variant = "Past"),
    fertility_hfd %>%
        group_by(year) %>%
        summarise(tfr = sum(asfr)) %>%
        mutate(source = "Human Fertility Database", variant = "Past")
)

tfr <- bind_rows(
    tfr_gapminder %>% mutate(source = "Gapminder (2017)", variant = "Past") %>% filter(year < 1897),
    fertility_wpp %>%
        group_by(variant, year) %>%
        summarise(tfr = sum(asfr)) %>%
        mutate(source = "UN World Population Prospects") %>% filter(year > 2019),
    fertility_hfc %>%
        group_by(year) %>%
        summarise(tfr = sum(asfr)) %>%
        mutate(source = "Human Fertility Collection", variant = "Past") %>%
        filter(year >= 1897 & year < 1933),
    fertility_hfd %>%
        group_by(year) %>%
        summarise(tfr = sum(asfr)) %>%
        mutate(source = "Human Fertility Database", variant = "Past") %>%
        filter(year >= 1933 & year <= 2019)
)

fertility_all <- bind_rows(
    fertility_gapminder %>% mutate(source = "Gapminder (2017)", variant = "Past"),
    fertility_wpp %>% mutate(source = "UN World Population Prospects"),
    fertility_hfc %>% mutate(source = "Human Fertility Collection", variant = "Past"),
    fertility_hfd %>% mutate(source = "Human Fertility Database", variant = "Past")
)
fertility_all <- fertility_all %>% complete(nesting(year, source, variant), age = 12:75, fill = list(
    asfr = 0, asfr1 = 0, asfr2 = 0, asfr3 = 0, asfr4 = 0, asfr5 = 0,
    bc = 0, bc1 = 0, bc2 = 0, bc3 = 0, bc4 = 0, bc5 = 0
))

fertility <- bind_rows(
    fertility_gapminder %>% mutate(source = "Gapminder (2017)", variant = "Past") %>% filter(year < 1897),
    fertility_wpp %>% mutate(source = "UN World Population Prospects") %>% filter(year > 2019),
    fertility_hfc %>% mutate(source = "Human Fertility Collection", variant = "Past") %>% filter(year >= 1897 & year < 1933),
    fertility_hfd %>% mutate(source = "Human Fertility Database", variant = "Past") %>% filter(year >= 1933 & year <= 2019)
)
fertility <- fertility %>% complete(nesting(year, source, variant), age = 12:75, fill = list(
    asfr = 0, asfr1 = 0, asfr2 = 0, asfr3 = 0, asfr4 = 0, asfr5 = 0,
    bc = 0, bc1 = 0, bc2 = 0, bc3 = 0, bc4 = 0, bc5 = 0
))

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

dir.create(here("work", "02-import-female-fertility"), showWarnings = FALSE)
write_rds(tfr, here("work", "02-import-female-fertility", "tfr.rds"))
write_rds(tfr_all, here("work", "02-import-female-fertility", "tfr_all.rds"))
write_rds(fertility, here("work", "02-import-female-fertility", "fertility.rds"))
write_rds(fertility_all, here("work", "02-import-female-fertility", "fertility_all.rds"))

# ---------------------------------------------------------------------------- #
# Plot the results
# ---------------------------------------------------------------------------- #

dir.create(here("graphs", "02-import-female-fertility"), showWarnings = FALSE)

pdf(file = here("graphs", "02-import-female-fertility", "tfr-sources.pdf"), height = 5, width = 6)
print(tfr_all %>% filter(variant == "Medium" | variant == "Past") %>% ggplot() +
    geom_line(aes(x = year, y = tfr, group = source, color = source), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_x_continuous(breaks = seq(1850, 2100, 25), name = "") +
    scale_y_continuous(breaks = seq(1.5, 6, 0.5), limits = c(1.5, 6), name = "total fertility rate") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "vertical"))
dev.off()

pdf(file = here("graphs", "02-import-female-fertility", "tfr-selected-sources.pdf"), height = 5, width = 6)
print(tfr %>% filter(variant == "Medium" | variant == "Past") %>% ggplot() +
    geom_line(aes(x = year, y = tfr, group = source, color = source), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_x_continuous(breaks = seq(1850, 2100, 25), name = "") +
    scale_y_continuous(breaks = seq(1.5, 6, 0.5), limits = c(1.5, 6), name = "total fertility rate") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = "vertical"))
dev.off()

pdf(file = here("graphs", "02-import-female-fertility", "tfr-forecasts.pdf"), height = 8, width = 12)
print(tfr_all %>%
    filter(source == "UN World Population Prospects") %>%
    complete(year = 1950:2100, nesting(source, variant)) %>%
    group_by(year) %>%
    fill(tfr, .direction = "updown") %>%
    mutate(forecast = if_else(year > 2019, "forecast", "data")) %>%
    ggplot() +
    facet_wrap(vars(variant)) +
    geom_line(aes(x = year, y = tfr, linetype = forecast), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_linetype_manual(values = c("forecast" = "dashed", "data" = "solid"), guide = "none") +
    scale_x_continuous(breaks = seq(1850, 2100, 25), name = "") +
    ylab("total fertility rate") +
    theme_bw() +
    theme(legend.position = "right", legend.title = element_blank(), legend.direction = "vertical"))
dev.off()


