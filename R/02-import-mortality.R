# ---------------------------------------------------------------------------- #
# Import Mortality Data
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(readxl)
library(glue)

options(dplyr.summarise.inform = FALSE)

# ---------------------------------------------------------------------------- #
# Human Mortality Database (1933-2018)
# ---------------------------------------------------------------------------- #

# Men
life_table_men <- read_fwf(
    file = here("raw-data", "hmd", "mltper_1x1.txt"),
    col_positions = fwf_positions(
        start = c(1, 15, 24, 33, 42, 48, 56, 64, 72, 81),
        end = c(15, 24, 33, 42, 48, 56, 64, 72, 81, 87),
        col_names = c("Year", "Age", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")
    ),
    col_types = cols(.default = col_number()),
    skip = 3
)

# Women
life_table_women <- read_fwf(
    file = here("raw-data", "hmd", "fltper_1x1.txt"),
    col_positions = fwf_positions(
        start = c(1, 15, 24, 33, 42, 48, 56, 64, 72, 81),
        end = c(15, 24, 33, 42, 48, 56, 64, 72, 81, 87),
        col_names = c("Year", "Age", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")
    ),
    col_types = cols(.default = col_number()),
    skip = 3
)

life_table_hmd <- bind_rows(
    life_table_men %>% mutate(sex = "male"),
    life_table_women %>% mutate(sex = "female")
)
life_table_hmd <- life_table_hmd %>%
    rename(year = Year, age = Age) %>%
    select(sex, year, age, qx)

# ---------------------------------------------------------------------------- #
# Human Life-Table Database (1900-1932)
# ---------------------------------------------------------------------------- #

life_table_hltd <- read_csv(
    file = here("raw-data", "hltd", "USA.csv"),
    col_types = cols(
        .default = col_double(),
        Country = col_character(),
        Ethnicity = col_character(),
        `Ref-ID` = col_character()
    )
)

life_table_hltd <- life_table_hltd %>% filter(
    (Year1 == Year2) &
    (Ethnicity == 0) &
    (Year1 <= 1932) &
    (TypeLT == 1)
)

life_table_hltd <- life_table_hltd %>% transmute(
    year = Year1,
    age = Age,
    sex = if_else(Sex == 1, "male", "female"),
    qx = `q(x)`
)
# Low quality above 100
life_table_hltd <- life_table_hltd %>% group_by(year, sex) %>% mutate(
    qx = if_else(age > 100 & age < max(age), 0.5, qx)
) %>% ungroup()

# ---------------------------------------------------------------------------- #
# Haines data (before 1900)
# ---------------------------------------------------------------------------- #

life_table_haines <- map_dfr(seq(1850, 1900, 10), function(year) {
    lt <- read_excel(
        path = here("raw-data", "haines", "haines-life-tables.xlsx"),
        sheet = glue("{year}")
    )

    lt <- lt %>%
        filter(sex == "male" | sex == "female") %>%
        select(year, sex, age = age1, lx, qx)

    return(lt)
})

# Interpolate by age
life_table_haines <- life_table_haines %>%
    complete(age = 0:80, sex, year) %>%
    group_by(year, sex) %>%
    arrange(year, sex, age) %>%
    mutate(lx = exp(splinefun(age, log(lx), method = "monoH.FC")(age))) %>%
    mutate(qx = 1 - lead(lx)/lx) %>%
    filter(age < 80) %>%
    ungroup()

# Interpolate by year
life_table_haines <- life_table_haines %>%
    complete(age, sex, year = 1850:1900) %>%
    group_by(sex, age) %>%
    arrange(sex, age, year) %>%
    mutate(qx = exp(splinefun(year, log(qx), method = "monoH.FC")(year))) %>%
    ungroup() %>%
    arrange(year, sex, age) %>%
    filter(year < 1900) %>%
    select(-lx)

# For 80 and above, before 1900: rescale mortality rates from 1900
life_table_haines <- life_table_haines %>%
    complete(age = 0:105, year, sex) %>%
    left_join(life_table_hltd %>% filter(year == 1900) %>% select(sex, age, qx1900 = qx)) %>%
    group_by(sex, year) %>%
    arrange(sex, year, age) %>%
    mutate(ratio = if_else(age > 100, 1, qx/qx1900)) %>%
    mutate(ratio = approx(age, ratio, xout = age)$y) %>%
    mutate(qx = if_else(is.na(qx), qx1900*ratio, qx)) %>%
    select(-c(qx1900, ratio))

# ---------------------------------------------------------------------------- #
# UN World Population Prospects
# ---------------------------------------------------------------------------- #

wpp_url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES"
# Medium variant
if (!file.exists(here("raw-data", "wpp", "WPP2019_Life_Table_Medium.csv"))) {
    download.file(
        url = glue("{wpp_url}/WPP2019_Life_Table_Medium.csv"),
        destfile = here("raw-data", "wpp", "WPP2019_Life_Table_Medium.csv")
    )
}
# Other variants
if (!file.exists(here("raw-data", "wpp", "WPP2019_Life_Table_OtherVariants.csv"))) {
    download.file(
        url = glue("{wpp_url}/WPP2019_Life_Table_OtherVariants.csv"),
        destfile = here("raw-data", "wpp", "WPP2019_Life_Table_OtherVariants.csv")
    )
}

life_table_wpp <- bind_rows(
    read_csv(
        file = here("raw-data", "wpp", "WPP2019_Life_Table_Medium.csv"),
        na = c("NULL", ""),
        col_types = cols(
            .default = col_double(),
            Location = col_character(),
            Variant = col_character(),
            Time = col_character(),
            Sex = col_character(),
            AgeGrp = col_character()
        )
    ),
    read_csv(
        file = here("raw-data", "wpp", "WPP2019_Life_Table_OtherVariants.csv"),
        na = c("NULL", ""),
        col_types = cols(
            .default = col_double(),
            Location = col_character(),
            Variant = col_character(),
            Time = col_character(),
            Sex = col_character(),
            AgeGrp = col_character()
        )
    )
)
life_table_wpp <- life_table_wpp %>% filter(LocID == 840 & Sex != "Total")

life_table_wpp <- life_table_wpp %>% transmute(
    variant = str_replace(str_to_title(Variant), fixed("Pi"), "PI"),
    year = MidPeriod,
    sex = str_to_lower(Sex),
    age_start = AgeGrpStart,
    age_span = AgeGrpSpan,
    lx = lx
)

# Interpolate by age
life_table_wpp <- life_table_wpp %>%
    complete(age_start = 0:100, sex, year, variant) %>%
    filter(year >= 2020 | variant == "Medium") %>%
    group_by(variant, year, sex) %>%
    arrange(variant, year, sex, age_start) %>%
    mutate(lx = exp(splinefun(age_start, log(lx), method = "monoH.FC")(age_start))) %>%
    mutate(qx = 1 - lead(lx)/lx) %>%
    mutate(qx = if_else(age_start == 100, 1, qx)) %>%
    ungroup() %>%
    select(-age_span, -lx) %>%
    rename(age = age_start)

# Interpolate by year
life_table_wpp <- life_table_wpp %>%
    complete(variant, age, sex, year = 1950:2100) %>%
    # Interpolate medium first (to anchor the interpolation of the other
    # scenarios pre-2023)
    group_by(variant, sex, age) %>%
    arrange(variant, sex, age, year) %>%
    mutate(qx = if_else(
        variant == "Medium",
        exp(splinefun(year, log(qx), method = "monoH.FC")(year)),
        qx
    )) %>%
    # Copy the medium variant into each variant for 2015-2020 to anchor
    # interpolation afterwards
    group_by(age, sex, year) %>%
    mutate(qx = if_else(year == 2020, first(na.omit(qx)), qx)) %>%
    ungroup() %>%
    filter(year >= 2020 | variant == "Medium") %>%
    mutate(variant = if_else(year < 2020, "Past", variant)) %>%
    group_by(variant, sex, age) %>%
    arrange(variant, sex, age, year) %>%
    mutate(qx = exp(splinefun(year, log(qx), method = "monoH.FC")(year))) %>%
    ungroup() %>%
    arrange(variant, year, sex, age)

# Add mortality variants that are the same as other scenarios but not directly
# included in the data (see table II.3 in
# <https://population.un.org/wpp/Publications/Files/WPP2019_Methodology.pdf>)
life_table_wpp <- bind_rows(
    life_table_wpp,
    # Scenarios that assume median mortality
    life_table_wpp %>%
        filter(variant == "Median PI") %>%
        complete(age, sex, year, variant = c("High", "Instant Replacement", "Low", "Zero Migration", "Constant Fertility")) %>%
        group_by(age, sex, year) %>%
        fill(qx, .direction = "updown") %>%
        ungroup() %>%
        filter(variant != "Median PI"),
    # Scenarios that assume constant mortality after 2015-2020
    life_table_wpp %>%
        filter(variant == "Past") %>%
        group_by(variant, age, sex) %>%
        complete(year = 2020:2100) %>%
        mutate(qx = if_else(year == 2020, qx, NA_real_)) %>%
        fill(qx, .direction = "updown") %>%
        filter(year >= 2021) %>%
        ungroup() %>%
        complete(age, sex, year, variant = c("Constant Mortality", "Momentum", "No Change")) %>%
        group_by(age, sex, year) %>%
        fill(qx, .direction = "updown") %>%
        ungroup() %>%
        filter(variant != "Past")
)

# ---------------------------------------------------------------------------- #
# Combine the data and check their consistency
# ---------------------------------------------------------------------------- #

life_table_all <- bind_rows(
    life_table_hmd %>% mutate(source = "Human Mortality Database", variant = "Past"),
    life_table_hltd %>% mutate(source = "Human Life Table Database", variant = "Past"),
    life_table_haines %>% mutate(source = "Haines (1998)", variant = "Past"),
    life_table_wpp %>% mutate(source = "UN World Population Prospects")
)

life_table <- bind_rows(
    life_table_hmd %>% mutate(source = "Human Mortality Database", variant = "Past"),
    life_table_hltd %>% mutate(source = "Human Life Table Database", variant = "Past"),
    life_table_haines %>% mutate(source = "Haines (1998)", variant = "Past"),
    life_table_wpp %>% mutate(source = "UN World Population Prospects") %>% filter(year >= 2020)
)

life_expectancy <- life_table %>%
    group_by(source, variant, sex, year) %>%
    arrange(source, variant, sex, year, age) %>%
    mutate(px = cumprod(c(1, 1 - qx[-length(qx)]))) %>%
    mutate(dx = -diff(c(px, 0))) %>%
    arrange(source, variant, sex, year, desc(age)) %>%
    mutate(ex = cumsum(age*dx)/px) %>%
    arrange(source, variant, sex, year, age)

dir.create(here("work", "02-import-mortality"), showWarnings = FALSE)
write_rds(life_table_all, here("work", "02-import-mortality", "life_table_all.rds"))
write_rds(life_table, here("work", "02-import-mortality", "life_table.rds"))
write_rds(life_expectancy, here("work", "02-import-mortality", "life_expectancy.rds"))

# ---------------------------------------------------------------------------- #
# Plot the results
# ---------------------------------------------------------------------------- #

dir.create(here("graphs", "02-import-mortality"), showWarnings = FALSE)

pdf(file = here("graphs", "02-import-mortality", "life-expectancy.pdf"), height = 5, width = 9)
print(life_expectancy %>%
    filter(variant == "Medium" | variant == "Past") %>%
    filter(age %in% c(0, 15, 50, 70)) %>%
    ggplot() +
    facet_grid(cols = vars(sex)) +
    geom_line(aes(x = year, y = ex, group = interaction(age, source), color = source), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    ylab("life expectancy") +
    scale_y_continuous(breaks = seq(40, 90, 10)) +
    scale_x_continuous(breaks = seq(1850, 2100, 25), name = "") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank()))
dev.off()

pdf(file = here("graphs", "02-import-mortality", "life-expectancy-male-forecasts.pdf"), height = 8, width = 12)
print(life_expectancy %>%
    filter(age %in% c(0, 15, 50, 70) & sex == "male") %>%
    ungroup() %>%
    complete(year, age, variant) %>%
    group_by(age, year) %>%
    fill(ex, .direction = "updown") %>%
    filter(variant != "Past") %>%
    ggplot() +
    facet_wrap(vars(variant)) +
    geom_line(aes(x = year, y = ex, color = as.factor(age)), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    ylab("life expectancy") +
    scale_y_continuous(breaks = seq(40, 90, 10)) +
    scale_x_continuous(breaks = seq(1850, 2100, 50), name = "") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank()))
dev.off()

pdf(file = here("graphs", "02-import-mortality", "life-expectancy-female-forecasts.pdf"), height = 8, width = 12)
print(life_expectancy %>%
    filter(age %in% c(0, 15, 50, 70) & sex == "female") %>%
    ungroup() %>%
    complete(year, age, variant) %>%
    group_by(age, year) %>%
    fill(ex, .direction = "updown") %>%
    filter(variant != "Past") %>%
    ggplot() +
    facet_wrap(vars(variant)) +
    geom_line(aes(x = year, y = ex, color = as.factor(age)), size = 0.7) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    ylab("life expectancy") +
    scale_y_continuous(breaks = seq(40, 90, 10)) +
    scale_x_continuous(breaks = seq(1850, 2100, 50), name = "") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank()))
dev.off()

