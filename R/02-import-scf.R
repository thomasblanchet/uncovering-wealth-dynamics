# ---------------------------------------------------------------------------- #
# Import microdata from the Survey of Consumer Finances and precursor surveys
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(readr)
library(progressr)
library(haven)
library(glue)

handlers(handler_progress(
    format = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta"
))

# ---------------------------------------------------------------------------- #
# 1962 Survey of Financial Characteristics of Consumers and
# 1963 Survey of Changes in Family Finances
# ---------------------------------------------------------------------------- #

dir.create(here("raw-data", "scf"), showWarnings = FALSE)

# Download the data from the Federal Reserve website, if necessary
if (!file.exists(here("raw-data", "scf", "sfcc6263.dta"))) {
    download.file(
        url = "https://www.federalreserve.gov/econres/files/6263_codebk6263.txt",
        destfile = here("raw-data", "scf", "6263_codebk6263.txt")
    )
    download.file(
        url = "https://www.federalreserve.gov/econres/files/6263_sfcc6263s.zip",
        destfile = here("raw-data", "scf", "6263_sfcc6263s.zip")
    )
    unzip(
        zipfile = here("raw-data", "scf", "6263_sfcc6263s.zip"),
        exdir = here("raw-data", "scf")
    )
    unlink(here("raw-data", "scf", "6263_sfcc6263s.zip"))
}

scf1962 <- read_dta(here("raw-data", "scf", "sfcc6263.dta"))

# Assets
scf1962_assets <- scf1962 %>% select(
    v5:v28,
    v45:v48, v50:v53, v55:v58, v60:v63, v65:v68, v70:v74,
    v75:v79, v85:v88, v91:v101, v103, v105, v107, v109, v112:v114, v176
)
scf1962_assets <- rowSums(scf1962_assets, na.rm = TRUE)
# Debt
scf1962_debt <- scf1962 %>% select(
    v42:v44, v104, v106, v108, v110, v115:v124
)
scf1962_debt <- rowSums(scf1962_debt, na.rm = TRUE)
# Inheritance
scf1962_inheritance <- scf1962 %>% select(x329:x334)
scf1962_inheritance <- rowSums(scf1962_inheritance, na.rm = TRUE)
# Income
scf1962_income <- scf1962 %>% select(x269, x304:x329)
scf1962_income <- rowSums(scf1962_income, na.rm = TRUE)

# Create individual dataset with relevant information
scf1962 <- scf1962 %>% transmute(
    weight = v4/1000,
    age_head = v180,
    age_spouse = v189,
    sex_head = if_else(v181 == 2, "female", "male"),
    number_adults = if_else(v182 == 1, 2, 1),
    wealth = scf1962_assets - scf1962_debt,
    income = scf1962_income,
    inheritance = scf1962_inheritance
)
scf1962 <- scf1962 %>%
    mutate(income = income/number_adults) %>%
    mutate(wealth = wealth/number_adults) %>%
    mutate(inheritance = inheritance/number_adults) %>%
    mutate(couple = if_else(number_adults > 1, 1, 0)) %>%
    mutate(hid = row_number()) %>%
    uncount(number_adults, .id = "member") %>%
    mutate(sex = case_when(
        member == 1 & sex_head == "male" ~ "male",
        member == 1 & sex_head == "female" ~ "female",
        member == 2 & sex_head == "male" ~ "female",
        member == 2 & sex_head == "female" ~ "male"
    )) %>%
    mutate(age = if_else(member == 1, age_head, age_spouse)) %>%
    select(-c(age_head, age_spouse, member, sex_head)) %>%
    mutate(year = 1962)

scf <- scf1962

# ---------------------------------------------------------------------------- #
# 1983 Survey of Consumer Finances
# ---------------------------------------------------------------------------- #

# Download the data from the Federal Reserve website, if necessary
if (!file.exists(here("raw-data", "scf", "scf83b.dta"))) {
    download.file(
        url = "https://www.federalreserve.gov/econres/files/1983_codebk83.txt",
        destfile = here("raw-data", "scf", "1983_codebk83.txt")
    )
    download.file(
        url = "https://www.federalreserve.gov/econres/files/1983_scf83bs.zip",
        destfile = here("raw-data", "scf", "1983_scf83bs.zip")
    )
    unzip(
        zipfile = here("raw-data", "scf", "1983_scf83bs.zip"),
        exdir = here("raw-data", "scf")
    )
    unlink(here("raw-data", "scf", "1983_scf83bs.zip"))
}

scf1983 <- read_dta(here("raw-data", "scf", "scf83b.dta"))

scf1983 <- scf1983 %>% transmute(
    weight = b3005,
    age_head = b4503,
    age_spouse = b3127,
    sex_head = if_else(b3126 == 2, "female", "male"),
    number_adults = if_else(b3112 == 1, 2, 1),
    # Remove vehicles from wealth since they are excluded for national accounts
    wealth = b3305 - b3320 - b3902,
    income = b3201,
    inheritance = b3213
)
scf1983 <- scf1983 %>%
    na.omit() %>%
    mutate(income = income/number_adults) %>%
    mutate(wealth = wealth/number_adults) %>%
    mutate(inheritance = inheritance/number_adults) %>%
    mutate(couple = if_else(number_adults > 1, 1, 0)) %>%
    mutate(hid = row_number()) %>%
    uncount(number_adults, .id = "member") %>%
    mutate(sex = case_when(
        member == 1 & sex_head == "male" ~ "male",
        member == 1 & sex_head == "female" ~ "female",
        member == 2 & sex_head == "male" ~ "female",
        member == 2 & sex_head == "female" ~ "male"
    )) %>%
    mutate(age = if_else(member == 1, age_head, age_spouse)) %>%
    select(-c(age_head, age_spouse, member, sex_head)) %>%
    mutate(year = 1983)

scf <- bind_rows(scf, scf1983)

# ---------------------------------------------------------------------------- #
# 1986 Survey of Consumer Finances
# ---------------------------------------------------------------------------- #

# Download the data from the Federal Reserve website, if necessary
if (!file.exists(here("raw-data", "scf", "scf86b.dta"))) {
    download.file(
        url = "https://www.federalreserve.gov/econres/files/1986_codebk86.txt",
        destfile = here("raw-data", "scf", "1986_codebk86.txt")
    )
    download.file(
        url = "https://www.federalreserve.gov/econres/files/1986_scf86bs.zip",
        destfile = here("raw-data", "scf", "1986_scf86bs.zip")
    )
    unzip(
        zipfile = here("raw-data", "scf", "1986_scf86bs.zip"),
        exdir = here("raw-data", "scf")
    )
    unlink(here("raw-data", "scf", "1986_scf86bs.zip"))
}

scf1986 <- read_dta(here("raw-data", "scf", "scf86b.dta"))

scf1986 <- scf1986 %>% transmute(
    weight = c1012,
    age_head = c1113,
    age_spouse = c1115,
    sex_head = if_else(c1117 == 2, "female", "male"),
    number_adults = if_else(c1119 == 1, 2, 1),
    # Remove vehicles from wealth since they are excluded for national accounts
    wealth = c1449 - c1455 - c1421,
    income = c1301,
    inheritance = if_else(c1317 == 1 | c1318 == 1, c1316, 0)
)
scf1986 <- scf1986 %>%
    na.omit() %>%
    mutate(income = income/number_adults) %>%
    mutate(inheritance = inheritance/number_adults) %>%
    mutate(wealth = wealth/number_adults) %>%
    mutate(couple = if_else(number_adults > 1, 1, 0)) %>%
    mutate(hid = row_number()) %>%
    uncount(number_adults, .id = "member") %>%
    mutate(sex = case_when(
        member == 1 & sex_head == "male" ~ "male",
        member == 1 & sex_head == "female" ~ "female",
        member == 2 & sex_head == "male" ~ "female",
        member == 2 & sex_head == "female" ~ "male"
    )) %>%
    mutate(age = if_else(member == 1, age_head, age_spouse)) %>%
    select(-c(age_head, age_spouse, member, sex_head)) %>%
    mutate(year = 1986)

scf <- bind_rows(scf, scf1986)

# ---------------------------------------------------------------------------- #
# 1989-2016 Surveys of Consumer Finances
# ---------------------------------------------------------------------------- #

scf <- scf %>% mutate(implicate = 1)

for (year in seq(1989, 2019, 3)) {
    # Name of the codebook file
    if (year < 2000) {
        file_codebook <- paste0("codebk", substr(year, 3, 4), ".txt")
    } else {
        file_codebook <- paste0("codebk", year, ".txt")
    }
    # Name of the full data file
    if (year <= 2001) {
        file_full_zip <- paste0("scf", substr(year, 3, 4), "s.zip")
    } else {
        file_full_zip <- paste0("scf", year, "s.zip")
    }
    if (year == 1992) {
        file_full_dta <- paste0("p", substr(year, 3, 4), "i4.dta")
    } else {
        file_full_dta <- paste0("p", substr(year, 3, 4), "i6.dta")
    }
    # Name of the summary data file
    file_summary_zip <- paste0("scfp", year, "s.zip")
    file_summary_dta <- paste0("rscfp", year, ".dta")

    # Download the data from the Federal Reserve website, if necessary
    if (!file.exists(here("raw-data", "scf", file_summary_dta))) {
        # Codebook
        download.file(
            url = glue("https://www.federalreserve.gov/econres/files/{file_codebook}"),
            destfile = here("raw-data", "scf", file_codebook)
        )

        # Full data file
        download.file(
            url = glue("https://www.federalreserve.gov/econres/files/{file_full_zip}"),
            destfile = here("raw-data", "scf", file_full_zip)
        )
        unzip(
            zipfil = here("raw-data", "scf", file_full_zip),
            exdir = here("raw-data", "scf")
        )
        unlink(here("raw-data", "scf", file_full_zip))

        # Summary file
        download.file(
            url = glue("https://www.federalreserve.gov/econres/files/{file_summary_zip}"),
            destfile = here("raw-data", "scf", file_summary_zip)
        )
        unzip(
            zipfil = here("raw-data", "scf", file_summary_zip),
            exdir = here("raw-data", "scf")
        )
        unlink(here("raw-data", "scf", file_summary_zip))
    }

    # Import net wealth from the summary file
    scf_summary <- read_dta(here("raw-data", "scf", file_summary_dta))
    if (year == 1989) {
        scf_summary <- scf_summary %>% mutate(id = x1, implicate = x1 %% 10)
    } else {
        scf_summary <- scf_summary %>% mutate(id = y1, implicate = y1 %% 10)
    }
    scf_summary <- scf_summary %>% transmute(id, implicate,
        weight = wgt,
        number_adults = if_else(married == 1, 2, 1),
        income = income,
        # Remove vehicles and other consumer durables from net wealth
        wealth = networth - vehic - othnfin
    )

    # Import age and inheritance from full file
    scf_full <- read_dta(here("raw-data", "scf", file_full_dta))
    colnames(scf_full) <- str_to_upper(colnames(scf_full))
    if (year == 1989) {
        scf_full <- scf_full %>% mutate(id = X1)
    } else {
        scf_full <- scf_full %>% mutate(id = Y1)
    }
    if (year < 1995) {
        scf_full <- scf_full %>% transmute(id,
            age_head = X8022,
            age_spouse = X104,
            sex_head = if_else(X8021 == 2, "female", "male"),
            inheritance = if_else(X5725 == 12, X5724, 0) + if_else(X5727 == 12, X5726, 0)
        )
    } else {
        scf_full <- scf_full %>% transmute(id,
            age_head = X8022,
            age_spouse = X104,
            sex_head = if_else(X8021 == 2, "female", "male"),
            inheritance = if_else(X5725 == 12, X5724, 0)
        )
    }

    scf_summary <-     scf_summary %>% left_join(scf_full)

    # Turn into an individual dataset
    scf_summary <-     scf_summary %>%
        na.omit() %>%
        mutate(income = income/number_adults) %>%
        mutate(wealth = wealth/number_adults) %>%
        mutate(inheritance = inheritance/number_adults) %>%
        mutate(couple = if_else(number_adults > 1, 1, 0)) %>%
        mutate(hid = row_number()) %>%
        uncount(number_adults, .id = "member") %>%
        mutate(sex = case_when(
            member == 1 & sex_head == "male" ~ "male",
            member == 1 & sex_head == "female" ~ "female",
            member == 2 & sex_head == "male" ~ "female",
            member == 2 & sex_head == "female" ~ "male"
        )) %>%
        mutate(age = if_else(member == 1, age_head, age_spouse)) %>%
        select(-c(age_head, age_spouse, member, id, sex_head)) %>%
        mutate(year = year)

    scf <- bind_rows(scf, scf_summary)
}

# Clean the data
scf <- scf %>% mutate(inheritance = if_else(inheritance == -9, 0, inheritance))
scf <- scf %>% group_by(year) %>% mutate(weight = weight/sum(weight)) %>% ungroup()

# Interpolate the survey from 1962 to 2019 by mixing adjacent years
set.seed(19920902)
with_progress({
    p <- progressor(along = c(1962, 1964, 1966:2019))
    years_scf <- unique(scf$year)
    scf_inter <- map_dfr(c(1962, 1964, 1966:2019), function(year) {
        if (year %in% years_scf) {
            p()
            return(scf[scf$year == year, ])
        }

        year_before <- max(years_scf[years_scf <= year])
        year_after <- min(years_scf[years_scf > year])

        lambda <- (year - year_before)/(year_after - year_before)

        scf_before <- scf[scf$year == year_before, ]
        scf_after <- scf[scf$year == year_after, ]

        mean_wealth_before <- weighted.mean(scf_before$wealth, scf_before$weight)
        mean_wealth_after <- weighted.mean(scf_after$wealth, scf_after$weight)
        mean_wealth <- (1 - lambda)*mean_wealth_before + lambda*mean_wealth_after

        scf_before$wealth <- scf_before$wealth/mean_wealth_before
        scf_before$inheritance <- scf_before$inheritance/mean_wealth_before

        scf_after$wealth <- scf_after$wealth/mean_wealth_after
        scf_after$inheritance <- scf_after$inheritance/mean_wealth_after

        hid_before <- unique(scf_before$hid)
        hid_after <- unique(scf_after$hid)

        sample_before <- sample(hid_before, size = (1 - lambda)*length(hid_before), replace = FALSE)
        sample_after <- sample(hid_after, size = lambda*length(hid_after), replace = FALSE)

        df <- bind_rows(
            scf_before[scf_before$hid %in% sample_before, ],
            scf_after[scf_after$hid %in% sample_after, ],
            .id = "svy"
        )

        df <- df %>% group_by(svy, hid) %>% mutate(hid = cur_group_id()) %>% ungroup() %>% select(-svy)
        df$year <- year

        df$wealth <- df$wealth*mean_wealth
        df$inheritance <- df$inheritance*mean_wealth

        p()
        return(df)
    })
})


# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

dir.create(here("work", "02-import-scf"))

write_rds(scf, here("work", "02-import-scf", "scf.rds"))
write_rds(scf_inter, here("work", "02-import-scf", "scf_inter.rds"))
