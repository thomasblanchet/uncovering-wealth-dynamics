# ---------------------------------------------------------------------------- #
# Combine DINA data on wealth with some SCF data for demographics
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)

options(dplyr.summarise.inform = FALSE)

linear_calibration <- read_rds(here("work", "01-utils", "linear_calibration.rds"))
stat_match <- read_rds(here("work", "01-utils", "stat_match.rds"))
population <- read_rds(here("work", "02-import-population", "population.rds"))
life_table <- read_rds(here("work", "02-import-mortality", "life_table.rds"))
dina_micro <- read_rds(here("work", "02-import-dina", "dina_micro.rds"))
scf_inter <- read_rds(here("work", "02-import-scf", "scf_inter.rds"))

# Use age data from the SCF by preserving the emprical copula between age
# and net wealth. We treat separately singles and married couples to preserve
# the join distribution of the age of couples. We only use the rank in the
# age distribution from the SCF, since we use real demographic data for the
# actual age.

# First, reweight DINA to get the right gender ratios (very small impact):
# we perform the reweighting so that weigths within couples remain the same
pop_sex <- population %>%
    filter(variant == "Past" | variant == "Medium") %>%
    filter(age >= 20) %>%
    group_by(year) %>%
    summarise(pop_female = sum(population*(sex == "female")), pop_male = sum(population*(sex == "male"))) %>%
    ungroup() %>%
    mutate(pop_total = pop_male + pop_female)

dina_micro_calib <- dina_micro %>%
    select(year, id, weight, sex) %>%
    mutate(female = if_else(sex == "female", 1, 0)) %>%
    group_by(year, id) %>%
    summarise(cons = n(), weight = mean(weight), female = sum(female)) %>%
    left_join(pop_sex) %>%
    group_by(year) %>%
    group_split() %>%
    map_dfr(~ {
        d <- .x$weight
        X <- as.matrix(.x[, c("cons", "female")])

        # Calibration totals
        M <- c(.x$pop_total[1], .x$pop_female[1])

        # Perform calibration
        w <- linear_calibration(d, X, M)

        return(tibble(year = .x$year[1], id = .x$id, weight = w))
    })

dina_micro <- dina_micro %>% select(-weight) %>% left_join(dina_micro_calib)

# Then, we need to calibrate the SCF data to ensure the same number of
# couples/singles men/women as in the DINA data. Again we make sure that
# the weight remain the same within couples.
pop_sex_couples <- dina_micro %>%
    group_by(year, sex, couple) %>%
    summarise(pop = sum(weight)) %>%
    ungroup() %>%
    mutate(category = paste("pop", sex, if_else(couple == 1, "couple", "single"), sep = "_")) %>%
    select(year, category, pop) %>%
    pivot_wider(year, names_from = "category", values_from = "pop") %>%
    mutate(pop_couple = pop_female_couple + pop_male_couple)

scf_calib <- scf_inter %>%
    select(year, weight, hid, couple, sex) %>%
    mutate(
        female_single = if_else(sex == "female" & !couple, 1, 0),
        male_single   = if_else(sex == "male"   & !couple, 1, 0)
    ) %>%
    group_by(year, hid) %>%
    summarise(
        weight = mean(weight),
        female_single = sum(female_single),
        male_single = sum(male_single),
        couple = sum(couple)
    ) %>%
    left_join(pop_sex_couples) %>%
    group_by(year) %>%
    mutate(weight = weight*(pop_couple + pop_male_single + pop_female_single)) %>%
    group_by(year) %>%
    group_split() %>%
    map_dfr(~ {
        d <- .x$weight
        X <- as.matrix(.x[, c("female_single", "male_single", "couple")])

        # Calibration totals
        M <- c(.x$pop_female_single[1], .x$pop_male_single[1], .x$pop_couple[1])

        # Perform calibration
        w <- linear_calibration(d, X, M)

        return(tibble(year = .x$year[1], hid = .x$hid, weight = w))
    })

scf_inter <- scf_inter %>% select(-weight) %>% left_join(scf_calib)

# Now that we have consistent data for sex ratios and couples, calculate the
# rank in the age distribution based on the SCF
set.seed(19920902)
scf_age <- scf_inter %>%
    # Random value to break ties randomly
    mutate(rnd = runif(n())) %>%
    select(hid, weight, wealth, age, sex, year, couple, rnd) %>%
    group_by(year, sex) %>%
    arrange(year, sex, age, rnd) %>%
    mutate(rank_age = (cumsum(weight) - weight/2)/sum(weight)) %>%
    ungroup() %>%
    select(-rnd)

scf_age_single_male <- scf_age %>%
    filter(couple == 0 & sex == "male") %>%
    select(year, hid, sex, weight, wealth, rank_age)
scf_age_single_female <- scf_age %>%
    filter(couple == 0 & sex == "female") %>%
    select(year, hid, sex, weight, wealth, rank_age)
scf_age_couple <- scf_age %>%
    filter(couple == 1) %>%
    select(year, hid, weight, wealth, rank_age, sex) %>%
    group_by(year, hid) %>%
    summarise(
        weight = mean(weight),
        wealth = mean(wealth),
        rank_age_male = rank_age[sex == "male"],
        rank_age_female = rank_age[sex == "female"]
    ) %>%
    ungroup()

# Match the couples
dina_micro_couple <- dina_micro %>% filter(couple == 1) %>%
    group_by(year, id) %>%
    summarise(
        wealth      = mean(wealth),
        weight      = mean(weight),
        pre_labor   = mean(pre_labor),
        pre_capital = mean(pre_capital),
        pre_income  = mean(pre_income),
        labor       = mean(labor),
        capital     = mean(capital),
        income      = mean(income),
        dicsh       = mean(dicsh),
        fikgi       = mean(fikgi),
        fiinc       = mean(fiinc),
        fninc       = mean(fninc),
        wealth      = mean(wealth),
        gains       = mean(gains),
        growth      = mean(growth)
    ) %>%
    group_by(year) %>%
    group_split() %>%
    map_dfr(~ {
        df <- stat_match(
            .x,
            scf_age_couple[scf_age_couple$year == .x$year[1], ] %>% select(-c(year, hid)),
            by = "wealth",
            weight = "weight"
        )
        df$id <- 1:nrow(df)
        df$year <- .x$year[1]
        return(df)
    })

# Match the single males
dina_micro_single_male <- dina_micro %>% filter(couple == 0 & sex == "male") %>%
    select(-couple) %>%
    group_by(year) %>%
    group_split() %>%
    map_dfr(~ {
        df <- stat_match(
            .x,
            scf_age_single_male[scf_age_single_male$year == .x$year[1], ] %>% select(-c(year, hid, sex)),
            by = "wealth",
            weight = "weight"
        )
        df$id <- 1:nrow(df)
        df$year <- .x$year[1]
        return(df)
    })

# Match the single females
dina_micro_single_female <- dina_micro %>% filter(couple == 0 & sex == "female") %>%
    select(-couple) %>%
    group_by(year) %>%
    group_split() %>%
    map_dfr(~ {
        df <- stat_match(
            .x,
            scf_age_single_female[scf_age_single_female$year == .x$year[1], ] %>% select(-c(year, hid, sex)),
            by = "wealth",
            weight = "weight"
        )
        df$id <- 1:nrow(df)
        df$year <- .x$year[1]
        return(df)
    })

# Put together couples, single males and females
dina_micro <- bind_rows(
    dina_micro_couple %>% mutate(rank_age = rank_age_male, sex = "male", couple = 1, orig = "couple"),
    dina_micro_couple %>% mutate(rank_age = rank_age_female, sex = "female", couple = 1, orig = "couple"),
    dina_micro_single_male %>% mutate(couple = 0, orig = "single_males"),
    dina_micro_single_female %>% mutate(couple = 0, orig = "single_females")
)
dina_micro <- dina_micro %>%
    filter(weight > 0) %>%
    group_by(year, id, orig) %>%
    mutate(id = cur_group_id()) %>%
    ungroup() %>%
    select(-orig) %>%
    arrange(year, id)

# ---------------------------------------------------------------------------- #
# Give people their age based on demographic data
# ---------------------------------------------------------------------------- #

adult_population <- population %>%
    select(-source) %>%
    filter(variant == "Past" | variant == "Medium") %>%
    filter(age >= 20) %>%
    group_by(year, sex) %>%
    arrange(year, sex, desc(age)) %>%
    mutate(cumfreq = 1 - cumsum(population)/sum(population)) %>%
    left_join(life_table) %>%
    ungroup()

dina_micro <- dina_micro %>% group_by(year, sex) %>% group_split() %>% map_dfr(~ {
    data_sex <- .x$sex[1]
    data_year <- .x$year[1]

    pop <- adult_population %>% filter(sex == data_sex & year == data_year)

    .x <- .x %>% mutate(age = 19 + cut(rank_age,
        breaks = c(pop$cumfreq, 1),
        include.lowest = TRUE,
        labels = FALSE
    ))

    .x <- .x %>% left_join(pop %>% select(age, qx), by = "age")

    return(.x)
})
dina_micro <- dina_micro %>% arrange(year, id, sex)

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

dir.create(here("work", "03-combine-calibrate-microdata"), showWarnings = FALSE)
write_rds(dina_micro, here("work", "03-combine-calibrate-microdata", "dina_micro.rds"))
