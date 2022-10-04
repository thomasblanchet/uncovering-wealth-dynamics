# ---------------------------------------------------------------------------- #
# Combine census data with female fertility rates to get male
# fertility rates
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(progressr)
library(collapse)
library(glue)
library(zoo)
library(furrr)

handlers(handler_progress(
    format = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta"
))
options(dplyr.summarise.inform = FALSE)

census_data <- read_rds(here("work", "02-import-census", "census_data.rds"))
population  <- read_rds(here("work", "02-import-population", "population.rds"))
fertility   <- read_rds(here("work", "02-import-female-fertility", "fertility.rds"))

# Keep weights aside
census_weights <- census_data %>% fgroup_by(year, serial) %>% fselect(year, serial, hhwt) %>% fmean()

# Only keep people with identifiable spouse
census_data <- census_data %>% filter(sploc > 0)

# Identify couples
census_spouses <- census_data %>%
    mutate(member1 = pmin(pernum, sploc), member2 = pmax(pernum, sploc)) %>%
    select(year, serial, member1, member2) %>%
    distinct() %>%
    group_by(year) %>%
    mutate(couple_id = 1:n()) %>%
    ungroup()
census_spouses <- census_spouses %>%
    pivot_longer(-c(year, serial, couple_id), names_to = "member", values_to = "pernum") %>%
    select(-member)
census_spouses <- census_spouses %>% left_join(census_data %>% select(year, serial, pernum, sex, age))

# Only keep mixed-sex couples
census_spouses_mixed_sex <- census_spouses %>%
    mutate(num_male = (sex == 1), num_female = (sex == 2)) %>%
    fgroup_by(year, serial, couple_id) %>%
    fselect(year, serial, couple_id, num_male, num_female) %>%
    fsum() %>%
    filter(num_male == 1 & num_female == 1) %>%
    select(-num_male, -num_female)
census_spouses <- census_spouses %>% inner_join(census_spouses_mixed_sex)

# Census microdata with mixed-sex couples and their age
census_spouses <- census_spouses %>%
    mutate(sex = if_else(sex == 1, "male", "female")) %>%
    pivot_wider(c(year, serial, couple_id), names_from = "sex", values_from = "age") %>%
    # Early population do not provide details above 75, so it is better to
    # assume that no one 75 or above (male and female) has children
    filter(female < 75 & male < 75) %>%
    left_join(census_weights)

# ---------------------------------------------------------------------------- #
# Combine census and population data to get age-specific fertility
# rates for male
# ---------------------------------------------------------------------------- #

fertility_male <- population %>%
    group_by(variant, year, age) %>%
    summarise(
        male = sum(population*(sex == "male")),
        female = sum(population*(sex == "female")),
        total = sum(population)
    ) %>%
    ungroup() %>%
    filter(age >= 12 & age < 75) %>%
    left_join(fertility) %>%
    group_by(variant, year) %>%
    group_split()

plan(sequential)
with_progress({
    set.seed(19920902)
    p <- progressor(along = fertility_male)
    fertility_male <- fertility_male %>% future_map_dfr(~ {
        # Find the census sample corresponding to the current year: when not available,
        # interpolate the data by mixing two census samples
        year <- .x$year[1]
        variant <- .x$variant[1]

        if (year == min(census_spouses$year)) {
            census <- census_spouses[census_spouses$year == min(census_spouses$year), ]
        } else if (year >= max(census_spouses$year)) {
            # After the last census: keep joint age distribution constant
            census <- census_spouses[census_spouses$year == max(census_spouses$year), ]
        } else {
            year_before <- max(census_spouses$year[census_spouses$year < year])
            year_after <- min(census_spouses$year[census_spouses$year >= year])

            lambda <- (year - year_before)/(year_after - year_before)

            census_before <- census_spouses[census_spouses$year == year_before, ]
            census_after <- census_spouses[census_spouses$year == year_after, ]

            census <- bind_rows(
                sample_frac(census_before, size = 1 - lambda),
                sample_frac(census_after, size = lambda)
            )
        }

        # Calculate age pyramid for women in mixed-sex couples
        pyramid_females <- census %>%
            group_by(female) %>%
            summarise(population = sum(hhwt)) %>%
            rename(age = female)

        # Calculate number of birth by female and age
        age_bc <- .x %>% transmute(age,
            bc = female*asfr,
            bc1 = female*asfr1,
            bc2 = female*asfr2,
            bc3 = female*asfr3,
            bc4 = female*asfr4,
            bc5 = female*asfr5
        )

        # Store total birth count (useful for rescaling later in cases where some
        # ages are missing from census data, in which case a discrepancy will appear)
        total_birth <- sum(age_bc$bc)
        total_birth1 <- sum(age_bc$bc1)
        total_birth2 <- sum(age_bc$bc2)
        total_birth3 <- sum(age_bc$bc3)
        total_birth4 <- sum(age_bc$bc4)
        total_birth5 <- sum(age_bc$bc5)

        # Calculate ASFR by women in mixed-sex couples assuming only women in mixed-sex
        # couples have children
        asfr_couples <- age_bc %>%
            left_join(pyramid_females, by = "age") %>%
            mutate(asfr = bc/population) %>%
            mutate(asfr1 = bc1/population) %>%
            mutate(asfr2 = bc2/population) %>%
            mutate(asfr3 = bc3/population) %>%
            mutate(asfr4 = bc4/population) %>%
            mutate(asfr5 = bc5/population) %>%
            rename(female = age) %>%
            select(-c(population, starts_with("bc")))
        asfr_couples[is.na(asfr_couples)] <- 0

        # Apply that ASFR to couples in the census data to get birth count
        # by age of males
        bc_age_male <- census %>%
            inner_join(asfr_couples, by = "female") %>%
            group_by(male) %>%
            summarise(
                bc = sum(hhwt*asfr, na.rm = TRUE),
                bc1 = sum(hhwt*asfr1, na.rm = TRUE),
                bc2 = sum(hhwt*asfr2, na.rm = TRUE),
                bc3 = sum(hhwt*asfr3, na.rm = TRUE),
                bc4 = sum(hhwt*asfr4, na.rm = TRUE),
                bc5 = sum(hhwt*asfr5, na.rm = TRUE)
            ) %>%
            rename(age = male) %>%
            select(age, starts_with("bc")) %>%
            ungroup()

        bc_age_male <- bc_age_male %>% complete(age = 12:74)
        bc_age_male <- bc_age_male %>% arrange(age) %>% mutate(
            bc  = exp(rollapply(log(bc),  width = 5, FUN = mean, align = "center", partial = TRUE, na.rm = TRUE)),
            bc1 = exp(rollapply(log(bc1), width = 5, FUN = mean, align = "center", partial = TRUE, na.rm = TRUE)),
            bc2 = exp(rollapply(log(bc2), width = 5, FUN = mean, align = "center", partial = TRUE, na.rm = TRUE)),
            bc3 = exp(rollapply(log(bc3), width = 5, FUN = mean, align = "center", partial = TRUE, na.rm = TRUE)),
            bc4 = exp(rollapply(log(bc4), width = 5, FUN = mean, align = "center", partial = TRUE, na.rm = TRUE)),
            bc5 = exp(rollapply(log(bc5), width = 5, FUN = mean, align = "center", partial = TRUE, na.rm = TRUE))
        )

        # Normalize the birth count by the total male population to get
        # the final male ASFR
        male_asfr <- bc_age_male %>% right_join(.x %>% select(age, male), by = "age")
        male_asfr[is.na(male_asfr)] <- 0
        male_asfr <- male_asfr %>%
            mutate(bc = bc*total_birth/sum(bc)) %>%
            mutate(bc1 = bc1*total_birth1/sum(bc1)) %>%
            mutate(bc2 = bc2*total_birth2/sum(bc2)) %>%
            mutate(bc3 = bc3*total_birth3/sum(bc3)) %>%
            mutate(bc4 = bc4*total_birth4/sum(bc4)) %>%
            mutate(bc5 = bc5*total_birth5/sum(bc5)) %>%
            mutate(asfr = bc/male) %>%
            mutate(asfr1 = bc1/male) %>%
            mutate(asfr2 = bc2/male) %>%
            mutate(asfr3 = bc3/male) %>%
            mutate(asfr4 = bc4/male) %>%
            mutate(asfr5 = bc5/male) %>%
            select(age, starts_with("bc"), starts_with("asfr")) %>%
            mutate(year = year, variant = variant)

        p()
        return(male_asfr)
    })
})

fertility_both <- bind_rows(
    fertility %>% mutate(sex = "female"),
    fertility_male %>% mutate(sex = "male")
)

dir.create(here("work", "03-estimate-male-fertility"), showWarnings = FALSE)
write_rds(fertility_both, here("work", "03-estimate-male-fertility", "fertility_both.rds"))

# ---------------------------------------------------------------------------- #
# Plot fertility rates
# ---------------------------------------------------------------------------- #

dir.create(here("graphs", "03-estimate-male-fertility"), showWarnings = FALSE)

pdf(file = here("graphs", "03-estimate-male-fertility", "fertility-rates.pdf"), height = 7, width = 7*(1 + sqrt(5))/2)

fertility_both %>%
    select(variant, year, sex, age, starts_with("asfr")) %>%
    select(-asfr) %>%
    pivot_longer(-c(variant, year, sex, age), values_to = "asfr", names_to = "birth_order") %>%
    mutate(birth_order = case_when(
        birth_order == "asfr1" ~ "1",
        birth_order == "asfr2" ~ "2",
        birth_order == "asfr3" ~ "3",
        birth_order == "asfr4" ~ "4",
        birth_order == "asfr5" ~ "5+"
    )) %>%
    group_by(year, variant) %>%
    group_split() %>%
    walk(~ {
        year <- .x$year[1]
        variant <- .x$variant[1]

        cat(glue("---> {year}, {variant}\n", .trim = FALSE))

        p <- .x %>% ggplot(aes(x = age, y = asfr, fill = birth_order)) +
            geom_col(position = position_stack(reverse = TRUE), na.rm = TRUE) +
            facet_grid(. ~ sex) +
            scale_x_continuous(limits = c(12, 75), breaks = seq(10, 70, 10)) +
            scale_y_continuous(limits = c(0, 0.35), breaks = seq(0, 0.35, 0.05)) +
            scale_fill_brewer(name = "birth order", type = "seq", palette = "PuBu") +
            ggtitle(glue("Age-specific fertility rates ({year}, {variant})")) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))

        print(p)
    })

dev.off()

