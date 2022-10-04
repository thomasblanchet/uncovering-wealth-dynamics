# ---------------------------------------------------------------------------- #
# Prepare data for fitting the model
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)

rectangular           <- read_rds(here("work", "01-utils", "rectangular.rds"))
nreg_drv0_grid        <- read_rds(here("work", "01-utils", "nreg_drv0_grid.rds"))
nreg_drv0             <- read_rds(here("work", "01-utils", "nreg_drv0.rds"))
population            <- read_rds(here("work", "02-import-population", "population.rds"))
life_table            <- read_rds(here("work", "02-import-mortality", "life_table.rds"))
marriage_divorce_rate <- read_rds(here("work", "03-estimate-marriage-rates", "marriage_divorce_rate.rds"))

model_distribution_birth       <- read_rds(here("work", "04-estimate-distribution-birth", "model_distribution_birth.rds"))
model_distribution_death       <- read_rds(here("work", "04-estimate-distribution-death", "model_distribution_death.rds"))
model_distribution_divorce     <- read_rds(here("work", "04-estimate-distribution-marriage", "model_distribution_divorce.rds"))
model_distribution_marriage    <- read_rds(here("work", "04-estimate-distribution-marriage", "model_distribution_marriage.rds"))
model_distribution_income      <- read_rds(here("work", "04-estimate-distribution-income", "model_distribution_income.rds"))
model_distribution_inheritance <- read_rds(here("work", "04-estimate-distribution-inheritance", "model_distribution_inheritance.rds"))
model_distribution_wealth      <- read_rds(here("work", "04-estimate-distribution-wealth", "model_distribution_wealth.rds"))

inheritance_rate <- read_rds(here("work", "04-estimate-distribution-inheritance", "inheritance_rate.rds"))
year_pivot       <- read_rds(here("work", "04-prepare-data", "year_pivot.rds"))

# ---------------------------------------------------------------------------- #
# Demographic parameters
# ---------------------------------------------------------------------------- #

pop_growth <- population %>%
    filter(variant == "Past" | variant == "Medium") %>%
    group_by(year) %>%
    summarise(population = sum(population)) %>%
    ungroup() %>%
    complete(year = full_seq(year, 1)) %>%
    arrange(year) %>%
    transmute(year, pop_growth = log(population) - log(lag(population)))

pop_death <- life_table %>%
    filter(variant == "Past" | variant == "Medium") %>%
    select(year, age, sex, variant, qx) %>%
    left_join(population %>% select(year, age, sex, variant, population)) %>%
    group_by(year, variant) %>%
    summarise(death_rate = weighted.mean(qx, population))

marriage_divorce_rate <- marriage_divorce_rate %>%
    left_join(population %>%
        filter(variant == "Past" | variant == "Medium") %>%
        select(year, age, sex, population)
    ) %>%
    group_by(year) %>%
    summarise(
        marriage_rate = weighted.mean(marriage_rate, population),
        divorce_rate = weighted.mean(divorce_rate, population)
    )

# ---------------------------------------------------------------------------- #
# Round create ID in datasets to join them on asinh_wealth
# ---------------------------------------------------------------------------- #

model_distribution_birth       <- model_distribution_birth       %>% mutate(wealth_bin = round(10*asinh_wealth))
model_distribution_death       <- model_distribution_death       %>% mutate(wealth_bin = round(10*asinh_wealth))
model_distribution_divorce     <- model_distribution_divorce     %>% mutate(wealth_bin = round(10*asinh_wealth))
model_distribution_marriage    <- model_distribution_marriage    %>% mutate(wealth_bin = round(10*asinh_wealth))
model_distribution_income      <- model_distribution_income      %>% mutate(wealth_bin = round(10*asinh_wealth))
model_distribution_inheritance <- model_distribution_inheritance %>% mutate(wealth_bin = round(10*asinh_wealth))
model_distribution_wealth      <- model_distribution_wealth      %>% mutate(wealth_bin = round(10*asinh_wealth))

prepare_data_model <- function(bw_effects, bw_sd_delta) {
    # ------------------------------------------------------------------------ #
    # Join all the distribution data in a single object
    # ------------------------------------------------------------------------ #

    model_all_data <- model_distribution_wealth %>%
        # Wealth distribution
        ungroup() %>%
        filter(include) %>%
        select(year, asinh_wealth, wealth, wealth_bin, deriv_log_density, lhs_deriv, cdf, density) %>%
        # Add income effects
        left_join(model_distribution_income %>% select(
            year, wealth_bin,
            mean_income = mean_income_smooth,
            sd_income = sd_income_smooth,
            sd_income_deriv = sd_income_deriv
        )) %>%
        # Add birth effect
        left_join(model_distribution_birth %>% select(wealth_bin, cdf_birth)) %>%
        # Add death effect
        left_join(model_distribution_death %>% select(year, wealth_bin, cdf_death)) %>%
        # Add demographic parameters for birth/death effects
        left_join(pop_growth) %>%
        left_join(pop_death) %>%
        # Add inheritance effect
        left_join(model_distribution_inheritance %>% select(
            year, wealth_bin,
            cdf_inheritance_before = cdf_before,
            cdf_inheritance_after = cdf_after
        )) %>%
        left_join(inheritance_rate) %>%
        # Add marriage/divorce effect
        left_join(model_distribution_marriage %>% select(year, wealth_bin,
            cdf_marriage_before = cdf_before,
            cdf_marriage_after = cdf_after
        )) %>%
        left_join(model_distribution_divorce %>% select(year, wealth_bin,
            cdf_divorce_before = cdf_before,
            cdf_divorce_after = cdf_after
        )) %>%
        # Add macro divorce and marriage rates
        left_join(marriage_divorce_rate)

    if (anyNA(model_all_data)) {
        stop("data contains missing values")
    }

    # ------------------------------------------------------------------------ #
    # Calculate the various effects
    # ------------------------------------------------------------------------ #

    model_all_data <- model_all_data %>% mutate(
        # Define birth rate as population + death rate (i.e. include migration in birth rate)
        birth_rate = pop_growth + death_rate,

        # Direct income-induced mobility
        diffu_income = -0.5*sd_income^2*deriv_log_density,

        # Effect of the derivative of income-induced diffusion
        deriv_adj_income = -sd_income*sd_income_deriv,

        # Ito adjustment for income because we use an asinh transform
        ito_adj_income = -0.5*sd_income^2*wealth/sqrt(1 + wealth^2),

        # Auxiliary processes effects
        birth_effect = -birth_rate*cdf_birth/density,
        death_effect = death_rate*cdf_death/density,
        pop_growth_effect = pop_growth*cdf/density,
        demographic_effect = birth_effect + death_effect + pop_growth_effect,
        inheritance_effect = -inheritance_rate*(cdf_inheritance_after - cdf_inheritance_before)/density,
        marriage_effect = -marriage_rate*(cdf_marriage_after - cdf_marriage_before)/density,
        divorce_effect = -divorce_rate*(cdf_divorce_after - cdf_divorce_before)/density
    )

    # Smooth some of the effects over time
    model_all_data <- model_all_data %>%
        mutate(period = if_else(year < year_pivot, 0, 1)) %>%
        group_by(period, asinh_wealth) %>%
        mutate(across(
            c(demographic_effect, birth_effect, death_effect, pop_growth_effect,
                inheritance_effect, marriage_effect, divorce_effect,
                deriv_adj_income, ito_adj_income, diffu_income),
            ~ nreg_drv0(year, ., bw = bw_effects)$y
        ))

    # Make adjustment to  LHS
    model_all_data <- model_all_data %>% mutate(
        lhs = lhs_deriv -
            demographic_effect -
            inheritance_effect -
            marriage_effect -
            divorce_effect -
            deriv_adj_income -
            ito_adj_income -
            diffu_income -
            mean_income
    )

    # Clean
    model_all_data <- model_all_data %>%
        transmute(wealth_bin, year, period, asinh_wealth,
            x = deriv_log_density, y = lhs,
            birth_rate, death_rate, pop_growth,
            lhs_deriv, demographic_effect, birth_effect, death_effect, pop_growth_effect,
            inheritance_effect, marriage_effect, divorce_effect, deriv_adj_income,
            ito_adj_income, diffu_income, mean_income, sd_income, sd_income_deriv
        ) %>%
        group_by(wealth_bin) %>%
        arrange(wealth_bin, year) %>%
        mutate(mean_x = nreg_drv0(year, x, bw = bw_sd_delta)$y) %>%
        mutate(mean_y = nreg_drv0(year, y, bw = bw_sd_delta)$y) %>%
        mutate(sd_x = sqrt(mean((x - mean_x)^2))) %>%
        mutate(sd_y = sqrt(mean((y - mean_y)^2))) %>%
        select(-mean_x, -mean_y) %>%
        ungroup()

    return(model_all_data)
}

model_all_data <- prepare_data_model(10, 5)

# ---------------------------------------------------------------------------- #
# Save results
# ---------------------------------------------------------------------------- #

dir.create(here("work", "05-prepare-data-model"), showWarnings = FALSE)
write_rds(model_all_data,     here("work", "05-prepare-data-model", "model_all_data.rds"))
write_rds(prepare_data_model, here("work", "05-prepare-data-model", "prepare_data_model.rds"))
write_rds(pop_growth,         here("work", "05-prepare-data-model", "pop_growth.rds"))
write_rds(pop_death,          here("work", "05-prepare-data-model", "pop_death.rds"))
