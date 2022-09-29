# ---------------------------------------------------------------------------- #
# Prepare data to run simulations
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)

options(dplyr.summarise.inform = FALSE)

rectangular    <- read_rds(here("work", "01-utils", "rectangular.rds"))
nreg_drv0_grid <- read_rds(here("work", "01-utils", "nreg_drv0_grid.rds"))
nreg_drv1_grid <- read_rds(here("work", "01-utils", "nreg_drv1_grid.rds"))
nreg_drv0      <- read_rds(here("work", "01-utils", "nreg_drv0.rds"))
nreg_drv1      <- read_rds(here("work", "01-utils", "nreg_drv1.rds"))

population <- read_rds(here("work", "02-import-population", "population.rds"))
life_table <- read_rds(here("work", "02-import-mortality", "life_table.rds"))

dina_micro_children           <- read_rds(here("work", "03-estimate-intergenerational-linkages", "dina_micro_children.rds"))
dina_micro_inheritance_params <- read_rds(here("work", "03-estimate-inheritance-process", "dina_micro_inheritance_params.rds"))

model_micro_data <- read_rds(here("work", "04-prepare-data", "model_micro_data.rds"))
model_macro_data <- read_rds(here("work", "04-prepare-data", "model_macro_data.rds"))
dx               <- read_rds(here("work", "04-prepare-data", "dx.rds"))
grid_cutpoints   <- read_rds(here("work", "04-prepare-data", "grid_cutpoints.rds"))
grid_midpoints   <- read_rds(here("work", "04-prepare-data", "grid_midpoints.rds"))
year_pivot       <- read_rds(here("work", "04-prepare-data", "year_pivot.rds"))

model_distribution_income <- read_rds(here("work", "04-estimate-distribution-income", "model_distribution_income.rds"))
model_distribution_wealth <- read_rds(here("work", "04-estimate-distribution-wealth", "model_distribution_wealth.rds"))

model_all_data <- read_rds(here("work", "05-fit-model", "model_all_data.rds"))
model_params   <- read_rds(here("work", "05-fit-model", "model_params.rds"))

set.seed(19920902)

# ---------------------------------------------------------------------------- #
# Initial draw from wealth distribution
# ---------------------------------------------------------------------------- #

gperc <- c(
    seq(0, 99000, 1000), seq(99100, 99900, 100),
    seq(99910, 99990, 10), seq(99991, 99999, 1)
)
gperc_size <- diff(c(gperc, 1e5))

gperc_unif <- pmap_dfr(list(seq_along(gperc), gperc, gperc_size), ~ {
    return(tibble(
        p = ..2,
        bracket = ..1,
        weight = ..3,
        u = runif(1000, ..2/1e5, (..2 + ..3)/1e5)
    ))
})

# Number of draws within each g-percentile
num_draws <- 1000

# Initial draw from the microdata
data_simul <- model_micro_data %>%
    filter(year == 1962) %>%
    filter(asinh(wealth) >= min(grid_cutpoints)) %>% # Reflective barrier
    arrange(wealth) %>%
    mutate(rank = (cumsum(weight) - weight/2)/sum(weight)) %>%
    mutate(bracket = findInterval(1e5*rank, gperc)) %>%
    group_by(year, bracket) %>%
    # Sample num_draws observations within each g-percentile
    slice_sample(n = num_draws, weight_by = weight, replace = TRUE) %>%
    mutate(weight = gperc_size[bracket], p = gperc[bracket]) %>%
    transmute(year, weight, p, wealth) %>%
    ungroup() %>%
    arrange(wealth)

# ---------------------------------------------------------------------------- #
# Demographic parameters by wealth
# ---------------------------------------------------------------------------- #

# Get death and injection rate
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

# Add age of children to simulate inheritance
model_micro_data <- model_micro_data %>%
    left_join(dina_micro_children %>% select(year, id, sex, starts_with("age_child")))

# Relative probability of receiving inheritance
model_micro_data <- model_micro_data %>%
    left_join(dina_micro_inheritance_params %>% select(year, id, sex, phi, cond_age_inh_rank_wealth))

data_simul_demog <- bind_rows(
    model_micro_data,
    # Create the years 1963 and 1965
    model_micro_data %>% filter(year == 1962 | year == 1964) %>% mutate(weight = weight/2, year = 1963),
    model_micro_data %>% filter(year == 1964 | year == 1966) %>% mutate(weight = weight/2, year = 1965)
)
data_simul_demog <- data_simul_demog %>%
    group_by(year) %>%
    arrange(year, wealth) %>%
    mutate(rank = (cumsum(weight) - weight/2)/sum(weight)) %>%
    mutate(bracket = findInterval(1e5*rank, gperc)) %>%
    group_by(year, bracket) %>%
    # Sample num_draws observations within each g-percentile, using the same scheme
    # as the resampling we will eventually do, so that we can match to the
    # resampled data by rank directly
    slice_sample(n = num_draws, weight_by = weight, replace = TRUE) %>%
    # Note: minor issue, no data for p = 99.9995% in 1985, so sample twice as much
    # from p = 99.9994%
    uncount(if_else(year == 1985 & gperc[bracket] == 99994, 2, 1), .id = "id") %>%
    mutate(bracket = if_else(id == 2, as.integer(bracket + 1), bracket)) %>%
    select(-id) %>%
    mutate(weight = gperc_size[bracket], p = gperc[bracket]) %>%
    ungroup() %>%
    arrange(year, wealth) %>%
    select(year, sex, age, couple, qx, age_spouse, qx_spouse, phi, cond_age_inh_rank_wealth, starts_with("age_child"))

if (any(data_simul_demog %>% group_by(year) %>% summarise(n = n()) %>% pull(n) != nrow(data_simul))) {
    stop("not the right number of observations")
}

# ---------------------------------------------------------------------------- #
# Income parameters by wealth g-percentile
# ---------------------------------------------------------------------------- #

simul_param_income <- model_distribution_income %>%
    mutate(density = replace_na(density, 0)) %>%
    group_by(year) %>%
    arrange(year, wealth) %>%
    mutate(cdf = dx*(cumsum(density) - density/2))

# Smooth income variance parameters and rescale them to the sd_income
# that was used to fit the model
simul_param_income <- simul_param_income %>% mutate(across(
    c(starts_with("var_"), starts_with("cov_")),
    ~ nreg_drv0(asinh_wealth, .x, bw = 1)$y
))
simul_param_income <- simul_param_income %>%
    mutate(var_income = sd_income_smooth^2) %>%
    mutate(var_income_recalc = var_labor + var_capital + var_gains +
            2*cov_labor_capital + 2*cov_labor_gains + 2*cov_capital_gains) %>%
    mutate(across(
        c(starts_with("var_"), starts_with("cov_")),
        ~ .x/var_income_recalc*var_income
    )) %>%
    select(-var_income_recalc, -var_income, -sd_income_smooth, sd_income)

simul_param_income <- simul_param_income %>% filter(include) %>% group_split() %>% map_dfr(~ {
    return(tibble(
        year = first(.x$year),
        p = gperc,
        wealth = suppressWarnings(approx(x = .x$cdf, y = .x$wealth, xout = gperc/1e5, rule = 2)$y),

        mean_labor   = suppressWarnings(approx(x = .x$cdf, y = .x$mean_labor_smooth, xout = gperc/1e5, rule = 2)$y),
        mean_capital = suppressWarnings(approx(x = .x$cdf, y = .x$mean_capital_smooth, xout = gperc/1e5, rule = 2)$y),
        mean_gains   = suppressWarnings(approx(x = .x$cdf, y = .x$mean_gains_smooth, xout = gperc/1e5, rule = 2)$y),

        mean_pre_labor   = suppressWarnings(approx(x = .x$cdf, y = .x$mean_pre_labor_smooth, xout = gperc/1e5, rule = 2)$y),
        mean_pre_capital = suppressWarnings(approx(x = .x$cdf, y = .x$mean_pre_capital_smooth, xout = gperc/1e5, rule = 2)$y),

        var_labor   = suppressWarnings(approx(x = .x$cdf, y = .x$var_labor, xout = gperc/1e5, rule = 2)$y),
        var_capital = suppressWarnings(approx(x = .x$cdf, y = .x$var_capital, xout = gperc/1e5, rule = 2)$y),
        var_gains   = suppressWarnings(approx(x = .x$cdf, y = .x$var_gains, xout = gperc/1e5, rule = 2)$y),

        cov_labor_capital = suppressWarnings(approx(x = .x$cdf, y = .x$cov_labor_capital, xout = gperc/1e5, rule = 2)$y),
        cov_labor_gains   = suppressWarnings(approx(x = .x$cdf, y = .x$cov_labor_gains, xout = gperc/1e5, rule = 2)$y),
        cov_capital_gains = suppressWarnings(approx(x = .x$cdf, y = .x$cov_capital_gains, xout = gperc/1e5, rule = 2)$y)
    ))
})

# Remove the dependency on wealth for labor
simul_param_income <- simul_param_income %>%
    mutate(mean_labor = mean_labor*sqrt(1 + wealth^2)) %>%
    mutate(mean_pre_labor = mean_pre_labor*sqrt(1 + wealth^2)) %>%
    mutate(var_labor = var_labor*(1 + wealth^2)) %>%
    mutate(cov_labor_capital = cov_labor_capital*sqrt(1 + wealth^2)) %>%
    mutate(cov_labor_gains = cov_labor_gains*sqrt(1 + wealth^2)) %>%
    select(-wealth)

# Add years 1963 & 1965
simul_param_income <- simul_param_income %>%
    complete(p, year = full_seq(year, 1)) %>%
    group_by(p) %>%
    mutate(across(
        c(starts_with("mean_"), starts_with("var_"), starts_with("cov_")),
        ~ approx(x = year, y = .x, xout = year)$y
    )) %>%
    group_by(year) %>%
    arrange(year, p) %>%
    ungroup()

if (anyNA(simul_param_income)) {
    stop("NAs in simul_param_income")
}

# ---------------------------------------------------------------------------- #
# Macroeconomic data
# ---------------------------------------------------------------------------- #

growth_rates <- model_macro_data %>%
    select(year, growth_rate) %>%
    mutate(period = (year > year_pivot)) %>%
    group_by(period) %>%
    mutate(growth_rate = nreg_drv0(year, growth_rate, bw = 12.5)$y) %>%
    filter(year >= 1962)

# Retrieve national income of the year to be able to compute the level of
# inheritance taxation
avg_national_income <- model_macro_data %>% transmute(year, avg = national_income/adult_population)

# ---------------------------------------------------------------------------- #
# Drift and diffusion
# ---------------------------------------------------------------------------- #

# Drift/diffusion parameters of consumption
model_params <- model_params %>% transmute(
    asinh_wealth,
    diffu = diffu_star_smooth,
    diffu_deriv = diffu_star_deriv,
    drift1 = drift_corr_per1,
    drift2 = drift_corr_per2
)

model_effects <- model_all_data %>%
    left_join(model_params %>% transmute(wealth_bin = round(10*asinh_wealth), diffu, drift1, drift2, diffu_deriv)) %>%
    left_join(model_distribution_wealth %>% transmute(wealth_bin = round(10*asinh_wealth), deriv_log_density)) %>%
    mutate(drift1_total_actual = ito_adj_income + mean_income + drift1) %>%
    mutate(drift2_total_actual = ito_adj_income + mean_income + drift2) %>%
    mutate(diffu_total_actual = diffu + 0.5*sd_income^2) %>%
    mutate(diffu_deriv_total_actual = diffu_deriv + sd_income*sd_income_deriv) %>%
    complete(wealth_bin, year = full_seq(year, 1)) %>%
    group_by(wealth_bin) %>%
    arrange(wealth_bin, year) %>%
    mutate(across(
        -c(year, period, asinh_wealth),
        ~ suppressWarnings(approx(x = year, y = .x, xout = year)$y)
    )) %>%
    ungroup()

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

dir.create(here("work", "07-prepare-simulation"), showWarnings = FALSE)

write_rds(data_simul,          here("work", "07-prepare-simulation", "data_simul.rds"))
write_rds(data_simul_demog,    here("work", "07-prepare-simulation", "data_simul_demog.rds"))
write_rds(simul_param_income,  here("work", "07-prepare-simulation", "simul_param_income.rds"))
write_rds(growth_rates,        here("work", "07-prepare-simulation", "growth_rates.rds"))
write_rds(model_params,        here("work", "07-prepare-simulation", "model_params.rds"))
write_rds(model_effects,       here("work", "07-prepare-simulation", "model_effects.rds"))
write_rds(avg_national_income, here("work", "07-prepare-simulation", "avg_national_income.rds"))
write_rds(pop_growth,          here("work", "07-prepare-simulation", "pop_growth.rds"))
write_rds(pop_death,           here("work", "07-prepare-simulation", "pop_death.rds"))
write_rds(gperc,               here("work", "07-prepare-simulation", "gperc.rds"))
write_rds(gperc_size,          here("work", "07-prepare-simulation", "gperc_size.rds"))
write_rds(gperc_unif,          here("work", "07-prepare-simulation", "gperc_unif.rds"))
write_rds(num_draws,           here("work", "07-prepare-simulation", "num_draws.rds"))
