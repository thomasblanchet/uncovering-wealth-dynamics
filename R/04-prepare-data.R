# ---------------------------------------------------------------------------- #
# Prepare the data for estimation (rescaling, asinh scale)
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)

options(dplyr.summarise.inform = FALSE)

dina_macro <- read_rds(here("work", "02-import-dina", "dina_macro.rds"))
dina_micro <- read_rds(here("work", "03-combine-calibrate-microdata", "dina_micro.rds"))
dina_micro_inheritance <- read_rds(here("work", "03-estimate-inheritance-process", "dina_micro_inheritance.rds"))
dina_micro_marriage <- read_rds(here("work", "03-estimate-marriage-process", "dina_micro_marriage.rds"))

# ---------------------------------------------------------------------------- #
# Define grid
# ---------------------------------------------------------------------------- #

dx <- 0.1
grid_cutpoints <- seq(
    from = ceiling(10*asinh(-1))/10 - dx/2,
    to = ceiling(max(asinh(dina_micro$wealth))) + dx/2,
    by = dx
)
grid_midpoints <- seq(
    from = ceiling(10*asinh(-1))/10,
    to = ceiling(max(asinh(dina_micro$wealth))),
    by = dx
)

# Identify the year where the trend reversal usually happen
year_pivot <- 1978

# ---------------------------------------------------------------------------- #
# Calculate average national income and growth rates
# ---------------------------------------------------------------------------- #

model_macro_data <- dina_macro %>% arrange(year) %>% mutate(
    current_avg_income  = national_income/adult_population,
    constant_avg_income = current_avg_income/deflator,
    # The growth rate to consider is the real one because we used real capital gains,
    # so inflation is already removed
    growth_rate = (constant_avg_income - lag(constant_avg_income))/constant_avg_income,
    inflation = (deflator - lag(deflator))/deflator
)
# ---------------------------------------------------------------------------- #
# Normalize the microdata by average national income
# ---------------------------------------------------------------------------- #

data_qx <- dina_micro %>%
    select(year, id, sex, age, qx) %>%
    pivot_wider(c(year, id), names_from = sex, values_from = c(qx, age))
dina_micro <- dina_micro %>% left_join(data_qx, by = c("year", "id")) %>%
    mutate(qx_spouse = if_else(sex == "female", qx_male, qx_female)) %>%
    mutate(age_spouse = if_else(sex == "female", age_male, age_female)) %>%
    select(-qx_male, -qx_female, -age_male, -age_female)

model_micro_data <- dina_micro %>%
    select(id, year, weight, age, sex, couple, qx, age_spouse, qx_spouse, labor, capital, income, gains, wealth, pre_labor, pre_capital) %>%
    left_join(model_macro_data %>% select(year, current_avg_income, growth_rate, inflation)) %>%
    mutate(
        labor   = labor/current_avg_income,
        capital = capital/current_avg_income,
        income  = income/current_avg_income,
        gains   = gains/current_avg_income,
        wealth  = wealth/current_avg_income,

        pre_labor   = pre_labor/current_avg_income,
        pre_capital = pre_capital/current_avg_income
    ) %>%
    # Include capital gains in income
    mutate(income = income + gains) %>%
    filter(!is.na(income)) %>%
    select(-current_avg_income)

model_micro_inheritance <- dina_micro_inheritance %>%
    left_join(model_macro_data %>% select(year, current_avg_income)) %>%
    mutate(
        wealth              = wealth/current_avg_income,
        transfer_parent     = transfer_parent/current_avg_income,
        transfer_spouse     = transfer_spouse/current_avg_income,
        transfer_estate_tax = transfer_estate_tax/current_avg_income
    ) %>%
    select(-current_avg_income)

model_micro_marriage <- dina_micro_marriage %>%
    select(year, weight, is_getting_married, is_getting_divorced, wealth,
        wealth_marriage, wealth_divorce, wealth_new) %>%
    left_join(model_macro_data %>% select(year, current_avg_income)) %>%
    mutate(
        wealth          = wealth/current_avg_income,
        wealth_marriage = wealth_marriage/current_avg_income,
        wealth_divorce  = wealth_divorce/current_avg_income
    ) %>%
    select(-current_avg_income)

# ---------------------------------------------------------------------------- #
# Adapt the data to asinh transform used for the model
# ---------------------------------------------------------------------------- #

model_micro_data <- model_micro_data %>% mutate(
    asinh_wealth = asinh(wealth),
    # Income "pseudo-ratios" to be used in the SDE
    ratio_income  = income/sqrt(1 + wealth^2),
    ratio_labor   = labor/sqrt(1 + wealth^2),
    ratio_gains   = gains/sqrt(1 + wealth^2),
    ratio_capital = capital/sqrt(1 + wealth^2),
    # For pretax incomes
    ratio_pre_labor = pre_labor/sqrt(1 + wealth^2),
    ratio_pre_capital = pre_capital/sqrt(1 + wealth^2),
    # Effect of growth
    growth_effect   = -growth_rate*wealth/sqrt(1 + wealth^2),
    inflation_effect = -inflation*wealth/sqrt(1 + wealth^2)
)

model_micro_inheritance <- model_micro_inheritance %>% mutate(
    asinh_wealth = asinh(wealth)
)

model_micro_marriage <- model_micro_marriage %>% mutate(
    asinh_wealth          = asinh(wealth),
    asinh_wealth_marriage = asinh(wealth_marriage),
    asinh_wealth_divorce  = asinh(wealth_divorce)
)

# ---------------------------------------------------------------------------- #
# Remove values below the reflecting barrier and redistribute weight
# uniformly
# ---------------------------------------------------------------------------- #

model_micro_data <- model_micro_data %>%
    group_by(year) %>%
    mutate(sumwgt = sum(weight)) %>%
    filter(asinh_wealth >= min(grid_cutpoints)) %>%
    mutate(weight = weight/sum(weight)*sumwgt) %>%
    select(-sumwgt) %>%
    ungroup()

model_micro_inheritance <- model_micro_inheritance %>%
    group_by(year) %>%
    mutate(sumwgt = sum(weight)) %>%
    filter(asinh_wealth >= min(grid_cutpoints)) %>%
    mutate(weight = weight/sum(weight)*sumwgt) %>%
    select(-sumwgt) %>%
    ungroup()

model_micro_marriage <- model_micro_marriage %>%
    group_by(year) %>%
    mutate(sumwgt = sum(weight)) %>%
    filter(asinh_wealth >= min(grid_cutpoints)) %>%
    mutate(weight = weight/sum(weight)*sumwgt) %>%
    select(-sumwgt) %>%
    ungroup()

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

dir.create(here("work", "04-prepare-data"), showWarnings = FALSE)

write_rds(model_macro_data, here("work", "04-prepare-data", "model_macro_data.rds"))
write_rds(model_micro_data, here("work", "04-prepare-data", "model_micro_data.rds"))
write_rds(model_micro_inheritance, here("work", "04-prepare-data", "model_micro_inheritance.rds"))
write_rds(model_micro_marriage, here("work", "04-prepare-data", "model_micro_marriage.rds"))

write_rds(dx, here("work", "04-prepare-data", "dx.rds"))
write_rds(grid_cutpoints, here("work", "04-prepare-data", "grid_cutpoints.rds"))
write_rds(grid_midpoints, here("work", "04-prepare-data", "grid_midpoints.rds"))
write_rds(year_pivot, here("work", "04-prepare-data", "year_pivot.rds"))
