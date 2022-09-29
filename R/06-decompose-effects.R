# ---------------------------------------------------------------------------- #
# Decompose the role of the different effects
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(xtable)
library(scales)

dina_micro                <- read_rds(here("work", "02-import-dina", "dina_micro.rds"))
dina_macro                <- read_rds(here("work", "02-import-dina", "dina_macro.rds"))
model_micro_data          <- read_rds(here("work", "04-prepare-data", "model_micro_data.rds"))
model_macro_data          <- read_rds(here("work", "04-prepare-data", "model_macro_data.rds"))
year_pivot                <- read_rds(here("work", "04-prepare-data", "year_pivot.rds"))
model_distribution_wealth <- read_rds(here("work", "04-estimate-distribution-wealth", "model_distribution_wealth.rds"))
model_all_data            <- read_rds(here("work", "05-fit-model", "model_all_data.rds"))

dir.create(here("graphs", "06-decompose-effects"), showWarnings = FALSE)

# ---------------------------------------------------------------------------- #
# Growth of the top 1% in the DINA data
# ---------------------------------------------------------------------------- #

gperc <- c(
    seq(0, 99000, 1000), seq(99100, 99900, 100),
    seq(99910, 99990, 10), seq(99991, 99999, 1)
)
gperc_size <- diff(c(gperc, 1e5))

dina_top1 <- dina_micro %>%
    group_by(year) %>%
    arrange(year, wealth) %>%
    mutate(rank = (cumsum(weight) - weight/2)/sum(weight)) %>%
    filter(rank >= 0.99) %>%
    summarise(wealth = weighted.mean(wealth, weight)) %>%
    left_join(dina_macro %>% select(year, deflator)) %>%
    mutate(wealth = wealth/deflator) %>%
    arrange(year) %>%
    mutate(growth = wealth/lag(wealth) - 1)

growth1962_1978 <- mean(dina_top1$growth[dina_top1$year <= 1978], na.rm = TRUE)
growth1979_2019 <- mean(dina_top1$growth[dina_top1$year >= 1979], na.rm = TRUE)

# ---------------------------------------------------------------------------- #
# Decomposition of the effects
# ---------------------------------------------------------------------------- #

effects_summary <- model_micro_data %>%
    mutate(wealth_bin = round(asinh_wealth*10)) %>%
    inner_join(model_all_data %>% select(-asinh_wealth)) %>%
    left_join(model_distribution_wealth %>% transmute(year, wealth_bin = round(asinh_wealth*10), deriv_log_density)) %>%
    group_by(year) %>%
    mutate(conso = if_else(year <= year_pivot, drift_corr_per1, drift_corr_per2)) %>%
    # Ito adjustments
    mutate(conso = conso + diffu_star_smooth*wealth/sqrt(1 + wealth^2)) %>%
    mutate(mean_income = mean_income - ito_adj_income) %>%
    arrange(year, asinh_wealth) %>%
    mutate(rank = (cumsum(weight) - weight/2)/sum(weight)) %>%
    filter(rank >= 0.99) %>%
    summarise(across(
        c(lhs_deriv, demographic_effect, birth_effect, death_effect, pop_growth_effect,
            birth_rate, death_rate, pop_growth, conso, ratio_labor, ratio_capital, ratio_gains,
            inheritance_effect, marriage_effect, divorce_effect, deriv_adj_income,
            ito_adj_income, diffu_income, mean_income, sd_income, sd_income_deriv,
            diffu_star, diffu_star_smooth, diffu_star_deriv, deriv_log_density),
        ~ weighted.mean(.x, weight*wealth)
    )) %>%
    left_join(model_macro_data %>% select(year, growth_rate))

effects_table <- effects_summary %>%
    mutate(period = if_else(year <= 1978, "1962-1978", "1979-2019")) %>%
    group_by(period) %>%
    summarise(
        demographic_effect = mean(demographic_effect),
        inheritance_effect = mean(inheritance_effect),

        marriage_effect = mean(marriage_effect + divorce_effect),

        drift = mean(mean_income + conso),
        drift_labor = mean(ratio_labor)*mean(mean_income)/(mean(ratio_labor + ratio_capital + ratio_gains)),
        drift_capital = mean(ratio_capital)*mean(mean_income)/(mean(ratio_labor + ratio_capital + ratio_gains)),
        drift_gains = mean(ratio_gains)*mean(mean_income)/(mean(ratio_labor + ratio_capital + ratio_gains)),
        drift_conso = mean(conso),

        mobility = -mean(-diffu_income + diffu_star_smooth*deriv_log_density),
        mobility_income = mean(diffu_income),
        mobility_conso = -mean(diffu_star_smooth*deriv_log_density),

        wealth_growth = mean(lhs_deriv + growth_rate)
    ) %>%
    # Adjust values to match growth
    mutate(wealth_growth = if_else(period == "1962-1978", growth1962_1978, growth1979_2019)) %>%
    mutate(tot = demographic_effect + inheritance_effect + marriage_effect + drift + mobility) %>%
    mutate(totabs = abs(demographic_effect) + abs(inheritance_effect) + abs(marriage_effect) +
            abs(drift_labor) + abs(drift_capital) + abs(drift_gains) + abs(drift_conso) +
            abs(mobility_income) + abs(mobility_conso)) %>%
    mutate(across(c(demographic_effect, inheritance_effect, marriage_effect, drift_labor, drift_capital,
        drift_gains, drift_conso, mobility_income, mobility_conso), ~ .x + (wealth_growth - tot)*abs(.x)/totabs)) %>%
    mutate(drift = drift_labor + drift_capital + drift_gains + drift_conso, mobility = mobility_income + mobility_conso) %>%
    select(-tot, -totabs) %>%
    # Put table in proper shape
    pivot_longer(-period) %>%
    pivot_wider(name, values_from = value, names_from = period) %>%
    mutate(delta = `1979-2019` - `1962-1978`)

colnames(effects_table) <- c("Effect", "1962--1978", "1979--2019", "Difference")
effects_table$Effect <- c(
    "Demography",
    "Inheritance",
    "Marriages \\& Divorces",
    "Drift",
    "\\quad \\textit{incl. Labor income}",
    "\\quad \\textit{incl. Capital income}",
    "\\quad \\textit{incl. Capital gains}",
    "\\quad \\textit{incl. Consumption}",
    "Mobility",
    "\\quad \\textit{incl. Income}",
    "\\quad \\textit{incl. Consumption}",
    "Equals: Average annual growth"
)
effects_table <- effects_table %>% mutate(`Difference` = paste0(
    if_else(`Difference` >= 0, "+", ""),
    case_when(
        Effect %in% c("Marriages \\& Divorces")
            ~ percent_format(accuracy = 0.01, prefix = "$", suffix = "%$")(`Difference`),
        Effect %in% c("\\quad \\textit{incl. Labor income}")
            ~ percent_format(accuracy = 0.001, prefix = "$", suffix = "%$")(`Difference`),
        TRUE
            ~ percent_format(accuracy = 0.1, prefix = "$", suffix = "%$")(`Difference`)
    )
))
effects_table <- effects_table %>% mutate(across(
    c(`1962--1978`, `1979--2019`),
    ~ if_else(
        Effect == "Marriages \\& Divorces",
        percent_format(accuracy = 0.01, prefix = "$", suffix = "%$")(.x),
        percent_format(accuracy = 0.1, prefix = "$", suffix = "%$")(.x),
    )
))

sink(here("graphs", "06-decompose-effects", "decomposition.tex"))
print(xtable(effects_table, align = "llrrr"),
    booktabs = TRUE,
    sanitize.text.function = function(x) str_replace(x, coll("%"), "\\%"),
    include.rownames = FALSE,
    hline.after = c(-1, 0, nrow(effects_table) - 1, nrow(effects_table)),
    add.to.row = list(pos = list(1, 2, 3, 8), command = rep("[0.5em]", 4)),

)
sink()
