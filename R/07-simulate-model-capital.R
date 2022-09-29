# ---------------------------------------------------------------------------- #
# Simulate the model
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(glue)
library(VineCopula)
library(scales)
library(ggtext)

options(dplyr.summarise.inform = FALSE)

dir.create(here("graphs", "07-simulate-model-capital"), showWarnings = FALSE)
dir.create(here("work", "07-simulate-model-capital"), showWarnings = FALSE)

set.seed(19920902)

stat_match     <- read_rds(here("work", "01-utils", "stat_match.rds"))
top_shares     <- read_rds(here("work", "01-utils", "top_shares.rds"))
rectangular    <- read_rds(here("work", "01-utils", "rectangular.rds"))
nreg_drv0_grid <- read_rds(here("work", "01-utils", "nreg_drv0_grid.rds"))
nreg_drv1_grid <- read_rds(here("work", "01-utils", "nreg_drv1_grid.rds"))
nreg_drv0      <- read_rds(here("work", "01-utils", "nreg_drv0.rds"))
nreg_drv1      <- read_rds(here("work", "01-utils", "nreg_drv1.rds"))

tax_schedule_summary <- read_rds(here("work", "02-import-estate-tax-schedule", "tax_schedule_summary.rds"))
tax_schedule_details <- read_rds(here("work", "02-import-estate-tax-schedule", "tax_schedule_details.rds"))
estate_tax           <- read_rds(here("work", "02-import-estate-tax-schedule", "estate_tax.rds"))

marriage_divorce_rate <- read_rds(here("work", "03-estimate-marriage-rates", "marriage_divorce_rate.rds"))
copula_inheritance    <- read_rds(here("work", "03-estimate-inheritance-process", "copula_inheritance.rds"))

copula_marriage     <- read_rds(here("work", "03-estimate-marriage-process", "copula_marriage.rds"))
spouse_wealth_share <- read_rds(here("work", "03-estimate-marriage-process", "spouse_wealth_share.rds"))

model_micro_birth <- read_rds(here("work", "04-estimate-distribution-birth", "model_micro_birth.rds"))

dx             <- read_rds(here("work", "04-prepare-data", "dx.rds"))
grid_cutpoints <- read_rds(here("work", "04-prepare-data", "grid_cutpoints.rds"))
grid_midpoints <- read_rds(here("work", "04-prepare-data", "grid_midpoints.rds"))
year_pivot     <- read_rds(here("work", "04-prepare-data", "year_pivot.rds"))

model_all_data <- read_rds(here("work", "05-fit-model", "model_all_data.rds"))

simul_savings            <- read_rds(here("work", "07-simulation-functions", "simul_savings.rds"))
simul_marriages_divorces <- read_rds(here("work", "07-simulation-functions", "simul_marriages_divorces.rds"))
simul_inheritance        <- read_rds(here("work", "07-simulation-functions", "simul_inheritance.rds"))
simul_birth_death        <- read_rds(here("work", "07-simulation-functions", "simul_birth_death.rds"))

model_params        <- read_rds(here("work", "07-prepare-simulation", "model_params.rds"))
data_simul          <- read_rds(here("work", "07-prepare-simulation", "data_simul.rds"))
data_simul_demog    <- read_rds(here("work", "07-prepare-simulation", "data_simul_demog.rds"))
simul_param_income  <- read_rds(here("work", "07-prepare-simulation", "simul_param_income.rds"))
growth_rates        <- read_rds(here("work", "07-prepare-simulation", "growth_rates.rds"))
avg_national_income <- read_rds(here("work", "07-prepare-simulation", "avg_national_income.rds"))
pop_growth          <- read_rds(here("work", "07-prepare-simulation", "pop_growth.rds"))
pop_death           <- read_rds(here("work", "07-prepare-simulation", "pop_death.rds"))
gperc               <- read_rds(here("work", "07-prepare-simulation", "gperc.rds"))
gperc_size          <- read_rds(here("work", "07-prepare-simulation", "gperc_size.rds"))
gperc_unif          <- read_rds(here("work", "07-prepare-simulation", "gperc_unif.rds"))
num_draws           <- read_rds(here("work", "07-prepare-simulation", "num_draws.rds"))

# ---------------------------------------------------------------------------- #
# Run the simulation
# ---------------------------------------------------------------------------- #

# Counterfactual capital income
simul_param_income <- simul_param_income %>%
    group_by(p) %>%
    mutate(mean_capital_pre1978 = mean(mean_capital[year <= 1978])) %>%
    mutate(var_capital_pre1978 = mean(var_capital[year <= 1978])) %>%
    mutate(mean_gains_pre1978 = mean(mean_gains[year <= 1978])) %>%
    mutate(var_gains_pre1978 = mean(var_capital[year <= 1978])) %>%
    mutate(cov_labor_capital_pre1978 = mean(cov_labor_capital/sqrt(var_labor*var_capital))*sqrt(var_capital_pre1978*var_labor)) %>%
    mutate(cov_capital_gains_pre1978 = mean(cov_capital_gains/sqrt(var_capital*var_gains))*sqrt(var_capital_pre1978*var_gains_pre1978)) %>%
    mutate(cov_labor_gains_pre1978 = mean(cov_labor_gains/sqrt(var_labor*var_gains))*sqrt(var_gains_pre1978*var_labor)) %>%
    mutate(mean_capital = if_else(year <= 1978, mean_capital, mean_capital_pre1978)) %>%
    mutate(var_capital = if_else(year <= 1978, var_capital, var_capital_pre1978)) %>%
    mutate(mean_gains = if_else(year <= 1978, mean_gains, mean_gains_pre1978)) %>%
    mutate(var_gains = if_else(year <= 1978, var_gains, var_gains_pre1978)) %>%
    mutate(cov_labor_gains = if_else(year <= 1978, cov_labor_gains, cov_labor_gains_pre1978)) %>%
    mutate(cov_labor_capital = if_else(year <= 1978, cov_labor_capital, cov_labor_capital_pre1978)) %>%
    mutate(cov_capital_gains = if_else(year <= 1978, cov_capital_gains, cov_capital_gains_pre1978))

last_year <- 2019

data_simul_init <- data_simul

data_simul_effects <- NULL
data_simul_tabul <- NULL

for (i in 1:5) {
    data_simul <- data_simul_init
    for (yr in 1963:2100) {
        # # Testing purposes only: after the first year
        # if (yr > 1963) {
        #     data_simul_tabul <- bind_rows(data_simul_tabul, data_simul_tabul %>%
        #         filter(year == 1963 & replicate == i) %>%
        #         mutate(year = yr, replicate = i))
        #     data_simul_effects <- bind_rows(data_simul_effects, data_simul_effects %>%
        #         filter(year == 1963 & replicate == i) %>%
        #         mutate(year = yr, replicate = i))
        #     cat(glue("* {yr} => Filled with dummy data"), "\n")
        #     next
        # }

        # ------------------------------------------------------------------------ #
        # Original simulated distribution
        # ------------------------------------------------------------------------ #

        data_simul_distrib <- data_simul %>%
            mutate(asinh_wealth = asinh(wealth)) %>%
            mutate(weight = weight/sum(weight)) %>%
            mutate(wealth_bin = round(10*asinh_wealth)) %>%
            group_by(wealth_bin) %>%
            summarise(density = sum(weight)/dx) %>%
            mutate(asinh_wealth = wealth_bin/10) %>%
            arrange(desc(wealth_bin)) %>%
            mutate(cdf = 1 - dx*cumsum(density))

        # ------------------------------------------------------------------------ #
        # Simulate and age/sex distribution
        # ------------------------------------------------------------------------ #

        if (yr <= last_year) {
            data_simul_new <- bind_cols(
                data_simul %>% mutate(year = yr) %>% arrange(wealth),
                data_simul_demog %>% filter(year == yr) %>% select(-year)
            )
        } else {
            # For the future, just update mortality rates
            life_table_yr <- life_table %>%
                filter(year == yr) %>%
                filter(variant == "Past" | variant == "Medium") %>%
                select(sex, age, qx)
            life_table_yr_spouse <- life_table_yr %>% transmute(
                sex = if_else(sex == "male", "female", "male"),
                age_spouse = age,
                qx_spouse = qx
            )

            data_simul_new <- bind_cols(
                data_simul %>% mutate(year = yr) %>% arrange(wealth),
                data_simul_demog %>%
                    filter(year == last_year) %>%
                    select(-year, -qx, -qx_spouse) %>%
                    left_join(life_table_yr, by = c("age", "sex")) %>%
                    mutate(qx = replace_na(qx, 1)) %>% # 100+ people with no data: assume 100% mortality
                    # Also update mortality rate of spouse
                    left_join(life_table_yr_spouse, by = c("age_spouse", "sex"))
            )
        }

        # ------------------------------------------------------------------------ #
        # Effects of savings
        # ------------------------------------------------------------------------ #

        if (yr <= year_pivot) {
            param_savings_yr <- model_params %>% transmute(asinh_wealth, diffu, drift = drift1)
        } else {
            param_savings_yr <- model_params %>% transmute(asinh_wealth, diffu, drift = drift2)
        }

        if (yr <= last_year) {
            growth_rate_yr <- growth_rates %>% filter(year == yr) %>% pull(growth_rate)
            param_income_yr <- simul_param_income %>% filter(year == yr) %>% select(-year)
        } else {
            growth_rate_yr <- growth_rates %>% filter(year %in% c(2010:last_year)) %>% pull(growth_rate) %>% mean()
            param_income_yr <- simul_param_income %>% filter(year == last_year) %>% select(-year)
        }

        simul_savings_yr <- simul_savings(
            data_simul = data_simul_new,
            param_income = param_income_yr,
            param_savings = param_savings_yr,
            growth_rate = growth_rate_yr
        )

        data_simul_new <- simul_savings_yr$data_simul
        data_simul_effects_yr <- simul_savings_yr$effects

        # ------------------------------------------------------------------------ #
        # Simulate effect of marriages & divorces
        # ------------------------------------------------------------------------ #

        if (yr <= last_year) {
            marriage_divorce_rate_yr <- marriage_divorce_rate %>%
                filter(year == yr) %>% select(-year)
        } else {
            marriage_divorce_rate_yr <- marriage_divorce_rate %>%
                filter(year == last_year) %>% select(-year)
        }

        simul_marriages_divorces_yr <- simul_marriages_divorces(
            data_simul = data_simul_new,
            marriage_divorce_rate = marriage_divorce_rate_yr,
            copula_marriage = copula_marriage,
            spouse_wealth_share = spouse_wealth_share
        )

        data_simul_new <- simul_marriages_divorces_yr$data_simul
        data_simul_effects_yr <- data_simul_effects_yr %>%
            left_join(simul_marriages_divorces_yr$effects, by = "wealth_bin")

        # ------------------------------------------------------------------------ #
        # Simulate inheritance
        # ------------------------------------------------------------------------ #

        # Retrieve national income of the year to be able to compute the level of
        # inheritance taxation
        avg_national_income_yr <- avg_national_income %>% filter(year == min(yr, last_year)) %>% pull(avg)

        simul_inheritance_yr <- simul_inheritance(
            data_simul = data_simul_new,
            estate_tax_fun = function(w) estate_tax(w*avg_national_income_yr, min(yr, last_year))/avg_national_income_yr
        )

        data_simul_new <- simul_inheritance_yr$data_simul
        data_simul_effects_yr <- data_simul_effects_yr %>% left_join(simul_inheritance_yr$effects, by = "wealth_bin")

        # ------------------------------------------------------------------------ #
        # Simulate birth/death
        # ------------------------------------------------------------------------ #

        death_rate <- pop_death %>% filter(year == yr) %>% pull(death_rate)
        growth_rate <- pop_growth %>% filter(year == yr) %>% pull(pop_growth)
        birth_rate <- death_rate + growth_rate

        simul_birth_death_yr <- simul_birth_death(data_simul_new, birth_rate)

        data_simul_new <- simul_birth_death_yr$data_simul
        data_simul_effects_yr <- data_simul_effects_yr %>% left_join(simul_birth_death_yr$effects, by = "wealth_bin")

        # ------------------------------------------------------------------------ #
        # Resample the distribution
        # ------------------------------------------------------------------------ #

        data_simul_new <- data_simul_new %>%
            arrange(wealth) %>%
            mutate(rank = (cumsum(weight) - weight/2)/sum(weight)) %>%
            mutate(bracket = findInterval(1e5*rank, gperc)) %>%
            group_by(bracket) %>%
            # Sample num_draws observations within each g-percentile
            slice_sample(n = num_draws, weight_by = weight, replace = TRUE) %>%
            mutate(weight = gperc_size[bracket], p = gperc[bracket]) %>%
            transmute(year = yr, weight, p, wealth) %>%
            ungroup() %>%
            arrange(wealth)

        cat(glue("* {yr} => {round(100*top_shares(data_simul_new$wealth, data_simul_new$weight, p = 0.99), 2)}%"), "\n")

        data_simul <- data_simul_new
        data_simul_effects <- bind_rows(data_simul_effects, data_simul_effects_yr %>% mutate(year = yr, replicate = i))

        # ------------------------------------------------------------------------ #
        # Tabulate the distribution
        # ------------------------------------------------------------------------ #

        data_simul_tabul <- bind_rows(
            data_simul_tabul,
            data_simul %>%
                arrange(wealth) %>%
                mutate(rank = (cumsum(weight) - weight/2)/sum(weight)) %>%
                mutate(bracket = findInterval(1e5*rank, gperc)) %>%
                group_by(year, bracket) %>%
                summarise(a = weighted.mean(wealth, weight)) %>%
                ungroup() %>%
                arrange(bracket) %>%
                mutate(p = gperc[bracket]) %>%
                mutate(n = diff(c(p, 1e5))) %>%
                mutate(s = a*n/1e5/weighted.mean(a, n)) %>%
                arrange(desc(p)) %>%
                mutate(ts = cumsum(s)) %>%
                mutate(bs = 1 - ts) %>%
                mutate(replicate = i) %>%
                arrange(p)
        )
    }
}

stop()

# ---------------------------------------------------------------------------- #
# Save results
# ---------------------------------------------------------------------------- #

write_rds(data_simul_tabul,   here("work", "07-simulate-model-capital", "data_simul_capital_tabul.rds"))
write_rds(data_simul_effects, here("work", "07-simulate-model-capital", "data_simul_capital_effects.rds"))

# ---------------------------------------------------------------------------- #
# Plot results
# ---------------------------------------------------------------------------- #

dina_distrib               <- read_rds(here("work", "02-import-dina", "dina_distrib.rds"))
model_micro_data           <- read_rds(here("work", "04-prepare-data", "model_micro_data.rds"))
data_simul_tabul           <- read_rds(here("work", "07-simulate-model", "data_simul_tabul.rds"))
data_simul_effects         <- read_rds(here("work", "07-simulate-model", "data_simul_effects.rds"))
data_simul_future_tabul    <- read_rds(here("work", "07-simulate-model-future", "data_simul_future_tabul.rds"))
data_simul_future_effects  <- read_rds(here("work", "07-simulate-model-future", "data_simul_future_effects.rds"))
data_simul_capital_tabul   <- read_rds(here("work", "07-simulate-model-capital", "data_simul_capital_tabul.rds"))
data_simul_capital_effects <- read_rds(here("work", "07-simulate-model-capital", "data_simul_capital_effects.rds"))

gperc <- c(
    seq(0, 99000, 1000), seq(99100, 99900, 100),
    seq(99910, 99990, 10), seq(99991, 99999, 1)
)
gperc_size <- diff(c(gperc, 1e5))

tabulations_combined <- bind_rows(
    model_micro_data %>%
        filter(asinh(wealth) >= min(grid_cutpoints)) %>% # Reflective barrier
        group_by(year) %>%
        arrange(year, wealth) %>%
        mutate(rank = (cumsum(weight) - weight/2)/sum(weight)) %>%
        mutate(bracket = findInterval(1e5*rank, gperc)) %>%
        group_by(year, bracket) %>%
        summarise(a = weighted.mean(wealth, weight)) %>%
        arrange(year, bracket) %>%
        mutate(p = gperc[bracket]) %>%
        mutate(n = diff(c(p, 1e5))) %>%
        mutate(s = a*n/1e5/weighted.mean(a, n)) %>%
        arrange(desc(p)) %>%
        mutate(ts = cumsum(s)) %>%
        mutate(bs = 1 - ts) %>%
        arrange(p) %>%
        mutate(type = "Observed"),
    data_simul_capital_tabul %>% mutate(type = "Simulated\n(counterfactual)"),
    data_simul_future_tabul %>% mutate(type = "Simulated\n(benchmark)")
)

# Add 1962 to simulation group (using real value, as this is the starting point)
tabulations_combined <- tabulations_combined %>% bind_rows(
    tabulations_combined %>% filter(year == 1962 & type == "Observed") %>% mutate(type = "Simulated\n(counterfactual)"),
    tabulations_combined %>% filter(year == 1962 & type == "Observed") %>% mutate(type = "Simulated\n(benchmark)"),
)

pdf(here("graphs", "07-simulate-model-capital", "actual-simul-top1-capital.pdf"), height = 3, width = 4)
print(
    tabulations_combined %>%
        filter(p == 99000) %>%
        bind_rows(dina_distrib %>% filter(year < 1962) %>% transmute(year, ts = wealth_top1, type = "Observed")) %>%
        group_by(type, year) %>%
        summarise(ts = median(ts)) %>%
        filter(year <= 2070) %>%
        mutate(t = if_else(type == "Observed", year, 5*floor((year - 1)/5))) %>%
        group_by(type, t) %>%
        summarise(year = mean(year), ts = median(ts)) %>%
        ggplot() +
        theme_bw() +
        geom_line(aes(x = year, y = ts, color = type), size = 0.7) +
        geom_point(aes(x = year, y = ts, color = type, shape = type), size = 1.5) +

        annotate("text", label = "Observed", x = 1928, y = 0.27, color = "#969A97", size = 3) +
        annotate("segment", x = 1930, xend = 1943, y = 0.28, yend = 0.31, color = "#969A97",
            arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +

        annotate("richtext",
            label = "<strong>Simulated</strong><br>(benchmark)",
            x = 1980,
            y = 0.40,
            color = "#2F4858",
            size = 3,
            fill = NA,
            label.color = NA
        ) +
        annotate("segment", x = 2000, xend = 2024, y = 0.40, yend = 0.375, color = "#2F4858",
            arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +

        annotate("segment", x = 2030, xend = 2050, y = 0.45, yend = 0.33,
            color = "#F26419", arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +
        annotate("richtext", x = 2010, y = 0.475,
            label = "With <strong>1962-1978 capital returns</strong>,<br>the <strong>top 1% share</strong> would be<br><strong>~5pp. lower</strong> in the long run",
            color = "#F26419",
            size = 3,
            fill = NA,
            label.color = NA) +

        scale_color_manual(values = c(`Observed` = "#969A97", `Simulated\n(benchmark)` = "#2F4858", `Simulated\n(counterfactual)` = "#F26419")) +
        scale_shape_manual(values = c(`Observed` = 1, `Simulated\n(counterfactual)` = 0, `Simulated\n(benchmark)` = 5),
            limits = c("Observed", "Simulated\n(benchmark)", "Simulated\n(counterfactual)")) +
        scale_y_continuous(name = "Top 1% share", limits = c(0.2, 0.5), breaks = seq(0.2, 0.5, 0.05),
            minor_breaks = NULL, labels = label_percent(accuracy = 1)) +
        scale_x_continuous(name = NULL, limits = c(1910, 2070), breaks = seq(1920, 2060, 20), minor_breaks = NULL) +
        theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
)
dev.off()
