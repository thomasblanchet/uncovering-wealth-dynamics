# ---------------------------------------------------------------------------- #
# Simulate the model
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(glue)
library(VineCopula)
library(scales)

options(dplyr.summarise.inform = FALSE)

dir.create(here("graphs", "07-simulate-model"), showWarnings = FALSE)
dir.create(here("work", "07-simulate-model"), showWarnings = FALSE)

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

data_simul_init <- data_simul

data_simul_effects <- NULL
data_simul_tabul <- NULL

for (i in 1:5) {
    data_simul <- data_simul_init
    for (yr in 1963:2019) {
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

        data_simul_new <- bind_cols(
            data_simul %>% mutate(year = yr) %>% arrange(wealth),
            data_simul_demog %>% filter(year == yr) %>% select(-year)
        )

        # ------------------------------------------------------------------------ #
        # Effects of savings
        # ------------------------------------------------------------------------ #

        if (yr <= year_pivot) {
            param_savings_yr <- model_params %>% transmute(asinh_wealth, diffu, drift = drift1)
        } else {
            param_savings_yr <- model_params %>% transmute(asinh_wealth, diffu, drift = drift2)
        }

        simul_savings_yr <- simul_savings(
            data_simul = data_simul_new,
            param_income = simul_param_income %>% filter(year == yr) %>% select(-year),
            param_savings = param_savings_yr,
            growth_rate = growth_rates %>% filter(year == yr) %>% pull(growth_rate)
        )

        data_simul_new <- simul_savings_yr$data_simul
        data_simul_effects_yr <- simul_savings_yr$effects

        # ------------------------------------------------------------------------ #
        # Simulate effect of marriages & divorces
        # ------------------------------------------------------------------------ #

        simul_marriages_divorces_yr <- simul_marriages_divorces(
            data_simul = data_simul_new,
            marriage_divorce_rate = marriage_divorce_rate %>% filter(year == yr) %>% select(-year),
            copula_marriage = copula_marriage,
            spouse_wealth_share = spouse_wealth_share
        )

        data_simul_new <- simul_marriages_divorces_yr$data_simul
        data_simul_effects_yr <- data_simul_effects_yr %>% left_join(simul_marriages_divorces_yr$effects, by = "wealth_bin")

        # ------------------------------------------------------------------------ #
        # Simulate inheritance
        # ------------------------------------------------------------------------ #

        # Retrieve national income of the year to be able to compute the level of
        # inheritance taxation
        avg_national_income_yr <- avg_national_income %>% filter(year == yr) %>% pull(avg)

        simul_inheritance_yr <- simul_inheritance(
            data_simul = data_simul_new,
            estate_tax_fun = function(w) estate_tax(w*avg_national_income_yr, yr)/avg_national_income_yr
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

write_rds(data_simul_tabul,   here("work", "07-simulate-model", "data_simul_tabul.rds"))
write_rds(data_simul_effects, here("work", "07-simulate-model", "data_simul_effects.rds"))

# ---------------------------------------------------------------------------- #
# Compare simulated effects with the real ones
# ---------------------------------------------------------------------------- #

model_micro_data   <- read_rds(here("work", "04-prepare-data", "model_micro_data.rds"))
data_simul_tabul   <- read_rds(here("work", "07-simulate-model", "data_simul_tabul.rds"))
data_simul_effects <- read_rds(here("work", "07-simulate-model", "data_simul_effects.rds"))

gperc <- c(
    seq(0, 99000, 1000), seq(99100, 99900, 100),
    seq(99910, 99990, 10), seq(99991, 99999, 1)
)
gperc_size <- diff(c(gperc, 1e5))

pdf(file = here("graphs", "07-simulate-model", "simul-actual.pdf"), height = 4, width = 6)

print(
    data_simul_effects %>%
        transmute(year, wealth_bin, asinh_wealth,
            effect_simul = -(cdf_after_marriages - cdf_before_marriages)/density_before_marriages) %>%
        inner_join(model_effects %>% transmute(year, wealth_bin, effect = marriage_effect + divorce_effect)) %>%
        mutate(period = if_else(year <= year_pivot, "1962-1978", "1979-2019")) %>%
        filter(wealth_bin >= 0) %>%
        group_by(wealth_bin, period) %>%
        summarise(effect_simul = mean(effect_simul), effect = mean(effect)) %>%
        mutate(asinh_wealth = wealth_bin/10) %>%
        ggplot() +
        geom_line(aes(x = asinh_wealth, y = effect, color = "Effective", group = period), size = 1) +
        geom_line(aes(x = asinh_wealth, y = effect_simul, color = "Simulated", group = period), size = 1) +
        scale_color_brewer(name = element_blank(), type = "qual", palette = "Set1") +
        scale_y_continuous(
            name = expression("Effect on "-partialdiff[t]~F(w)/f(w)),
            limits = c(-0.15, 0.15),
            breaks = seq(-0.15, 0.15, 0.05),
            labels = comma_format()
        ) +
        scale_x_continuous(
            name = "(wealth)/(average national income)",
            breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
            labels = function(.) round(sinh(.)),
            limits = asinh(c(0, 2000))
        ) +
        theme_bw() +
        ggtitle("Marriages & divorce")
)

print(
    data_simul_effects %>%
        transmute(year, wealth_bin, asinh_wealth,
            effect_simul = -(cdf_after_inheritance - cdf_before_inheritance)/density_before_inheritance) %>%
        inner_join(model_effects %>% transmute(year, wealth_bin, effect = inheritance_effect)) %>%
        mutate(period = if_else(year <= year_pivot, "1962-1978", "1979-2019")) %>%
        filter(wealth_bin >= 0) %>%
        group_by(wealth_bin, period) %>%
        summarise(effect_simul = mean(effect_simul), effect = mean(effect)) %>%
        mutate(asinh_wealth = wealth_bin/10) %>%
        ggplot() +
        geom_line(aes(x = asinh_wealth, y = effect, color = "Effective", group = period), size = 1) +
        geom_line(aes(x = asinh_wealth, y = effect_simul, color = "Simulated", group = period), size = 1) +
        scale_color_brewer(name = element_blank(), type = "qual", palette = "Set1") +
        scale_y_continuous(
            name = expression("Effect on "-partialdiff[t]~F(w)/f(w)),
            limits = c(-0.15, 0.15),
            breaks = seq(-0.15, 0.15, 0.05),
            labels = comma_format()
        ) +
        scale_x_continuous(
            name = "(wealth)/(average national income)",
            breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
            labels = function(.) round(sinh(.)),
            limits = asinh(c(0, 2000))
        ) +
        theme_bw() +
        ggtitle("Inheritance")
)

print(
    data_simul_effects %>%
        transmute(year, wealth_bin, asinh_wealth,
            effect_simul = -(cdf_after_birth_death - cdf_before_birth_death)/density_before_birth_death) %>%
        inner_join(model_effects %>% transmute(year, wealth_bin, effect = demographic_effect)) %>%
        mutate(period = if_else(year <= year_pivot, "1962-1978", "1979-2019")) %>%
        filter(wealth_bin >= 0) %>%
        group_by(wealth_bin, period) %>%
        summarise(effect_simul = mean(effect_simul), effect = mean(effect)) %>%
        mutate(asinh_wealth = wealth_bin/10) %>%
        ggplot() +
        geom_line(aes(x = asinh_wealth, y = effect, color = "Effective", group = period), size = 1) +
        geom_line(aes(x = asinh_wealth, y = effect_simul, color = "Simulated", group = period), size = 1) +
        scale_color_brewer(name = element_blank(), type = "qual", palette = "Set1") +
        scale_y_continuous(
            name = expression("Effect on "-partialdiff[t]~F(w)/f(w)),
            limits = c(-0.15, 0.15),
            breaks = seq(-0.15, 0.15, 0.05),
            labels = comma_format()
        ) +
        scale_x_continuous(
            name = "(wealth)/(average national income)",
            breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
            labels = function(.) round(sinh(.)),
            limits = asinh(c(0, 2000))
        ) +
        theme_bw() +
        ggtitle("Demography")
)

print(
    data_simul_effects %>%
        transmute(year, wealth_bin, asinh_wealth,
            effect_simul = -(cdf_after_savings - cdf_before_savings)/density_before_savings
        ) %>%
        inner_join(model_effects %>% transmute(year, wealth_bin,
            effect = if_else(year <= year_pivot, drift1, drift2) + mean_income
                + ito_adj_income - diffu_deriv_total_actual - deriv_log_density*diffu_total_actual
        )) %>%
        mutate(period = if_else(year <= year_pivot, "1962-1978", "1979-2019")) %>%
        filter(wealth_bin >= 0) %>%
        group_by(wealth_bin, period) %>%
        summarise(effect_simul = mean(effect_simul), effect = mean(effect)) %>%
        mutate(asinh_wealth = wealth_bin/10) %>%
        ggplot() +
        geom_line(aes(x = asinh_wealth, y = effect, color = "Effective", group = period), size = 1) +
        geom_line(aes(x = asinh_wealth, y = effect_simul, color = "Simulated", group = period), size = 1) +
        scale_color_brewer(name = element_blank(), type = "qual", palette = "Set1") +
        scale_y_continuous(
            name = expression("Effect on "-partialdiff[t]~F(w)/f(w)),
            limits = c(-0.15, 0.15),
            breaks = seq(-0.15, 0.15, 0.05),
            labels = comma_format()
        ) +
        scale_x_continuous(
            name = "(wealth)/(average national income)",
            breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
            labels = function(.) round(sinh(.)),
            limits = asinh(c(0, 2000))
        ) +
        theme_bw() +
        ggtitle("Income/Consumption")
)

dev.off()

# ---------------------------------------------------------------------------- #
# Compare the actual and simulated distributions
# ---------------------------------------------------------------------------- #

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
        mutate(type = "Observed values"),
    data_simul_tabul %>% group_by(year, p) %>% summarise(across(c(a, ts), mean)) %>% mutate(type = "Simulated values")
)

# Add 1962 to simulation group (using real value, as this is the starting point)
tabulations_combined <- tabulations_combined %>% bind_rows(
    tabulations_combined %>% filter(year == 1962 & type == "Observed values") %>% mutate(type = "Simulated values")
)

pdf(here("graphs", "07-simulate-model", "actual-simul-gic-1.pdf"), height = 3.5, width = 3.5)
print(
    tabulations_combined %>%
        group_by(type, year, p) %>%
        summarise(a = median(a)) %>%
        left_join(model_macro_data %>% select(year, constant_avg_income)) %>%
        mutate(a = a*constant_avg_income) %>%
        pivot_wider(c(type, p), names_from = year, values_from = a) %>%
        group_by(type) %>%
        mutate(n = row_number()) %>%
        filter(p >= 70000 & type == "Observed values") %>%
        ggplot() +
        theme_bw() +

        annotate("text", y = -0.03, x = 120, label = "1962-1977", color = "#000000") +
        annotate("text", y = 0.07, x = 118, label = "1978-2019", color = "#000000") +

        annotate("text", y = -0.01, x = 85, label = "Observed", color = rgb(47, 72, 88, max = 255)) +
        annotate("segment", x = 90.5, xend = 100, y = -0.01, yend = 0.026, color = rgb(47, 72, 88, max = 255),
            arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +
        annotate("segment", x = 90.5, xend = 100, y = -0.012, yend = -0.002, color = rgb(47, 72, 88, max = 255),
            arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +

        geom_line(aes(x = n, y = (`2019`/`1978`)^(1/(2019 - 1978)) - 1, color = type), size = 0.5) +
        geom_line(aes(x = n, y = (`1977`/`1962`)^(1/(1977 - 1962)) - 1, color = type), size = 0.5) +
        geom_point(aes(x = n, y = (`2019`/`1978`)^(1/(2019 - 1978)) - 1, color = type, shape = type), size = 1) +
        geom_point(aes(x = n, y = (`1977`/`1962`)^(1/(1977 - 1962)) - 1, color = type, shape = type), size = 1) +

        scale_color_manual(values = c(`Observed values` = rgb(47, 72, 88, max = 255), `Simulated values` = rgb(242, 100, 25, max = 255))) +
        scale_shape_manual(values = c(`Observed values` = 1, `Simulated values` = 2)) +
        scale_y_continuous(name = "Annualized growth rate", limits = c(-0.04, 0.08),
            labels = percent_format(accuracy = 1), breaks = seq(-0.04, 0.08, 0.02), minor_breaks = NULL) +
        scale_x_continuous(name = "Wealth percentile", breaks = c(51, 61, 71, 81, 91, 100, 109, 118, 127),
            labels = c("50%", "60%", "70%", "80%", "90%", "99%", "99.9%", "99.99%", "99.999%"), minor_breaks = NULL) +
        theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
)
dev.off()

pdf(here("graphs", "07-simulate-model", "actual-simul-gic-2.pdf"), height = 3.5, width = 3.5)
print(
    tabulations_combined %>%
        group_by(type, year, p) %>%
        summarise(a = median(a)) %>%
        left_join(model_macro_data %>% select(year, constant_avg_income)) %>%
        mutate(a = a*constant_avg_income) %>%
        pivot_wider(c(type, p), names_from = year, values_from = a) %>%
        group_by(type) %>%
        mutate(n = row_number()) %>%
        filter(p >= 70000) %>%
        ggplot() +
        theme_bw() +

        annotate("text", y = -0.03, x = 120, label = "1962-1977", color = "#000000") +
        annotate("text", y = 0.07, x = 115, label = "1978-2019", color = "#000000") +

        annotate("text", y = -0.01, x = 82, label = "Observed", color = rgb(47, 72, 88, max = 255)) +
        annotate("segment", x = 90.5, xend = 100, y = -0.01, yend = 0.026, color = rgb(47, 72, 88, max = 255),
            arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +
        annotate("segment", x = 90.5, xend = 100, y = -0.012, yend = -0.002, color = rgb(47, 72, 88, max = 255),
            arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +

        annotate("text", y = 0.01, x = 120.5, label = "Simulated", color = rgb(242, 100, 25, max = 255)) +
        annotate("segment", x = 115, xend = 111, y = 0.014, yend = 0.038, color = rgb(242, 100, 25, max = 255),
            arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +
        annotate("segment", x = 115, xend = 111, y = 0.006, yend = -0.011, color = rgb(242, 100, 25, max = 255),
            arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +

        geom_line(aes(x = n, y = (`2019`/`1978`)^(1/(2019 - 1978)) - 1, color = type), size = 0.5) +
        geom_line(aes(x = n, y = (`1977`/`1962`)^(1/(1977 - 1962)) - 1, color = type), size = 0.5) +
        geom_point(aes(x = n, y = (`2019`/`1978`)^(1/(2019 - 1978)) - 1, color = type, shape = type), size = 1) +
        geom_point(aes(x = n, y = (`1977`/`1962`)^(1/(1977 - 1962)) - 1, color = type, shape = type), size = 1) +

        scale_color_manual(values = c(`Observed values` = rgb(47, 72, 88, max = 255), `Simulated values` = rgb(242, 100, 25, max = 255))) +
        scale_shape_manual(values = c(`Observed values` = 1, `Simulated values` = 2)) +
        scale_y_continuous(name = "Annualized growth rate", limits = c(-0.04, 0.08),
            labels = percent_format(accuracy = 1), breaks = seq(-0.04, 0.08, 0.02), minor_breaks = NULL) +
        scale_x_continuous(name = "Wealth percentile", breaks = c(51, 61, 71, 81, 91, 100, 109, 118, 127),
            labels = c("50%", "60%", "70%", "80%", "90%", "99%", "99.9%", "99.99%", "Top\n0.001%"), minor_breaks = NULL) +
        theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank(),
            axis.text.x = element_text(size = 8))
)
dev.off()

pdf(here("graphs", "07-simulate-model", "actual-simul-top1.pdf"), height = 4, width = 6)
print(
    tabulations_combined %>%
        filter(p == 99000) %>%
        group_by(type, year) %>%
        summarise(ts = median(ts)) %>%
        ggplot() +
        theme_bw() +
        geom_line(aes(x = year, y = ts, color = type), size = 0.5) +
        geom_line(aes(x = year, y = ts, color = type), size = 0.5) +
        geom_point(aes(x = year, y = ts, color = type, shape = type), size = 2) +
        geom_point(aes(x = year, y = ts, color = type, shape = type), size = 2) +
        scale_color_brewer(type = "qual", palette = "Set1") +
        scale_shape_manual(values = c(`Observed values` = 1, `Simulated values` = 2)) +
        scale_y_continuous(name = "Top 1% share", limits = c(0.2, 0.4), breaks = seq(0.2, 0.4, 0.025), labels = label_percent()) +
        scale_x_continuous(name = NULL, breaks = seq(1960, 2020, 5), minor_breaks = NULL) +
        theme(legend.position = "right", legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, margin = margin(t = 0.4, unit = "cm")),
            axis.title.x = element_text(margin = margin(t = -0.4, b = -0.4, unit = "cm")))
)
dev.off()

