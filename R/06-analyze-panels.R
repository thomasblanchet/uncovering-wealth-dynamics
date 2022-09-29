# ---------------------------------------------------------------------------- #
# Measure drift/diffusion profiles from panel data
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(glue)

options(dplyr.summarise.inform = FALSE)

dir.create(here("graphs", "06-analyze-panels"), showWarnings = FALSE)

winsorize         <- read_rds(here("work", "01-utils", "winsorize.rds"))
weighted_var      <- read_rds(here("work", "01-utils", "weighted_var.rds"))
weighted_sd       <- read_rds(here("work", "01-utils", "weighted_sd.rds"))
weighted_quantile <- read_rds(here("work", "01-utils", "weighted_quantile.rds"))
dina_macro        <- read_rds(here("work", "02-import-dina", "dina_macro.rds"))
data_psid         <- read_rds(here("work", "02-import-psid", "data_psid.rds"))
scf_panel         <- read_rds(here("work", "02-import-scf-panel", "scf_panel.rds"))
model_micro_data  <- read_rds(here("work", "04-prepare-data", "model_micro_data.rds"))
year_pivot        <- read_rds(here("work", "04-prepare-data", "year_pivot.rds"))
model_params      <- read_rds(here("work", "05-fit-model", "model_params.rds"))
model_all_data    <- read_rds(here("work", "05-fit-model", "model_all_data.rds"))

# ---------------------------------------------------------------------------- #
# SCF
# ---------------------------------------------------------------------------- #

avg_income_2007 <- dina_macro %>% filter(year == 2007) %>%
    mutate(avg = national_income/adult_population) %>% pull(avg)

avg_income_2009 <- dina_macro %>% filter(year == 2009) %>%
    mutate(avg = national_income/adult_population) %>% pull(avg)

scf_panel <- scf_panel %>%
    mutate(wealth2007 = wealth2007/avg_income_2007) %>%
    mutate(income2007 = income2007/avg_income_2007) %>%
    mutate(wealth2009 = wealth2009/avg_income_2009) %>%
    mutate(income2009 = income2009/avg_income_2009)

scf_panel <- scf_panel %>%
    mutate(num_members = if_else(married2007 == 1, 2, 1)) %>%
    uncount(num_members, .remove = FALSE) %>%
    mutate(across(starts_with("income"), ~ .x/num_members)) %>%
    mutate(across(starts_with("wealth"), ~ .x/num_members))

# Actual savings/consumption, by percentile
growth2007_2009 <- (avg_income_2009/avg_income_2007)^(1/2) - 1

scf_params_perc <- scf_panel %>%
    mutate(wealth_growth = (wealth2009/wealth2007 - 1)/2) %>%
    mutate(conso = (income2007 + income2009)/2/wealth2007 - growth2007_2009 - wealth_growth) %>%
    mutate(conso = winsorize(conso, quantile(conso, probs = c(0.025, 0.975), na.rm = TRUE))) %>%
    mutate(wealth_growth = winsorize(wealth_growth, quantile(wealth_growth, probs = c(0.025, 0.975), na.rm = TRUE))) %>%
    arrange(wealth2007) %>%
    mutate(rank = 100*(cumsum(weight) - weight/2)/sum(weight)) %>%
    mutate(bracket = cut(rank, breaks = c(0, 50, 90, 99, 100), include.lowest = TRUE, labels = FALSE)) %>%
    group_by(bracket) %>%
    summarise(
        wealth = weighted.mean(wealth2007, weight, na.rm = TRUE),

        mean_wealth_growth = weighted.mean(wealth_growth, weight, na.rm = TRUE),
        mean_conso = weighted.mean(conso, weight, na.rm = TRUE),

        sd_wealth_growth = weighted_sd(wealth_growth, weight, na.rm = TRUE),
        sd_conso = weighted_sd(conso, weight, na.rm = TRUE)
    )

# Rank-rank correlation
scf_rank_corr <- scf_panel %>%
    arrange(wealth2007) %>%
    mutate(rank2007 = (cumsum(weight) - weight/2)/sum(weight)) %>%
    arrange(wealth2009) %>%
    mutate(rank2009 = (cumsum(weight) - weight/2)/sum(weight)) %>%
    mutate(bracket = cut(rank2007, breaks = c(0, 0.50, 0.90, 0.95, 0.99, 0.999, 1), labels = FALSE)) %>%
    group_by(bracket) %>%
    summarise(
        scf_rank_next_p10 = weighted_quantile(rank2009, weight, 0.10),
        scf_rank_next_p25 = weighted_quantile(rank2009, weight, 0.25),
        scf_rank_next_p50 = weighted_quantile(rank2009, weight, 0.50),
        scf_rank_next_p75 = weighted_quantile(rank2009, weight, 0.75),
        scf_rank_next_p90 = weighted_quantile(rank2009, weight, 0.90)
    )

model_rank_corr <- model_micro_data %>%
    filter(year == 2007) %>%
    group_by(year) %>%
    mutate(weight = weight/sum(weight)) %>%
    mutate(wealth_bin = round(10*asinh_wealth)) %>%
    inner_join(model_params %>% select(-asinh_wealth)) %>%
    inner_join(model_all_data %>% select(year, wealth_bin, sd_income, mean_income, ito_adj_income)) %>%
    mutate(drift = 2*(drift_corr_per2 + mean_income + ito_adj_income)) %>%
    mutate(diffu = sqrt(2)*sqrt(2*diffu_star_smooth + sd_income^2)) %>%
    mutate(asinh_wealth_next = rnorm(n(), mean = asinh_wealth + drift, sd = diffu)) %>%
    arrange(year, asinh_wealth) %>%
    mutate(rank = (cumsum(weight) - weight/2)/sum(weight)) %>%
    arrange(year, asinh_wealth_next) %>%
    mutate(rank_next = (cumsum(weight) - weight/2)/sum(weight)) %>%
    mutate(bracket = cut(rank, breaks = c(0, 0.50, 0.90, 0.95, 0.99, 0.999, 1), labels = FALSE)) %>%
    group_by(year, bracket) %>%
    summarise(
        rank_next_p10 = weighted_quantile(rank_next, weight, 0.10),
        rank_next_p25 = weighted_quantile(rank_next, weight, 0.25),
        rank_next_p50 = weighted_quantile(rank_next, weight, 0.50),
        rank_next_p75 = weighted_quantile(rank_next, weight, 0.75),
        rank_next_p90 = weighted_quantile(rank_next, weight, 0.90)
    ) %>%
    group_by(bracket) %>%
    summarise(across(starts_with("rank_next"), mean))

pdf(here("graphs", "06-analyze-panels", "mobility-rank-scf.pdf"), height = 3, width = 3.5)
p <- scf_rank_corr %>% left_join(model_rank_corr) %>% ggplot() +
    geom_linerange(aes(x = bracket - 0.1, ymin = -log(1 - rank_next_p25), ymax = -log(1 - rank_next_p75), color = "model"), alpha = 1, size = 1) +
    geom_point(aes(x = bracket - 0.1, y = -log(1 - rank_next_p50), color = "model"), alpha = 1) +

    geom_linerange(aes(x = bracket + 0.1, ymin = -log(1 - scf_rank_next_p25), ymax = -log(1 - scf_rank_next_p75), color = "SCF"), alpha = 1, size = 1) +
    geom_point(aes(x = bracket + 0.1, y = -log(1 - scf_rank_next_p50), color = "SCF"), alpha = 1) +

    annotate("text", x = 1.2, y = -log(1 - 0.9), label = "Model", color = "#2F4858", size = 3) +
    annotate("segment", x = 1.3, xend = 1.75, y = -log(1 - 0.85), yend = -log(1 - 0.7), color = "#2F4858",
        arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +

    annotate("text", x = 3, y = -log(1 - 0.3), label = "SCF", color = "#F26419", size = 3) +
    annotate("segment", x = 2.9, xend = 2.23, y = -log(1 - 0.48), yend = -log(1 - 0.7), color = "#F26419",
        arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +

    annotate("text", x = 5.6, y = -log(1 - 0.9), label = "Median\nrank", color = "#000000", size = 3) +
    annotate("segment", x = 5.6, xend = 4.95, y = -log(1 - 0.95), yend = -log(1 - 0.993), color = "#000000",
        arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +
    annotate("segment", x = 5.6, xend = 5.15, y = -log(1 - 0.95), yend = -log(1 - 0.992), color = "#000000",
        arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +

    annotate("text", x = 4.5, y = -log(1 - 0.9998), label = "50% in\nthis range", color = "#000000", size = 3) +
    annotate("segment", x = 4.5, xend = 4.9, y = -log(1 - 0.9995), yend = -log(1 - 0.997), color = "#000000",
        arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +
    annotate("segment", x = 4.5, xend = 5.1, y = -log(1 - 0.9995), yend = -log(1 - 0.9965), color = "#000000",
        arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +

    scale_color_manual(values = c(`model` = "#2F4858", `SCF` = "#F26419")) +
    scale_y_continuous(
        name = "wealth rank (2009)",
        limits = c(0, -log(1 - 0.9999)),
        breaks = -log(1 - c(0, 0.9, 0.99, 0.999, 0.9999)),
        minor_breaks = -log(1 - c(seq(0, 0.9, 0.1), seq(0.91, 0.99, 0.01), seq(0.991, 0.999, 0.001), seq(0.9991, 0.9999, 0.0001))),
        labels = paste0(100*c(0, 0.9, 0.99, 0.999, 0.9999), "%")
    ) +
    scale_x_continuous(
        name = "wealth bracket (2007)",
        breaks = 1:6,
        labels = c("0-\n50%", "50-\n90%", "90-\n95%", "95-\n99%", "99-\n99.9%", "99.9-\n100%"),
        minor_breaks = NULL
    ) +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_line(size = 0.2),
        panel.border = element_blank())
print(p)
dev.off()

# ---------------------------------------------------------------------------- #
# PSID
# ---------------------------------------------------------------------------- #

# Normalize by national income
data_psid <- data_psid %>%
    left_join(dina_macro %>% select(year, national_income, adult_population)) %>%
    mutate(wealth = wealth/(national_income/adult_population)) %>%
    mutate(income = income/(national_income/adult_population))

# Equal-split income and wealth
data_psid <- data_psid %>%
    mutate(married = (marstat != 5)) %>%
    mutate(weight = if_else(married, weight*2, weight)) %>%
    mutate(wealth = if_else(married, wealth/2, wealth)) %>%
    mutate(income = if_else(married, income/2, income))

# Drift/diffusion profiles, by percentile
psid_params_perc <- data_psid %>%
    filter(wealth_imputation == 0) %>%
    left_join(dina_macro %>% transmute(year, current_avg_income = national_income/adult_population)) %>%
    group_by(hid) %>%
    arrange(hid, year) %>%
    mutate(dt = lead(year) - year) %>%
    mutate(growth = (lead(current_avg_income)/current_avg_income - 1)/dt) %>%
    mutate(wealth_growth = (lead(wealth)/wealth - 1)/dt) %>%
    mutate(conso = income/wealth + growth - wealth_growth) %>%
    drop_na() %>%
    group_by(year) %>%
    # Winsorize conso and wealth growth
    mutate(conso = winsorize(conso, quantile(conso, probs = c(0.025, 0.975), na.rm = TRUE))) %>%
    mutate(wealth_growth = winsorize(wealth_growth, quantile(wealth_growth, probs = c(0.025, 0.975), na.rm = TRUE))) %>%
    arrange(year, wealth) %>%
    mutate(rank = 100*(cumsum(weight) - weight/2)/sum(weight)) %>%
    mutate(bracket = cut(rank, breaks = c(0, 50, 90, 99, 100), include.lowest = TRUE, labels = FALSE)) %>%
    group_by(bracket) %>%
    summarise(
        wealth = weighted.mean(wealth, weight, na.rm = TRUE),

        mean_wealth_growth = weighted.mean(wealth_growth, weight, na.rm = TRUE),
        mean_conso = weighted.mean(conso, weight, na.rm = TRUE),

        sd_wealth_growth = weighted_sd(wealth_growth, weight, na.rm = TRUE),
        sd_conso = weighted_sd(conso, weight, na.rm = TRUE)
    )

# Rank-rank correlation
psid_rank_corr <- data_psid %>%
    #filter(wealth_imputation == 0) %>%
    group_by(hid) %>%
    arrange(hid, year) %>%
    mutate(dt = lead(year) - year) %>%
    filter(dt == 5) %>%
    mutate(wealth_next = lead(wealth)) %>%
    filter(!is.na(wealth_next)) %>%
    group_by(year) %>%
    arrange(year, wealth) %>%
    mutate(rank = (cumsum(weight) - weight/2)/sum(weight)) %>%
    arrange(year, wealth_next) %>%
    mutate(rank_next = (cumsum(weight) - weight/2)/sum(weight)) %>%
    mutate(bracket = cut(rank, breaks = c(0, 0.50, 0.90, 0.95, 1), labels = FALSE)) %>%
    group_by(year, bracket) %>%
    summarise(
        psid_rank_next_p10 = weighted_quantile(rank_next, weight, 0.10),
        psid_rank_next_p25 = weighted_quantile(rank_next, weight, 0.25),
        psid_rank_next_p50 = weighted_quantile(rank_next, weight, 0.50),
        psid_rank_next_p75 = weighted_quantile(rank_next, weight, 0.75),
        psid_rank_next_p90 = weighted_quantile(rank_next, weight, 0.90)
    ) %>%
    group_by(bracket) %>%
    summarise(across(starts_with("psid_rank_next"), mean))

# Rank-rank correlations, model
model_rank_corr <- model_micro_data %>%
    filter(year >= 1984) %>%
    #filter(year == 2007) %>%
    group_by(year) %>%
    mutate(weight = weight/sum(weight)) %>%
    mutate(wealth_bin = round(10*asinh_wealth)) %>%
    inner_join(model_params %>% select(-asinh_wealth)) %>%
    inner_join(model_all_data %>% select(year, wealth_bin, sd_income, mean_income, ito_adj_income)) %>%
    mutate(drift = 5*(drift_corr_per2 + mean_income + ito_adj_income)) %>%
    mutate(diffu = sqrt(5)*sqrt(2*diffu_star_smooth + sd_income^2)) %>%
    mutate(asinh_wealth_next = rnorm(n(), mean = asinh_wealth + drift, sd = diffu)) %>%
    arrange(year, asinh_wealth) %>%
    mutate(rank = (cumsum(weight) - weight/2)/sum(weight)) %>%
    arrange(year, asinh_wealth_next) %>%
    mutate(rank_next = (cumsum(weight) - weight/2)/sum(weight)) %>%
    mutate(bracket = cut(rank, breaks = c(0, 0.50, 0.90, 0.95, 1), labels = FALSE)) %>%
    group_by(year, bracket) %>%
    summarise(
        rank_next_p10 = weighted_quantile(rank_next, weight, 0.10),
        rank_next_p25 = weighted_quantile(rank_next, weight, 0.25),
        rank_next_p50 = weighted_quantile(rank_next, weight, 0.50),
        rank_next_p75 = weighted_quantile(rank_next, weight, 0.75),
        rank_next_p90 = weighted_quantile(rank_next, weight, 0.90)
    ) %>%
    group_by(bracket) %>%
    summarise(across(starts_with("rank_next"), mean))

pdf(here("graphs", "06-analyze-panels", "mobility-rank-psid.pdf"), height = 3, width = 3.5)
p <- model_rank_corr %>% left_join(psid_rank_corr) %>% ggplot() +
    geom_linerange(aes(x = bracket - 0.1, ymin = -log(1 - rank_next_p25), ymax = -log(1 - rank_next_p75), color = "model"), alpha = 1, size = 1) +
    geom_point(aes(x = bracket - 0.1, y = -log(1 - rank_next_p50), color = "model"), alpha = 1) +

    geom_linerange(aes(x = bracket + 0.1, ymin = -log(1 - psid_rank_next_p25), ymax = -log(1 - psid_rank_next_p75), color = "PSID"), alpha = 1, size = 1) +
    geom_point(aes(x = bracket + 0.1, y = -log(1 - psid_rank_next_p50), color = "PSID"), alpha = 1) +

    annotate("text", x = 1.5, y = -log(1 - 0.85), label = "Model", color = "#2F4858", size = 3) +
    annotate("segment", x = 1.6, xend = 1.85, y = -log(1 - 0.82), yend = -log(1 - 0.7), color = "#2F4858",
        arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +

    annotate("text", x = 2.5, y = -log(1 - 0.5), label = "PSID", color = "#A31621", size = 3) +
    annotate("segment", x = 2.4, xend = 2.18, y = -log(1 - 0.58), yend = -log(1 - 0.7), color = "#A31621",
        arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +

    annotate("text", x = 3.4, y = -log(1 - 0.5), label = "Median\nrank", color = "#000000", size = 3) +
    annotate("segment", x = 3.4, xend = 2.95, y = -log(1 - 0.65), yend = -log(1 - 0.86), color = "#000000",
        arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +
    annotate("segment", x = 3.4, xend = 3.15, y = -log(1 - 0.65), yend = -log(1 - 0.88), color = "#000000",
        arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +

    annotate("text", x = 2.5, y = -log(1 - 0.98), label = "50% in\nthis range", color = "#000000", size = 3) +
    annotate("segment", x = 2.5, xend = 2.85, y = -log(1 - 0.97), yend = -log(1 - 0.92), color = "#000000",
        arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +
    annotate("segment", x = 2.5, xend = 3.05, y = -log(1 - 0.97), yend = -log(1 - 0.94), color = "#000000",
        arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open")) +

    scale_color_manual(values = c(`model` = "#2F4858", `PSID` = "#A31621")) +
    scale_y_continuous(
        name = "wealth rank, year t + 5",
        limits = c(0, -log(1 - 0.99)),
        breaks = -log(1 - c(0, 0.9, 0.99)),
        minor_breaks = -log(1 - c(seq(0, 0.9, 0.1), seq(0.91, 0.99, 0.01))),
        labels = paste0(100*c(0, 0.9, 0.99), "%")
    ) +
    scale_x_continuous(name = "wealth bracket, year t", labels = c("0-\n50%", "50-\n90%", "90-\n95%", "95-\n100%"), minor_breaks = NULL) +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
print(p)
dev.off()

# ---------------------------------------------------------------------------- #
# Table for the propensity to consume from different sources
# ---------------------------------------------------------------------------- #

model_savings <- model_micro_data %>%
    group_by(year) %>%
    mutate(weight = weight/sum(weight)) %>%
    mutate(wealth_bin = round(10*asinh_wealth)) %>%
    inner_join(model_params %>% select(-asinh_wealth)) %>%
    # Calculate true mean consumption (correcting for the effect of the
    # asinh transform on the drift)
    mutate(conso = -if_else(year <= year_pivot,
        (drift_corr_per1 + diffu_star_smooth*wealth/sqrt(1 + wealth^2))*sqrt(1 + wealth^2),
        (drift_corr_per2 + diffu_star_smooth*wealth/sqrt(1 + wealth^2))*sqrt(1 + wealth^2),
    )) %>%
    arrange(year, wealth) %>%
    mutate(rank = 100*(cumsum(weight) - weight/2)/sum(weight)) %>%
    mutate(bracket = cut(rank, breaks = c(0, 50, 90, 99, 100), include.lowest = TRUE, labels = FALSE)) %>%
    group_by(year, bracket) %>%
    summarise(across(c(income, gains, conso, wealth), ~ weighted.mean(., weight))) %>%
    mutate(period = if_else(year <= year_pivot, glue("1962-{year_pivot}"), glue("{year_pivot + 1}-2019"))) %>%
    group_by(bracket, period) %>%
    summarise(
        wealth = mean(wealth),
        income = mean((income + gains)/wealth),
        conso = mean(conso/wealth)
    )

model_sd_savings <- model_micro_data %>%
    group_by(year) %>%
    mutate(weight = weight/sum(weight)) %>%
    mutate(wealth_bin = round(10*asinh_wealth)) %>%
    inner_join(model_params %>% select(-asinh_wealth)) %>%
    inner_join(model_all_data %>% select(year, wealth_bin, sd_income)) %>%
    # Calculate sd of conso
    mutate(sd_conso = sqrt(2*diffu_star_smooth)*sqrt(1 + wealth^2)) %>%
    mutate(sd_savings = sqrt(2*diffu_star_smooth + sd_income^2)*sqrt(1 + wealth^2)) %>%
    arrange(year, wealth) %>%
    mutate(rank = 100*(cumsum(weight) - weight/2)/sum(weight)) %>%
    mutate(bracket = cut(rank, breaks = c(0, 50, 90, 99, 100), include.lowest = TRUE, labels = FALSE)) %>%
    group_by(year, bracket) %>%
    summarise(across(c(wealth, sd_conso, sd_savings), ~ weighted.mean(., weight))) %>%
    group_by(bracket) %>%
    summarise(
        sd_conso = mean(sd_conso/wealth),
        sd_savings = mean(sd_savings/wealth)
    )

sink(here("graphs", "06-analyze-panels", "consumption.tex"))
cat(glue("

\\begin{tabular}{@{}cccccc@{}}
\\toprule
                      &          & \\multicolumn{2}{c}{Model}                                                               & SCF                                               & PSID                                               \\\\
                      &          & 1962--1977                                 & 1978--2019                                 & 2007--2009                                        & 1984--2019                                         \\\\ \\midrule
\\multirow{4}{*}{Mean} & 50--90\\% & <<round(100*model_savings[3, 'conso'])>>\\% & <<round(100*model_savings[4, 'conso'])>>\\% & <<round(100*scf_params_perc[2, 'mean_conso'])>>\\% & <<round(100*psid_params_perc[2, 'mean_conso'])>>\\% \\\\ \\cmidrule(l){2-6}
                      & 90--99\\% & <<round(100*model_savings[5, 'conso'])>>\\% & <<round(100*model_savings[6, 'conso'])>>\\% & <<round(100*scf_params_perc[3, 'mean_conso'])>>\\% & <<round(100*psid_params_perc[3, 'mean_conso'])>>\\% \\\\ \\cmidrule(l){2-6}
                      & Top 1\\%  & <<round(100*model_savings[7, 'conso'])>>\\% & <<round(100*model_savings[8, 'conso'])>>\\% & <<round(100*scf_params_perc[4, 'mean_conso'])>>\\% & <<round(100*psid_params_perc[4, 'mean_conso'])>>\\% \\\\ \\midrule
\\multirow{4}{*}{SD}   & 50--90\\% & \\multicolumn{2}{c}{<<round(100*model_sd_savings[2, 'sd_conso'])>>\\%}                    & <<round(100*scf_params_perc[2, 'sd_conso'])>>\\%   & <<round(100*psid_params_perc[2, 'sd_conso'])>>\\%   \\\\ \\cmidrule(l){2-6}
                      & 90--99\\% & \\multicolumn{2}{c}{<<round(100*model_sd_savings[3, 'sd_conso'])>>\\%}                    & <<round(100*scf_params_perc[3, 'sd_conso'])>>\\%   & <<round(100*psid_params_perc[3, 'sd_conso'])>>\\%   \\\\ \\cmidrule(l){2-6}
                      & Top 1\\%  & \\multicolumn{2}{c}{<<round(100*model_sd_savings[4, 'sd_conso'])>>\\%}                    & <<round(100*scf_params_perc[4, 'sd_conso'])>>\\%   & <<round(100*psid_params_perc[4, 'sd_conso'])>>\\%   \\\\ \\bottomrule
\\end{tabular}

", .open = "<<", .close = ">>"))
sink()
