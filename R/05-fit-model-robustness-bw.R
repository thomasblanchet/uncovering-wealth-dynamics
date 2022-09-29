# ---------------------------------------------------------------------------- #
# Check robustness of the results to adjusting bandwidth parameters
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(progressr)
library(glue)
library(latex2exp)
library(cowplot)
library(scales)
library(cmdstanr)

handlers(handler_progress(
    format = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta"
))
options(dplyr.summarise.inform = FALSE)

deming         <- read_rds(here("work", "01-utils", "deming.rds"))
winsorize      <- read_rds(here("work", "01-utils", "winsorize.rds"))
rectangular    <- read_rds(here("work", "01-utils", "rectangular.rds"))
nreg_drv0_grid <- read_rds(here("work", "01-utils", "nreg_drv0_grid.rds"))
nreg_drv0      <- read_rds(here("work", "01-utils", "nreg_drv0.rds"))
nreg_drv1_grid <- read_rds(here("work", "01-utils", "nreg_drv1_grid.rds"))
nreg_drv1      <- read_rds(here("work", "01-utils", "nreg_drv1.rds"))
weighted_sd    <- read_rds(here("work", "01-utils", "weighted_sd.rds"))
weighted_var   <- read_rds(here("work", "01-utils", "weighted_var.rds"))
weighted_cov   <- read_rds(here("work", "01-utils", "weighted_cov.rds"))

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

estimate_distribution_income <- read_rds(here("work", "04-estimate-distribution-income", "estimate_distribution_income.rds"))
estimate_distribution_wealth <- read_rds(here("work", "04-estimate-distribution-wealth", "estimate_distribution_wealth.rds"))

inheritance_rate <- read_rds(here("work", "04-estimate-distribution-inheritance", "inheritance_rate.rds"))

model_micro_data <- read_rds(here("work", "04-prepare-data", "model_micro_data.rds"))
dx               <- read_rds(here("work", "04-prepare-data", "dx.rds"))
grid_cutpoints   <- read_rds(here("work", "04-prepare-data", "grid_cutpoints.rds"))
grid_midpoints   <- read_rds(here("work", "04-prepare-data", "grid_midpoints.rds"))
year_pivot       <- read_rds(here("work", "04-prepare-data", "year_pivot.rds"))

model_all_data     <- read_rds(here("work", "05-prepare-data-model", "model_all_data.rds"))
prepare_data_model <- read_rds(here("work", "05-prepare-data-model", "prepare_data_model.rds"))
pop_growth         <- read_rds(here("work", "05-prepare-data-model", "pop_growth.rds"))
pop_death          <- read_rds(here("work", "05-prepare-data-model", "pop_death.rds"))

dir.create(here("work", "05-fit-model-robustness-bw"), showWarnings = FALSE)
dir.create(here("graphs", "05-fit-model-robustness-bw"), showWarnings = FALSE)

# ---------------------------------------------------------------------------- #
# Function to create alternative versions of the dataset using different
# bandwidths
# ---------------------------------------------------------------------------- #

model_distribution_birth       <- model_distribution_birth       %>% mutate(wealth_bin = round(10*asinh_wealth))
model_distribution_death       <- model_distribution_death       %>% mutate(wealth_bin = round(10*asinh_wealth))
model_distribution_divorce     <- model_distribution_divorce     %>% mutate(wealth_bin = round(10*asinh_wealth))
model_distribution_marriage    <- model_distribution_marriage    %>% mutate(wealth_bin = round(10*asinh_wealth))
model_distribution_income      <- model_distribution_income      %>% mutate(wealth_bin = round(10*asinh_wealth))
model_distribution_inheritance <- model_distribution_inheritance %>% mutate(wealth_bin = round(10*asinh_wealth))
model_distribution_wealth      <- model_distribution_wealth      %>% mutate(wealth_bin = round(10*asinh_wealth))

estimate_alternate_parameters <- function(bw_income_t, bw_sd_income_w,
                                          bw_log_density, bw_log_density_smooth,
                                          bw_deriv_log_density, bw_deriv_log_ccdf,
                                          bw_survival,
                                          bw_effects, bw_sd_delta,
                                          bw_diffu) {

    # Prepare data with alternative bandwidths
    model_distribution_income <- estimate_distribution_income(bw_income_t, bw_sd_income_w)
    model_distribution_wealth <- estimate_distribution_wealth(bw_log_density, bw_log_density_smooth,
        bw_deriv_log_density, bw_deriv_log_ccdf, bw_survival)
    model_all_data <- prepare_data_model(bw_effects, bw_sd_delta)

    # Estimate parameters
    model_params <- model_all_data %>% group_by(wealth_bin) %>% group_split() %>% map_dfr(~ {
        # Ratio of variances for the Deming regression
        delta <- (mean(.x$sd_y)/mean(.x$sd_x))^2

        # Partial out the effect of the period
        po_fit_y <- lm(y ~ period, data = .x)
        po_coef_y_per1 <- coef(po_fit_y)["(Intercept)"]
        po_coef_y_per2 <- coef(po_fit_y)["(Intercept)"] + coef(po_fit_y)["period"]
        .x$y_po_pred <- predict(po_fit_y)
        .x$y_po <- .x$y - .x$y_po_pred

        po_fit_x <- lm(x ~ period, data = .x)
        po_coef_x_per1 <- coef(po_fit_x)["(Intercept)"]
        po_coef_x_per2 <- coef(po_fit_x)["(Intercept)"] + coef(po_fit_x)["period"]
        .x$x_po_pred <- predict(po_fit_x)
        .x$x_po <- .x$x - .x$x_po_pred

        # Fit the regression
        fit <- deming(.x$y_po, .x$x_po, delta = delta)
        drift_per1 <- fit$intercept + po_coef_y_per1 - po_coef_x_per1*fit$slope
        drift_per2 <- fit$intercept + po_coef_y_per2 - po_coef_x_per2*fit$slope
        diffu <- -fit$slope

        return(tibble(
            wealth_bin = first(.x$wealth_bin),
            asinh_wealth = first(.x$asinh_wealth),
            drift_star_per1 = drift_per1,
            drift_star_per2 = drift_per2,
            diffu_star = diffu,
            delta = delta
        ))
    })

    # Smoothing + derivative calculation for diffusion parameter
    model_params <- model_params %>%
        mutate(diffu_star_smooth = nreg_drv0(asinh_wealth, diffu_star, bw = bw_diffu)$y) %>%
        mutate(diffu_star_deriv = nreg_drv1(asinh_wealth, diffu_star, bw = bw_diffu)$y)

    # Having smoothed diffusion, recalculate the drift
    model_params <- model_all_data %>% select(-asinh_wealth) %>% left_join(model_params) %>%
        group_by(wealth_bin) %>%
        summarise(
            asinh_wealth = first(asinh_wealth),
            diffu_star = first(diffu_star),
            diffu_star_smooth = first(diffu_star_smooth),
            diffu_star_deriv = first(diffu_star_deriv),
            delta = first(delta),

            drift_star_per1 = mean(y[period == 0]) + diffu_star_smooth*mean(x[period == 0]),
            drift_star_per2 = mean(y[period == 1]) + diffu_star_smooth*mean(x[period == 1])
        ) %>%
        # And then make the correction for diffusion
        mutate(drift_corr_per1 = drift_star_per1 + diffu_star_deriv) %>%
        mutate(drift_corr_per2 = drift_star_per2 + diffu_star_deriv) %>%
        ungroup()

    return(model_params)
}

# ---------------------------------------------------------------------------- #
# Create variants by adjusting bandwidth parameters at random
# ---------------------------------------------------------------------------- #

coef_adj <- c(1/5, 1/4, 1/3, 1/2, 1, 2, 3, 4, 5)
nvariants <- 5

set.seed(19920902)
with_progress({
    p <- progressor(steps = nvariants)

    model_params_variants <- map_dfr(1:nvariants, function(i) {
        bw_income_t           <- 12.5 * sample(size = 1, coef_adj)
        bw_sd_income_w        <- 1    * sample(size = 1, coef_adj)
        bw_log_density        <- 0.2  * sample(size = 1, coef_adj)
        bw_log_density_smooth <- 1.5  * sample(size = 1, coef_adj)
        bw_deriv_log_density  <- 5    * sample(size = 1, coef_adj)
        bw_deriv_log_ccdf     <- 15   * sample(size = 1, coef_adj)
        bw_survival           <- 2    * sample(size = 1, coef_adj)
        bw_effects            <- 10   * sample(size = 1, coef_adj)
        bw_sd_delta           <- 5    * sample(size = 1, coef_adj)
        bw_diffu              <- 0.5  * sample(size = 1, coef_adj[coef_adj >= 1])

        model_params <- estimate_alternate_parameters(bw_income_t, bw_sd_income_w,
            bw_log_density, bw_log_density_smooth, bw_deriv_log_density,
            bw_deriv_log_ccdf, bw_survival, bw_effects, bw_sd_delta, bw_diffu)

        p()

        return(model_params %>% mutate(i = i))
    })
})

write_rds(model_params_variants, here("work", "05-fit-model-robustness-bw", "model_params_variants.rds"))

# ---------------------------------------------------------------------------- #
# Plot results
# ---------------------------------------------------------------------------- #

model_params_variants <- read_rds(here("work", "05-fit-model-robustness-bw", "model_params_variants.rds"))

pdf(here("graphs", "05-fit-model-robustness-bw", "robustness-diffu-bw.pdf"), width = 5, height = 4)
print(model_params_variants %>%
    group_by(asinh_wealth) %>%
    summarise(
        min = min(diffu_star_smooth),
        max = max(diffu_star_smooth),
        p10 = quantile(diffu_star_smooth, probs = 0.10),
        p90 = quantile(diffu_star_smooth, probs = 0.90)
    ) %>%
    ggplot() +
    geom_ribbon(aes(x = asinh_wealth, ymin = sqrt(2*min), max = sqrt(2*max), fill = "Full range")) +
    geom_ribbon(aes(x = asinh_wealth, ymin = sqrt(2*p10), max = sqrt(2*p90), fill = "80% of estimates")) +
    scale_fill_manual(name = NULL, values = c(`Full range` = "#999999", `90% of estimates` = "#666666")) +
    scale_x_continuous(
        name = NULL,
        breaks = asinh(c(-1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-1, 2000))
    ) +
    scale_y_continuous(name = expression(sigma), limits = c(0, 0.8)) +
    theme_bw() +
    theme(legend.position = "bottom"))
dev.off()

pdf(here("graphs", "05-fit-model-robustness-bw", "robustness-drift1-bw.pdf"), width = 5, height = 4)
print(model_params_variants %>%
    group_by(asinh_wealth) %>%
    summarise(
        min = min(drift_corr_per1),
        max = max(drift_corr_per1),
        p10 = quantile(drift_corr_per1, probs = 0.10),
        p90 = quantile(drift_corr_per1, probs = 0.90)
    ) %>%
    ggplot() +
    geom_ribbon(aes(x = asinh_wealth, ymin = min, max = max, fill = "Full range")) +
    geom_ribbon(aes(x = asinh_wealth, ymin = p10, max = p90, fill = "80% of estimates")) +
    scale_fill_manual(name = NULL, values = c(`Full range` = "#999999", `90% of estimates` = "#666666")) +
    scale_x_continuous(
        name = NULL,
        breaks = asinh(c(-1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-1, 2000))
    ) +
    scale_y_continuous(name = expression(mu[0]), limits = c(-1, 0)) +
    theme_bw() +
    theme(legend.position = "bottom"))
dev.off()

pdf(here("graphs", "05-fit-model-robustness-bw", "robustness-drift2-bw.pdf"), width = 5, height = 4)
print(model_params_variants %>%
    group_by(asinh_wealth) %>%
    summarise(
        min = min(drift_corr_per2),
        max = max(drift_corr_per2),
        p10 = quantile(drift_corr_per2, probs = 0.10),
        p90 = quantile(drift_corr_per2, probs = 0.90)
    ) %>%
    ggplot() +
    geom_ribbon(aes(x = asinh_wealth, ymin = min, max = max, fill = "Full range")) +
    geom_ribbon(aes(x = asinh_wealth, ymin = p10, max = p90, fill = "80% of estimates")) +
    scale_fill_manual(name = NULL, values = c(`Full range` = "#999999", `90% of estimates` = "#666666")) +
    scale_x_continuous(
        name = NULL,
        breaks = asinh(c(-1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-1, 2000))
    ) +
    scale_y_continuous(name = expression(mu[1]), limits = c(-1, 0)) +
    theme_bw() +
    theme(legend.position = "bottom"))
dev.off()

# ---------------------------------------------------------------------------- #
# Make summary plot
# ---------------------------------------------------------------------------- #

model_params <- read_rds(here("work", "05-fit-model", "model_params.rds"))

pdf(here("graphs", "05-fit-model-robustness-bw", "robustness-bw.pdf"), width = 3.5, height = 3.5)
model_params_variants %>%
    bind_rows(model_params %>% select(asinh_wealth, drift_corr_per1, drift_corr_per2, diffu_star_smooth)) %>%
    group_by(asinh_wealth) %>%
    mutate(
        wealth = sinh(asinh_wealth),
        drift_corr_per1 = -drift_corr_per1 - diffu_star_smooth*wealth/sqrt(1 + wealth^2),
        drift_corr_per2 = -drift_corr_per2 - diffu_star_smooth*wealth/sqrt(1 + wealth^2)
    ) %>%
    summarise(across(
        c(diffu_star_smooth, drift_corr_per1, drift_corr_per2),
        list(min = ~ pmax(quantile(.x, probs = 0.025), 0), max = ~ pmin(quantile(.x, probs = 0.975), 1)), .names = "{.fn}_{.col}")) %>%
    ggplot() +
    theme_bw() +

    annotate("text", label = "std. deviation", x = asinh(20), y = 0.95, color = "#F26419", size = 4) +
    annotate("segment", x = asinh(30), xend = asinh(3), y = 0.9, yend = 0.60, color = "#F26419",
        arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

    annotate("text", label = "mean\n(1962-1978)", x = asinh(300), y = 0.8, color = "#2F4858", size = 4, lineheight = 0.85) +
    annotate("segment", x = asinh(300), xend = asinh(1500), y = 0.75, yend = 0.21, color = "#2F4858",
        arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

    annotate("text", label = "mean\n(1979-2019)", x = asinh(70), y = 0.60, color = "#86BBD8", size = 4, lineheight = 0.85) +
    annotate("segment", x = asinh(70), xend = asinh(500), y = 0.55, yend = 0.10, color = "#86BBD8",
        arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

    geom_ribbon(aes(x = asinh_wealth, ymin = sqrt(2*min_diffu_star_smooth), ymax = sqrt(2*max_diffu_star_smooth)),
        fill = "#F26419", color = "#F26419", alpha = 0.5) +

    geom_ribbon(aes(x = asinh_wealth, ymin = min_drift_corr_per2, ymax = max_drift_corr_per2),
        fill = "#86BBD8", color = "#86BBD8", alpha = 0.5) +

    geom_ribbon(aes(x = asinh_wealth, ymin = min_drift_corr_per1, ymax = max_drift_corr_per1),
        fill = "#2F4858", color = "#2F4858", alpha = 0.5) +

    scale_x_continuous(
        name = "wealth (multiple of average income)",
        breaks = asinh(c(0, 1, 10, 100, 1000, 10000)),
        minor_breaks = asinh(c(seq(-1, 1, 1), seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100), seq(1000, 10000, 1000))),
        #minor_breaks = NULL,
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-1, 2000))
    ) +

    scale_y_continuous(name = TeX("$\\sqrt{1 + wealth^2}$"),
        breaks = seq(0, 1, 0.1), minor_breaks = NULL,
        labels = label_percent(accuracy = 1), limits = c(0, 1)) +

    theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
dev.off()
