# ---------------------------------------------------------------------------- #
# Check robustness of the results to the parameter delta in the Deming
# regression
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(progressr)
library(glue)
library(latex2exp)
library(cowplot)
library(scales)

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

model_all_data <- read_rds(here("work", "05-prepare-data-model", "model_all_data.rds"))

dir.create(here("graphs", "05-fit-model-robustness-delta"), showWarnings = FALSE)

# ---------------------------------------------------------------------------- #
# Run alternate regressions, where we multiply the benchmark delta by
# 0, 0.5, 1, 2 & +Inf
# ---------------------------------------------------------------------------- #

model_param_robustness <- map_dfr(c(0, 0.2, 0.5, 1, 2, 5, +Inf), function(coef) {
    with_progress({
        p <- progressor(along = unique(model_all_data$wealth_bin))
        model_params <- model_all_data %>% group_by(wealth_bin) %>% group_split() %>% map_dfr(~ {
            # Ratio of variances for the Deming regression
            delta <- (mean(.x$sd_y)/mean(.x$sd_x))^2*coef

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

            p()

            return(tibble(
                wealth_bin = first(.x$wealth_bin),
                asinh_wealth = first(.x$asinh_wealth),
                drift_star_per1 = drift_per1,
                drift_star_per2 = drift_per2,
                diffu_star = diffu,
                delta = delta,
                coef = coef
            ))
        })
    })
    return(model_params)
})

# ---------------------------------------------------------------------------- #
# Smoothing + derivative calculation for diffusion parameter
# ---------------------------------------------------------------------------- #

model_param_robustness <- model_param_robustness %>%
    group_by(coef) %>%
    mutate(diffu_star_smooth = nreg_drv0(asinh_wealth, diffu_star, bw = 1)$y) %>%
    mutate(diffu_star_deriv = nreg_drv1(asinh_wealth, diffu_star, bw = 1)$y)

# Having smoothed diffusion, recalculate the drift
model_param_robustness <- model_all_data %>% select(-asinh_wealth) %>% left_join(model_param_robustness) %>%
    group_by(coef, wealth_bin) %>%
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

# ---------------------------------------------------------------------------- #
# Compare results
# ---------------------------------------------------------------------------- #

param_plot_diffu <- model_param_robustness %>%
    mutate(par = sqrt(2*diffu_star_smooth)) %>%
    pivot_wider(id_cols = asinh_wealth, names_from = coef, values_from = par)

p11 <- ggplot(param_plot_diffu) +
    geom_line(aes(x = asinh_wealth, y = `1`), size = 1, color = "#000000") +
    scale_y_continuous(name = expression(sigma), limits = c(0, 0.8)) +
    scale_x_continuous(
        name = NULL,
        breaks = asinh(c(-1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-1, 2000))
    ) +
    ggtitle(expression("Benchmark" ~ delta == tilde(delta))) +
    theme_bw()
p12 <- ggplot(param_plot_diffu) +
    geom_line(aes(x = asinh_wealth, y = `0.5`), size = 1, color = "#000000") +
    geom_line(aes(x = asinh_wealth, y = `2`), size = 1, color = "#000000") +
    scale_y_continuous(name = NULL, limits = c(0, 0.8)) +
    scale_x_continuous(
        name = NULL,
        breaks = asinh(c(-1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-1, 2000))
    ) +
    ggtitle(expression("Variants" ~ delta %in% group("{", list(tilde(delta)/2, 2*tilde(delta)), "}"))) +
    theme_bw()
p13 <- ggplot(param_plot_diffu) +
    geom_line(aes(x = asinh_wealth, y = `0`), size = 1, color = "#000000") +
    geom_line(aes(x = asinh_wealth, y = `Inf`), size = 1, color = "#000000") +
    scale_y_continuous(name = NULL, limits = c(0, 0.8)) +
    scale_x_continuous(
        name = NULL,
        breaks = asinh(c(-1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-1, 2000))
    ) +
    ggtitle(expression("Variants" ~ delta %in% group("{", list(0, +infinity), "}"))) +
    theme_bw()

param_plot_drift1 <- model_param_robustness %>%
    pivot_wider(id_cols = asinh_wealth, names_from = coef, values_from = drift_corr_per1)

p21 <- ggplot(param_plot_drift1) +
    geom_line(aes(x = asinh_wealth, y = `1`), size = 1, color = "#000000") +
    scale_y_continuous(name = expression(mu[0]), limits = c(-1, 0.2)) +
    scale_x_continuous(
        name = NULL,
        breaks = asinh(c(-1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-1, 2000))
    ) +
    theme_bw()
p22 <- ggplot(param_plot_drift1) +
    geom_line(aes(x = asinh_wealth, y = `0.5`), size = 1, color = "#000000") +
    geom_line(aes(x = asinh_wealth, y = `2`), size = 1, color = "#000000") +
    scale_y_continuous(name = NULL, limits = c(-1, 0.2)) +
    scale_x_continuous(
        name = NULL,
        breaks = asinh(c(-1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-1, 2000))
    ) +
    theme_bw()
p23 <- ggplot(param_plot_drift1) +
    geom_line(aes(x = asinh_wealth, y = `0`), size = 1, color = "#000000") +
    geom_line(aes(x = asinh_wealth, y = `Inf`), size = 1, color = "#000000") +
    scale_y_continuous(name = NULL, limits = c(-1, 0.2)) +
    scale_x_continuous(
        name = NULL,
        breaks = asinh(c(-1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-1, 2000))
    ) +
    theme_bw()

param_plot_drift2 <- model_param_robustness %>%
    pivot_wider(id_cols = asinh_wealth, names_from = coef, values_from = drift_corr_per2)

p31 <- ggplot(param_plot_drift2) +
    geom_line(aes(x = asinh_wealth, y = `1`), size = 1, color = "#000000") +
    scale_y_continuous(name = expression(mu[1]), limits = c(-1, 0.2)) +
    scale_x_continuous(
        name = NULL,
        breaks = asinh(c(-1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-1, 2000))
    ) +
    theme_bw()
p32 <- ggplot(param_plot_drift2) +
    geom_line(aes(x = asinh_wealth, y = `0.5`), size = 1, color = "#000000") +
    geom_line(aes(x = asinh_wealth, y = `2`), size = 1, color = "#000000") +
    scale_y_continuous(name = NULL, limits = c(-1, 0.2)) +
    scale_x_continuous(
        name = NULL,
        breaks = asinh(c(-1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-1, 2000))
    ) +
    theme_bw()
p33 <- ggplot(param_plot_drift2) +
    geom_line(aes(x = asinh_wealth, y = `0`), size = 1, color = "#000000") +
    geom_line(aes(x = asinh_wealth, y = `Inf`), size = 1, color = "#000000") +
    scale_y_continuous(name = NULL, limits = c(-1, 0.2)) +
    scale_x_continuous(
        name = NULL,
        breaks = asinh(c(-1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-1, 2000))
    ) +
    theme_bw()

pdf(here("graphs", "05-fit-model-robustness-delta", "deming-robustness-delta.pdf"), height = 12, width = 16)
print(plot_grid(p11, p12, p13, p21, p22, p23, p31, p32, p33, align = "hv", nrow = 3))
dev.off()

# ---------------------------------------------------------------------------- #
# Plot with range of estimates
# ---------------------------------------------------------------------------- #

pdf(here("graphs", "05-fit-model-robustness-delta", "robustness-delta.pdf"), width = 3.5, height = 3.5)
p <- model_param_robustness %>%
    filter(delta > 0 & !is.infinite(delta)) %>%
    group_by(asinh_wealth) %>%
    mutate(
        wealth = sinh(asinh_wealth),
        drift_corr_per1 = -(drift_corr_per1 + diffu_star_smooth*wealth/sqrt(1 + wealth^2)),
        drift_corr_per2 = -(drift_corr_per2 + diffu_star_smooth*wealth/sqrt(1 + wealth^2))
    ) %>%
    summarise(across(
        c(diffu_star_smooth, drift_corr_per1, drift_corr_per2),
        list(min = min, max = max), .names = "{.fn}_{.col}")) %>%
    ggplot() +
    theme_bw() +

    annotate("text", label = "std. deviation", x = asinh(2), y = 0.9, color = "#F26419", size = 4) +
    annotate("segment", x = asinh(2), xend = asinh(3), y = 0.85, yend = 0.6, color = "#F26419",
        arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

    annotate("text", label = "mean\n(1962-1978)", x = asinh(500), y = 0.35, color = "#2F4858", size = 4, lineheight = 0.85) +
    annotate("segment", x = asinh(500), xend = asinh(400), y = 0.30, yend = 0.21, color = "#2F4858",
        arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

    annotate("text", label = "mean\n(1979-2019)", x = asinh(1), y = 0.1, color = "#86BBD8", size = 4, lineheight = 0.85) +
    annotate("segment", x = asinh(1), xend = asinh(2), y = 0.15, yend = 0.25, color = "#86BBD8",
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
print(p)
dev.off()


