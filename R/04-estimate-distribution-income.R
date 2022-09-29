# ---------------------------------------------------------------------------- #
# Estimate densities, CDF, etc. over a tight grid
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(cmdstanr)
library(progressr)
library(scales)
library(glue)
library(cowplot)

handlers(handler_progress(
    format = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta"
))
options(dplyr.summarise.inform = FALSE)

rectangular <- read_rds(here("work", "01-utils", "rectangular.rds"))
nreg_drv0_grid <- read_rds(here("work", "01-utils", "nreg_drv0_grid.rds"))
nreg_drv1_grid <- read_rds(here("work", "01-utils", "nreg_drv1_grid.rds"))
nreg_drv0 <- read_rds(here("work", "01-utils", "nreg_drv0.rds"))
nreg_drv1 <- read_rds(here("work", "01-utils", "nreg_drv1.rds"))
weighted_cov <- read_rds(here("work", "01-utils", "weighted_cov.rds"))
weighted_var <- read_rds(here("work", "01-utils", "weighted_var.rds"))
weighted_sd <- read_rds(here("work", "01-utils", "weighted_sd.rds"))

model_micro_data <- read_rds(here("work", "04-prepare-data", "model_micro_data.rds"))

dx <- read_rds(here("work", "04-prepare-data", "dx.rds"))
grid_cutpoints <- read_rds(here("work", "04-prepare-data", "grid_cutpoints.rds"))
grid_midpoints <- read_rds(here("work", "04-prepare-data", "grid_midpoints.rds"))
year_pivot <- read_rds(here("work", "04-prepare-data", "year_pivot.rds"))

# ---------------------------------------------------------------------------- #
# Calculate income and its variance over the grid
# ---------------------------------------------------------------------------- #

estimate_distribution_income <- function(bw_income_t, bw_sd_income_w) {

    model_distribution_income <- model_micro_data %>%
        group_by(year) %>%
        mutate(weight = weight/sum(weight)) %>%
        mutate(bin = findInterval(asinh_wealth, grid_cutpoints)) %>%
        group_by(year, bin) %>%
        summarise(
            mean_labor   = weighted.mean(ratio_labor, weight, na.rm = TRUE),
            mean_capital = weighted.mean(ratio_capital, weight, na.rm = TRUE),
            mean_gains   = weighted.mean(ratio_gains, weight, na.rm = TRUE),

            mean_pre_labor   = weighted.mean(ratio_pre_labor, weight, na.rm = TRUE),
            mean_pre_capital = weighted.mean(ratio_pre_capital, weight, na.rm = TRUE),

            sd_income = weighted_sd(ratio_income + ratio_gains, weight, na.rm = TRUE),

            var_labor   = weighted_var(ratio_labor, weight, na.rm = TRUE),
            var_capital = weighted_var(ratio_capital, weight, na.rm = TRUE),
            var_gains   = weighted_var(ratio_gains, weight, na.rm = TRUE),

            cov_labor_capital = weighted_cov(ratio_labor, ratio_capital, weight, na.rm = TRUE),
            cov_labor_gains   = weighted_cov(ratio_labor, ratio_gains, weight, na.rm = TRUE),
            cov_capital_gains = weighted_cov(ratio_capital, ratio_gains, weight, na.rm = TRUE),

            growth_effect = weighted.mean(growth_effect, weight, na.rm = TRUE),
            inflation_effect = weighted.mean(inflation_effect, weight, na.rm = TRUE),

            density = sum(weight)/dx
        ) %>%
        group_by(year) %>%
        complete(bin = seq_along(grid_midpoints)) %>%
        arrange(year, bin) %>%
        mutate(asinh_wealth = grid_midpoints[bin]) %>%
        mutate(wealth = sinh(asinh_wealth)) %>%
        mutate(include = if_else(wealth >= -1 & wealth <= 2000, TRUE, FALSE))

    # Smoothed (over time) version of income-induced drift
    model_distribution_income <- model_distribution_income %>%
        mutate(period = if_else(year < year_pivot, 0, 1)) %>%
        group_by(bin, period) %>%
        mutate(across(
            c(mean_labor, mean_capital, mean_gains, mean_pre_labor, mean_pre_capital, growth_effect, inflation_effect),
            ~ nreg_drv0(year, .x, bw = bw_income_t)$y,
            #mean,
            .names = "{.col}_smooth"
        ))

    # Smoothed version of overall income
    model_distribution_income <- model_distribution_income %>%
        mutate(mean_income_smooth = mean_labor_smooth + mean_capital_smooth + mean_gains_smooth + growth_effect_smooth)

    # Smoothed version of income-induced diffusion as well as derivative
    model_distribution_income <- model_distribution_income %>%
        group_by(year) %>%
        arrange(year, asinh_wealth) %>%
        mutate(sd_income_smooth = nreg_drv0(asinh_wealth, sd_income, bw = bw_sd_income_w)$y) %>%
        mutate(sd_income_deriv = nreg_drv1(asinh_wealth, sd_income_smooth, bw = bw_sd_income_w)$y) %>%
        mutate(sd_income_smooth = if_else(include, sd_income_smooth, NA_real_)) %>%
        mutate(sd_income_deriv = if_else(include, sd_income_deriv, NA_real_))

    return(model_distribution_income)
}

model_distribution_income <- estimate_distribution_income(12.5, 1)

# ---------------------------------------------------------------------------- #
# Plot
# ---------------------------------------------------------------------------- #

dir.create(here("graphs", "04-estimate-distribution-income"), showWarnings = FALSE)

# Mean income, by wealth and type of income
with_progress({
    p <- progressor(along = unique(model_distribution_income$year))
    pdf(file = here("graphs", "04-estimate-distribution-income", "distribution-income-mean.pdf"), height = 5, width = 6)
    model_distribution_income %>%
        pivot_longer(c(mean_labor, mean_capital, mean_gains, growth_effect)) %>%
        group_by(year) %>%
        group_split() %>%
        walk(~ {
            print(
                .x %>% ggplot() +
                    annotate("rect", xmin = asinh(10), xmax = asinh(1000), ymin = -Inf, ymax = Inf,
                        fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
                    geom_col(aes(x = asinh_wealth, y = value, fill = name), color = "#333333", size = 0.2, na.rm = TRUE) +
                    scale_y_continuous(
                        name = expression(income/sqrt(1 + wealth^2)),
                        #limits = c(-0.2, 1),
                        #breaks = seq(-0.2, 1, 0.2)
                    ) +
                    scale_x_continuous(
                        name = "wealth",
                        breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
                        labels = function(.) round(sinh(.)),
                        limits = asinh(c(-2, 5000))
                    ) +
                    scale_fill_brewer(
                        name = NULL,
                        type = "qual",
                        palette = "Set1",
                        labels = c(
                            `mean_labor`    = "labor income",
                            `mean_capital`  = "capital income",
                            `mean_gains`    = "capital gains",
                            `growth_effect` = "growth"
                        )
                    ) +
                    ggtitle(glue("{.x$year[1]}")) +
                    theme_bw() +
                    theme(legend.position = "bottom", legend.direction = "horizontal")
            )
            p()
        })
    dev.off()
})

# Standard deviation of income, by wealth, value and derivative
with_progress({
    p <- progressor(along = unique(model_distribution_income$year))
    pdf(file = here("graphs", "04-estimate-distribution-income", "distribution-income-sd.pdf"), height = 8, width = 6)
    model_distribution_income %>%
        group_by(year) %>%
        group_split() %>%
        walk(~ {
            plot1 <- .x %>% ggplot() +
                annotate("rect", xmin = asinh(-1), xmax = asinh(2000), ymin = -Inf, ymax = Inf,
                    fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
                geom_col(aes(x = asinh_wealth, y = pmin(sd_income, 0.5), fill = include, color = include), size = 0.2, na.rm = TRUE) +
                geom_line(aes(x = asinh_wealth, y = sd_income_smooth), na.rm = TRUE, size = 1, color = "#000000") +
                scale_y_continuous(
                    name = "income-induced diffusion",
                    #limits = c(0, 0.5),
                    #breaks = seq(0, 0.5, 0.1)
                ) +
                scale_x_continuous(
                    name = "wealth",
                    breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
                    labels = function(.) round(sinh(.)),
                    limits = asinh(c(-2, 5000))
                ) +
                scale_color_manual(values = c(`FALSE` = "#AAAAAA", `TRUE` = "#666666")) +
                scale_fill_manual(values = c(`FALSE` = "#CCCCCC", `TRUE` = "#999999")) +
                ggtitle(glue("{.x$year[1]}")) +
                theme_bw() +
                theme(legend.position = "none")

        plot2 <- .x %>% ggplot() +
            annotate("rect", xmin = asinh(-1), xmax = asinh(2000), ymin = -Inf, ymax = Inf,
                fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
            geom_line(aes(x = asinh_wealth, y = sd_income_deriv), na.rm = TRUE, size = 1, color = "#000000") +
            scale_y_continuous(
                name = "derivative",
                labels = comma_format(),
                #limits = c(-0.1, 0.1),
                #breaks = seq(-0.1, 0.1, 0.02)
            ) +
            scale_x_continuous(
                name = "wealth",
                breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
                labels = function(.) round(sinh(.)),
                limits = asinh(c(-2, 5000))
            ) +
            ggtitle(glue("{.x$year[1]}")) +
            theme_bw() +
            theme(legend.position = "none")

        print(plot_grid(plot1, plot2, ncol = 1, align = "v", axis = "l"))
        p()
    })
    dev.off()
})

# Income-induced drift and diffusion, time series
with_progress({
    p <- progressor(along = unique(model_distribution_income$bin[model_distribution_income$include]))
    pdf(file = here("graphs", "04-estimate-distribution-income", "distribution-income-time-series.pdf"), height = 8, width = 6)
    model_distribution_income %>% filter(include) %>% group_by(bin) %>% group_split() %>% walk(~ {
        plot1 <- .x %>% ggplot() +
            geom_line(aes(x = year, y = mean_labor + mean_capital, color = "income", group = period), size = 0.5, alpha = 0.5) +
            geom_line(aes(x = year, y = mean_labor_smooth + mean_capital_smooth, color = "income", group = period), size = 0.7) +

            geom_line(aes(x = year, y = mean_gains, color = "capital gains", group = period), size = 0.5, alpha = 0.5) +
            geom_line(aes(x = year, y = mean_gains_smooth, color = "capital gains", group = period), size = 0.7) +

            geom_line(aes(x = year, y = growth_effect, color = "growth", group = period), size = 0.5, alpha = 0.5) +
            geom_line(aes(x = year, y = growth_effect_smooth, color = "growth", group = period), size = 0.7) +

            geom_line(aes(x = year, y = inflation_effect, color = "inflation", group = period), size = 0.5, alpha = 0.5) +
            geom_line(aes(x = year, y = inflation_effect_smooth, color = "inflation", group = period), size = 0.7) +

            scale_y_continuous(name = "drift", labels = comma_format()
                #limits = c(-0.2, 0.25), breaks = seq(-0.2, 0.25, 0.05)
            ) +
            scale_x_continuous(name = NULL, breaks = seq(1960, 2020, 5)) +
            scale_color_brewer(name = NULL, type = "qual", palette = "Set1") +

            ggtitle(glue("wealth ~ {round(first(.x$wealth), 1)} times average national income")) +
            theme_bw() +
            theme(legend.position = "bottom")

        plot2 <- .x %>% ggplot() +
            geom_line(aes(x = year, y = sd_income), size = 0.5, alpha = 0.3) +
            geom_line(aes(x = year, y = sd_income_smooth), size = 0.7) +

            scale_y_continuous(name = "diffusion", labels = comma_format()
                #limits = c(0, 0.2), breaks = seq(0, 0.2, 0.05)
            ) +
            scale_x_continuous(name = NULL, breaks = seq(1960, 2020, 5)) +

            theme_bw() +
            theme(legend.position = "none")

        print(plot_grid(plot1, plot2, ncol = 1, align = "v", axis = "l"))
        p()
    })
    dev.off()
})

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

dir.create(here("work", "04-estimate-distribution-income"), showWarnings = FALSE)

write_rds(model_distribution_income, here("work", "04-estimate-distribution-income", "model_distribution_income.rds"))
write_rds(estimate_distribution_income, here("work", "04-estimate-distribution-income", "estimate_distribution_income.rds"))
