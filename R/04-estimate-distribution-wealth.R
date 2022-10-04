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

rectangular    <- read_rds(here("work", "01-utils", "rectangular.rds"))
nreg_drv0_grid <- read_rds(here("work", "01-utils", "nreg_drv0_grid.rds"))
nreg_drv1_grid <- read_rds(here("work", "01-utils", "nreg_drv1_grid.rds"))
nreg_drv0      <- read_rds(here("work", "01-utils", "nreg_drv0.rds"))
nreg_drv1      <- read_rds(here("work", "01-utils", "nreg_drv1.rds"))

model_micro_data <- read_rds(here("work", "04-prepare-data", "model_micro_data.rds"))

dx             <- read_rds(here("work", "04-prepare-data", "dx.rds"))
grid_cutpoints <- read_rds(here("work", "04-prepare-data", "grid_cutpoints.rds"))
grid_midpoints <- read_rds(here("work", "04-prepare-data", "grid_midpoints.rds"))
year_pivot     <- read_rds(here("work", "04-prepare-data", "year_pivot.rds"))

# ---------------------------------------------------------------------------- #
# Estimate density (histogram + smoothing)
# ---------------------------------------------------------------------------- #

estimate_distribution_wealth <- function(bw_log_density, bw_log_density_smooth,
                                         bw_deriv_log_density, bw_deriv_log_ccdf,
                                         bw_survival) {

    #bw_log_density        <- 0.2
    #bw_log_density_smooth <- 1.5
    #bw_deriv_log_density  <- 5
    #bw_deriv_log_ccdf     <- 15
    #bw_survival           <- 2

    model_distribution_wealth <- model_micro_data %>%
        group_by(year) %>%
        mutate(weight = weight/sum(weight)) %>%
        mutate(bin = findInterval(asinh_wealth, grid_cutpoints)) %>%
        filter(bin >= 1 & bin < length(grid_cutpoints)) %>%
        group_by(year, bin) %>%
        summarise(density = sum(weight)/dx) %>%
        mutate(log_density = log(density)) %>%
        group_by(year) %>%
        complete(bin = seq_along(grid_midpoints)) %>%
        arrange(year, bin) %>%
        mutate(
            asinh_wealth = grid_midpoints[bin],
            density = if_else(is.na(density), 0, density)
        ) %>%
        mutate(log_density_smooth = nreg_drv0(asinh_wealth, log_density, bw = bw_log_density)$y) %>%
        mutate(deriv_log_density = nreg_drv1(asinh_wealth, log_density_smooth, bw = bw_log_density_smooth)$y) %>%
        mutate(density_smooth = exp(log_density_smooth)) %>%
        mutate(wealth = sinh(asinh_wealth)) %>%
        mutate(include = if_else(wealth >= -1  & wealth <= 2000, TRUE, FALSE)) %>%
        mutate(log_density_smooth = if_else(include, log_density_smooth, NA_real_)) %>%
        mutate(deriv_log_density = if_else(include, deriv_log_density, NA_real_))

    if (any(model_distribution_wealth$density == 0 & model_distribution_wealth$include)) {
        stop("some included bins have no observations")
    }

    model_distribution_wealth <- model_distribution_wealth %>%
        group_by(bin) %>%
        arrange(bin, year) %>%
        mutate(deriv_log_density_smooth = nreg_drv0(year, deriv_log_density, bw = bw_deriv_log_density)$y)

    # ------------------------------------------------------------------------ #
    # Estimate the CDF and its time derivative
    # ------------------------------------------------------------------------ #

    model_distribution_wealth <- model_distribution_wealth %>%
        group_by(year) %>%
        arrange(year, asinh_wealth) %>%
        mutate(cdf = dx*(cumsum(density) - density/2)) %>%
        mutate(cdf = if_else(abs(cdf - 1) < 1e-12, NA_real_, cdf)) %>%
        mutate(ccdf = 1 - cdf) %>%
        mutate(log_ccdf = log(ccdf)) %>%
        ungroup()

    stan_logis_model <- cmdstan_model(here("stan", "logistic-growth-curves.stan"))
    model_distribution_wealth <- model_distribution_wealth %>%
        mutate(period = if_else(year < year_pivot, 0, 1)) %>%
        group_by(bin, period) %>%
        group_split() %>%
        map_dfr(~ {
            if (!first(.x$include)) {
                return(.x)
            }

            n <- nrow(.x)
            y <- .x$log_ccdf
            z <- y - log1p(-exp(y))
            t <- .x$year - min(.x$year)

            stan_logis_fit <- stan_logis_model$optimize(
                data = list(n = n, t = t, y = y),
                init = list(list(z0 = first(z), zinf = last(z), half_life = 10, gamma = 0.5, sigma = 0.1)),
                iter = 1e6
            )

            if (stan_logis_fit$return_codes() != 0) {
                stop(glue("problem with bin {first(.x$bin)}"))
            }

            y0   <- stan_logis_fit$mle()["y0"]
            yinf <- stan_logis_fit$mle()["yinf"]
            rate <- stan_logis_fit$mle()["rate"]

            # Calculate function and its derivative
            .x <- .x %>%
                mutate(log_ccdf_logis = yinf/(1 + (yinf/y0 - 1)*exp(-rate*t))) %>%
                mutate(log_ccdf_logis_deriv = (exp(rate*t)*rate*y0*yinf*(-y0 + yinf))/((-1 + exp(rate*t))*y0 + yinf)^2)

            return(.x)
        })

    # Divide by survival function to get LHS derivative
    model_distribution_wealth <- model_distribution_wealth %>%
        group_by(bin) %>%
        arrange(bin, year) %>%
        mutate(inv_survival = nreg_drv0(year, ccdf/density, bw = bw_survival)$y) %>%
        mutate(lhs_deriv = log_ccdf_logis_deriv*inv_survival) %>%
        mutate(dt = c(0, diff(year))) %>%
        mutate(ccdf_time_smooth = cumsum(density*lhs_deriv*dt)) %>%
        mutate(ccdf_time_smooth = ccdf_time_smooth - mean(ccdf_time_smooth - ccdf, na.rm = TRUE))

    return(model_distribution_wealth)
}

model_distribution_wealth <- estimate_distribution_wealth(0.2, 1.5, 5, 15, 2)

# ---------------------------------------------------------------------------- #
# Plot distributions
# ---------------------------------------------------------------------------- #

dir.create(here("graphs", "04-estimate-distribution-wealth"), showWarnings = FALSE)

with_progress({
    p <- progressor(along = unique(model_distribution_wealth$year))
    pdf(file = here("graphs", "04-estimate-distribution-wealth", "distribution-density-cdf-wealth.pdf"), height = 12, width = 6)
    model_distribution_wealth %>% group_by(year) %>% group_split() %>% walk(~ {
        plot1 <- .x %>% ggplot() +
            annotate("rect", xmin = asinh(-1), xmax = asinh(2000), ymin = -Inf, ymax = Inf,
                fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
            geom_col(aes(x = asinh_wealth, y = 15 + log_density, fill = include, color = include)) +
            geom_line(aes(x = asinh_wealth, y = 15 + log_density_smooth), na.rm = TRUE, size = 1, color = "#000000") +
            scale_y_continuous(
                name = expression(f(w)),
                breaks = seq(0, 15 + 1, 2),
                labels = math_format(format = function(.) . - 15),
                limits = c(0, 15 + 1)
            ) +
            scale_x_continuous(
                name = NULL,
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
            geom_line(aes(x = asinh_wealth, y = deriv_log_density), na.rm = TRUE, size = 1, color = "#000000") +
            scale_y_continuous(name = expression(partialdiff[w]~log(f(w))), limits = c(-3, 4), breaks = seq(-3, 4, 1)) +
            scale_x_continuous(
                name = NULL,
                breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
                labels = function(.) round(sinh(.)),
                limits = asinh(c(-2, 5000))
            ) +
            theme_bw() +
            theme(legend.position = "none")

        plot3 <- .x %>% ggplot() +
            annotate("rect", xmin = asinh(-1), xmax = asinh(2000), ymin = -Inf, ymax = Inf,
                fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
            geom_line(aes(x = asinh_wealth, y = lhs_deriv), na.rm = TRUE, size = 1, color = "#000000") +
            scale_y_continuous(
                name = expression(-partialdiff[t]~F(w)/f(w)),
                limits = c(-0.15, 0.15),
                breaks = seq(-0.15, 0.15, 0.05),
                labels = comma_format()
            ) +
            scale_x_continuous(
                name = NULL,
                breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
                labels = function(.) round(sinh(.)),
                limits = asinh(c(-2, 5000))
            ) +
            theme_bw() +
            theme(legend.position = "none")

        print(plot_grid(plot1, plot2, plot3, ncol = 1, align = "v", axis = "l"))
        p()
    })
    dev.off()
})

with_progress({
    p <- progressor(along = unique(model_distribution_wealth$bin[model_distribution_wealth$include]))
    pdf(file = here("graphs", "04-estimate-distribution-wealth", "distribution-cdf-time.pdf"), height = 12, width = 6)
    model_distribution_wealth %>% filter(include) %>% group_by(bin) %>% group_split() %>% walk(~ {
        plot1 <- .x %>% ggplot() +
            geom_line(aes(x = year, y = 14 + log(ccdf)), size = 0.7, color = "#CCCCCC") +
            geom_line(aes(x = year, y = 14 + log(ccdf_time_smooth)), size = 1, color = "#000000") +
            scale_y_continuous(
                name = expression(1 - F(w)),
                breaks = seq(0, 14, 2),
                labels = math_format(format = function(.) . - 14) #,
                # limits = c(0, 14)
            ) +
            scale_x_continuous(name = NULL, breaks = seq(1910, 2020, 5)) +
            ggtitle(glue("wealth ~ {round(first(.x$wealth), 1)} times average national income")) +
            theme_bw() +
            theme(legend.position = "none")

        plot2 <- .x %>% ggplot() +
            geom_line(aes(x = year, y = lhs_deriv, group = period), na.rm = TRUE, size = 1, color = "#000000") +
            scale_y_continuous(name = expression(-partialdiff[t]~F(w)/f(w)), labels = comma_format()) +
            scale_x_continuous(name = NULL, breaks = seq(1910, 2020, 5)) +
            theme_bw() +
            theme(legend.position = "none")

        plot3 <- .x %>% ggplot() +
            geom_line(aes(x = year, y = deriv_log_density, group = period), na.rm = TRUE, size = 1, color = "#CCCCCC") +
            geom_line(aes(x = year, y = deriv_log_density_smooth, group = period), na.rm = TRUE, size = 1, color = "#000000") +
            scale_y_continuous(name = expression(-partialdiff[w]~f(w)/f(w)), labels = comma_format()) +
            scale_x_continuous(name = NULL, breaks = seq(1910, 2020, 5)) +
            theme_bw() +
            theme(legend.position = "none")

        print(plot_grid(plot1, plot2, plot3, ncol = 1, align = "v", axis = "l"))
        p()
    })
    dev.off()
})

# Select results for presentation
pdf(file = here("graphs", "04-estimate-distribution-wealth", "density-wealth-1978.pdf"), height = 3, width = 4)
print(
    model_distribution_wealth %>% filter(year == 1978) %>% ggplot() +
        geom_col(aes(x = asinh_wealth, y = 13 + log_density, fill = include, color = include), size = 0.3) +
        geom_line(aes(x = asinh_wealth, y = 13 + log_density_smooth), na.rm = TRUE, size = 1, color = "#000000") +
        scale_y_continuous(
            name = expression(log~f[t](w)),
            breaks = seq(1, 20, 2),
            labels = ~ (. - 13),
            minor_breaks = NULL,
            limits = c(0, 14)
        ) +
        scale_x_continuous(
            name = "multiple of average national income",
            breaks = asinh(c(0, 1, 10, 100, 1000, 10000)),
            minor_breaks = NULL,
            labels = function(.) round(sinh(.)),
            limits = asinh(c(-1, 2000))
        ) +
        scale_color_manual(values = c(`FALSE` = "#AAAAAA", `TRUE` = "#AAAAAA")) +
        scale_fill_manual(values = c(`FALSE` = "#CCCCCC", `TRUE` = "#CCCCCC")) +
        theme_bw() +
        theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
)
dev.off()

pdf(file = here("graphs", "04-estimate-distribution-wealth", "density-wealth-2019.pdf"), height = 3, width = 4)
print(
    model_distribution_wealth %>% filter(year == 2019) %>% ggplot() +
        geom_col(aes(x = asinh_wealth, y = 13 + log_density, fill = include, color = include), size = 0.3) +
        geom_line(aes(x = asinh_wealth, y = 13 + log_density_smooth), na.rm = TRUE, size = 1, color = "#000000") +
        scale_y_continuous(
            name = expression(log~f[t](w)),
            breaks = seq(1, 20, 2),
            labels = ~ (. - 13),
            minor_breaks = NULL,
            limits = c(0, 14)
        ) +
        scale_x_continuous(
            name = "multiple of average national income",
            breaks = asinh(c(0, 1, 10, 100, 1000, 10000)),
            minor_breaks = NULL,
            labels = function(.) round(sinh(.)),
            limits = asinh(c(-1, 2000))
        ) +
        scale_color_manual(values = c(`FALSE` = "#AAAAAA", `TRUE` = "#AAAAAA")) +
        scale_fill_manual(values = c(`FALSE` = "#CCCCCC", `TRUE` = "#CCCCCC")) +
        theme_bw() +
        theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
)
dev.off()

pdf(file = here("graphs", "04-estimate-distribution-wealth", "density-wealth-1978-2019.pdf"), height = 3.5, width = 3.5)
print(
    model_distribution_wealth %>% filter(year %in% c(1978, 2019)) %>% ggplot() +
        geom_line(aes(x = asinh_wealth, y = 13 + log_density, color = as.factor(year)), na.rm = TRUE, size = 1) +

        annotate("text", x = asinh(800), y = -6.5 + 13, label = "2019", color = "#F26419", size = 4) +
        annotate("text", x = asinh(100), y = -8.5 + 13, label = "1978", color = "#2F4858", size = 4) +

        scale_y_continuous(
            name = expression(log~f[t](w)),
            breaks = seq(1, 20, 2),
            labels = ~ (. - 13),
            minor_breaks = NULL,
            limits = c(0, 14)
        ) +
        scale_x_continuous(
            name = "multiple of average income",
            breaks = asinh(c(0, 1, 10, 100, 1000, 10000)),
            minor_breaks = asinh(c(seq(-1, 1, 1), seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100), seq(1000, 10000, 1000))),
            #minor_breaks = NULL,
            labels = function(.) round(sinh(.)),
            limits = asinh(c(-1, 2000))
        ) +
        scale_color_manual(values = c(`1978` = "#2F4858", `2019` = "#F26419")) +
        theme_bw() +
        theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
)
dev.off()

pdf(file = here("graphs", "04-estimate-distribution-wealth", "deriv-cdf.pdf"), height = 3.5, width = 3.5)
print(
    model_distribution_wealth %>% filter(bin == 69) %>% ggplot() +
        geom_line(aes(x = year, y = log(ccdf)), size = 0.8, color = "#999999") +
        geom_line(aes(x = year, y = log(ccdf_time_smooth)), size = 1, color = "#2F4858") +

        annotate("text", label = "1978", x = 2000, y = -8.25 + 0.05, color = "#000000", size = 4) +
        annotate("segment", x = 1996, xend = 1978, y = -8.27 + 0.05, yend = -8.5 + 0.05, color = "#000000",
            arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

        annotate("text", label = "raw data", x = 1975, y = -7, color = "#999999", size = 4) +
        annotate("segment", x = 1975, xend = 1970.5, y = -7.1, yend = -7.7, color = "#999999",
            arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

        annotate("text", label = "logistic growth fit", x = 1990, y = -6.5, color = "#2F4858", size = 4) +
        annotate("segment", x = 1990, xend = 1995, y = -6.65, yend = -7.05, color = "#2F4858",
            arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

        scale_y_continuous(
            name = expression(log(1 - F[t](w))),
            breaks = seq(-9, 14, 0.5),
            limits = c(-8.75, -6.25),
            minor_breaks = NULL
        ) +
        scale_x_continuous(name = "year", breaks = seq(1960, 2020, 10), minor_breaks = NULL) +
        theme_bw() +
        theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
)
dev.off()

# ---------------------------------------------------------------------------- #
# Save results
# ---------------------------------------------------------------------------- #

dir.create(here("work", "04-estimate-distribution-wealth"), showWarnings = FALSE)
write_rds(model_distribution_wealth,    here("work", "04-estimate-distribution-wealth", "model_distribution_wealth.rds"))
write_rds(estimate_distribution_wealth, here("work", "04-estimate-distribution-wealth", "estimate_distribution_wealth.rds"))
