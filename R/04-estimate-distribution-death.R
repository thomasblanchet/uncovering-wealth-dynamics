# ---------------------------------------------------------------------------- #
# Estimate distribution at death
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(progressr)
library(scales)
library(glue)
library(ggpubr)

handlers(handler_progress(
    format = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta"
))
options(dplyr.summarise.inform = FALSE)

rectangular    <- read_rds(here("work", "01-utils", "rectangular.rds"))
nreg_drv0_grid <- read_rds(here("work", "01-utils", "nreg_drv0_grid.rds"))
nreg_drv1_grid <- read_rds(here("work", "01-utils", "nreg_drv1_grid.rds"))
nreg_drv0      <- read_rds(here("work", "01-utils", "nreg_drv0.rds"))
nreg_drv1      <- read_rds(here("work", "01-utils", "nreg_drv1.rds"))
weighted_cov   <- read_rds(here("work", "01-utils", "weighted_cov.rds"))
weighted_var   <- read_rds(here("work", "01-utils", "weighted_var.rds"))
weighted_sd    <- read_rds(here("work", "01-utils", "weighted_sd.rds"))

model_micro_data <- read_rds(here("work", "04-prepare-data", "model_micro_data.rds"))

dx             <- read_rds(here("work", "04-prepare-data", "dx.rds"))
grid_cutpoints <- read_rds(here("work", "04-prepare-data", "grid_cutpoints.rds"))
grid_midpoints <- read_rds(here("work", "04-prepare-data", "grid_midpoints.rds"))
year_pivot     <- read_rds(here("work", "04-prepare-data", "year_pivot.rds"))

# ---------------------------------------------------------------------------- #
# Estimate distribution
# ---------------------------------------------------------------------------- #

model_distribution_death <- model_micro_data %>%
    mutate(weight = weight*qx) %>%
    group_by(year) %>%
    mutate(weight = weight/sum(weight)) %>%
    mutate(bin = findInterval(asinh_wealth, grid_cutpoints)) %>%
    group_by(year, bin) %>%
    summarise(density_death = sum(weight)/dx) %>%
    group_by(year) %>%
    complete(bin = seq_along(grid_midpoints)) %>%
    arrange(year, bin) %>%
    mutate(asinh_wealth = grid_midpoints[bin], density_death = if_else(is.na(density_death), 0, density_death)) %>%
    mutate(wealth = sinh(asinh_wealth)) %>%
    mutate(include = if_else(wealth >= -1 & wealth <= 2000, TRUE, FALSE))

model_distribution_death <- model_distribution_death %>%
    group_by(year) %>%
    arrange(year, asinh_wealth) %>%
    mutate(cdf_death = dx*(cumsum(density_death) - density_death/2)) %>%
    mutate(cdf_death = if_else(abs(cdf_death - 1) < 1e-12, NA_real_, cdf_death)) %>%
    mutate(log_ccdf_death = log(1 - cdf_death)) %>%
    ungroup()

# ---------------------------------------------------------------------------- #
# Plot distribution
# ---------------------------------------------------------------------------- #

dir.create(here("graphs", "04-estimate-distribution-death"), showWarnings = FALSE)

with_progress({
    p <- progressor(along = unique(model_distribution_death$year))
    pdf(file = here("graphs", "04-estimate-distribution-death", "wealth-distribution-death.pdf"), height = 8, width = 6)
    model_distribution_death %>%
        group_by(year) %>% group_split() %>% walk(~ {
            plot1 <- ggplot(.) +
                annotate("rect", xmin = asinh(-1), xmax = asinh(2000), ymin = -Inf, ymax = Inf,
                    fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
                geom_col(aes(x = asinh_wealth, y = 15 + log(density_death), fill = include, color = include)) +
                scale_y_continuous(
                    name = "density",
                    breaks = 0:(15 + 1),
                    labels = math_format(format = function(.) . - 15),
                    limits = c(0, 15 + 1)
                ) +
                scale_x_continuous(
                    name = "wealth at death",
                    breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
                    labels = function(.) round(sinh(.)),
                    limits = asinh(c(-2, 5000))
                ) +
                scale_color_manual(values = c(`FALSE` = "#AAAAAA", `TRUE` = "#666666")) +
                scale_fill_manual(values = c(`FALSE` = "#CCCCCC", `TRUE` = "#999999")) +
                ggtitle(glue("{.x$year[1]}")) +
                theme_bw() +
                theme(legend.position = "none")

            plot2 <- ggplot(.) +
                annotate("rect", xmin = asinh(-1), xmax = asinh(2000), ymin = -Inf, ymax = Inf,
                    fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
                geom_line(aes(x = asinh_wealth, y = log_ccdf_death), na.rm = TRUE, color = "#AAAAAA") +
                geom_point(aes(x = asinh_wealth, y = log_ccdf_death, color = include), na.rm = TRUE, size = 0.7) +
                scale_y_continuous(
                    name = "CCDF",
                    limits = c(-16, 0),
                    breaks = seq(-16, 0, 2)
                ) +
                scale_x_continuous(
                    name = "wealth at death",
                    breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
                    labels = function(.) round(sinh(.)),
                    limits = asinh(c(-2, 5000))
                ) +
                scale_color_manual(values = c(`FALSE` = "#AAAAAA", `TRUE` = "#666666")) +
                theme_bw() +
                theme(legend.position = "none")

            print(ggarrange(plot1, plot2, ncol = 1, align = "h"))
            p()
        })
    dev.off()
})

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

dir.create(here("work", "04-estimate-distribution-death"), showWarnings = FALSE)
write_rds(model_distribution_death, here("work", "04-estimate-distribution-death", "model_distribution_death.rds"))
