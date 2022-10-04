# ---------------------------------------------------------------------------- #
# Estimate distribution at "birth"
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

model_micro_birth <- model_micro_data %>%
    filter(age == 20) %>%
    group_by(year) %>%
    mutate(weight = weight/sum(weight)) %>%
    ungroup() %>%
    mutate(weight = weight/sum(weight)) %>%
    select(id, weight, asinh_wealth, wealth)

density_smooth <- density(
    model_micro_birth$asinh_wealth,
    weights = model_micro_birth$weight,
    from = min(grid_midpoints),
    to = max(grid_midpoints),
    n = length(grid_midpoints),
    bw = bw.nrd0(model_micro_birth %>% filter(wealth >= 10) %>% pull(asinh_wealth)),
    adjust = 2
)$y

model_distribution_birth <- model_micro_birth %>%
    mutate(bin = findInterval(asinh_wealth, grid_cutpoints)) %>%
    group_by(bin) %>%
    summarise(density_birth = sum(weight)/dx) %>%
    complete(bin = seq_along(grid_midpoints)) %>%
    arrange(bin) %>%
    mutate(
        asinh_wealth = grid_midpoints[bin],
        density_birth = if_else(is.na(density_birth), 0, density_birth),
        density_birth_smooth = density_smooth[bin]
    ) %>%
    mutate(wealth = sinh(asinh_wealth)) %>%
    mutate(include = if_else(wealth >= 10 & wealth <= 1000, TRUE, FALSE)) %>%
    # Rescale smoothed density at the top to match true population value
    mutate(density_birth_smooth = density_birth_smooth/sum(density_birth_smooth[wealth >= 10])*sum(density_birth[wealth >= 10]))

model_distribution_birth <- model_distribution_birth %>%
    arrange(asinh_wealth) %>%
    mutate(density_hybrid = if_else(wealth >= 10, density_birth_smooth, density_birth)) %>%
    mutate(cdf_birth = dx*(cumsum(density_hybrid) - density_hybrid/2)) %>%
    mutate(cdf_birth = if_else(abs(cdf_birth - 1) < 1e-12, NA_real_, cdf_birth)) %>%
    mutate(log_ccdf_birth = log(1 - cdf_birth)) %>%
    ungroup()

# ---------------------------------------------------------------------------- #
# Plot distribution
# ---------------------------------------------------------------------------- #

dir.create(here("graphs", "04-estimate-distribution-birth"), showWarnings = FALSE)

pdf(file = here("graphs", "04-estimate-distribution-birth", "wealth-distribution-birth.pdf"), height = 8, width = 6)

plot1 <- model_distribution_birth %>%
    mutate(density_birth_smooth = if_else(include, density_birth_smooth, NA_real_)) %>%
    ggplot() +
    annotate("rect", xmin = asinh(10), xmax = asinh(1000), ymin = -Inf, ymax = Inf,
        fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
    geom_col(aes(x = asinh_wealth, y = 16 + log(density_birth), fill = include, color = include)) +
    geom_line(aes(x = asinh_wealth, y = 16 + log(density_birth_smooth)), size = 0.7) +
    scale_y_continuous(
        name = "density",
        breaks = seq(0, 16 + 2, 2),
        labels = math_format(format = function(.) . - 16),
        limits = c(0, 16 + 2)
    ) +
    scale_x_continuous(
        name = "wealth at 'birth'",
        breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-2, 5000))
    ) +
    scale_color_manual(values = c(`FALSE` = "#AAAAAA", `TRUE` = "#666666")) +
    scale_fill_manual(values = c(`FALSE` = "#CCCCCC", `TRUE` = "#999999")) +
    theme_bw() +
    theme(legend.position = "none")

plot2 <- ggplot(model_distribution_birth) +
    annotate("rect", xmin = asinh(10), xmax = asinh(1000), ymin = -Inf, ymax = Inf,
        fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
    geom_line(aes(x = asinh_wealth, y = log_ccdf_birth), na.rm = TRUE, color = "#AAAAAA") +
    geom_point(aes(x = asinh_wealth, y = log_ccdf_birth, color = include), na.rm = TRUE, size = 0.7) +
    scale_y_continuous(
        name = "CCDF",
        limits = c(-20, 0),
        breaks = seq(-20, 0, 2)
    ) +
    scale_x_continuous(
        name = "wealth at 'birth'",
        breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
        labels = function(.) round(sinh(.)),
        limits = asinh(c(-2, 5000))
    ) +
    scale_color_manual(values = c(`FALSE` = "#AAAAAA", `TRUE` = "#666666")) +
    theme_bw() +
    theme(legend.position = "none")

print(ggarrange(plot1, plot2, ncol = 1, align = "h"))

dev.off()

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

dir.create(here("work", "04-estimate-distribution-birth"), showWarnings = FALSE)
write_rds(model_distribution_birth, here("work", "04-estimate-distribution-birth", "model_distribution_birth.rds"))
write_rds(model_micro_birth,        here("work", "04-estimate-distribution-birth", "model_micro_birth.rds"))
