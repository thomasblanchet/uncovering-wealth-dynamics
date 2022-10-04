# ---------------------------------------------------------------------------- #
# Estimate distributions related to the marriage process
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

model_micro_marriage <- read_rds(here("work", "04-prepare-data", "model_micro_marriage.rds"))

dx         <- read_rds(here("work", "04-prepare-data", "dx.rds"))
year_pivot <- read_rds(here("work", "04-prepare-data", "year_pivot.rds"))

# ---------------------------------------------------------------------------- #
# Re-define grid
# ---------------------------------------------------------------------------- #

all_wealth <- c(
    model_micro_marriage$asinh_wealth,
    model_micro_marriage$asinh_wealth_marriage,
    model_micro_marriage$asinh_wealth_divorce
)

grid_cutpoints <- seq(
    from = floor(min(all_wealth)) - dx/2,
    to = ceiling(max(all_wealth)) + dx/2,
    by = dx
)
grid_midpoints <- seq(
    from = floor(min(all_wealth)),
    to = ceiling(max(all_wealth)),
    by = dx
)

# ---------------------------------------------------------------------------- #
# Estimate distributions for marriage
# ---------------------------------------------------------------------------- #

model_distribution_marriage <- model_micro_marriage %>% filter(is_getting_married)

model_distribution_marriage <- model_distribution_marriage %>%
    select(year, weight, asinh_wealth, asinh_wealth_marriage) %>%
    rename(before = asinh_wealth, after = asinh_wealth_marriage) %>%
    pivot_longer(c(before, after), names_to = "variable", values_to = "asinh_wealth")

model_distribution_marriage <- model_distribution_marriage %>%
    group_by(variable, year) %>%
    mutate(weight = weight/sum(weight)) %>%
    mutate(bin = findInterval(asinh_wealth, grid_cutpoints)) %>%
    group_by(variable, year, bin) %>%
    summarise(density = sum(weight)/dx) %>%
    group_by(variable, year) %>%
    complete(bin = seq_along(grid_midpoints)) %>%
    arrange(variable, year, bin) %>%
    mutate(
        asinh_wealth = grid_midpoints[bin],
        density = if_else(is.na(density), 0, density)
    )

model_distribution_marriage <- model_distribution_marriage %>%
    group_by(variable, year) %>%
    arrange(variable, year, asinh_wealth) %>%
    mutate(cdf = dx*(cumsum(density) - density/2)) %>%
    mutate(cdf = if_else(abs(cdf - 1) < 1e-12, NA_real_, cdf)) %>%
    mutate(log_ccdf = log(1 - cdf)) %>%
    ungroup()

model_distribution_marriage <- model_distribution_marriage %>% pivot_wider(
    id_cols = c(year, bin, asinh_wealth),
    values_from = c(density, cdf, log_ccdf),
    names_from = variable
)

model_distribution_marriage <- model_distribution_marriage %>%
    mutate(wealth = sinh(asinh_wealth)) %>%
    mutate(include = if_else(wealth >= -1 & wealth <= 2000, TRUE, FALSE))

# ---------------------------------------------------------------------------- #
# Estimate distributions for divorces
# ---------------------------------------------------------------------------- #

model_distribution_divorce <- model_micro_marriage %>% filter(is_getting_divorced)

model_distribution_divorce <- model_distribution_divorce %>%
    select(year, weight, asinh_wealth, asinh_wealth_divorce) %>%
    rename(before = asinh_wealth, after = asinh_wealth_divorce) %>%
    pivot_longer(c(before, after), names_to = "variable", values_to = "asinh_wealth")

model_distribution_divorce <- model_distribution_divorce %>%
    group_by(variable, year) %>%
    mutate(weight = weight/sum(weight)) %>%
    mutate(bin = findInterval(asinh_wealth, grid_cutpoints)) %>%
    group_by(variable, year, bin) %>%
    summarise(density = sum(weight)/dx) %>%
    group_by(variable, year) %>%
    complete(bin = seq_along(grid_midpoints)) %>%
    arrange(variable, year, bin) %>%
    mutate(
        asinh_wealth = grid_midpoints[bin],
        density = if_else(is.na(density), 0, density)
    )

model_distribution_divorce <- model_distribution_divorce %>%
    group_by(variable, year) %>%
    arrange(variable, year, asinh_wealth) %>%
    mutate(cdf = dx*(cumsum(density) - density/2)) %>%
    mutate(cdf = if_else(abs(cdf - 1) < 1e-12, NA_real_, cdf)) %>%
    mutate(log_ccdf = log(1 - cdf)) %>%
    ungroup()

model_distribution_divorce <- model_distribution_divorce %>% pivot_wider(
    id_cols = c(year, bin, asinh_wealth),
    values_from = c(density, cdf, log_ccdf),
    names_from = variable
)

model_distribution_divorce <- model_distribution_divorce %>%
    mutate(wealth = sinh(asinh_wealth)) %>%
    mutate(include = if_else(wealth >= -1 & wealth <= 2000, TRUE, FALSE))

# ---------------------------------------------------------------------------- #
# Plot
# ---------------------------------------------------------------------------- #

dir.create(here("graphs", "04-estimate-distribution-marriage"), showWarnings = FALSE)

# For marriages
with_progress({
    p <- progressor(along = unique(model_distribution_marriage$year))
    pdf(file = here("graphs", "04-estimate-distribution-marriage", "wealth-distribution-marriages.pdf"), height = 8, width = 8)
    model_distribution_marriage %>%
        group_by(year) %>% group_split() %>% walk(~ {
            print(
                ggplot(.) +
                    annotate("rect", xmin = asinh(10), xmax = asinh(1000), ymin = -Inf, ymax = Inf,
                        fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
                    geom_line(aes(x = asinh_wealth, y = log_ccdf_before, color = "before marriage"), na.rm = TRUE, size = 0.7) +
                    geom_line(aes(x = asinh_wealth, y = log_ccdf_after, color = "after marriage"), na.rm = TRUE, size = 0.7) +
                    scale_y_continuous(
                        name = "CCDF",
                        limits = c(-16, 0),
                        breaks = seq(-16, 0, 2)
                    ) +
                    scale_x_continuous(
                        name = "wealth",
                        breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
                        labels = function(.) round(sinh(.)),
                        limits = asinh(c(-2, 5000))
                    ) +
                    scale_color_brewer(name = NULL, type = "qual", palette = "Set1") +
                    theme_bw() +
                    theme(legend.position = "bottom", legend.direction = "vertical")
            )
            p()
        })
    dev.off()
})

# For divorces
with_progress({
    p <- progressor(along = unique(model_distribution_divorce$year))
    pdf(file = here("graphs", "04-estimate-distribution-marriage", "wealth-distribution-divorces.pdf"), height = 8, width = 8)
    model_distribution_divorce %>%
        group_by(year) %>% group_split() %>% walk(~ {
            print(
                ggplot(.) +
                    annotate("rect", xmin = asinh(-1), xmax = asinh(2000), ymin = -Inf, ymax = Inf,
                        fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
                    geom_line(aes(x = asinh_wealth, y = log_ccdf_before, color = "before divorce"), na.rm = TRUE, size = 0.7) +
                    geom_line(aes(x = asinh_wealth, y = log_ccdf_after, color = "after divorce"), na.rm = TRUE, size = 0.7) +
                    scale_y_continuous(
                        name = "CCDF",
                        limits = c(-16, 0),
                        breaks = seq(-16, 0, 2)
                    ) +
                    scale_x_continuous(
                        name = "wealth",
                        breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
                        labels = function(.) round(sinh(.)),
                        limits = asinh(c(-2, 5000))
                    ) +
                    scale_color_brewer(name = NULL, type = "qual", palette = "Set1") +
                    theme_bw() +
                    theme(legend.position = "bottom", legend.direction = "vertical")
            )
            p()
        })
    dev.off()
})

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

dir.create(here("work", "04-estimate-distribution-marriage"), showWarnings = FALSE)
write_rds(model_distribution_marriage, here("work", "04-estimate-distribution-marriage", "model_distribution_marriage.rds"))
write_rds(model_distribution_divorce,  here("work", "04-estimate-distribution-marriage", "model_distribution_divorce.rds"))
