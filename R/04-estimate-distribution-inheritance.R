# ---------------------------------------------------------------------------- #
# Estimate inheritance distribution
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

model_micro_inheritance <- read_rds(here("work", "04-prepare-data", "model_micro_inheritance.rds"))

dx             <- read_rds(here("work", "04-prepare-data", "dx.rds"))
grid_cutpoints <- read_rds(here("work", "04-prepare-data", "grid_cutpoints.rds"))
grid_midpoints <- read_rds(here("work", "04-prepare-data", "grid_midpoints.rds"))
year_pivot     <- read_rds(here("work", "04-prepare-data", "year_pivot.rds"))

# ---------------------------------------------------------------------------- #
# Calculate distributions
# ---------------------------------------------------------------------------- #

model_distribution_inheritance <- model_micro_inheritance %>% mutate(
    transfer_spouse     = if_else(is.na(transfer_spouse), 0, transfer_spouse),
    transfer_parent     = if_else(is.na(transfer_parent), 0, transfer_parent),
    transfer_estate_tax = if_else(is.na(transfer_estate_tax), 0, transfer_estate_tax),
    has_transfer        = (transfer_spouse + transfer_parent > 0)
)

# Calculate overall probability of inheritance
inheritance_rate <- model_distribution_inheritance %>%
    group_by(year) %>%
    summarise(inheritance_rate = sum(weight*has_transfer)/sum(weight)) %>%
    ungroup()

# Now that we have that overall probability, we can keep only heirs
model_distribution_inheritance <- model_distribution_inheritance %>% filter(has_transfer)

# Calculate distributions before and after inheritance, with some intermediate
# scenarios
model_distribution_inheritance <- model_distribution_inheritance %>% mutate(
    before = asinh(wealth),
    after = asinh(wealth + transfer_spouse + transfer_parent),
    after_spouse = asinh(wealth + transfer_spouse),
    after_parent = asinh(wealth + transfer_parent),
    after_pretax = asinh(wealth + transfer_spouse + transfer_parent + transfer_estate_tax)
)

model_distribution_inheritance <- model_distribution_inheritance %>%
    select(year, weight, before, after, after_spouse, after_parent, after_pretax) %>%
    pivot_longer(c(before, after, after_spouse, after_parent, after_pretax),
        values_to = "asinh_wealth", names_to = "variable")

model_distribution_inheritance <- model_distribution_inheritance %>%
    group_by(variable, year) %>%
    mutate(weight = weight/sum(weight)) %>%
    mutate(bin = findInterval(asinh_wealth, grid_cutpoints)) %>%
    filter(bin >= 1 & bin < length(grid_cutpoints)) %>%
    group_by(variable, year, bin) %>%
    summarise(density = sum(weight)/dx) %>%
    group_by(variable, year) %>%
    complete(bin = seq_along(grid_midpoints)) %>%
    arrange(variable, year, bin) %>%
    mutate(
        asinh_wealth = grid_midpoints[bin],
        density = if_else(is.na(density), 0, density)
    )

model_distribution_inheritance <- model_distribution_inheritance %>%
    group_by(variable, year) %>%
    arrange(variable, year, asinh_wealth) %>%
    mutate(cdf = dx*(cumsum(density) - density/2)) %>%
    mutate(cdf = if_else(abs(cdf - 1) < 1e-12, NA_real_, cdf)) %>%
    mutate(log_ccdf = log(1 - cdf)) %>%
    ungroup()

model_distribution_inheritance <- model_distribution_inheritance %>% pivot_wider(
    id_cols = c(year, bin, asinh_wealth),
    values_from = c(density, cdf, log_ccdf),
    names_from = variable
)

model_distribution_inheritance <- model_distribution_inheritance %>%
    mutate(wealth = sinh(asinh_wealth)) %>%
    mutate(include = if_else(wealth >= -1 & wealth <= 2000, TRUE, FALSE))

# ---------------------------------------------------------------------------- #
# Plot distributions
# ---------------------------------------------------------------------------- #

dir.create(here("graphs", "04-estimate-distribution-inheritance"), showWarnings = FALSE)

# Heirs before inheritance
with_progress({
    p <- progressor(along = unique(model_distribution_inheritance$year))
    pdf(file = here("graphs", "04-estimate-distribution-inheritance", "wealth-distribution-inheritance-before.pdf"), height = 8, width = 6)
    model_distribution_inheritance %>%
        group_by(year) %>% group_split() %>% walk(~ {
            plot1 <- ggplot(.) +
                annotate("rect", xmin = asinh(-1), xmax = asinh(2000), ymin = -Inf, ymax = Inf,
                    fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
                geom_col(aes(x = asinh_wealth, y = 15 + log(density_before), fill = include, color = include)) +
                scale_y_continuous(
                    name = "density",
                    breaks = 0:(15 + 1),
                    labels = math_format(format = function(.) . - 15),
                    limits = c(0, 15 + 1)
                ) +
                scale_x_continuous(
                    name = "wealth before inheritance",
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
                geom_point(aes(x = asinh_wealth, y = log_ccdf_before), na.rm = TRUE, size = 1, color = "#999999") +
                scale_y_continuous(
                    name = "CCDF",
                    limits = c(-16, 0),
                    breaks = seq(-16, 0, 2)
                ) +
                scale_x_continuous(
                    name = "wealth before inheritance",
                    breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
                    labels = function(.) round(sinh(.)),
                    limits = asinh(c(-2, 5000))
                ) +
                theme_bw() +
                theme(legend.position = "none")

            print(ggarrange(plot1, plot2, ncol = 1, align = "h"))
            p()
        })
    dev.off()
})

# Heirs after inheritance
with_progress({
    p <- progressor(along = unique(model_distribution_inheritance$year))
    pdf(file = here("graphs", "04-estimate-distribution-inheritance", "wealth-distribution-inheritance-after.pdf"), height = 8, width = 6)
    model_distribution_inheritance %>%
        group_by(year) %>% group_split() %>% walk(~ {
            plot1 <- ggplot(.) +
                annotate("rect", xmin = asinh(-1), xmax = asinh(2000), ymin = -Inf, ymax = Inf,
                    fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
                geom_col(aes(x = asinh_wealth, y = 15 + log(density_after), fill = include, color = include)) +
                scale_y_continuous(
                    name = "density",
                    breaks = 0:(15 + 1),
                    labels = math_format(format = function(.) . - 15),
                    limits = c(0, 15 + 1)
                ) +
                scale_x_continuous(
                    name = "wealth after inheritance",
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
                geom_point(aes(x = asinh_wealth, y = log_ccdf_after), na.rm = TRUE, size = 1, color = "#999999") +
                scale_y_continuous(
                    name = "CCDF",
                    limits = c(-16, 0),
                    breaks = seq(-16, 0, 2)
                ) +
                scale_x_continuous(
                    name = "wealth after inheritance",
                    breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
                    labels = function(.) round(sinh(.)),
                    limits = asinh(c(-2, 5000))
                ) +
                theme_bw() +
                theme(legend.position = "none")

            print(ggarrange(plot1, plot2, ncol = 1, align = "h"))
            p()
        })
    dev.off()
})

# Compare the different effects
with_progress({
    p <- progressor(along = unique(model_distribution_inheritance$year))
    pdf(file = here("graphs", "04-estimate-distribution-inheritance", "wealth-distribution-inheritance-effects.pdf"), height = 8, width = 8)
    model_distribution_inheritance %>%
        group_by(year) %>% group_split() %>% walk(~ {
            print(
                ggplot(.) +
                    annotate("rect", xmin = asinh(-1), xmax = asinh(2000), ymin = -Inf, ymax = Inf,
                        fill = "#CCCCCC", alpha = 0.5, linetype = "dashed", color = "#000000") +
                    geom_line(aes(x = asinh_wealth, y = log_ccdf_before, color = "before inheritance"), na.rm = TRUE, size = 0.7) +
                    geom_line(aes(x = asinh_wealth, y = log_ccdf_after, color = "after inheritance"), na.rm = TRUE, size = 0.7) +
                    geom_line(aes(x = asinh_wealth, y = log_ccdf_after_parent, color = "after inheritance from parents"), na.rm = TRUE, size = 0.7) +
                    geom_line(aes(x = asinh_wealth, y = log_ccdf_after_spouse, color = "after inheritance from spouse"), na.rm = TRUE, size = 0.7) +
                    geom_line(aes(x = asinh_wealth, y = log_ccdf_after_pretax, color = "after inheritance, before estate taxation"), na.rm = TRUE, size = 0.7) +
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

dir.create(here("work", "04-estimate-distribution-inheritance"), showWarnings = FALSE)
write_rds(model_distribution_inheritance, here("work", "04-estimate-distribution-inheritance", "model_distribution_inheritance.rds"))
write_rds(inheritance_rate, here("work", "04-estimate-distribution-inheritance", "inheritance_rate.rds"))
