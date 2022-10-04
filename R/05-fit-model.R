# ---------------------------------------------------------------------------- #
# Fit the model
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(progressr)
library(ggrepel)
library(glue)

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

dir.create(here("graphs", "05-fit-model"), showWarnings = FALSE)
dir.create(here("work", "05-fit-model"), showWarnings = FALSE)

# ---------------------------------------------------------------------------- #
# Estimate Deming regression locally throughout the distribution
# ---------------------------------------------------------------------------- #

with_progress({
    p <- progressor(along = model_all_data)
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

        p()

        return(tibble(
            wealth_bin = first(.x$wealth_bin),
            asinh_wealth = first(.x$asinh_wealth),
            drift_star_per1 = drift_per1,
            drift_star_per2 = drift_per2,
            diffu_star = diffu,
            delta = delta
        ))
    })
})

# ---------------------------------------------------------------------------- #
# Smoothing + derivative calculation for diffusion parameter
# ---------------------------------------------------------------------------- #

model_params <- model_params %>%
    mutate(diffu_star = winsorize(diffu_star, c(0, quantile(diffu_star, 0.99)))) %>%
    mutate(diffu_star_smooth = nreg_drv0(asinh_wealth, diffu_star, bw = 0.5)$y) %>%
    mutate(diffu_star_deriv = nreg_drv1(asinh_wealth, diffu_star_smooth, bw = 0.5)$y)

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

# Estimate residuals
model_all_data <- model_all_data %>%
    left_join(model_params %>% select(-asinh_wealth)) %>%
    mutate(slope = -diffu_star_smooth) %>%
    mutate(intercept = if_else(period == 0, drift_star_per1, drift_star_per2)) %>%
    mutate(xfit = (-(intercept*slope) + delta*x + slope*y)/(delta + slope^2)) %>%
    mutate(yfit = slope*xfit + intercept) %>%
    mutate(res_y = y - yfit) %>%
    mutate(res_x = x - xfit)

pdf(here("graphs", "05-fit-model", "fit-diffu.pdf"), height = 4, width = 6)
print(model_params %>% ggplot() +
    geom_line(aes(x = asinh_wealth, y = sqrt(2*diffu_star)), color = "#CCCCCC", size = 0.5) +
    geom_point(aes(x = asinh_wealth, y = sqrt(2*diffu_star)), color = "#CCCCCC", size = 0.5) +
    geom_line(aes(x = asinh_wealth, y = sqrt(2*diffu_star_smooth)), size = 1) +
    scale_x_continuous(
        name = "wealth (multiple of average income)",
        breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
        labels = function(.) round(sinh(.))
    ) +
    scale_y_continuous(name = expression(sigma), limits = c(0, 1)) +
    theme_bw())
dev.off()

pdf(here("graphs", "05-fit-model", "fit-drift1.pdf"), height = 4, width = 6)
print(model_params %>% ggplot() +
    geom_line(aes(x = asinh_wealth, y = drift_star_per1), color = "#CCCCCC", size = 1) +
    geom_line(aes(x = asinh_wealth, y = drift_corr_per1), size = 1) +
    scale_x_continuous(
        name = "wealth (multiple of average income)",
        breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
        labels = function(.) round(sinh(.))
    ) +
    scale_y_continuous(name = "drift") +
    theme_bw())
dev.off()

pdf(here("graphs", "05-fit-model", "fit-drift2.pdf"), height = 4, width = 6)
print(model_params %>% ggplot() +
    geom_line(aes(x = asinh_wealth, y = drift_star_per2), color = "#CCCCCC", size = 1) +
    geom_line(aes(x = asinh_wealth, y = drift_corr_per2), size = 1) +
    scale_x_continuous(
        name = "wealth (multiple of average income)",
        breaks = asinh(c(-2, -1, 0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)),
        labels = function(.) round(sinh(.))
    ) +
    scale_y_continuous(name = "drift") +
    theme_bw())
dev.off()

# Plot the fit
pdf(here("graphs", "05-fit-model", "deming-fit.pdf"), height = 6, width = 6)
with_progress({
    p <- progressor(along = unique(model_all_data$wealth_bin))
    model_all_data %>% group_by(wealth_bin) %>% group_split() %>% walk(~ {
        plot <- .x %>% ggplot() +
            # Regression line
            geom_abline(intercept = first(.x$drift_star_per1), slope = first(.x$slope), size = 1, color = "#e41a1c") +
            geom_abline(intercept = first(.x$drift_star_per2), slope = first(.x$slope), size = 1, color = "#e41a1c") +
            # Points in the regression, inside current bin
            geom_segment(aes(x = x, y = y, xend = xfit, yend = yfit),
                color = '#000000', size = 0.7, alpha = 0.3) +
            geom_text_repel(aes(x = x, y = y, label = year), color = "#377eb8") +
            geom_point(aes(x = x, y = y), color = "#377eb8") +
            # Titles and labels
            xlab("X") +
            ylab("Y") +
            ggtitle(glue("wealth ~ {round(sinh(.x$asinh_wealth[1]), 1)} times average national income")) +
            theme_bw()
        print(plot)

        p()
        return(.x)
    })
})
dev.off()

# Selected graphs for presentation
pdf(here("graphs", "05-fit-model", "phase-diagram-1.pdf"), height = 4, width = 4.3)
p <- model_all_data %>% filter(wealth_bin == 69 & year >= 1978) %>%
    ggplot() +
    # Titles and labels
    xlab("RHS (~ inequality level)") +
    ylab("LHS (~ inequality change + other effects)") +
    scale_x_continuous(limits = c(-2.0, -1.35), breaks = seq(-2, -1.3, 0.1), minor_breaks = NULL) +
    scale_y_continuous(limits = c(-0.11, 0), breaks = seq(-0.125, 0, 0.025), minor_breaks = NULL) +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
print(p)
dev.off()

pdf(here("graphs", "05-fit-model", "phase-diagram-2.pdf"), height = 4, width = 4.3)
p <- model_all_data %>% filter(wealth_bin == 69 & year >= 1978) %>%
    ggplot() +
    # Points in the regression, inside current bin
    geom_point(aes(x = x, y = y), color = rgb(47, 72, 88, max = 255)) +
    # Titles and labels
    xlab("RHS (~ inequality level)") +
    ylab("LHS (~ inequality change + other effects)") +
    scale_x_continuous(limits = c(-2.0, -1.35), breaks = seq(-2, -1.3, 0.1), minor_breaks = NULL) +
    scale_y_continuous(limits = c(-0.11, 0), breaks = seq(-0.125, 0, 0.025), minor_breaks = NULL) +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
print(p)
dev.off()

pdf(here("graphs", "05-fit-model", "phase-diagram-3.pdf"), height = 4, width = 4.3)
p <- model_all_data %>% filter(wealth_bin == 69 & year >= 1978) %>%
    ggplot() +
    # Points in the regression, inside current bin
    geom_text_repel(aes(x = x, y = y, label = year), color = rgb(47, 72, 88, max = 255), size = 2.5, max.overlaps = 20, seed = 19920902) +
    geom_point(aes(x = x, y = y), color = rgb(47, 72, 88, max = 255)) +
    # Titles and labels
    xlab("RHS (~ inequality level)") +
    ylab("LHS (~ inequality change + other effects)") +
    scale_x_continuous(limits = c(-2.0, -1.35), breaks = seq(-2, -1.3, 0.1), minor_breaks = NULL) +
    scale_y_continuous(limits = c(-0.11, 0), breaks = seq(-0.125, 0, 0.025), minor_breaks = NULL) +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
print(p)
dev.off()

pdf(here("graphs", "05-fit-model", "phase-diagram-4.pdf"), height = 4, width = 4.3)
p <- model_all_data %>% filter(wealth_bin == 69 & year >= 1978) %>%
    ggplot() +
    # Regression line
    geom_abline(
        intercept = model_all_data %>% filter(wealth_bin == 69) %>% pull(drift_star_per2) %>% first(),
        slope = model_all_data %>% filter(wealth_bin == 69) %>% pull(slope) %>% first(),
        size = 1,
        color = rgb(242, 100, 25, max = 255)
    ) +
    # Points in the regression, inside current bin
    geom_text_repel(aes(x = x, y = y, label = year), color = rgb(47, 72, 88, max = 255), size = 2.5, max.overlaps = 20, seed = 19920902) +
    geom_point(aes(x = x, y = y), color = rgb(47, 72, 88, max = 255)) +
    # Titles and labels
    xlab("RHS (~ inequality level)") +
    ylab("LHS (~ inequality change + other effects)") +
    scale_x_continuous(limits = c(-2.0, -1.35), breaks = seq(-2, -1.3, 0.1), minor_breaks = NULL) +
    scale_y_continuous(limits = c(-0.11, 0), breaks = seq(-0.125, 0, 0.025), minor_breaks = NULL) +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
print(p)
dev.off()

pdf(here("graphs", "05-fit-model", "phase-diagram-5.pdf"), height = 4, width = 4.3)
p <- model_all_data %>% filter(wealth_bin == 69) %>%
    ggplot() +
    # Regression line
    geom_abline(
        intercept = model_all_data %>% filter(wealth_bin == 69) %>% pull(drift_star_per2) %>% first(),
        slope = model_all_data %>% filter(wealth_bin == 69) %>% pull(slope) %>% first(),
        size = 1,
        color = rgb(242, 100, 25, max = 255)
    ) +
    # Points in the regression, inside current bin
    geom_text_repel(aes(x = x, y = y, label = year), color = rgb(47, 72, 88, max = 255), size = 2.5, max.overlaps = 20, seed = 19920902) +
    geom_point(aes(x = x, y = y), color = rgb(47, 72, 88, max = 255)) +
    # Titles and labels
    xlab("RHS (~ inequality level)") +
    ylab("LHS (~ inequality change + other effects)") +
    scale_x_continuous(limits = c(-2.0, -1.35), breaks = seq(-2, -1.3, 0.1), minor_breaks = NULL) +
    scale_y_continuous(limits = c(-0.11, 0), breaks = seq(-0.125, 0, 0.025), minor_breaks = NULL) +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
print(p)
dev.off()

pdf(here("graphs", "05-fit-model", "phase-diagram-6.pdf"), height = 4, width = 4.3)
p <- model_all_data %>% filter(wealth_bin == 69) %>%
    ggplot() +
    # Regression line
    geom_abline(
        intercept = model_all_data %>% filter(wealth_bin == 69) %>% pull(drift_star_per1) %>% first(),
        slope = model_all_data %>% filter(wealth_bin == 69) %>% pull(slope) %>% first(),
        size = 1,
        color = rgb(242, 100, 25, max = 255)
    ) +
    geom_abline(
        intercept = model_all_data %>% filter(wealth_bin == 69) %>% pull(drift_star_per2) %>% first(),
        slope = model_all_data %>% filter(wealth_bin == 69) %>% pull(slope) %>% first(),
        size = 1,
        color = rgb(242, 100, 25, max = 255)
    ) +
    # Points in the regression, inside current bin
    geom_text_repel(aes(x = x, y = y, label = year), color = rgb(47, 72, 88, max = 255), size = 2.5, max.overlaps = 20, seed = 19920902) +
    geom_point(aes(x = x, y = y), color = rgb(47, 72, 88, max = 255)) +
    # Titles and labels
    xlab("RHS (~ inequality level)") +
    ylab("LHS (~ inequality change + other effects)") +
    scale_x_continuous(limits = c(-2.0, -1.35), breaks = seq(-2, -1.3, 0.1), minor_breaks = NULL) +
    scale_y_continuous(limits = c(-0.11, 0), breaks = seq(-0.125, 0, 0.025), minor_breaks = NULL) +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
print(p)
dev.off()

pdf(here("graphs", "05-fit-model", "phase-portrait.pdf"), height = 3.5, width = 3.5)
p <- model_all_data %>% filter(wealth_bin == 69) %>%
    ggplot() +
    # Regression line
    geom_abline(
        intercept = model_all_data %>% filter(wealth_bin == 69) %>% pull(drift_star_per1) %>% first(),
        slope = model_all_data %>% filter(wealth_bin == 69) %>% pull(slope) %>% first(),
        size = 1,
        color = rgb(242, 100, 25, max = 255)
    ) +
    geom_abline(
        intercept = model_all_data %>% filter(wealth_bin == 69) %>% pull(drift_star_per2) %>% first(),
        slope = model_all_data %>% filter(wealth_bin == 69) %>% pull(slope) %>% first(),
        size = 1,
        color = rgb(242, 100, 25, max = 255)
    ) +
    # Points in the regression, inside current bin
    geom_text_repel(aes(x = x, y = y, label = year), color = rgb(47, 72, 88, max = 255), size = 2.5, max.overlaps = 20, seed = 19920902) +
    geom_point(aes(x = x, y = y), color = rgb(47, 72, 88, max = 255)) +
    # Titles and labels
    xlab("RHS (~ inequality level)") +
    ylab("LHS (~ inequality change + other effects)") +
    scale_x_continuous(limits = c(-2.0, -1.35), breaks = seq(-2, -1.3, 0.1), minor_breaks = NULL) +
    scale_y_continuous(limits = c(-0.11, 0), breaks = seq(-0.125, 0, 0.025), minor_breaks = NULL) +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
print(p)
dev.off()

# ---------------------------------------------------------------------------- #
# Save results
# ---------------------------------------------------------------------------- #

write_rds(model_params,   here("work", "05-fit-model", "model_params.rds"))
write_rds(model_all_data, here("work", "05-fit-model", "model_all_data.rds"))
