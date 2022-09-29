# ---------------------------------------------------------------------------- #
# Bootstrap parameters
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(progressr)
library(glue)
library(Matrix)
library(expm)
library(zoo)
library(scales)

handlers(handler_progress(
    format = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta"
))
options(dplyr.summarise.inform = FALSE)

deming         <- read_rds(here("work", "01-utils", "deming.rds"))
rectangular    <- read_rds(here("work", "01-utils", "rectangular.rds"))
nreg_drv0_grid <- read_rds(here("work", "01-utils", "nreg_drv0_grid.rds"))
nreg_drv0      <- read_rds(here("work", "01-utils", "nreg_drv0.rds"))
nreg_drv1_grid <- read_rds(here("work", "01-utils", "nreg_drv1_grid.rds"))
nreg_drv1      <- read_rds(here("work", "01-utils", "nreg_drv1.rds"))

model_all_data     <- read_rds(here("work", "05-prepare-data-model", "model_all_data.rds"))
prepare_data_model <- read_rds(here("work", "05-prepare-data-model", "prepare_data_model.rds"))
pop_growth         <- read_rds(here("work", "05-prepare-data-model", "pop_growth.rds"))
pop_death          <- read_rds(here("work", "05-prepare-data-model", "pop_death.rds"))

model_params   <- read_rds(here("work", "05-fit-model", "model_params.rds"))
model_all_data <- read_rds(here("work", "05-fit-model", "model_all_data.rds"))

dir.create(here("graphs", "05-fit-model-bootstrap"), showWarnings = FALSE)

# ---------------------------------------------------------------------------- #
# Estimate model for residuals
# ---------------------------------------------------------------------------- #

# In each wealth bin, the scalar product that correspond to the projection
# performed by the Deming regression is
#
#     (x1, y1).(x2, y2) -> I simulate delta*x1*x2 + y1*y2
#
# To quantify the residual, we use the scalar product of the residual vector
# with the normal vector to the fit (since all residuals have the
# same direction, we don't model both x and y, that would be redundant.)
#
# With the scalar product above, the normal vector is collinear to
# (slope, -delta). I normalize it by its norm as induced by the scalar
# product, ie. sqrt(delta*(slope^2 + delta))

# Sanity check: using the scalar product defined above, the residual should
# be orthogonal to the fit.

discr <- model_all_data %>%
    transmute(proj = abs(delta*res_x + res_y*slope)) %>%
    pull(proj) %>%
    max()

if (discr > 1e-12) {
    stop("residuals not orthogonal to fit")
}

# Calculate the projection of the residual
model_all_data <- model_all_data %>%
    group_by(wealth_bin) %>%
    mutate(norm = sqrt(delta*(slope^2 + delta))) %>%
    mutate(res = (delta*res_x*slope - delta*res_y)/norm) %>%
    #mutate(sd_res = sd(res)) %>%
    mutate(sd_res = sqrt(mean(res^2))) %>%
    mutate(res = res/sd_res) %>%
    ungroup()

# Correlation coefficients across time
rho_t <- model_all_data %>%
    # Time lag
    group_by(wealth_bin) %>%
    arrange(wealth_bin, year) %>%
    mutate(lag_res = lag(res)) %>%
    # Time autocorrelation
    summarise(rho_t = cor(res, lag_res, use = "pairwise.complete.obs"))

# Correlation coefficients across wealth bins
rho_w <- model_all_data %>%
    # Wealth bin lag
    group_by(year) %>%
    arrange(year, wealth_bin) %>%
    mutate(lag_res = lag(res)) %>%
    # Wealth autocorrelation
    summarise(rho_w = cor(res, lag_res, use = "pairwise.complete.obs"))
# Here, not sign of differences between years, so we take the average
rho_w <- mean(rho_w$rho_w)

# Full correlation matrix + Cholesky decomposition
all_t <- sort(unique(model_all_data$year))
dist_t <- outer(all_t, all_t, function(x, y) abs(y - x))
all_w <- sort(unique(model_all_data$wealth_bin))
dist_w <- outer(all_w, all_w, function(x, y) abs(y - x))

cov_res_w <- Matrix(rho_w^dist_w)

cov_res_t <- lapply(rho_t$rho_t, function(rho) rho^dist_t)
cov_res_t <- lapply(cov_res_t, sqrtm)
cov_res_t <- .bdiag(cov_res_t)

cov_res <- kronecker(cov_res_w, Diagonal(nrow(dist_t)))
cov_res <- cov_res_t %*% cov_res %*% cov_res_t
cov_res <- as.matrix(cov_res)
chol_res <- t(chol(cov_res))

# Simulate bootstrap datasets
set.seed(19920902)
model_all_data <- model_all_data %>% arrange(asinh_wealth, year)
n <- nrow(chol_res)
nrep <- 50
with_progress({
    p <- progressor(step = nrep)
    model_data_brep <- map_dfr(1:nrep, ~ {
        slope <- model_all_data$slope
        delta <- model_all_data$delta
        norm <- model_all_data$norm
        sd_res <- model_all_data$sd_res

        # Simulate residuals
        res <- chol_res %*% matrix(data = rnorm(n), nrow = n, ncol = 1)
        res <- res*sd_res

        bsres_x <- res*slope/norm
        bsres_y <- -res*delta/norm

        # Create bootstrap dataset
        data_brep <- model_all_data %>% arrange(asinh_wealth, year) %>% transmute(
            wealth_bin, year, period, asinh_wealth, delta,
            rep = .x,
            x = xfit + bsres_x,
            y = yfit + bsres_y
        )

        p()
        return(data_brep)
    })
})

# ---------------------------------------------------------------------------- #
# Bootstrap estimations of the parameter
# ---------------------------------------------------------------------------- #

# Ratio of variances for the Deming regression
model_data_brep <- model_data_brep %>%
    group_by(rep, wealth_bin) %>%
    arrange(rep, wealth_bin, year) %>%
    mutate(mean_x = rollapply(x, 9, mean, partial = TRUE, na.rm = TRUE)) %>%
    mutate(mean_y = rollapply(y, 9, mean, partial = TRUE, na.rm = TRUE)) %>%
    mutate(sd_x = sqrt(mean((x - mean_x)^2))) %>%
    mutate(sd_y = sqrt(mean((y - mean_y)^2))) %>%
    select(-mean_x, -mean_y) %>%
    ungroup()

model_bs_fit <- model_data_brep %>% group_by(rep, wealth_bin) %>% group_split()

with_progress({
    p <- progressor(along = model_bs_fit)
    model_bs_fit <- model_bs_fit %>% map_dfr(~ {
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
            rep = first(.x$rep),
            wealth_bin = first(.x$wealth_bin),
            asinh_wealth = first(.x$asinh_wealth),
            drift_star_per1 = drift_per1,
            drift_star_per2 = drift_per2,
            diffu_star = diffu,
            delta = delta
        ))
    })
})

model_bs_fit <- model_bs_fit %>%
    group_by(rep) %>%
    arrange(rep, asinh_wealth) %>%
    mutate(diffu_star_smooth = nreg_drv0(asinh_wealth, diffu_star, bw = 1)$y) %>%
    mutate(diffu_star_deriv = nreg_drv1(asinh_wealth, diffu_star, bw = 1)$y) %>%
    mutate(drift_corr_per1 = drift_star_per1 + diffu_star_deriv) %>%
    mutate(drift_corr_per2 = drift_star_per2 + diffu_star_deriv)

# ---------------------------------------------------------------------------- #
# Plot estimates with confidence intervals
# ---------------------------------------------------------------------------- #

model_param_stderr <- model_bs_fit %>%
    # Transformation of the parameters
    mutate(wealth = sinh(asinh_wealth)) %>%
    mutate(diffu_trans = sign(diffu_star_smooth)*sqrt(2*abs(diffu_star_smooth))) %>%
    mutate(drift_corr_per1 = drift_corr_per1 + diffu_star_smooth*wealth/sqrt(1 + wealth^2)) %>%
    mutate(drift_corr_per2 = drift_corr_per2 + diffu_star_smooth*wealth/sqrt(1 + wealth^2)) %>%
    group_by(wealth_bin) %>%
    summarise(across(
        .cols = c(diffu_trans, drift_corr_per1, drift_corr_per2),
        .fns = list(mean = mean, sd = sd),
        .names = "{.fn}_{.col}"
    ))

model_param_stderr <- model_params %>%
    mutate(wealth = sinh(asinh_wealth)) %>%
    mutate(diffu_trans = sqrt(2*diffu_star_smooth)) %>%
    mutate(drift_corr_per1 = drift_corr_per1 + diffu_star_smooth*wealth/sqrt(1 + wealth^2)) %>%
    mutate(drift_corr_per2 = drift_corr_per2 + diffu_star_smooth*wealth/sqrt(1 + wealth^2)) %>%
    left_join(model_param_stderr)

# Wealth > average income, plot as % of wealth
pdf(here("graphs", "05-fit-model-bootstrap", "diffu-drift-ci-top.pdf"), width = 3.5, height = 3.5)
p <- model_param_stderr %>%
    filter(wealth >= 1) %>%

    mutate(diffu_trans = diffu_trans*sqrt(1 + wealth^2)/wealth) %>%
    mutate(drift_corr_per1 = -drift_corr_per1*sqrt(1 + wealth^2)/wealth) %>%
    mutate(drift_corr_per2 = -drift_corr_per2*sqrt(1 + wealth^2)/wealth) %>%

    ggplot() +
    theme_bw() +
    geom_ribbon(aes(
        x = asinh_wealth,
        ymin = diffu_trans - 1.96*sd_diffu_trans,
        ymax = diffu_trans + 1.96*sd_diffu_trans
    ), fill = "#F26419", alpha = 0.5) +
    geom_line(aes(x = asinh_wealth, y = diffu_trans), color = "#F26419", size = 1) +

    geom_ribbon(aes(
        x = asinh_wealth,
        ymin = drift_corr_per2 - 1.96*sd_drift_corr_per2,
        ymax = drift_corr_per2 + 1.96*sd_drift_corr_per2,
    ), fill = "#86BBD8", alpha = 0.5) +
    geom_line(aes(x = asinh_wealth, y = drift_corr_per2), color = "#86BBD8", size = 1) +

    geom_ribbon(aes(
        x = asinh_wealth,
        ymin = drift_corr_per1 - 1.96*sd_drift_corr_per1,
        ymax = drift_corr_per1 + 1.96*sd_drift_corr_per1,
    ), fill = "#2F4858", alpha = 0.5) +
    geom_line(aes(x = asinh_wealth, y = drift_corr_per1), color = "#2F4858", size = 1) +

    annotate("text", label = "std. deviation", x = asinh(50), y = 0.6, color = "#F26419", size = 4) +
    annotate("segment", x = asinh(50), xend = asinh(20), y = 0.55, yend = 0.35, color = "#F26419",
        arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

    annotate("text", label = "mean\n(1962-1978)", x = asinh(500), y = 0.37, color = "#2F4858", size = 4, lineheight = 0.85) +
    annotate("segment", x = asinh(500), xend = asinh(600), y = 0.32, yend = 0.23, color = "#2F4858",
        arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

    annotate("text", label = "mean\n(1979-2019)", x = asinh(600), y = -0.02, color = "#86BBD8", size = 4, lineheight = 0.85) +
    annotate("segment", x = asinh(600), xend = asinh(700), y = 0.03, yend = 0.11, color = "#86BBD8",
        arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

    scale_x_continuous(
        name = "wealth (multiple of average income)",
        breaks = asinh(c(0, 1, 10, 100, 1000, 10000)),
        minor_breaks = asinh(c(seq(-1, 1, 1), seq(1, 10, 1), seq(10, 100, 10), seq(100, 1000, 100), seq(1000, 10000, 1000))),
        #minor_breaks = NULL,
        labels = function(.) round(sinh(.)),
        limits = asinh(c(1, 2000))
    ) +

    scale_y_continuous(name = "% of wealth", breaks = seq(0, 1, 0.1), limits = c(-0.05, 0.8), minor_breaks = NULL, labels = label_percent(accuracy = 1)) +

    theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
print(p)
dev.off()

# Wealth w average income, plot as % of income
pdf(here("graphs", "05-fit-model-bootstrap", "diffu-drift-ci-bot.pdf"), width = 3.5, height = 3.5)
p <- model_param_stderr %>%
    filter(wealth <= 1) %>%

    mutate(diffu_trans = diffu_trans*sqrt(1 + wealth^2)) %>%
    mutate(drift_corr_per1 = drift_corr_per1*sqrt(1 + wealth^2)) %>%
    mutate(drift_corr_per2 = drift_corr_per2*sqrt(1 + wealth^2)) %>%

    ggplot() +
    theme_bw() +
    geom_ribbon(aes(
        x = wealth,
        ymin = diffu_trans - 1.96*sd_diffu_trans,
        ymax = diffu_trans + 1.96*sd_diffu_trans
    ), fill = "#F26419", alpha = 0.5) +
    geom_line(aes(x = wealth, y = diffu_trans), color = "#F26419", size = 1) +

    geom_ribbon(aes(
        x = wealth,
        ymin = drift_corr_per2 - 1.96*sd_drift_corr_per2,
        ymax = drift_corr_per2 + 1.96*sd_drift_corr_per2,
    ), fill = "#86BBD8", alpha = 0.5) +
    geom_line(aes(x = wealth, y = drift_corr_per2), color = "#86BBD8", size = 1) +

    geom_ribbon(aes(
        x = wealth,
        ymin = drift_corr_per1 - 1.96*sd_drift_corr_per1,
        ymax = drift_corr_per1 + 1.96*sd_drift_corr_per1,
    ), fill = "#2F4858", alpha = 0.5) +
    geom_line(aes(x = wealth, y = drift_corr_per1), color = "#2F4858", size = 1) +

    annotate("text", label = "std. deviation", x = 0.4, y = 0.85, color = "#F26419", size = 4) +
    annotate("segment", x = 0.4, xend = 0.3, y = 0.75, yend = 0.55, color = "#F26419",
        arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

    annotate("text", label = "mean\n(1962-1978)", x = -0.3, y = -1.5, color = "#2F4858", size = 4, lineheight = 0.85) +
    annotate("segment", x = -0.3, xend = -0.4, y = -1.33, yend = -1.05, color = "#2F4858",
        arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

    annotate("text", label = "mean\n(1979-2019)", x = 0.55, y = -0.05, color = "#86BBD8", size = 4, lineheight = 0.85) +
    annotate("segment", x = 0.55, xend = 0.35, y = -0.25, yend = -0.5, color = "#86BBD8",
        arrow = arrow(length = unit(0.30, "cm"), ends = "last", type = "open"), size = 0.7) +

    scale_x_continuous(
        name = "wealth (multiple of average income)",
        breaks = seq(-1, 1, 0.5),
        limits = c(-1, 1),
        minor_breaks = NULL
    ) +

    scale_y_continuous(name = "% of income", breaks = seq(-2, 1, 0.25), minor_breaks = NULL, labels = label_percent(accuracy = 1)) +

    theme(legend.position = "none", panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
print(p)
dev.off()
