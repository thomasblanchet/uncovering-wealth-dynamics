# ---------------------------------------------------------------------------- #
# Effects of wealth taxes
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(latex2exp)
library(scales)
library(progressr)

handlers(handler_progress(
    format = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta"
))
options(dplyr.summarise.inform = FALSE)

top_shares       <- read_rds(here("work", "01-utils", "top_shares.rds"))
dina_macro       <- read_rds(here("work", "02-import-dina", "dina_macro.rds"))
model_micro_data <- read_rds(here("work", "04-prepare-data", "model_micro_data.rds"))
model_all_data   <- read_rds(here("work", "05-fit-model", "model_all_data.rds"))
model_params     <- read_rds(here("work", "05-fit-model", "model_params.rds"))

dir.create(here("graphs", "08-wealth-tax-simulations"), showWarnings = FALSE)

# ---------------------------------------------------------------------------- #
# Create a distribution taxable (positive) wealth
# ---------------------------------------------------------------------------- #

tax_base_data <- model_micro_data %>%
    # Keep last year
    filter(year == 2019) %>%
    # Import diffusion parameters
    mutate(wealth_bin = round(10*asinh_wealth)) %>%
    left_join(model_params %>% select(-asinh_wealth)) %>%
    left_join(model_all_data %>% select(year, wealth_bin, sd_income)) %>%
    mutate(sd_income = suppressWarnings(approx(x = wealth, y = sd_income, xout = wealth, rule = 2)$y)) %>%
    mutate(diffu_star_smooth = suppressWarnings(approx(x = wealth, y = diffu_star_smooth, xout = wealth, rule = 2)$y)) %>%
    mutate(conso = suppressWarnings(approx(x = wealth, y = drift_corr_per2 + diffu_star_smooth*wealth/sqrt(1 + wealth^2), xout = wealth, rule = 2)$y)) %>%
    # Calculate diffusion
    mutate(diffu = (0.5*sd_income^2 + diffu_star_smooth)*(1 + wealth^2)) %>%
    mutate(conso = -conso*sqrt(1 + wealth^2)) %>%
    select(weight, wealth, diffu, conso, income) %>%
    arrange(wealth) %>%
    mutate(dw = case_when(
        row_number() == 1 ~ (lead(wealth) - wealth),
        row_number() == n() ~ (wealth - lag(wealth)),
        TRUE ~ (lead(wealth) - lag(wealth))/2
    ))

avg_income2019 <- dina_macro %>% filter(year == 2019) %>%
    mutate(avg_income = national_income/adult_population) %>% pull(avg_income)
threshold2019 <- 50e6/avg_income2019

# ---------------------------------------------------------------------------- #
# Simulate tax base for wealth tax with lump-sum rebate
# ---------------------------------------------------------------------------- #

simulate_tax_base_rebate <- function(tax_base, threshold, rate, elas_conso, elas_evasion) {
    # Tax paid
    tax_paid <- pmax(0, rate*pmax(tax_base$wealth*(1 - rate)^elas_evasion - threshold))
    # Get adjustment factor from progressive tax
    adj_tax <- exp(-cumsum(tax_paid/tax_base$diffu*tax_base$dw))
    # Get adjustment factor from consumption increase
    adj_conso <- exp(-cumsum(tax_base$conso*((1 - rate)^(-elas_conso) - 1)*(tax_base$wealth >= threshold)/tax_base$diffu*tax_base$dw))

    # Iteratively estimate average tax paid and rebate effect
    avg_tax_paid <- weighted.mean(tax_paid, tax_base$weight*adj_tax*adj_conso)
    repeat {
        # Get adjustment factor from lump-sum redistribution of tax
        adj_rebate <- exp(cumsum(avg_tax_paid/tax_base$diffu*tax_base$dw))
        avg_tax_paid_new <- weighted.mean(tax_paid, tax_base$weight*adj_tax*adj_conso*adj_rebate)
        if (abs(avg_tax_paid_new - avg_tax_paid)/(1 + abs(avg_tax_paid)) < 1e-12) {
            tax_base$weight_new <- tax_base$weight*adj_tax*adj_conso*adj_rebate
            tax_base$tax_paid <- tax_paid
            return(tax_base)
        } else {
            avg_tax_paid <- avg_tax_paid_new
        }
    }
}

simulate_tax_base_norebate <- function(tax_base, threshold, rate, elas_conso, elas_evasion) {
    # Tax paid
    tax_paid <- pmax(0, rate*pmax(tax_base$wealth*(1 - rate)^elas_evasion - threshold))
    # Get adjustment factor from progressive tax
    adj_tax <- exp(-cumsum(tax_paid/tax_base$diffu*tax_base$dw))
    # Get adjustment factor from consumption increase
    adj_conso <- exp(-cumsum(tax_base$conso*((1 - rate)^(-elas_conso) - 1)*(tax_base$wealth >= threshold)/tax_base$diffu*tax_base$dw))

    tax_base$weight_new <- tax_base$weight*adj_tax*adj_conso
    tax_base$tax_paid <- tax_paid
    tax_base$wealth_new <- tax_base$wealth
    return(tax_base)
}

tax_revenue <- tibble(
    rate = seq(0, 0.50, 0.001),
    revenue_static = map_dbl(seq(0, 0.50, 0.001), ~ {
        return(weighted.mean(pmax(tax_base_data$wealth - threshold2019, 0)*.x, tax_base_data$weight))
    }),
    revenue_none = map_dbl(seq(0, 0.50, 0.001), ~ {
        tax_base_simul <- simulate_tax_base_norebate(tax_base_data, threshold2019, .x, 0, 0)
        return(weighted.mean(tax_base_simul$tax_paid, tax_base_simul$weight_new))
    }),
    revenue_benchmark = map_dbl(seq(0, 0.50, 0.001), ~ {
        tax_base_simul <- simulate_tax_base_norebate(tax_base_data, threshold2019, .x, 1, 1)
        return(weighted.mean(tax_base_simul$tax_paid, tax_base_simul$weight_new))
    }),
    revenue_high = map_dbl(seq(0, 0.50, 0.001), ~ {
        tax_base_simul <- simulate_tax_base_norebate(tax_base_data, threshold2019, .x, 3, 3)
        return(weighted.mean(tax_base_simul$tax_paid, tax_base_simul$weight_new))
    })
)

pdf(file.path("graphs", "08-wealth-tax-simulations", "laffer-wealth-tax.pdf"), width = 4, height = 4)
p <- tax_revenue %>% ggplot() +
    theme_bw() +

    geom_line(aes(x = rate, y = revenue_static), size = 1.5, color = rgb(242, 100, 25, maxColorValue = 255)) +
    annotate("text", x = 0.12, y = 0.0230, label = "short-run,\nstatic", color = rgb(242, 100, 25, maxColorValue = 255), size = 3.5) +

    geom_line(aes(x = rate, y = revenue_none), size = 1.5, color = rgb(130, 2, 99, maxColorValue = 255)) +
    annotate("text", x = 0.43, y = 0.0230, label = "long-run,\nno behavioral\nresponse", color = rgb(130, 2, 99, maxColorValue = 255), size = 3.5) +

    geom_line(aes(x = rate, y = revenue_benchmark), size = 1.5, color = rgb(134, 187, 216, maxColorValue = 255)) +
    annotate("text", x = 0.35, y = 0.0122, label = "long-run,\nmedium response",
        color = rgb(134, 187, 216, maxColorValue = 255), size = 3.5) +
    annotate("text", x = 0.35, y = 0.0098, label = TeX("$\\epsilon=1, \\eta=1$"),
        color = rgb(134, 187, 216, maxColorValue = 255), size = 3.5) +

    annotate("segment",
        x = tax_revenue$rate[which.max(tax_revenue$revenue_benchmark)],
        xend = tax_revenue$rate[which.max(tax_revenue$revenue_benchmark)],
        y = 0,
        yend = max(tax_revenue$revenue_benchmark),
        linetype = "dashed", size = 1, color = rgb(134, 187, 216, maxColorValue = 255)) +

    geom_line(aes(x = rate, y = revenue_high), size = 1.5, color = rgb(51, 101, 138, maxColorValue = 255)) +
    annotate("text", x = 0.27, y = 0.0048, label = "long-run,\nhigh response",
        color = rgb(51, 101, 138, maxColorValue = 255), size = 3.5) +
    annotate("text", x = 0.27, y = 0.0025, label = TeX("$\\epsilon=3, \\eta=3$"),
        color = rgb(51, 101, 138, maxColorValue = 255), size = 3.5) +

    annotate("segment",
        x = tax_revenue$rate[which.max(tax_revenue$revenue_high)],
        xend = tax_revenue$rate[which.max(tax_revenue$revenue_high)],
        y = 0,
        yend = max(tax_revenue$revenue_high),
        linetype = "dashed", size = 1, color = rgb(51, 101, 138, maxColorValue = 255)) +

    scale_x_continuous(name = "top marginal tax rate", breaks = seq(0, 0.5, 0.1), minor_breaks = NULL, labels = percent_format(accuracy = 1)) +
    scale_y_continuous(name = "tax revenue (% of national income)",
        breaks = seq(0, 0.1, 0.0025),
        minor_breaks = NULL,
        labels = percent_format(accuracy = 0.01),
        limits = c(0, 0.025)
    ) +
    theme(panel.grid.major = element_line(size = 0.2), legend.position = "none",
        panel.border = element_blank(), panel.background = element_blank())
print(p)
dev.off()

# Revenue-maximizing tax rate as a function of elasticity parameters
max_revenue <- expand_grid(elas_conso = seq(0, 8, 0.1), elas_evasion = seq(0, 8, 0.1))
with_progress({
    p <- progressor(steps = nrow(max_revenue))
    for (i in 1:nrow(max_revenue)) {
        max_rate <- optimize(
            f = function(rate) {
                tax_base_simul <- simulate_tax_base_norebate(tax_base_data, threshold2019, rate, max_revenue$elas_conso[i], max_revenue$elas_evasion[i])
                return(weighted.mean(tax_base_simul$tax_paid, tax_base_simul$weight_new))
            },
            lower = 0,
            upper = 0.99,
            maximum = TRUE
        )$maximum
        max_revenue[i, "max_rate"] <- max_rate
        p()
    }
})

pdf(file.path("graphs", "08-wealth-tax-simulations", "abacus-wealth-tax.pdf"), width = 4, height = 4)
p <- max_revenue %>% ggplot() + theme_bw() +
    geom_contour_filled(aes(x = elas_conso, y = elas_evasion, z = max_rate),
        breaks = c(0, 0.02, 0.03, 0.04, 0.05, 0.07, 0.1, 0.2, 1)) +

    annotate("label", x = 0.6, y = 0.3, label = ">20%",   size = 3.5, label.padding = unit(0.2, "lines"), label.r = unit(0, "lines")) +
    annotate("label", x = 1.0, y = 1.2, label = "10-20%", size = 3.5, label.padding = unit(0.2, "lines"), label.r = unit(0, "lines")) +
    annotate("label", x = 1.3, y = 2.5, label = "7-10%",  size = 3.5, label.padding = unit(0.2, "lines"), label.r = unit(0, "lines")) +
    annotate("label", x = 2.6, y = 3.4, label = "5-7%",   size = 3.5, label.padding = unit(0.2, "lines"), label.r = unit(0, "lines")) +
    annotate("label", x = 3.3, y = 4.5, label = "4-5%",   size = 3.5, label.padding = unit(0.2, "lines"), label.r = unit(0, "lines")) +
    annotate("label", x = 5.2, y = 5.4, label = "3-4%",   size = 3.5, label.padding = unit(0.2, "lines"), label.r = unit(0, "lines")) +
    annotate("label", x = 7.0, y = 7.0, label = "2-3%",   size = 3.5, label.padding = unit(0.2, "lines"), label.r = unit(0, "lines")) +

    scale_fill_brewer(type = "seq", palette = "BuPu") +

    scale_x_continuous(name = TeX("consumption elasticity ($\\eta$)"), breaks = seq(0, 8, 2), minor_breaks = NULL) +
    scale_y_continuous(name = TeX("tax evasion elasticity ($\\epsilon$)"), breaks = seq(0, 8, 2), minor_breaks = NULL) +
    theme(panel.grid.major = element_line(size = 0.2), legend.position = "none",
        panel.border = element_blank(), panel.ontop = TRUE, panel.background = element_blank())
print(p)
dev.off()

# ---------------------------------------------------------------------------- #
# Effect on the distribution, with the rebate
# ---------------------------------------------------------------------------- #

tax_base_norebate <- simulate_tax_base_norebate(tax_base_data, threshold2019, 0.12, 1, 1)
tax_base_rebate <- simulate_tax_base_rebate(tax_base_data, threshold2019, 0.12, 1, 1)

data_distrib_tax <- expand_grid(
    version = c("Initial distribution", "With a 12% wealth tax above $50m (no rebate)", "With a 12% wealth tax above $50m (with lump-sum rebate)"),
    group = c("Bottom 50%", "Middle 40%", "Next 9%", "Top 1%")
)

data_distrib_tax$share <- c(
    diff(-top_shares(tax_base_data$wealth, p = c(0, 0.5, 0.9, 0.99, 1), weight = tax_base_data$weight)),
    diff(-top_shares(tax_base_norebate$wealth, p = c(0, 0.5, 0.9, 0.99, 1), weight = tax_base_norebate$weight_new)),
    diff(-top_shares(tax_base_rebate$wealth, p = c(0, 0.5, 0.9, 0.99, 1), weight = tax_base_rebate$weight_new))
)

pdf(file.path("graphs", "08-wealth-tax-simulations", "distribution-wealth-tax-rebate.pdf"), width = 5, height = 4)
p <- data_distrib_tax %>% ggplot() + theme_bw() +
    geom_col(aes(x = group, y = share, fill = version), position = "dodge") +
    scale_y_continuous(name = "Share of wealth", labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = c(
        `Initial distribution` = rgb(47, 72, 88, max = 255),
        `With a 12% wealth tax above $50m (no rebate)` = rgb(246, 174, 45, max = 255),
        `With a 12% wealth tax above $50m (with lump-sum rebate)` = rgb(242, 100, 25, max = 255)
    )) +
    theme(panel.grid.major = element_line(size = 0.2), panel.border = element_blank(),
        panel.background = element_blank(), legend.position = "bottom", legend.direction = "vertical",
        legend.title = element_blank(), axis.title.x = element_blank())
print(p)
dev.off()
