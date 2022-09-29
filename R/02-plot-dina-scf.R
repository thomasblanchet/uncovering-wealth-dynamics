# ---------------------------------------------------------------------------- #
# Plot wealth inequality
# ---------------------------------------------------------------------------- #

library(here)
library(tidyverse)
library(scales)

dir.create(here("graphs", "02-plot-dina-scf"), showWarnings = FALSE)

top_shares <- read_rds(here("work", "01-utils", "top_shares.rds"))
winsorize <- read_rds(here("work", "01-utils", "winsorize.rds"))
weighted_quantile <- read_rds(here("work", "01-utils", "weighted_quantile.rds"))
rectangular <- read_rds(here("work", "01-utils", "rectangular.rds"))
nreg_drv0_grid <- read_rds(here("work", "01-utils", "nreg_drv0_grid.rds"))
nreg_drv0 <- read_rds(here("work", "01-utils", "nreg_drv0.rds"))

dina_distrib <- read_rds(here("work", "02-import-dina", "dina_distrib.rds"))
dina_micro <- read_rds(here("work", "02-import-dina", "dina_micro.rds"))
dina_macro <- read_rds(here("work", "02-import-dina", "dina_macro.rds"))

scf <- read_rds(here("work", "02-import-scf", "scf.rds"))
scf_plus <- read_rds(here("work", "02-import-scf-plus", "scf_plus.rds"))

# ---------------------------------------------------------------------------- #
# Plot the evolution of tax rates
# ---------------------------------------------------------------------------- #

gperc <- c(
    seq(0, 99000, 1000), seq(99100, 99900, 100),
    seq(99910, 99990, 10), seq(99991, 99999, 1)
)
gperc_size <- diff(c(gperc, 1e5))

dina_tax_rates <- dina_micro %>%
    group_by(year) %>%
    arrange(year, wealth) %>%
    mutate(rank = (cumsum(weight) - weight/2)/sum(weight)) %>%
    mutate(bracket = findInterval(1e5*rank, gperc)) %>%
    group_by(year, bracket) %>%
    summarise(across(
        c(wealth, income, labor, capital, pre_income, pre_labor, pre_capital, tax_k, tax_l),
        ~ weighted.mean(.x, weight)
    )) %>%
    mutate(n = gperc_size[bracket], p = gperc[bracket])

dina_tax_rates <- dina_tax_rates %>%
    group_by(year) %>%
    mutate(rate_k = if_else(pre_capital <= 0, 0, pmax(0, tax_k)/pmax(pre_capital, 0))) %>%
    mutate(rate_l = if_else(pre_labor <= 0, 0, pmax(0, tax_l)/pmax(pre_labor, 0)))

# Counterfactual shares evolution
dina_tax_rates <- dina_tax_rates %>%
    group_by(p) %>%
    arrange(p, year) %>%
    mutate(rate_k_init = first(rate_k)) %>%
    mutate(rate_l_init = first(rate_l)) %>%
    mutate(labor_alt = labor + rate_l*pmax(0, pre_labor) - rate_l_init*pmax(0, pre_labor)) %>%
    mutate(capital_alt = capital + rate_k*pmax(0, pre_capital) - rate_k_init*pmax(0, pre_capital)) %>%
    group_by(year) %>%
    mutate(share = n*(labor + capital)/sum(n*(labor + capital))) %>%
    mutate(share_alt = n*(labor_alt + capital_alt)/sum(n*(labor_alt + capital_alt))) %>%
    mutate(group = if_else(p >= 99000, "top", "bottom")) %>%
    group_by(year, group) %>%
    summarise(across(c(share, share_alt), sum))

dina_tax_rates %>% filter(group == "top") %>% ggplot() +
    geom_line(aes(x = year, share, color = "actual"), size = 1) +
    geom_line(aes(x = year, share_alt, color = "alternative"), size = 1) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()


# Private vs. public micro files
dina_public_wealth_shares <- dina_micro %>%
    group_by(year) %>%
    summarise(
        top01_public = suppressWarnings(top_shares(wealth, weight, 0.999)),
        top1_public  = suppressWarnings(top_shares(wealth, weight, 0.990)),
        top10_public = suppressWarnings(top_shares(wealth, weight, 0.900))
    ) %>%
    left_join(dina_distrib %>% transmute(year, top10 = wealth_top10, top1 = wealth_top1))

pdf(here("graphs", "02-plot-dina-scf", "dina-top10-private-public.pdf"), height = 3, width = 5)
print(dina_public_wealth_shares %>% ggplot() +
        geom_line(aes(x = year, y = top10_public, color = "public sample"), na.rm = TRUE) +
        geom_line(aes(x = year, y = top10, color = "full sample"), na.rm = TRUE) +
        scale_color_brewer(type = "qual", palette = "Set1") +
        scale_x_continuous(breaks = seq(1960, 2020, 5)) +
        scale_y_continuous(labels = percent, breaks = seq(0.55, 0.8, 0.05), limits = c(0.55, 0.8)) +
        ylab("top 10% wealth share") +
        theme_bw() +
        theme(
            legend.position = c(0.16, 0.85),
            legend.background = element_rect(linetype = "solid", colour = "black", size = 0.3),
            legend.title = element_blank(),
            legend.margin = margin(0, 5, 5, 5)
        ))
dev.off()

pdf(here("graphs", "02-plot-dina-scf", "dina-top1-private-public.pdf"), height = 3, width = 5)
print(dina_public_wealth_shares %>% ggplot() +
        geom_line(aes(x = year, y = top1_public, color = "public sample")) +
        geom_line(aes(x = year, y = top1, color = "full sample")) +
        scale_color_brewer(type = "qual", palette = "Set1") +
        scale_x_continuous(breaks = seq(1960, 2020, 5)) +
        scale_y_continuous(labels = percent, breaks = seq(0.2, 0.4, 0.05), limits = c(0.2, 0.4)) +
        ylab("top 1% wealth share") +
        theme_bw() +
        theme(
            legend.position = c(0.16, 0.85),
            legend.background = element_rect(linetype = "solid", colour = "black", size = 0.3),
            legend.title = element_blank(),
            legend.margin = margin(0, 5, 5, 5)
        ))
dev.off()

# Average wealth of top 1% vs. bottom 99%
avg_wealth_top_bottom <- dina_micro %>%
    left_join(dina_macro %>% select(year, national_income, adult_population)) %>%
    mutate(wealth = wealth/(national_income/adult_population)) %>%
    select(year, weight, wealth) %>%
    group_by(year) %>%
    arrange(desc(wealth)) %>%
    mutate(rank = 1 - cumsum(weight)/sum(weight)) %>%
    summarise(
        avg_top    = weighted.mean(wealth[rank >= 0.99], weight[rank >= 0.99]),
        avg_bottom = weighted.mean(wealth[rank < 0.99], weight[rank < 0.99])
    ) %>%
    mutate(
        top1 = 0.01*avg_top/(0.99*avg_bottom + 0.01*avg_top),
        top1_fixtop = if_else(year >= 1980, 0.01*avg_top[year == 1980]/(0.99*avg_bottom + 0.01*avg_top[year == 1980]), NA_real_),
        top1_fixbot = if_else(year >= 1980, 0.01*avg_top/(0.99*avg_bottom[year == 1980] + 0.01*avg_top), NA_real_)
    )

pdf(here("graphs", "02-plot-dina-scf", "avg-wealth-top-bottom.pdf"), height = 3, width = 5)
print(avg_wealth_top_bottom %>% ggplot() +
        geom_line(aes(x = year, y = avg_top), color = "#377eb8", size = 0.75) +
        geom_line(aes(x = year, y = avg_bottom*10), color = "#e41a1c", size = 0.75) +
        annotate(
            geom = "segment",
            x = 1980,
            y = 150,
            xend = 1980,
            yend = avg_wealth_top_bottom$avg_top[avg_wealth_top_bottom$year == 1980],
            color = "#377eb8",
            arrow = arrow(length = unit(0.1, "inches"))
        ) +
        annotate(
            geom = "label",
            x = 1980,
            y = 150,
            label = "Since 1980, the average wealth of the top 1% grew \nfrom ~70x to ~150x average national income...",
            color = "#377eb8",
            size = 2.5
        ) +
        annotate(
            geom = "segment",
            x = 2001,
            y = 50,
            xend = 1995,
            yend = avg_wealth_top_bottom$avg_bottom[avg_wealth_top_bottom$year == 1995]*10,
            color = "#e41a1c",
            arrow = arrow(length = unit(0.1, "inches"))
        ) +
        annotate(
            geom = "label",
            x = 2001,
            y = 50,
            label = "...meanwhile, the average wealth of the bottom 99% \nstayed at ~2.5x national income",
            color = "#e41a1c",
            size = 2.5
        ) +
        scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, 5), name = NULL) +
        scale_y_continuous(limits = c(0, 175), breaks = seq(0, 175, 25), name = "top 1%", sec.axis = sec_axis(~./10, breaks = seq(0, 17.5, 2.5), name = "bottom 99%")) +
        theme_bw() +
        theme(legend.position = "none"))
dev.off()

pdf(here("graphs", "02-plot-dina-scf", "top1-fixed-top-bottom.pdf"), height = 3, width = 5)
print(avg_wealth_top_bottom %>% ggplot() +
        geom_line(aes(x = year, y = top1_fixtop), color = "#377eb8", size = 0.75) +
        geom_line(aes(x = year, y = top1_fixbot), color = "#e41a1c", size = 0.75) +
        geom_line(aes(x = year, y = top1), color = "#4daf4a", size = 0.75) +
        annotate(
            geom = "segment",
            x = 1977,
            y = 0.33,
            xend = 1970,
            yend = avg_wealth_top_bottom$top1[avg_wealth_top_bottom$year == 1970],
            color = "#4daf4a",
            arrow = arrow(length = unit(0.1, "inches"))
        ) +
        annotate(
            geom = "label",
            x = 1977,
            y = 0.33,
            label = "top 1% wealth share",
            color = "#4daf4a",
            size = 2.5
        ) +
        annotate(
            geom = "segment",
            x = 1980,
            y = 0.42,
            xend = 1998,
            yend = avg_wealth_top_bottom$top1_fixbot[avg_wealth_top_bottom$year == 1998],
            color = "#e41a1c",
            arrow = arrow(length = unit(0.1, "inches"))
        ) +
        annotate(
            geom = "label",
            x = 1980,
            y = 0.42,
            label = "top 1% wealth share assuming \n1980 wealth/income ratio for the bottom 99%",
            color = "#e41a1c",
            size = 2.5
        ) +
        annotate(
            geom = "segment",
            x = 1985,
            y = 0.17,
            xend = 1990,
            yend = avg_wealth_top_bottom$top1_fixtop[avg_wealth_top_bottom$year == 1990],
            color = "#377eb8",
            arrow = arrow(length = unit(0.1, "inches"))
        ) +
        annotate(
            geom = "label",
            x = 1985,
            y = 0.17,
            label = "top 1% wealth share assuming \n1980 wealth/income ratio for the top 1%",
            color = "#377eb8",
            size = 2.5
        ) +
        scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, 5)) +
        scale_y_continuous(limits = c(0.15, 0.45), breaks = seq(0.15, 0.45, 0.05), labels = percent, name = NULL) +
        theme_bw() +
        theme(legend.position = "none"))
dev.off()

dina_wealth_shares <- dina_distrib %>%
    rename(`top 10%` = wealth_top10, `top 1%` = wealth_top1) %>%
    pivot_longer(-year, names_to = "perc") %>%
    mutate(source = "DINA")

scf_wealth_shares <- scf %>%
    group_by(year, implicate) %>%
    summarise(
        top1 = top_shares(wealth, weight, 0.99),
        top10 = top_shares(wealth, weight, 0.90)
    ) %>%
    group_by(year) %>%
    summarise(
        `top 1%` = mean(top1),
        `top 10%` = mean(top10)
    ) %>%
    pivot_longer(-year, names_to = "perc") %>%
    mutate(source = "SCF")

scf_plus_wealth_shares <- scf_plus %>%
    group_by(year, implicate) %>%
    summarise(
        top1 = top_shares(wealth, weight, 0.99),
        top10 = top_shares(wealth, weight, 0.90)
    ) %>%
    group_by(year) %>%
    summarise(
        `top 1%` = mean(top1),
        `top 10%` = mean(top10)
    ) %>%
    pivot_longer(-year, names_to = "perc") %>%
    mutate(source = "SCF+")

pdf(here("graphs", "02-plot-dina-scf", "wealth-shares.pdf"), height = 3.5, width = 5)
print(bind_rows(dina_wealth_shares, scf_wealth_shares, scf_plus_wealth_shares) %>%
        ggplot(aes(x = year, y = value, group = paste(perc, source), linetype = source, shape = source, color = source)) +
        geom_line(size = 0.75) +
        geom_point(size = 1.7) +
        geom_label(x = 1930, y = 0.55, label = "top 1%", label.r = unit(0, units = "in"), color = "black") +
        geom_label(x = 1930, y = 0.92, label = "top 10%", label.r = unit(0, units = "in"), color = "black") +
        scale_y_continuous(
            limits = c(0.2, 1),
            breaks = seq(0.2, 1, 0.1),
            labels = function(x) percent(x, accuracy = 1)
        ) +
        scale_x_continuous(limits = c(1910, 2020), breaks = seq(1910, 2020, 10), minor_breaks = seq(1910, 2020, 5)) +
        scale_color_manual(values = c(
            "SCF"  = "#4daf4a",
            "SCF+" = "#e41a1c",
            "DINA" = "#377eb8"
        )) +
        scale_linetype_manual(values = c(
            "SCF"  = "solid",
            "SCF+" = "solid",
            "DINA" = "solid"
        )) +
        scale_shape_manual(values = c(
            "SCF"  = 16,
            "SCF+" = 17,
            "DINA" = NA
        )) +
        ylab(NULL) +
        xlab(NULL) +
        theme_bw() +
        theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()

data_plot_top1 <- bind_rows(
    dina_wealth_shares %>% filter(perc == "top 1%") %>% transmute(
        year, value,
        source = "Saez and Zucman (2020)"
    ),
    scf_plus_wealth_shares %>% filter(perc == "top 1%") %>% transmute(
        year, value,
        source = "Survey of Consumer Finances\n(Kuhn, Schularick and Steins, 2020)"
    )
)

pdf(here("graphs", "02-plot-dina-scf", "wealth-shares-top1.pdf"), height = 3, width = 5)
print(data_plot_top1 %>%
        ggplot(aes(x = year, y = value, linetype = source, shape = source, color = source)) +
        geom_line(size = 0.75) +
        geom_point(size = 1.7) +
        scale_y_continuous(
            limits = c(0.2, 0.5),
            breaks = seq(0.2, 0.5, 0.05),
            minor_breaks = NULL,
            labels = function(x) percent(x, accuracy = 1)
        ) +


        annotate("segment", x = 1928, xend = 1941, y = 0.3, yend = 0.352, color = rgb(47, 72, 88, max = 255),
            arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open"), size = 0.6) +
        annotate("text", x = 1925, y = 0.28, size = 3,
            label = "Saez and\nZucman (2020)", color = rgb(47, 72, 88, max = 255)) +


        annotate("segment", x = 1990, xend = 1995, y = 0.43, yend = 0.35, color = rgb(242, 100, 25, max = 255),
            arrow = arrow(length = unit(0.20, "cm"), ends = "last", type = "open"), size = 0.6) +
        annotate("text", x = 1990, y = 0.450, size = 3,
            label = "Survey of Consumer Finances (SCF+)\n(Kuhn, Schularick and Steins, 2020)", color = rgb(242, 100, 25, max = 255)) +

        scale_x_continuous(limits = c(1910, 2020), breaks = seq(1910, 2020, 10), minor_breaks = NULL) +
        scale_color_manual(values = c(
            "Survey of Consumer Finances\n(Kuhn, Schularick and Steins, 2020)" = rgb(242, 100, 25, max = 255),
            "Saez and Zucman (2020)" = rgb(47, 72, 88, max = 255)
        )) +
        scale_linetype_manual(values = c(
            "Survey of Consumer Finances\n(Kuhn, Schularick and Steins, 2020)" = "solid",
            "Saez and Zucman (2020)" = "solid"
        )) +
        scale_shape_manual(values = c(
            "Survey of Consumer Finances\n(Kuhn, Schularick and Steins, 2020)" = 17,
            "Saez and Zucman (2020)" = NA
        )) +
        ylab(NULL) +
        xlab(NULL) +
        theme_bw() +
        theme(legend.position = "none", legend.title = element_blank(), panel.grid.major = element_line(size = 0.2), panel.border = element_blank())
)
dev.off()

pdf(here("graphs", "02-plot-dina-scf", "wealth-income-ratio.pdf"), height = 3, width = 5)
print(dina_macro %>% ggplot(aes(x = year, y = household_wealth/national_income)) +
        geom_line(color = "#377eb8", size = 0.75) +
        scale_y_continuous(labels = percent, breaks = seq(2, 6, 0.5)) +
        scale_x_continuous(limits = c(1910, 2020), breaks = seq(1910, 2020, 10), minor_breaks = seq(1910, 2020, 5)) +
        ylab(NULL) +
        xlab(NULL) +
        theme_bw())
dev.off()

dina_income_inequality <- dina_micro %>%
    group_by(year) %>%
    summarise(
        top1_income = suppressWarnings(top_shares(pre_income, weight, 0.99)),
        top1_labor = suppressWarnings(top_shares(pre_labor, weight, 0.99))
    )

pdf(here("graphs", "02-plot-dina-scf", "inequality-capital-labor.pdf"), height = 3, width = 5)
print(dina_income_inequality %>% ggplot() +
        geom_line(aes(x = year, y = top1_income), color = "#377eb8", size = 0.75) +
        annotate(
            geom = "segment",
            x = 1975,
            y = 0.175,
            xend = 1984,
            yend = dina_income_inequality$top1_income[dina_income_inequality$year == 1984],
            color = "#377eb8",
            arrow = arrow(length = unit(0.1, "inches"))
        ) +
        annotate(
            geom = "label",
            x = 1975,
            y = 0.175,
            label = "top 1% share of\npre-tax national income",
            color = "#377eb8",
            size = 3
        ) +
        geom_line(aes(x = year, y = top1_labor), color = "#e41a1c", size = 0.75) +
        annotate(
            geom = "segment",
            x = 2005,
            y = 0.075,
            xend = 1997,
            yend = dina_income_inequality$top1_labor[dina_income_inequality$year == 1997],
            color = "#e41a1c",
            arrow = arrow(length = unit(0.1, "inches"))
        ) +
        annotate(
            geom = "label",
            x = 2005,
            y = 0.075,
            label = "top 1% share of\npre-tax labor income",
            color = "#e41a1c",
            size = 3
        ) +
        scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, 5)) +
        scale_y_continuous(limits = c(0.04, 0.22), breaks = seq(0.05, 0.25, 0.05), labels = percent) +
        ylab(NULL) +
        xlab(NULL) +
        theme_bw())
dev.off()

# ---------------------------------------------------------------------------- #
# Plot income and average income by wealth
# ---------------------------------------------------------------------------- #

scf_sde <- scf %>%
    filter(year >= 1989 & year <= 2019) %>%
    left_join(dina_macro %>% select(year, national_income, household_wealth, adult_population, deflator, growth)) %>%
    mutate(national_income = national_income/adult_population/deflator) %>%
    mutate(household_wealth = household_wealth/adult_population/deflator) %>%
    mutate(income = income/weighted.mean(income, weight)) %>%
    mutate(wealth = wealth/weighted.mean(wealth, weight)*household_wealth/national_income) %>%
    select(implicate, year, weight, income, wealth, growth)

scf_sde <- scf_sde %>% mutate(
    asinh_wealth = asinh(wealth),
    ratio_income = (income - growth*wealth)/sqrt(1 + wealth^2)
)

scf_income_diffusion <- scf_sde %>%
    filter(implicate == 1) %>%
    group_by(year) %>%
    group_split() %>%
    map_dfr(~ {
        .x$ratio_income <- winsorize(.x$ratio_income,
            range = weighted_quantile(.x$ratio_income, .x$weight, probs = c(0.005, 0.995)))

        .x$avg_ratio_income <- nreg_drv0(
            x = .x$asinh_wealth,
            y = .x$ratio_income,
            weight = .x$weight,
            alpha = 0.2
        )$y
        .x$var_ratio_income <- nreg_drv0(
            x = .x$asinh_wealth,
            y = (.x$ratio_income - .x$avg_ratio_income)^2,
            weight = .x$weight,
            alpha = 0.3
        )$y

        return(.x)
    })

# Income drift in SCF
pdf(file = here("graphs", "02-plot-dina-scf", "scf-income-drift.pdf"), height = 3, width = 5)
scf_income_diffusion %>%
    group_by(year) %>%
    group_split() %>%
    walk(~ {
    p <- suppressWarnings(.x %>% ggplot() +
            geom_point(aes(x = asinh_wealth, y = ratio_income, alpha = weight), na.rm = TRUE) +
            geom_line(aes(x = asinh_wealth, y = avg_ratio_income), size = 1, color = "#377eb8", na.rm = TRUE) +
            scale_alpha_continuous(range = c(0.05, 0.2)) +
            scale_x_continuous(
                breaks = asinh(c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)),
                limits = asinh(c(0, 1000)),
                labels = sinh
            ) +
            scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, 0.25)) +
            xlab("wealth/national income") +
            ylab(NULL) +
            ggtitle(.x$year[1]) +
            theme_bw() +
            theme(
                legend.position = "none",
                plot.title = element_text(hjust = 0.5)
            ))

    print(p)
})
dev.off()

# Income diffusion in SCF
pdf(file = here("graphs", "02-plot-dina-scf", "scf-income-diffusion.pdf"), height = 3, width = 5)
scf_income_diffusion %>%
    group_by(year) %>%
    group_split() %>%
    walk(~ {
        p <- suppressWarnings(.x %>% ggplot() +
                geom_line(aes(x = asinh_wealth, y = var_ratio_income), size = 1, color = "#377eb8", na.rm = TRUE) +
                scale_alpha_continuous(range = c(0.05, 0.2)) +
                scale_x_continuous(
                    breaks = asinh(c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)),
                    limits = asinh(c(0, 1000)),
                    labels = sinh
                ) +
                scale_y_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, 0.025)) +
                xlab("wealth/national income") +
                ylab(NULL) +
                ggtitle(.x$year[1]) +
                theme_bw() +
                theme(
                    legend.position = "none",
                    plot.title = element_text(hjust = 0.5)
                ))

        print(p)
    })
dev.off()
