# ---------------------------------------------------------------------------- #
# Import historical data on the estate tax schedule
# ---------------------------------------------------------------------------- #

library(here)
library(tidyverse)
library(readxl)
library(scales)

dina_macro <- read_rds(here("work", "02-import-dina", "dina_macro.rds"))

tax_schedule_summary <- read_excel(
    path = here("raw-data", "estate-tax", "estate-tax-schedule.xlsx"),
    sheet = "summary-schedule"
)

tax_schedule_details <- read_excel(
    path = here("raw-data", "estate-tax", "estate-tax-schedule.xlsx"),
    sheet = "detailed-schedule"
)

# Expand to get one tax schedule per year
tax_schedule_details <- tax_schedule_details %>%
    mutate(n_years = year_end - year_start + 1) %>%
    uncount(n_years, .id = "year") %>%
    mutate(year = year_start + year - 1) %>%
    select(-c(year_start, year_end)) %>%
    arrange(year, bracket)

# Calculate amount of tax to be paid in each bracket
tax_schedule_details <- tax_schedule_details %>%
    group_by(year) %>%
    mutate(bracket_size = c(diff(bracket), NA)) %>%
    mutate(tax_bracket = bracket_size*rate) %>%
    mutate(tax_cumul = c(0, cumsum(tax_bracket)[-n()])) %>%
    select(-c(bracket_size, tax_bracket))

# Create a function to compute the amount of estate tax in each year
estate_tax <- function(estate_value, year) {
    exemption <- tax_schedule_summary$exemption[tax_schedule_summary$year == year]
    surtax <- tax_schedule_summary$surtax[tax_schedule_summary$year == year]

    schedule <- tax_schedule_details[tax_schedule_details$year == year, ]

    taxable_estate <- pmax(estate_value - exemption, 0)

    brackets <- cut(taxable_estate,
        breaks = c(schedule$bracket, Inf),
        include.lowest = TRUE,
        labels = FALSE
    )

    taxable_bracket <- schedule$bracket[brackets]
    taxable_rate <- schedule$rate[brackets]
    taxable_cumul <- schedule$tax_cumul[brackets]

    tax_amount <- taxable_cumul + taxable_rate*(taxable_estate - taxable_bracket)
    tax_amount <- tax_amount*(1 + surtax)

    return(tax_amount)
}

dir.create(here("work", "02-import-estate-tax-schedule"), showWarnings = FALSE)
write_rds(tax_schedule_summary, here("work", "02-import-estate-tax-schedule", "tax_schedule_summary.rds"))
write_rds(tax_schedule_details, here("work", "02-import-estate-tax-schedule", "tax_schedule_details.rds"))
write_rds(estate_tax,           here("work", "02-import-estate-tax-schedule", "estate_tax.rds"))

# ---------------------------------------------------------------------------- #
# Plot the marginal tax rates
# ---------------------------------------------------------------------------- #

dir.create(here("graphs", "02-import-estate-tax-schedule"), showWarnings = FALSE)

pdf(file = here("graphs", "02-import-estate-tax-schedule", "marginal-estate-tax-rate.pdf"), height = 3.5, width = 3.5)

p <- tax_schedule_summary %>% ggplot(aes(x = year, y = top_rate)) +
    geom_line(size = 1) + #geom_point(size = 0.5) +
    scale_x_continuous(limits = c(1910, 2020), breaks = seq(1900, 2020, 20), minor_breaks = NULL) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = percent_format(accuracy = 1), minor_breaks = NULL) +
    ylab("top marginal tax rate") +
    theme_bw() +
    theme(panel.grid.major = element_line(size = 0.2), legend.position = "none",
        panel.border = element_blank(), panel.background = element_blank())
print(p)

dev.off()

# ---------------------------------------------------------------------------- #
# Plot the tax schedule
# ---------------------------------------------------------------------------- #

pdf(file = here("graphs", "02-import-estate-tax-schedule", "estate-tax-schedule.pdf"), height = 3.5, width = 3.5)

y <- 10^seq(log10(1), log10(1e9), length.out = 10000)
df <- tibble(
    wealth = y,
    `1950` = estate_tax(y, 1950)/y,
    `1970` = estate_tax(y, 1970)/y,
    `1990` = estate_tax(y, 1990)/y,
    `2010` = estate_tax(y, 2010)/y
)
df <- df %>% pivot_longer(-wealth, names_to = "year", names_transform = list(year = as.integer), values_to = "average_tax_rate")
df <- df %>% left_join(dina_macro %>% transmute(year, deflator = deflator/deflator[year == 2020])) %>% mutate(wealth = wealth/deflator)
p <- df %>%
    ggplot(aes(x = wealth, y = average_tax_rate, group = as.factor(year), color = as.factor(year))) +
    geom_line(size = 1) +
    annotate("text", x = 5e8, y = 0.30, label = "2010", color = "#5e3c99") +
    annotate("text", x = 5e8, y = 0.50, label = "1990", color = "#b2abd2") +
    annotate("text", x = 3.5e7, y = 0.70, label = "1970", color = "#fdb863") +
    annotate("text", x = 4e8, y = 0.65, label = "1950", color = "#e66101") +
    scale_x_log10(
        limits = c(1e5, 1e9),
        breaks = c(1e5, 1e6, 1e7, 1e8, 1e9),
        minor_breaks = unlist(lapply(5:8, function(i) c(1:9)*10^i)),
        labels = c("$100k", "$1m", "$10m", "$100m", "$1bn")
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = percent_format(accuracy = 1), minor_breaks = NULL) +
    scale_color_brewer(name = "year", type = "div", palette = "PuOr") +
    scale_linetype_discrete(name = "year") +
    ylab("averate estate tax rate") +
    xlab("wealth (constant 2020 $)") +
    theme_bw() +
    theme(panel.grid.major = element_line(size = 0.2), legend.position = "none",
        panel.border = element_blank(), panel.background = element_blank())
print(p)

dev.off()



