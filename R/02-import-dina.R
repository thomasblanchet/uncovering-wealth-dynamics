# ---------------------------------------------------------------------------- #
# Import the DINA data
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(readr)
library(progressr)
library(haven)
library(glue)
library(readxl)
library(scales)

handlers(handler_progress(
    format = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta"
))

top_shares <- read_rds(here("work", "01-utils", "top_shares.rds"))

# ---------------------------------------------------------------------------- #
# Import the DINA micro files
# ---------------------------------------------------------------------------- #

with_progress({
    p <- progressor(along = c(1962, 1964, 1966:2019))
    dina_micro <- map_dfr(c(1962, 1964, 1966:2019), function(year) {
        file <- here("raw-data", "dina-micro", glue("usdina{year}.dta"))
        data <- read_dta(file)
        # Calculate post-tax income concepts
        data <- data %>% mutate(
            # Correct unit for weights
            weight = dweght/1e5,

            # Divide income tax into capital vs. labor
            share_k = pmax(pkinc, 0)/(pmax(pkinc, 0) + pmax(plinc, 0)),
            # If no labor and no capital income, assume 25% capital, 75% labor
            # (arguably these people wouldn't pay taxes anyway)
            share_k = if_else(is.na(share_k), 0.25, share_k),
            ditax_k = ditax*share_k,

            # Post-tax capital income
            pokin = pkinc # Pre-tax capital income
                + pkbek # Capital share of social insurance income
                + pkpbk # Capital share of pension benefits
                - fkprk # Sales and excise taxes falling on capital
                - ditax_k # Direct taxes falling on capital
                - propbustax # Business property tax
                - proprestax # Residential property tax
                - estatetax # Estate tax
                - corptax, # Corporate tax
            # Everything else is considered post-tax labor income
            polin = poinc - pokin,

            # Taxes on capital and labor
            tax_k = fkprk + ditax_k + propbustax + proprestax + estatetax + corptax,
            tax_l = pmax(tax - tax_k, 0)
        )
        data <- data %>% select(id, weight, female,
            # Income
            plinc, pkinc, polin, pokin,
            dicsh, fikgi, fiinc, fninc,
            # Wealth
            hweal, hwequ, hwfix, hwhou, hwbus, hwpen, hwdeb, rentalmort, ownermort, nonmort,
            # Taxes
            tax_k, tax_l,
            # Keep estate tax: we'll add it to post-tax income afterwards, since
            # the model will generate the estate tax itself
            estatetax
        )
        # Use equal-split in all income/wealth concepts
        data <- data %>%
            group_by(id) %>%
            mutate(across(-c(weight, female), mean)) %>%
            ungroup()
        data <- data %>% mutate(year = year)

        p()
        return(data)
    })
})

dina_micro <- dina_micro %>% filter(!is.na(weight))
# Remove Stata labels and format
for (col in colnames(dina_micro)) {
    attr(dina_micro[, col], "label") <- NULL
    attr(dina_micro[, col], "format.stata") <- NULL
}

# ---------------------------------------------------------------------------- #
# Import the DINA macro data
# ---------------------------------------------------------------------------- #

dina_macro_ta0 <- read_excel(
    here("raw-data", "dina-macro", "PSZ2020AppendixTablesI(Aggreg).xlsx"),
    sheet = "TA0",
    range = "A9:Z116",
    col_names = FALSE
)
dina_macro_ta0 <- dina_macro_ta0 %>% transmute(
    year = ...1,
    national_income = ...2*1e9,
    household_wealth = ...3*1e9,
    adult_population = ...20*1e3,
    deflator = 1/...26
)

dina_macro_td2 <- read_excel(
    here("raw-data", "dina-macro", "PSZ2020AppendixTablesI(Aggreg).xlsx"),
    sheet = "TD2",
    range = "A10:M117",
    col_names = FALSE
)
dina_macro_td2 <- dina_macro_td2 %>% transmute(
    year = ...1,
    saving_rate = ...11
)

dina_macro_tsd1 <- read_excel(
    here("raw-data", "dina-macro", "PSZ2020AppendixTablesI(Aggreg).xlsx"),
    sheet = "TSD1",
    range = "A8:N115",
    col_names = FALSE
)
dina_macro_tsd1 <- dina_macro_tsd1 %>% transmute(
    year = ...1,
    gains_wea = ...2,
    gains_hou = ...5,
    gains_equ = ...9,
    gains_fix = ...11,
    gains_bus = ...12,
    gains_pen = ...13
)
dina_macro_tsd1 <- dina_macro_tsd1 %>% right_join(dina_macro_ta0)

# ---------------------------------------------------------------------------- #
# Import the long-run wealth distribution series
# ---------------------------------------------------------------------------- #

dina_distrib <- read_excel(
    here("raw-data", "dina-macro", "PSZ2020AppendixTablesII(Distrib).xlsx"),
    sheet = "TE1",
    range = "A10:G116",
    col_names = FALSE
)

dina_distrib <- dina_distrib %>% transmute(
    year = ...1,
    wealth_top10 = ...5,
    wealth_top1 = ...7
)

# ---------------------------------------------------------------------------- #
# Combine micro and macro data
# ---------------------------------------------------------------------------- #

# Estimate capital gains via the capitalization method
dina_micro <- dina_micro %>% left_join(dina_macro_tsd1) %>% mutate(
    gains = hwequ*gains_equ +
        hwfix*gains_fix +
        hwhou*gains_hou +
        hwbus*gains_bus +
        hwpen*gains_pen
)
dina_micro <- dina_micro %>% select(-c(
    gains_wea, gains_hou, gains_equ, gains_fix, gains_bus, gains_pen,
    national_income, household_wealth, adult_population, deflator
))

# Get national income growth rate
dina_macro <- dina_macro_tsd1 %>% left_join(dina_macro_td2)
dina_macro <- dina_macro %>% mutate(growth = (national_income - lag(national_income))/national_income)

# Calibrate micro data (correct tiny remaining discrepancies)
dina_micro <- dina_micro %>% left_join(dina_macro) %>% group_by(year) %>% mutate(
    # Rescale pre-tax income
    ptinc = plinc + pkinc,
    plinc = plinc/(sum(weight*ptinc)/national_income),
    pkinc = pkinc/(sum(weight*ptinc)/national_income),
    ptinc = ptinc/(sum(weight*ptinc)/national_income),
    # Rescale post-tax income
    poinc = polin + pokin,
    polin = polin/(sum(weight*poinc)/national_income),
    pokin = pokin/(sum(weight*poinc)/national_income),
    dicsh = dicsh/(sum(weight*poinc)/national_income),
    estatetax = estatetax/(sum(weight*poinc)/national_income),
    poinc = poinc/(sum(weight*poinc)/national_income),
    # Rescale household wealth
    hweal = hweal/(sum(weight*hweal)/household_wealth)
) %>% ungroup()

# Plot national income including and excluding capital gains
dir.create(here("graphs", "02-import-dina"), showWarnings = FALSE)

pdf(file = here("graphs", "02-import-dina", "national-income-macro.pdf"), height = 3.5, width = 3.5)
print(suppressWarnings(dina_micro %>%
        group_by(year) %>%
        summarise(
            income = sum(ptinc*weight)/sum(weight),
            income_kg = sum((ptinc + gains)*weight)/sum(weight)
        ) %>%
        left_join(dina_macro %>% select(year, deflator)) %>%
        transmute(year, `excluding capital gains` = income/deflator*deflator[year == 2019], `including capital gains` = income_kg/deflator*deflator[year == 2019]) %>%
        pivot_longer(-year, names_to = "variable") %>%
        ggplot() +
        geom_line(aes(x = year, y = value/1e3, color = variable, group = variable), size = 1) +
        geom_point(aes(x = year, y = value/1e3, color = variable, shape = variable), size = 2) +
        annotate("text", x = 1992, y = 87, label = "with capital\ngains", color = rgb(242, 100, 25, max = 255), size = 3) +
        annotate("text", x = 2017, y = 61, label = "without\ncapital\ngains", color = rgb(47, 72, 88, max = 255), size = 3) +
        scale_x_continuous(breaks = seq(1960, 2020, 10), minor_breaks = NULL) +
        scale_y_continuous(breaks = seq(20, 100, 10), labels = label_dollar(suffix = "k"), limits = c(20, 100), minor_breaks = NULL) +
        scale_color_manual(values = c(`excluding capital gains` = rgb(47, 72, 88, max = 255), `including capital gains` = rgb(242, 100, 25, max = 255))) +
        xlab("year") +
        ylab("net national income per adult (2019 USD)") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.2), legend.position = "none",
            panel.border = element_blank(), panel.background = element_blank())
))
dev.off()

# Plot top 1% income share with and without capital gains
pdf(file = here("graphs", "02-import-dina", "national-income-share.pdf"), height = 3.5, width = 3.5)
print(suppressWarnings(dina_micro %>%
        group_by(year) %>%
        summarise(
            `excluding capital gains` = top_shares(poinc, weight, 0.99),
            `including capital gains` = top_shares(poinc + gains, weight, 0.99)
        ) %>%
        pivot_longer(-year, names_to = "variable") %>%
        ggplot() +
        geom_line(aes(x = year, y = value, color = variable, group = variable), size = 1) +
        geom_point(aes(x = year, y = value, color = variable, group = variable, shape = variable), size = 2) +
        annotate("text", x = 1975, y = 0.14, label = "with capital\ngains", color = rgb(242, 100, 25, max = 255), size = 3) +
        annotate("text", x = 2016, y = 0.125, label = "without\ncapital\ngains", color = rgb(47, 72, 88, max = 255), size = 3) +
        scale_x_continuous(breaks = seq(1960, 2020, 10), minor_breaks = NULL) +
        scale_y_continuous(limits = c(0.05, 0.20), breaks = seq(0.05, 0.20, 0.05), labels = label_percent(accuracy = 1), minor_breaks = NULL) +
        scale_color_manual(values = c(`excluding capital gains` = rgb(47, 72, 88, max = 255), `including capital gains` = rgb(242, 100, 25, max = 255))) +
        xlab("year") +
        ylab("top 1% share of post-tax national income") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.2), legend.position = "none",
            panel.border = element_blank(), panel.background = element_blank())
))
dev.off()

# ---------------------------------------------------------------------------- #
# Final clean-up & save
# ---------------------------------------------------------------------------- #

dina_micro <- dina_micro %>% transmute(
    year        = year,
    id          = id,
    weight      = weight,
    sex         = if_else(as.logical(female), "female", "male"),
    pre_labor   = plinc,
    pre_capital = pkinc,
    pre_income  = pre_labor + pre_capital,
    labor       = polin,
    # Do not remove the estate from post-tax income because it will be
    # endogenous to the model (and in any case it is poorly imputed in the
    # DINA data)
    capital     = pokin + estatetax,
    income      = labor + capital,
    dicsh       = dicsh + estatetax,
    fikgi       = fikgi,
    fiinc       = fiinc,
    fninc       = fninc,
    wealth      = hweal,
    gains       = gains,
    growth      = growth,
    tax_k       = tax_k,
    tax_l       = tax_l,
    # Other wealth components
    hwequ, hwfix, hwhou, hwbus, hwpen, rentalmort, ownermort, nonmort
)

# Identify couples vs. singles
dina_micro <- dina_micro %>%
    group_by(year, id) %>%
    mutate(couple = n()) %>%
    mutate(couple = if_else(couple > 1, 1, 0)) %>%
    ungroup()

# ---------------------------------------------------------------------------- #
# Save results
# ---------------------------------------------------------------------------- #

dir.create(here("work", "02-import-dina"), showWarnings = FALSE)

write_rds(dina_distrib,    here("work", "02-import-dina", "dina_distrib.rds"))
write_rds(dina_macro,      here("work", "02-import-dina", "dina_macro.rds"))
write_rds(dina_macro_ta0,  here("work", "02-import-dina", "dina_macro_ta0.rds"))
write_rds(dina_macro_td2,  here("work", "02-import-dina", "dina_macro_td2.rds"))
write_rds(dina_macro_tsd1, here("work", "02-import-dina", "dina_macro_tsd1.rds"))
write_rds(dina_micro,      here("work", "02-import-dina", "dina_micro.rds"))
