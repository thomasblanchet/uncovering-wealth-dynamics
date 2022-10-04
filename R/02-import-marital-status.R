# ---------------------------------------------------------------------------- #
# Import marital status by age
# ---------------------------------------------------------------------------- #

library(here)
library(tidyverse)
library(scales)
library(readxl)

options(dplyr.summarise.inform = FALSE)

census_data <- read_rds(here("work", "02-import-census", "census_data.rds"))

# ---------------------------------------------------------------------------- #
# Import marital status by age from the census microdata
# ---------------------------------------------------------------------------- #

marital_status_table <- census_data %>%
    filter(year >= 1950) %>%
    filter(age >= 20) %>%
    mutate(age = pmin(age, 90)) %>%
    group_by(year, age, sex, marst) %>%
    summarise(count = sum(perwt), num_obs = n()) %>%
    ungroup()

marital_status_table <- marital_status_table %>%
    complete(year, age, marst, sex, fill = list(count = 0, num_obs = 0)) %>%
    complete(year = 1960:2018, age, marst, sex)

marital_status_table <- marital_status_table %>%
    group_by(age, marst, sex) %>%
    arrange(age, marst, sex, year) %>%
    mutate(
        count = splinefun(x = year, y = count, method = "monoH.FC")(year),
        num_obs = splinefun(x = year, y = num_obs, method = "monoH.FC")(year)
    )

marital_status_table <- marital_status_table %>%
    ungroup() %>%
    mutate(sex = if_else(sex == 1, "male", "female"))

# Plot age pyramid by marital status
dir.create(here("graphs", "02-import-marital-status"), showWarnings = FALSE)
pdf(file = here("graphs", "02-import-marital-status", "age-marital-status.pdf"), height = 7, width = 7*(1 + sqrt(5))/2)
marital_status_table %>% group_by(year) %>% group_split() %>% walk(function(data) {
    p <- data %>% ggplot(aes(x = age, y = count, fill = as.factor(marst))) +
        coord_flip() +
        facet_grid(. ~ sex) +
        scale_x_continuous(limits = c(19, 91), breaks = seq(20, 90, 10)) +
        scale_y_continuous(limits = c(0, 4000000), breaks = seq(0, 4000000, 1000000), labels = comma) +
        scale_fill_brewer(name = "marital status", type = "qual", palette = "Dark2",
            labels = c("Married, spouse present", "Married, spouse absent",
                "Separated", "Divorced", "Widowed", "Never married")) +
        geom_col(position = position_stack(reverse = TRUE)) +
        ggtitle(paste("Age pyramid by sex and marital status,", data$year[1])) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
    print(p)
})
dev.off()

# Create simplified categories
marital_status_table <- marital_status_table %>%
    ungroup() %>%
    mutate(married = case_when(
        marst %in% c(1, 2) ~ "married",
        marst %in% c(3, 4) ~ "separated",
        marst %in% c(5)    ~ "widowed",
        marst %in% c(6)    ~ "never_married"
    ))

marital_status_table <- marital_status_table %>%
    group_by(age, sex, year, married) %>%
    summarise(count = sum(count), num_obs = sum(num_obs)) %>%
    ungroup()

# Plot proportions by cohort
pdf(file = here("graphs", "02-import-marital-status", "proportions-marital-status.pdf"), height = 7, width = 7*(1 + sqrt(5))/2)
marital_status_table %>% group_by(year) %>% group_split() %>% walk(function(data) {
    data <- data %>%
        group_by(age, sex) %>%
        mutate(count = count/sum(count)) %>%
        ungroup() %>%
        mutate(married_int = case_when(
            married == "married" ~ 2,
            married == "separated" ~ 3,
            married == "widowed" ~ 4,
            married == "never_married" ~ 1
        ))

    p <- data %>% ggplot(aes(x = age, y = count, fill = as.factor(married_int))) +
        coord_flip() +
        facet_grid(. ~ sex) +
        scale_x_continuous(limits = c(19, 91), breaks = seq(20, 90, 10)) +
        scale_y_continuous(limits = c(0, 1.01), breaks = seq(0, 1, 0.1), labels = percent) +
        scale_fill_brewer(name = "marital status", type = "qual", palette = "Dark2",
            labels = c("Never married", "Married", "Separated", "Widowed")) +
        geom_col(position = position_stack(reverse = TRUE)) +
        ggtitle(paste("Age pyramid by sex and marital status,", data$year[1])) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
    print(p)
})
dev.off()

# ---------------------------------------------------------------------------- #
# Import yearly marriage and divorce aggregate rates
# ---------------------------------------------------------------------------- #

marriage_divorce_macro_pre2000 <- read_excel(
    path = here("raw-data", "uscb-marital", "07s0076.xls"),
    col_names = FALSE,
    range = "A13:K51"
)
marriage_divorce_macro_pre2000 <- marriage_divorce_macro_pre2000 %>% transmute(
    year = parse_integer(substr(...1, 1, 4)),
    macro_rate_marriage = ...10/1000,
    macro_rate_divorce = ...11/1000
)

marriage_macro <- read_excel(
    path = here("raw-data", "nvss", "national-marriage-divorce-rates-00-19.xlsx"),
    col_names = FALSE,
    range = "A4:D23"
)
marriage_macro <- marriage_macro %>% transmute(
    year = parse_integer(substr(...1, 1, 4)),
    macro_rate_marriage = ...4/1000
)

divorce_macro <- read_excel(
    path = here("raw-data", "nvss", "national-marriage-divorce-rates-00-19.xlsx"),
    col_names = FALSE,
    range = "A34:D53"
)
divorce_macro <- divorce_macro %>% transmute(
    year = parse_integer(substr(...1, 1, 4)),
    macro_rate_divorce = ...4/1000
)

marriage_divorce_macro <- bind_rows(
    marriage_divorce_macro_pre2000 %>% filter(year < 2000),
    full_join(marriage_macro, divorce_macro)
)

pdf(file = here("graphs", "02-import-marital-status", "marriage-divorce-rates.pdf"), height = 3.5, width = 3.5)
print(
    marriage_divorce_macro %>% ggplot() +
        geom_line(aes(x = year, y = macro_rate_marriage), color = "#542788", size = 1) +
        #geom_point(aes(x = year, y = macro_rate_marriage, color = "Marriage rate")) +
        geom_line(aes(x = year, y = macro_rate_divorce), color = "#b35806", size = 1) +
        #geom_point(aes(x = year, y = macro_rate_divorce, color = "Divorce rate")) +

        annotate("text", x = 2010, y = 0.009, label = "crude\nmarriage\nrate", color = "#542788") +
        annotate("text", x = 1960, y = 0.004, label = "crude\ndivorce\nrate", color = "#b35806") +

        scale_y_continuous(name = NULL, labels = label_percent(accuracy = 0.1), limits = c(0, 0.012), breaks = seq(0, 0.012, 0.001), minor_breaks = NULL) +
        scale_x_continuous(name = NULL, breaks = seq(1950, 2020, 10), minor_breaks = NULL) +
        scale_color_brewer(type = "qual", palette = "Set1") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.2), legend.position = "none",
            panel.border = element_blank(), panel.background = element_blank())
)
dev.off()

# Interpolate
marriage_divorce_macro <- marriage_divorce_macro %>%
    complete(year = full_seq(year, 1)) %>%
    mutate(
        macro_rate_marriage = approx(x = year, y = macro_rate_marriage, xout = year)$y,
        macro_rate_divorce = approx(x = year, y = macro_rate_divorce, xout = year)$y
    )

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

dir.create(here("work", "02-import-marital-status"), showWarnings = FALSE)
write_rds(marital_status_table,   here("work", "02-import-marital-status", "marital_status_table.rds"))
write_rds(marriage_divorce_macro, here("work", "02-import-marital-status", "marriage_divorce_macro.rds"))
