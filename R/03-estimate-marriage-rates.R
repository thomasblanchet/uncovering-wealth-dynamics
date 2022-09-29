# ---------------------------------------------------------------------------- #
# Estimate marriage rates by age
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(progressr)
library(scales)

handlers(handler_progress(
    format = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta"
))
options(dplyr.summarise.inform = FALSE)

winsorize <- read_rds(here("work", "01-utils", "winsorize.rds"))
marital_status_table <- read_rds(here("work", "02-import-marital-status", "marital_status_table.rds"))
marriage_divorce_macro <- read_rds(here("work", "02-import-marital-status", "marriage_divorce_macro.rds"))
population <- read_rds(here("work", "02-import-population", "population.rds"))

# Estimate as proportion
marital_status_table <- marital_status_table %>%
    group_by(year, age, sex) %>%
    mutate(prop = count/sum(count)) %>%
    ungroup()

# Reshape
marital_status_table <- marital_status_table %>%
    pivot_wider(c(year, sex, age), values_from = c("prop", "num_obs"), names_from = "married") %>%
    rename(married = prop_married, never_married = prop_never_married, separated = prop_separated)

# Match with proportion of married/singles that are one year older in the following
# year (same cohort)
marriage_divorce_rate <- marital_status_table %>%
    mutate(cohort = year - age) %>%
    group_by(sex, cohort) %>%
    arrange(sex, cohort, year) %>%
    mutate(married_next = lead(married)) %>%
    mutate(separated_next = lead(separated)) %>%
    mutate(never_married_next = lead(never_married)) %>%
    ungroup()

# Estimate marriage rate by looking at the population of people never married
marriage_divorce_rate <- marriage_divorce_rate %>% mutate(
    marriage_rate = 1 - never_married_next/never_married,

    divorce_rate = (separated_next - (1 - marriage_rate)*separated)/married,

    marriage_rate = winsorize(marriage_rate, quantile(marriage_rate, probs = c(0.1, 0.9), na.rm = TRUE)),
    divorce_rate = winsorize(divorce_rate, quantile(divorce_rate, probs = c(0.1, 0.9), na.rm = TRUE)),

    num_obs = pmin(num_obs_married, num_obs_never_married + num_obs_separated)
)

# Moving average to reduce noise
with_progress({
    p <- progressor(steps = nrow(marriage_divorce_rate))
    marriage_divorce_rate <- marriage_divorce_rate %>% group_by(sex) %>% group_split()
    marriage_divorce_rate <- marriage_divorce_rate %>% map_dfr(~ {
        .x$marriage_rate_smooth <- NA_real_
        .x$divorce_rate_smooth <- NA_real_
        for (i in 1:nrow(.x)) {
            wgt <- dunif(.x$age - .x$age[i], min = -5, max = 5)*dunif(.x$year - .x$year[i], min = -5, max = 5)
            .x$marriage_rate_smooth[i] <- weighted.mean(.x$marriage_rate, wgt, na.rm = TRUE)
            .x$divorce_rate_smooth[i] <- weighted.mean(.x$divorce_rate, wgt, na.rm = TRUE)
            p()
        }
        return(.x)
    })
})

marriage_divorce_rate <- marriage_divorce_rate %>%
    mutate(marriage_rate_smooth = if_else(is.na(marriage_rate_smooth), 0, marriage_rate_smooth)) %>%
    mutate(marriage_rate_smooth = pmax(marriage_rate_smooth, 0)) %>%
    mutate(divorce_rate_smooth = if_else(is.na(divorce_rate_smooth), 0, divorce_rate_smooth)) %>%
    mutate(divorce_rate_smooth = pmax(divorce_rate_smooth, 0))

# ---------------------------------------------------------------------------- #
# Plot results
# ---------------------------------------------------------------------------- #

dir.create(here("graphs", "03-estimate-marriage-rates"), showWarnings = FALSE)

pdf(file = here("graphs", "03-estimate-marriage-rates", "marriage-rate.pdf"), height = 3.5, width = 7)
print(
    marriage_divorce_rate %>% filter(year %in% seq(1960, 2010, 10) & age <= 90) %>% mutate(sex = if_else(sex == "male", "men", "women")) %>%
        ggplot(aes(x = as.numeric(age), y = marriage_rate_smooth, color = as.factor(year)), size = 0.7) +
        facet_grid(. ~ sex) +
        geom_line(size = 1) +
        scale_y_continuous(labels = percent, name = "marriage rate", limits = c(0, 0.12)) +
        scale_x_continuous(name = "age", breaks = seq(20, 90, 10)) +
        scale_color_brewer(type = "div", palette = "PuOr") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.2), legend.position = "right", strip.background = element_blank(),
            panel.border = element_blank(), panel.background = element_blank(), legend.title = element_blank())
)
dev.off()

pdf(file = here("graphs", "03-estimate-marriage-rates", "divorce-rate.pdf"), height = 4, width = 6)
print(marriage_divorce_rate %>% filter(year %in% seq(1960, 2010, 10) & age <= 90) %>%
    ggplot(aes(x = as.numeric(age), y = divorce_rate_smooth, color = as.factor(year)), size = 0.7) +
    facet_grid(. ~ sex) +
    geom_line() +
    scale_y_continuous(labels = percent, name = "divorce rate", limits = c(0, 0.12)) +
    scale_x_continuous(breaks = seq(20, 90, 10), name = "") +
    scale_colour_viridis_d() +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank()))
dev.off()

marriage_divorce_rate <- marriage_divorce_rate %>% transmute(year, sex, age,
    marriage_rate = marriage_rate_smooth,
    divorce_rate = divorce_rate_smooth
)

# Rescale to get rates out of full population consistent with macro statistics
marriage_divorce_rate <- marriage_divorce_rate %>%
    left_join(population) %>%
    left_join(marriage_divorce_macro) %>%
    group_by(year) %>%
    transmute(
        year, age, sex,
        marriage_rate = marriage_rate/weighted.mean(marriage_rate, population)*macro_rate_marriage,
        divorce_rate = divorce_rate/weighted.mean(divorce_rate, population)*macro_rate_divorce
    ) %>%
    ungroup()

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

dir.create(here("work", "03-estimate-marriage-rates"), showWarnings = FALSE)
write_rds(marriage_divorce_rate, here("work", "03-estimate-marriage-rates", "marriage_divorce_rate.rds"))
