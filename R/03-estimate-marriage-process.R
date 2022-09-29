# ---------------------------------------------------------------------------- #
# Estimate effect of marriage in each year in the DINA data
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(progressr)
library(quantreg)
library(VineCopula)
library(scales)

handlers(handler_progress(
    format = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta"
))
options(dplyr.summarise.inform = FALSE)

dir.create(here("graphs", "03-estimate-marriage-process"), showWarnings = FALSE)
dir.create(here("work", "03-estimate-marriage-process"), showWarnings = FALSE)

tricube <- read_rds(here("work", "01-utils", "tricube.rds"))
stat_match <- read_rds(here("work", "01-utils", "stat_match.rds"))
top_shares <- read_rds(here("work", "01-utils", "top_shares.rds"))
sipp_data <- read_rds(here("work", "02-import-sipp", "sipp_data.rds"))
dina_micro <- read_rds(here("work", "03-combine-calibrate-microdata", "dina_micro.rds"))
marriage_divorce_rate <- read_rds(here("work", "03-estimate-marriage-rates", "marriage_divorce_rate.rds"))
marriage_divorce_macro <- read_rds(here("work", "02-import-marital-status", "marriage_divorce_macro.rds"))

# Wealth data is only collected on an annual basis: so we aggregate the
# panel by year
sipp_panel <- sipp_data %>%
    rename(hid = ssuid) %>%
    rename(pid = pnum) %>%
    mutate(year = spanel + swave - 1) %>%
    group_by(hid, pid, year) %>%
    summarise(
        weight = mean(wpfinwgt),
        netwealth = mean(tnetworth),
        age = mean(tage),
        sex = mean(esex),
        marital_status = first(ems),
        spouse_pid = first(epnspouse)
    ) %>%
    ungroup()

# ---------------------------------------------------------------------------- #
# Estimate parameters of assortative mating (marriage)
# ---------------------------------------------------------------------------- #

# Only keep newlyweds and their spouse
id_newlyweds <- distinct(bind_rows(
    # Newlyweds previously in the panel
    sipp_panel %>%
        group_by(hid, pid) %>%
        arrange(hid, pid, year) %>%
        mutate(marital_status_prev = lag(marital_status)) %>%
        filter(marital_status_prev == 6 & marital_status == 1) %>%
        select(year, hid, pid),
    # Their spouse
    sipp_panel %>%
        group_by(hid, pid) %>%
        arrange(hid, pid, year) %>%
        mutate(marital_status_prev = lag(marital_status)) %>%
        filter(marital_status_prev == 6 & marital_status == 1) %>%
        ungroup() %>%
        select(year, hid, spouse_pid) %>%
        rename(pid = spouse_pid) %>%
        na.omit()
))
sipp_panel_newlyweds <- sipp_panel %>% inner_join(id_newlyweds)

# Determine ranks in the wealth distribution of newlyweds
sipp_panel_newlyweds <- sipp_panel_newlyweds %>%
    filter(!is.na(weight)) %>%
    group_by(year) %>%
    arrange(year, netwealth) %>%
    mutate(rank = (cumsum(weight) - weight/2)/sum(weight)) %>%
    ungroup()

# Use a nonparametric quantile regression to determine the rank in the wealth
# distribution of recently married people conditional on age
min_age <- min(sipp_panel_newlyweds$age)
max_age <- max(sipp_panel_newlyweds$age)
span <- 0.5
sipp_panel_newlyweds[, paste0("cond_perc", 1:99)] <- NA_real_
with_progress({
    p <- progressor(steps = max_age - min_age + 1)
    for (age in min_age:max_age) {
        if (any(sipp_panel_newlyweds$age == age)) {
            bd <- sort(abs(sipp_panel_newlyweds$age - age))[floor(span*nrow(sipp_panel_newlyweds))]
            kernel_weight <- tricube((sipp_panel_newlyweds$age - age)/bd)/bd
            data_kernel <- cbind(sipp_panel_newlyweds, kernel_weight = kernel_weight)[kernel_weight > 0, ]

            fit <- rq(rank ~ 1, weights = kernel_weight*weight, data = data_kernel, tau = c(1:99)/100)

            sipp_panel_newlyweds[sipp_panel_newlyweds$age == age, paste0("cond_perc", 1:99)] <- matrix(
                data = rep(predict(fit)[1, ], sum(sipp_panel_newlyweds$age == age)),
                ncol = 99,
                nrow = sum(sipp_panel_newlyweds$age == age),
                byrow = TRUE
            )
        }
        p()
    }
})

pdf(here("graphs", "03-estimate-marriage-process", "wealth-age-sipp-newlyweds.pdf"), height = 3.5, width = 3.5)
print(sipp_panel_newlyweds %>% select(c(age, paste0("cond_perc", 10*(1:9)))) %>% unique() %>% ggplot() +
    geom_line(aes(x = age, y = cond_perc10), size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = cond_perc20), size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = cond_perc30), size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = cond_perc40), size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = cond_perc50), size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = cond_perc60), size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = cond_perc70), size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = cond_perc80), size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = cond_perc90), size = 0.7, alpha = 0.8, na.rm = TRUE) +
    scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(limits = c(20, 80), breaks = seq(20, 90, 10)) +
    annotate(geom = "label", x = 65, y = 0.10, label = "1st decile") +
    annotate(geom = "label", x = 30, y = 0.97, label = "9th decile") +
    ylab("rank in the total wealth distribution") +
    theme_bw() +
    theme(legend.position = "none"))
dev.off()

sipp_panel_newlyweds <- sipp_panel_newlyweds %>% group_by(age) %>% group_split() %>% map_dfr(~ {
    x <- c(0, .x[1, paste0("cond_perc", 1:99)], 1)
    y <- (0:100)/100

    .x$cond_rank <- suppressWarnings(approx(x, y, xout = .x$rank)$y)

    return(.x %>% select(-starts_with("cond_perc")))
})

# Put spouses together
sipp_spouse <- sipp_panel_newlyweds %>% filter(!is.na(spouse_pid)) %>% transmute(
    hid, year,
    spouse_pid = pid,
    spouse_cond_rank = cond_rank
)
sipp_panel_newlyweds <- sipp_panel_newlyweds %>% left_join(sipp_spouse, by = c("hid", "year", "spouse_pid"))
sipp_panel_newlyweds <- sipp_panel_newlyweds %>% filter(!is.na(cond_rank) & !is.na(spouse_cond_rank))

# Remove duplicate couples
sipp_panel_newlyweds <- sipp_panel_newlyweds %>% mutate(couple_id = paste0(min(pid, spouse_pid), max(pid, spouse_pid)))
sipp_panel_newlyweds <- sipp_panel_newlyweds[!duplicated(sipp_panel_newlyweds[, c("hid", "couple_id")]), ]
sipp_panel_newlyweds <- sipp_panel_newlyweds %>% select(-couple_id)

# Find appropriate copula
copula_marriage <- BiCopSelect(
    u1 = sipp_panel_newlyweds$cond_rank,
    u2 = sipp_panel_newlyweds$spouse_cond_rank,
    weight = sipp_panel_newlyweds$weight,
    method = "itau",
    # Only consider single-parameter families
    familyset = c(1, 3, 4, 5, 6, 13, 14, 16, 23, 24, 26, 33, 34, 36),
    se = TRUE
)

pdf(here("graphs", "03-estimate-marriage-process", "spouse-wealth-copula.pdf"), height = 3.5, width = 3.5)
print(
    sipp_panel_newlyweds %>%
        ggplot(aes(x = cond_rank, y = spouse_cond_rank, alpha = weight)) +
        scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
        scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
        xlab("rank in the wealth distribution") +
        ylab("rank of spouse in the wealth distribution") +
        geom_point(na.rm = TRUE) +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.2), legend.position = "none",
            panel.border = element_blank(), panel.background = element_blank())
)
dev.off()

# ---------------------------------------------------------------------------- #
# Estimate parameters of assortative mating (divorce/separation)
# ---------------------------------------------------------------------------- #

# Keep people about to be separated and their spouse
id_separated <- distinct(bind_rows(
    # People about to be separated
    sipp_panel %>%
        group_by(hid, pid) %>%
        arrange(hid, pid, year) %>%
        mutate(marital_status_next = lead(marital_status)) %>%
        filter(marital_status == 1 & marital_status_next %in% c(4, 5)) %>%
        select(year, hid, pid),
    # Their spouse
    sipp_panel %>%
        group_by(hid, pid) %>%
        arrange(hid, pid, year) %>%
        mutate(marital_status_next = lead(marital_status)) %>%
        filter(marital_status == 1 & marital_status_next %in% c(4, 5)) %>%
        ungroup() %>%
        select(year, hid, spouse_pid) %>%
        rename(pid = spouse_pid) %>%
        na.omit()
))

sipp_panel_separated <- sipp_panel %>% inner_join(id_separated)

# Include spouse's net wealth
sipp_spouse <- sipp_panel_separated %>% filter(!is.na(spouse_pid)) %>% transmute(
    hid, year,
    spouse_pid = pid,
    spouse_netwealth = netwealth
)
sipp_panel_separated <- sipp_panel_separated %>% left_join(sipp_spouse, by = c("hid", "year", "spouse_pid"))
sipp_panel_separated <- sipp_panel_separated %>% filter(!is.na(netwealth) & !is.na(spouse_netwealth))

# Remove duplicate couples
sipp_panel_separated <- sipp_panel_separated %>% mutate(couple_id = paste0(min(pid, spouse_pid), max(pid, spouse_pid)))
sipp_panel_separated <- sipp_panel_separated[!duplicated(sipp_panel_separated[, c("hid", "couple_id")]), ]
sipp_panel_separated <- sipp_panel_separated %>% select(-couple_id)

# Add household wealth
sipp_panel_separated <- sipp_panel_separated %>% mutate(household_netwealth = netwealth + spouse_netwealth)

# Create sampling distribution corresponding to the share of wealth held by
# a member of a couple
spouse_wealth_share <- bind_rows(
    sipp_panel_separated %>% transmute(weight, wealth_share = netwealth/household_netwealth),
    sipp_panel_separated %>% transmute(weight, wealth_share = spouse_netwealth/household_netwealth)
)
spouse_wealth_share <- spouse_wealth_share %>% filter(wealth_share >= -1 & wealth_share <= 2)

# ---------------------------------------------------------------------------- #
# Simulate marriage/divorce effect
# ---------------------------------------------------------------------------- #

dina_micro_marriage <- dina_micro %>% left_join(marriage_divorce_rate)

# Some (very old) people have no marriage/divorce rate, we assume zero
dina_micro_marriage <- dina_micro_marriage %>% mutate(
    marriage_rate = replace_na(marriage_rate, 0),
    divorce_rate = replace_na(divorce_rate, 0)
)

with_progress({
    set.seed(19920902)
    dina_micro_marriage <- dina_micro_marriage %>% group_by(year) %>% group_split()
    p <- progressor(along = dina_micro_marriage)
    dina_micro_marriage <- dina_micro_marriage %>% map_dfr(~ {
        # Divorce and marriage rates are given out of the full population: we
        # estimate the fraction of singles and couples adjust the rates for
        # the simulation
        population <- .x %>% pull(weight) %>% sum()
        frac_couples <- .x %>% filter(couple == 1) %>% pull(weight) %>% sum()
        frac_singles <- .x %>% filter(couple == 0) %>% pull(weight) %>% sum()

        frac_couples <- frac_couples/population
        frac_singles <- frac_singles/population

        # Simulate marriages and separate samples
        newlyweds <- .x %>% filter(couple == 0) %>% mutate(weight = weight*marriage_rate/frac_singles)
        non_newlyweds <- .x %>% mutate(weight = if_else(couple == 0, weight*(1 - marriage_rate/frac_singles), weight))

        # Simulate the rank of each newlywed's spouse in the wealth distribution
        newlyweds <- newlyweds %>%
            arrange(wealth) %>%
            mutate(rank = (cumsum(weight) - weight/2)/sum(weight)) %>%
            # Numerical adjustment is case too close to 0 or 1 for BiCopCondSim function
            mutate(rank = case_when(
                rank >= (1 - 1e-6) ~ (1 - 1e-6),
                rank <= 1e-6 ~ 1e-6,
                TRUE ~ rank
            )) %>%
            # Conditional wealth rank using the copula
            mutate(rank_spouse = BiCopCondSim(N = n(), cond.val = rank, cond.var = 1, obj = copula_marriage))
        # Match newlyweds with a fictitious partner based on the simulated rank
        newlyweds_combined <- stat_match(
            newlyweds,
            newlyweds %>% transmute(weight, wealth_spouse = wealth, rank = rank_spouse),
            by = "rank",
            weight = "weight"
        )
        # Equal-split the wealth of the newlyweds
        newlyweds_combined <- newlyweds_combined %>% mutate(wealth_marriage = (wealth + wealth_spouse)/2)
        # Add back to full sample
        .x <- bind_rows(
            newlyweds_combined %>% mutate(is_getting_married = TRUE),
            non_newlyweds %>% mutate(is_getting_married = FALSE)
        )

        # Now, simulate divorces
        separating <- .x %>% filter(couple == 1) %>% mutate(weight = weight*divorce_rate/frac_couples)
        non_separating <- .x %>% mutate(weight = if_else(couple == 1, weight*(1 - divorce_rate/frac_couples), weight))

        # Simulate wealth retained by spouse
        separating <- separating %>% mutate(share_wealth_divorce = sample(
            x = spouse_wealth_share$wealth_share,
            size = n(),
            replace = TRUE,
            prob = spouse_wealth_share$weight
        ))
        # Note the 2 factor because wealth is initially in equal-split
        separating <- separating %>% mutate(wealth_divorce = 2*wealth*share_wealth_divorce)

        # Add back to full sample
        .x <- bind_rows(
            separating %>% mutate(is_getting_divorced = TRUE),
            non_separating %>% mutate(is_getting_divorced = FALSE)
        )

        # Maintain wealth of people that neither get married nor divorced
        .x <- .x %>% mutate(
            wealth_marriage = if_else(is_getting_married, wealth_marriage, wealth),
            wealth_divorce = if_else(is_getting_divorced, wealth_divorce, wealth),
            wealth_new = case_when(
                is_getting_married ~ wealth_marriage,
                is_getting_divorced ~ wealth_divorce,
                TRUE ~ wealth
            )
        )

        p()
        return(.x)
    })
})

dina_micro_marriage <- dina_micro_marriage %>% select(year, id, weight, wealth,
    is_getting_married, is_getting_divorced, marriage_rate, divorce_rate,
    wealth_marriage, wealth_divorce, wealth_new)

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

write_rds(dina_micro_marriage, here("work", "03-estimate-marriage-process", "dina_micro_marriage.rds"))
write_rds(copula_marriage, here("work", "03-estimate-marriage-process", "copula_marriage.rds"))
write_rds(spouse_wealth_share, here("work", "03-estimate-marriage-process", "spouse_wealth_share.rds"))

# ---------------------------------------------------------------------------- #
# Plot effect of marriage & divorce
# ---------------------------------------------------------------------------- #

rates_simul <- dina_micro_marriage %>% group_by(year) %>% summarise(
    frac_marriages = weighted.mean(is_getting_married, weight),
    frac_divorces = weighted.mean(is_getting_divorced, weight)
)
rates_simul <- full_join(rates_simul, marriage_divorce_macro)

pdf(here("graphs", "03-estimate-marriage-process", "marriage-divorce-data-vs-simul.pdf"), width = 5, height = 4)
print(
    rates_simul %>% ggplot() +
        geom_line(aes(x = year, y = frac_marriages, color = "Marriage rate (simulation)"), size = 0.7) +
        geom_line(aes(x = year, y = frac_divorces, color = "Divorce rate (simulation)"), size = 0.7) +
        geom_line(aes(x = year, y = macro_rate_marriage, color = "Marriage rate (data)"), size = 0.7) +
        geom_line(aes(x = year, y = macro_rate_divorce, color = "Divorce rate (data)"), size = 0.7) +
        scale_x_continuous(name = "", breaks = seq(1950, 2020, 10)) +
        scale_y_continuous(name = "rate (%)", labels = label_percent(), limits = c(0, 0.015), breaks = seq(0, 0.015, 0.0025)) +
        scale_color_brewer(type = "qual", palette = "Set1", guide = guide_legend(ncol = 2)) +
        theme_bw() +
        theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()

marriage_divorce_effect <- dina_micro_marriage %>% group_by(year) %>% summarise(
    top10_marriage = top_shares(wealth_marriage[is_getting_married], weight[is_getting_married], p = 0.90)
                    - top_shares(wealth[is_getting_married], weight[is_getting_married], p = 0.90),
    top10_divorce = top_shares(wealth_divorce[is_getting_divorced], weight[is_getting_divorced], p = 0.90)
                    - top_shares(wealth[is_getting_divorced], weight[is_getting_divorced], p = 0.90)
)

pdf(here("graphs", "03-estimate-marriage-process", "marriage-divorce-inequality-effect.pdf"), width = 5, height = 4)
print(
    marriage_divorce_effect %>% ggplot() +
        geom_line(aes(x = year, y = top10_marriage, color = "Effect of marriage (on marrying population)")) +
        geom_line(aes(x = year, y = top10_divorce, color = "Effect of divorce (on divorcing population)")) +
        scale_x_continuous(name = "", breaks = seq(1950, 2020, 10)) +
        scale_y_continuous(name = "change in top 10% share (pp.)", labels = label_percent()) +
        scale_color_brewer(type = "qual", palette = "Set1", guide = guide_legend(ncol = 1)) +
        theme_bw() +
        theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()
