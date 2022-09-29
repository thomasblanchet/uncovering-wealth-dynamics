# ---------------------------------------------------------------------------- #
# Functions to simulate the different effects in the simulation
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)

options(dplyr.summarise.inform = FALSE)

# ---------------------------------------------------------------------------- #
# Savings effect
# ---------------------------------------------------------------------------- #

simul_savings <- function(data_simul, param_income, param_savings, growth_rate) {
    # ------------------------------------------------------------------------ #
    # Distribution beforehand
    # ------------------------------------------------------------------------ #

    effects_before <- data_simul %>%
        mutate(asinh_wealth = asinh(wealth)) %>%
        mutate(weight = weight/sum(weight)) %>%
        mutate(wealth_bin = round(10*asinh_wealth)) %>%
        group_by(wealth_bin) %>%
        summarise(density_before_savings = sum(weight)/0.1) %>%
        mutate(asinh_wealth = wealth_bin/10) %>%
        arrange(desc(wealth_bin)) %>%
        mutate(cdf_before_savings = 1 - 0.1*cumsum(density_before_savings))

    # ------------------------------------------------------------------------ #
    # Income parameters
    # ------------------------------------------------------------------------ #

    data_simul <- data_simul %>%
        mutate(asinh_wealth = asinh(wealth)) %>%
        left_join(param_income, by = "p")

    # Calculate conditional mean and variance of income
    data_simul <- data_simul %>% mutate(
        mean_income = mean_labor/sqrt(1 + wealth^2)
            + mean_capital
            + mean_gains
    )
    data_simul <- data_simul %>% mutate(
        var_income = var_labor
            + var_capital*(1 + wealth^2)
            + var_gains*(1 + wealth^2)
            + 2*cov_labor_capital*sqrt(1 + wealth^2)
            + 2*cov_labor_gains*sqrt(1 + wealth^2)
            + 2*cov_capital_gains*(1 + wealth^2)
    )
    data_simul <- data_simul %>% mutate(
        sd_income = sqrt(var_income)/sqrt(1 + wealth^2)
    )

    # Calculate growth effect
    data_simul <- data_simul %>% mutate(growth_effect = growth_rate*wealth/sqrt(1 + wealth^2))

    # Calculate effect of the derivative of income
    data_simul <- data_simul %>%
        mutate(sd_income_deriv = nreg_drv1(asinh_wealth, sd_income, bw = 1)$y)

    # ------------------------------------------------------------------------ #
    # Savings parameters
    # ------------------------------------------------------------------------ #

    # Add drift and diffusion from savings
    data_simul <- data_simul %>%
        mutate(param = FALSE) %>%
        bind_rows(param_savings %>% mutate(param = TRUE)) %>%
        arrange(asinh_wealth) %>%
        mutate(across(
            c(diffu, drift),
            ~ approx(x = asinh_wealth, y = .x, xout = asinh_wealth, rule = 2)$y
        )) %>%
        filter(!param) %>%
        select(-param) %>%
        ungroup()

    # Calculate overall drift and diffusion
    data_simul <- data_simul %>% mutate(
        drift_total = drift + mean_income - 0.5*sd_income_deriv^2*wealth/sqrt(1 + wealth^2) - growth_effect,
        diffu_total = diffu + 0.5*sd_income^2
    )

    # ------------------------------------------------------------------------ #
    # Save the different effects
    # ------------------------------------------------------------------------ #

    effects <- data_simul %>%
        mutate(wealth_bin = round(10*asinh_wealth)) %>%
        group_by(wealth_bin) %>%
        mutate(
            drift_saving = drift,
            diffu_saving = diffu,
            drift_income = mean_income - 0.5*sd_income_deriv^2*wealth/sqrt(1 + wealth^2) - growth_effect,
            diffu_income = 0.5*sd_income^2
        ) %>%
        summarise(across(c(
            drift_saving, diffu_saving,
            drift_income, diffu_income,
            drift_total, diffu_total
        ), ~ weighted.mean(.x, weight)))

    # ------------------------------------------------------------------------ #
    # Simulate
    # ------------------------------------------------------------------------ #

    data_simul <- data_simul %>%
        mutate(asinh_wealth_chg = drift_total + sqrt(2*diffu_total)*rnorm(n())) %>%
        mutate(asinh_wealth_after = asinh(wealth) + asinh_wealth_chg) %>%
        mutate(wealth = pmax(sinh(asinh_wealth_after), -1)) %>%
        arrange(wealth)

    # ------------------------------------------------------------------------ #
    # Distribution after
    # ------------------------------------------------------------------------ #

    effects_after <- data_simul %>%
        mutate(asinh_wealth = asinh(wealth)) %>%
        mutate(weight = weight/sum(weight)) %>%
        mutate(wealth_bin = round(10*asinh_wealth)) %>%
        group_by(wealth_bin) %>%
        summarise(
            drift_income = weighted.mean(mean_income, weight),
            diffu_income = weighted.mean(0.5*sd_income^2, weight),

            drift_conso = weighted.mean(drift, weight),
            diffu_conso = weighted.mean(diffu, weight),

            drift_savings = weighted.mean(drift_total, weight),
            diffu_savings = weighted.mean(diffu_total, weight),

            density_after_savings = sum(weight)/0.1
        ) %>%
        arrange(desc(wealth_bin)) %>%
        mutate(cdf_after_savings = 1 - 0.1*cumsum(density_after_savings))

    data_simul <- data_simul %>% select(-c(asinh_wealth, asinh_wealth_chg,
        asinh_wealth_after, drift, diffu, mean_income, sd_income, growth_effect,
        drift_total, diffu_total, sd_income_deriv, mean_labor, mean_capital,
        mean_gains, var_labor, var_capital, var_gains, cov_labor_capital,
        cov_labor_gains, var_income, cov_capital_gains))

    return(list(data_simul = data_simul, effects = inner_join(effects_before, effects_after, by = "wealth_bin")))
}

# ---------------------------------------------------------------------------- #
# Marriages & divorces
# ---------------------------------------------------------------------------- #

simul_marriages_divorces <- function(data_simul, marriage_divorce_rate, copula_marriage, spouse_wealth_share) {
    # ------------------------------------------------------------------------ #
    # Distribution beforehand
    # ------------------------------------------------------------------------ #

    effects_before <- data_simul %>%
        mutate(asinh_wealth = asinh(wealth)) %>%
        mutate(weight = weight/sum(weight)) %>%
        mutate(wealth_bin = round(10*asinh_wealth)) %>%
        group_by(wealth_bin) %>%
        summarise(density_before_marriages = sum(weight)/0.1) %>%
        arrange(desc(wealth_bin)) %>%
        mutate(cdf_before_marriages = 1 - 0.1*cumsum(density_before_marriages))

    # ------------------------------------------------------------------------ #
    # Microsimulation of divorces
    # ------------------------------------------------------------------------ #

    # Match with marriage/divorce rates
    data_simul <- data_simul %>% left_join(marriage_divorce_rate, by = c("age", "sex"))

    # Some (very old) people have no marriage/divorce rate, we assume zero
    data_simul <- data_simul %>% mutate(
        marriage_rate = replace_na(marriage_rate, 0),
        divorce_rate = replace_na(divorce_rate, 0)
    )

    # Divorce and marriage rates are given out of the full population: we
    # estimate the fraction of singles and couples adjust the rates for
    # the simulation
    population <- data_simul %>% pull(weight) %>% sum()
    frac_couples <- data_simul %>% filter(couple == 1) %>% pull(weight) %>% sum()
    frac_singles <- data_simul %>% filter(couple == 0) %>% pull(weight) %>% sum()

    frac_couples <- frac_couples/population
    frac_singles <- frac_singles/population

    # Simulate marriages and separate samples
    newlyweds <- data_simul %>% filter(couple == 0) %>% mutate(weight = weight*marriage_rate/frac_singles)
    non_newlyweds <- data_simul %>% mutate(weight = if_else(couple == 0, weight*(1 - marriage_rate/frac_singles), weight))

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
    data_simul <- bind_rows(
        newlyweds_combined %>% mutate(is_getting_married = TRUE),
        non_newlyweds %>% mutate(is_getting_married = FALSE)
    )

    separating <- data_simul %>% filter(couple == 1) %>%
        mutate(weight = weight*divorce_rate/frac_couples)
    non_separating <- data_simul %>%
        mutate(weight = if_else(couple == 1, weight*(1 - divorce_rate/frac_couples), weight))

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
    data_simul <- bind_rows(
        separating %>% mutate(is_getting_divorced = TRUE),
        non_separating %>% mutate(is_getting_divorced = FALSE)
    )

    # Maintain wealth of people that neither get married nor divorced
    data_simul <- data_simul %>% mutate(
        wealth = case_when(
            is_getting_married ~ wealth_marriage,
            is_getting_divorced ~ wealth_divorce,
            TRUE ~ wealth
        )
    )

    data_simul <- data_simul %>% select(-c(is_getting_married, is_getting_divorced,
        share_wealth_divorce, rank_spouse, marriage_rate, divorce_rate, rank, wealth_spouse,
        wealth_marriage, wealth_divorce))

    # ------------------------------------------------------------------------ #
    # Distribution after
    # ------------------------------------------------------------------------ #

    effects_after <- data_simul %>%
        mutate(asinh_wealth = asinh(wealth)) %>%
        mutate(weight = weight/sum(weight)) %>%
        mutate(wealth_bin = round(10*asinh_wealth)) %>%
        group_by(wealth_bin) %>%
        summarise(density_after_marriages = sum(weight)/0.1) %>%
        arrange(desc(wealth_bin)) %>%
        mutate(cdf_after_marriages = 1 - 0.1*cumsum(density_after_marriages))

    return(list(data_simul = data_simul, effects = inner_join(effects_before, effects_after, by = "wealth_bin")))
}

# ---------------------------------------------------------------------------- #
# Inheritance
# ---------------------------------------------------------------------------- #

simul_inheritance <- function(data_simul, estate_tax_fun) {
    # ------------------------------------------------------------------------ #
    # Distribution beforehand
    # ------------------------------------------------------------------------ #

    effects_before <- data_simul %>%
        mutate(asinh_wealth = asinh(wealth)) %>%
        mutate(weight = weight/sum(weight)) %>%
        mutate(wealth_bin = round(10*asinh_wealth)) %>%
        group_by(wealth_bin) %>%
        summarise(density_before_inheritance = sum(weight)/0.1) %>%
        arrange(desc(wealth_bin)) %>%
        mutate(cdf_before_inheritance = 1 - 0.1*cumsum(density_before_inheritance))

    # ------------------------------------------------------------------------ #
    # Microsimulation of inheritances
    # ------------------------------------------------------------------------ #

    # Distribution of people who die with a surviving spouse: it is the sample
    # made up of couples, weighted by the probability of dying AND the
    # probability that their spouse does NOT die
    dead_with_surviving_spouse <- data_simul %>%
        filter(couple == 1) %>%
        mutate(weight = weight*qx*(1 - qx_spouse)) %>%
        mutate(status = "dead with surviving spouse")

    # Distribution of people who live with a dying spouse: it is the sample
    # made up of couples, weighted by the probability of NOT dying AND the
    # probability that their spouse die
    living_with_dead_spouse <- data_simul %>%
        filter(couple == 1) %>%
        mutate(weight = weight*qx_spouse*(1 - qx)) %>%
        mutate(status = "living with dead spouse")

    # Distribution of people who die without a surviving spouse: it combines
    # the sample of singles weighted by the probability of dying with the
    # samples of couples weighted by the probability of both spouses dying
    dead_without_surviving_spouse <- data_simul %>%
        mutate(weight = if_else(as.logical(couple), weight*qx*qx_spouse, weight*qx)) %>%
        mutate(status = "dying with no surviving spouse")

    # People with no change of status
    no_change <- data_simul %>%
        mutate(weight = if_else(as.logical(couple), weight*(1 - qx)*(1 - qx_spouse), weight*(1 - qx))) %>%
        mutate(status = "no change")

    # People living with a dead spouse receive the wealth of their spouse
    # without taxation
    living_with_dead_spouse <- living_with_dead_spouse %>% mutate(transfer_spouse = wealth)

    # For people who die without a surviving spouse: wealth goes to their
    # children in equal proportion, after payment of estate taxes
    transfer_children <- dead_without_surviving_spouse %>%
        mutate(estate_tax = estate_tax_fun(wealth)) %>%
        select(weight, wealth, estate_tax, starts_with("age_child")) %>%
        # Re-define IDs so that they are unique (because of the resampling)
        mutate(id = row_number()) %>%
        pivot_longer(-c(id, weight, wealth, estate_tax), names_to = "sex", values_to = "age", names_transform = list(sex = as.character)) %>%
        filter(!is.na(age)) %>%
        mutate(sex = as.character(sex)) %>%
        mutate(sex = if_else(str_detect(sex, "^age_child_female"), "female", "male")) %>%
        group_by(id) %>%
        mutate(nb_children = n()) %>%
        mutate(transfer_parent = pmax(0, (wealth - estate_tax)/nb_children)) %>%
        mutate(transfer_estate_tax = pmax(0, estate_tax/nb_children)) %>%
        filter(age >= 20) %>%
        ungroup() %>%
        select(weight, age, sex, transfer_parent, transfer_estate_tax) %>%
        arrange(sex, age, transfer_parent)

    living_with_dead_spouse <- living_with_dead_spouse %>% select(-starts_with("age_child"))
    no_change <- no_change %>% select(-starts_with("age_child"))

    # Distribute inheritance to each gender/age group of children among the living
    data_simul <- bind_rows(living_with_dead_spouse, no_change) %>% group_by(age, sex) %>% group_split() %>% map_dfr(~ {
        data_age <- .x$age[1]
        data_sex <- .x$sex[1]

        # Number of people to give inheritance to
        transfer_children_age_sex <- transfer_children %>% filter(age == data_age & sex == data_sex)
        if (nrow(transfer_children_age_sex) == 0) {
            # No inheritance to distribute, return the data as-is
            return(.x)
        }
        frac <- sum(transfer_children_age_sex$weight)/sum(.x$weight)

        # Divide each observations into a fraction that get inheritance,
        # and another one that doesn't
        data_no_inheritance <- .x %>% mutate(weight = weight*(1 - frac*phi))
        data_inheritance <- .x %>% mutate(weight = weight*frac*phi)

        # Distribute inheritance using the copula
        data_inheritance <- data_inheritance %>%
            arrange(wealth, runif(n())) %>%
            mutate(rank_wealth = (cumsum(weight) - weight/2)/sum(weight)) %>%
            # Use the inverse conditional distribution function (inverse h-function)
            # to simulate the rank in the inheritance distribution given the rank in
            # the wealth distribution
            mutate(rank_inheritance = BiCopHinv1(
                u1 = cond_age_inh_rank_wealth,
                u2 = runif(n()),
                family = copula_inheritance$family,
                par = copula_inheritance$par
            ))
        transfer_children_age_sex <- transfer_children_age_sex %>%
            arrange(transfer_parent, runif(n())) %>%
            mutate(rank_inheritance = (cumsum(weight) - weight/2)/sum(weight)) %>%
            select(-c(age, sex))

        # Match on the rank
        data_inheritance <- stat_match(
            data_inheritance,
            transfer_children_age_sex,
            by = "rank_inheritance",
            weight = "weight"
        )

        data_inheritance <- data_inheritance %>% select(-c(rank_wealth, rank_inheritance))

        return(bind_rows(data_inheritance, data_no_inheritance))
    })

    data_simul <- data_simul %>% mutate(
        transfer_spouse     = if_else(is.na(transfer_spouse), 0, transfer_spouse),
        transfer_parent     = if_else(is.na(transfer_parent), 0, transfer_parent),
        transfer_estate_tax = if_else(is.na(transfer_estate_tax), 0, transfer_estate_tax),
        has_transfer        = (transfer_spouse + transfer_parent > 0)
    )

    data_simul <- data_simul %>% mutate(
        wealth = if_else(has_transfer, wealth + transfer_spouse + transfer_parent, wealth)
    )

    data_simul <- data_simul %>% select(-c(
        status, transfer_spouse, transfer_parent, transfer_estate_tax, has_transfer
    ))

    # ------------------------------------------------------------------------ #
    # Distribution after
    # ------------------------------------------------------------------------ #

    effects_after <- data_simul %>%
        mutate(asinh_wealth = asinh(wealth)) %>%
        mutate(weight = weight/sum(weight)) %>%
        mutate(wealth_bin = round(10*asinh_wealth)) %>%
        group_by(wealth_bin) %>%
        summarise(density_after_inheritance = sum(weight)/0.1) %>%
        arrange(desc(wealth_bin)) %>%
        mutate(cdf_after_inheritance = 1 - 0.1*cumsum(density_after_inheritance))

    return(list(data_simul = data_simul, effects = inner_join(effects_before, effects_after, by = "wealth_bin")))
}

# ---------------------------------------------------------------------------- #
# Birth/death
# ---------------------------------------------------------------------------- #

simul_birth_death <- function(data_simul, injection_rate) {
    # ------------------------------------------------------------------------ #
    # Distribution beforehand
    # ------------------------------------------------------------------------ #

    effects_before <- data_simul %>%
        mutate(asinh_wealth = asinh(wealth)) %>%
        mutate(weight = weight/sum(weight)) %>%
        mutate(wealth_bin = round(10*asinh_wealth)) %>%
        group_by(wealth_bin) %>%
        summarise(density_before_birth_death = sum(weight)/0.1) %>%
        arrange(desc(wealth_bin)) %>%
        mutate(cdf_before_birth_death = 1 - 0.1*cumsum(density_before_birth_death))

    # ------------------------------------------------------------------------ #
    # Microsimulation of birth and death
    # ------------------------------------------------------------------------ #

    # Simulate death
    data_simul <- data_simul %>% mutate(weight = weight*(1 - qx))

    # Simulate births
    data_simul <- bind_rows(data_simul, model_micro_birth %>% mutate(
        weight = sum(data_simul$weight)*injection_rate*weight/sum(weight)
    ))

    # ------------------------------------------------------------------------ #
    # Distribution after
    # ------------------------------------------------------------------------ #

    effects_after <- data_simul %>%
        mutate(asinh_wealth = asinh(wealth)) %>%
        mutate(weight = weight/sum(weight)) %>%
        mutate(wealth_bin = round(10*asinh_wealth)) %>%
        group_by(wealth_bin) %>%
        summarise(density_after_birth_death = sum(weight)/0.1) %>%
        arrange(desc(wealth_bin)) %>%
        mutate(cdf_after_birth_death = 1 - 0.1*cumsum(density_after_birth_death))

    return(list(data_simul = data_simul, effects = inner_join(effects_before, effects_after, by = "wealth_bin")))
}

# ---------------------------------------------------------------------------- #
# Save functions
# ---------------------------------------------------------------------------- #

dir.create(here("work", "07-simulation-functions"), showWarnings = FALSE)

write_rds(simul_savings,            here("work", "07-simulation-functions", "simul_savings.rds"))
write_rds(simul_marriages_divorces, here("work", "07-simulation-functions", "simul_marriages_divorces.rds"))
write_rds(simul_inheritance,        here("work", "07-simulation-functions", "simul_inheritance.rds"))
write_rds(simul_birth_death,        here("work", "07-simulation-functions", "simul_birth_death.rds"))
