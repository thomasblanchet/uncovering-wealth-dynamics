# ---------------------------------------------------------------------------- #
# Estimate inheritance received in the DINA data
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(progressr)
library(quantreg)
library(VineCopula)
library(scales)
library(sandwich)

handlers(handler_progress(
    format = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta"
))
options(dplyr.summarise.inform = FALSE)

dir.create(here("graphs", "03-estimate-inheritance-process"), showWarnings = FALSE)
dir.create(here("work", "03-estimate-inheritance-process"), showWarnings = FALSE)

tricube <- read_rds(here("work", "01-utils", "tricube.rds"))
stat_match <- read_rds(here("work", "01-utils", "stat_match.rds"))
scf <- read_rds(here("work", "02-import-scf", "scf.rds"))
dina_micro_children <- read_rds(here("work", "03-estimate-intergenerational-linkages", "dina_micro_children.rds"))
tax_schedule_summary <- read_rds(here("work", "02-import-estate-tax-schedule", "tax_schedule_summary.rds"))
tax_schedule_details <- read_rds(here("work", "02-import-estate-tax-schedule", "tax_schedule_details.rds"))
estate_tax <- read_rds(here("work", "02-import-estate-tax-schedule", "estate_tax.rds"))

# ---------------------------------------------------------------------------- #
# The extensive margin:
#
# Estimate the following model to determine the probability of receiving any
# inheritance:
#
#   P(i|w, a) = P(i|a)*phi(F(w|a))
#
# With phi(.) constrained so that its integral over [0, 1] is one.
# ---------------------------------------------------------------------------- #

set.seed(19920902)
scf_ext <- scf %>%
    filter(year >= 1989) %>%
    mutate(has_inheritance = if_else(inheritance > 0, 1, 0)) %>%
    mutate(wealth = wealth - inheritance) %>%
    # Rank in the wealth distribution
    mutate(u = runif(n()), v = runif(n())) %>%
    group_by(year) %>%
    arrange(year, desc(wealth), u) %>%
    mutate(rank_wealth = 1 - cumsum(weight)/sum(weight)) %>%
    ungroup() %>%
    mutate(weight = weight/sum(weight)*n(), se_ia = NA)

# ---------------------------------------------------------------------------- #
# Nonparametrically estimate probability of receiving inheritance, by age
# ---------------------------------------------------------------------------- #

min_age <- min(scf_ext$age)
max_age <- max(scf_ext$age)
span <- 0.2
with_progress({
    p <- progressor(steps = max_age - min_age + 1)
    for (age in min_age:max_age) {
        bd <- sort(abs(scf_ext$age - age))[floor(span*nrow(scf_ext))]
        kernel_weight <- tricube((scf_ext$age - age)/bd)/bd
        data_kernel <- cbind(scf_ext, kernel_weight = kernel_weight)[kernel_weight > 0, ]

        fit <- lm(has_inheritance ~ 1, weights = weight*kernel_weight, data = data_kernel)
        sesq <- vcovHC(fit)

        scf_ext[scf_ext$age == age, "prob_ia"] <- predict(fit, scf_ext[scf_ext$age == age, ])
        scf_ext$se_ia[scf_ext$age == age] <- sqrt(sesq)

        p()
    }
})

pdf(here("graphs", "03-estimate-inheritance-process", "inheritance-extensive-age.pdf"), height = 3.5, width = 3.5)
print(
    scf_ext %>% select(c(age, prob_ia, se_ia)) %>% unique() %>% ggplot() +
        geom_ribbon(aes(x = age, ymin = prob_ia - 1.96*se_ia, ymax = prob_ia + 1.96*se_ia), alpha = 0.2, na.rm = TRUE) +
        geom_line(aes(x = age, y = prob_ia), size = 1, na.rm = TRUE) +
        scale_x_continuous(limits = c(20, 80), breaks = seq(20, 90, 10), minor_breaks = NULL) +
        scale_y_continuous(limits = c(0, 0.01), breaks = seq(0, 0.01, 0.001), minor_breaks = NULL, labels = percent_format(accuracy = 0.1)) +
        ylab("probability of receiving inheritance") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.2), legend.position = "none",
            panel.border = element_blank(), panel.background = element_blank())
)
dev.off()

# ---------------------------------------------------------------------------- #
# Use a nonparametric quantile regression to estimate the rank in the wealth
# distribution conditional on age
# ---------------------------------------------------------------------------- #

min_age <- min(scf_ext$age)
max_age <- max(scf_ext$age)
span <- 0.05
scf_ext[, paste0("cond_perc_wealth", 1:99)] <- NA_real_
with_progress({
    p <- progressor(steps = max_age - min_age + 1)
    for (age in min_age:max_age) {
        if (any(scf_ext$age == age)) {
            bd <- sort(abs(scf_ext$age - age))[floor(span*nrow(scf_ext))]
            kernel_weight <- tricube((scf_ext$age - age)/bd)/bd
            data_kernel <- cbind(scf_ext, kernel_weight = kernel_weight)[kernel_weight > 0, ]

            fit <- rq(rank_wealth ~ 1, weights = kernel_weight*weight, data = data_kernel, tau = c(1:99)/100)

            scf_ext[scf_ext$age == age, paste0("cond_perc_wealth", 1:99)] <- matrix(
                data = rep(predict(fit)[1, ], sum(scf_ext$age == age)),
                ncol = 99,
                nrow = sum(scf_ext$age == age),
                byrow = TRUE
            )
        }
        p()
    }
})


pdf(here("graphs", "03-estimate-inheritance-process", "inheritance-extensive-wealth-age.pdf"), height = 3.5, width = 3.5)
print(
    scf_ext %>% select(c(age, paste0("cond_perc_wealth", 10*(1:9)))) %>% unique() %>% ggplot() +
        geom_line(aes(x = age, y = cond_perc_wealth10, color = "1"), size = 1, alpha = 0.8, na.rm = TRUE) +
        geom_line(aes(x = age, y = cond_perc_wealth20, color = "2"), size = 1, alpha = 0.8, na.rm = TRUE) +
        geom_line(aes(x = age, y = cond_perc_wealth30, color = "3"), size = 1, alpha = 0.8, na.rm = TRUE) +
        geom_line(aes(x = age, y = cond_perc_wealth40, color = "4"), size = 1, alpha = 0.8, na.rm = TRUE) +
        geom_line(aes(x = age, y = cond_perc_wealth50, color = "5"), size = 1, alpha = 0.8, na.rm = TRUE) +
        geom_line(aes(x = age, y = cond_perc_wealth60, color = "6"), size = 1, alpha = 0.8, na.rm = TRUE) +
        geom_line(aes(x = age, y = cond_perc_wealth70, color = "7"), size = 1, alpha = 0.8, na.rm = TRUE) +
        geom_line(aes(x = age, y = cond_perc_wealth80, color = "8"), size = 1, alpha = 0.8, na.rm = TRUE) +
        geom_line(aes(x = age, y = cond_perc_wealth90, color = "9"), size = 1, alpha = 0.8, na.rm = TRUE) +
        scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, 0.1), minor_breaks = NULL) +
        scale_x_continuous(limits = c(20, 80), breaks = seq(20, 90, 10), minor_breaks = NULL) +
        scale_color_brewer(type = "div", palette = "PuOr") +
        annotate(geom = "text", x = 65, y = 0.12, label = "1st decile", color = "#b35806") +
        annotate(geom = "text", x = 30, y = 0.90, label = "9th decile", color = "#542788") +
        ylab("rank in the total wealth distribution") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.2), legend.position = "none",
            panel.border = element_blank(), panel.background = element_blank())
)
dev.off()

# ---------------------------------------------------------------------------- #
# Estimate the function 'phi' that give the probability of receiving
# inheritance as a function of of the rank if the wealth distribution
# conditional on age
# ---------------------------------------------------------------------------- #

cond_rank_wealth <- scf_ext %>% group_by(age) %>% group_split %>% map_dfr(~ {
    x <- c(0, .x[1, paste0("cond_perc_wealth", 1:99)], 1)
    y <- (0:100)/100

    .x$cond_rank_wealth <- approx(x, y, xout = .x$rank_wealth)$y

    return(.x %>% select(-starts_with("cond_perc_wealth")))
})

# Write 'phi' as a polynomial whose coefficients are constrained so that
# its integral over [0, 1] is equal to one. We define specific variables
# to that end.
degree_phi <- 3
data_phi <- data.frame(y = cond_rank_wealth$has_inheritance - cond_rank_wealth$prob_ia)
for (i in 1:degree_phi) {
    data_phi[, paste0("phi", i)] <- cond_rank_wealth$prob_ia*(cond_rank_wealth$cond_rank_wealth^i - 1/(i + 1))
}
fit_phi <- lm(
    formula = y ~ 0 + .,
    data = data_phi,
    weight = cond_rank_wealth$weight
)
sigma_phi <- vcovHC(fit_phi)

value_phi <- function(cond_rank_wealth) {
    prob <- 0
    for (i in 1:degree_phi) {
        prob <- prob + (cond_rank_wealth^i - 1/(i + 1))*coef(fit_phi)[paste0("phi", i)]
    }
    return(1 + prob)
}

se_phi <- function(cond_rank_wealth) {
    y <- as.matrix(sapply(1:degree_phi, function(i) cond_rank_wealth^i - 1/(i + 1)))
    return(sqrt(diag(y %*% sigma_phi %*% t(y))))
}

pdf(here("graphs", "03-estimate-inheritance-process", "phi-inheritance-wealth.pdf"), height = 3.5, width = 3.5)
print(
    data.frame(x = seq(0, 1, length.out = 1000)) %>%
        mutate(value_phi = value_phi(x), se_phi = se_phi(x)) %>%
        ggplot() +
        geom_ribbon(aes(x = x, ymin = value_phi - 1.96*se_phi, ymax = value_phi + 1.96*se_phi), alpha = 0.2) +
        geom_line(aes(x = x, y = value_phi), size = 1) +
        scale_y_continuous(limits = c(0, 2.2), breaks = seq(0, 2.25, 0.25), labels = percent_format(accuracy = 1), minor_breaks = NULL) +
        scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = percent_format(accuracy = 1), minor_breaks = NULL) +
        xlab("rank in the wealth distribution") +
        ylab("relative probability of receiving inheritance") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.2), legend.position = "none",
            panel.border = element_blank(), panel.background = element_blank())
)
dev.off()

# ---------------------------------------------------------------------------- #
# The intensive margin:
#
# Estimate a Spearman rank correlation coefficient between the level of wealth
# and the inheritance received, conditional on age. To that end, we partial
# out age on wealth and inheritance, by year, and then compute the Spearman
# correlation coefficient.
# ---------------------------------------------------------------------------- #

set.seed(19920902)
scf_int <- scf %>%
    filter(year >= 1989) %>%
    filter(inheritance > 0) %>%
    mutate(wealth = wealth - inheritance) %>%
    mutate(u = runif(n()), v = runif(n())) %>%
    group_by(year) %>%
    arrange(year, desc(wealth), u) %>%
    mutate(rank_wealth = 1 - cumsum(weight)/sum(weight)) %>%
    arrange(year, desc(inheritance), v) %>%
    mutate(rank_inheritance = 1 - cumsum(weight)/sum(weight)) %>%
    ungroup() %>%
    mutate(weight = weight/sum(weight)) %>%
    arrange(age)

# Raw correlation between wealth and inheritance
tau <- TauMatrix(cbind(scf_int$rank_wealth, scf_int$rank_inheritance), weights = scf_int$weight)[1, 2]
pdf(here("graphs", "03-estimate-inheritance-process", "inheritance-wealth.pdf"), height = 3.5, width = 3.5)
print(
    scf_int %>%
        ggplot(aes(x = rank_wealth, y = rank_inheritance, alpha = weight)) +
        scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
        scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
        xlab("percentile in the wealth distribution") +
        ylab("percentile in the inheritance distribution") +
        geom_point(na.rm = TRUE) +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.2), legend.position = "none",
            panel.border = element_blank(), panel.background = element_blank())
)
dev.off()

# ---------------------------------------------------------------------------- #
# Rank in the wealth distribution, conditional on age
# ---------------------------------------------------------------------------- #

min_age <- min(scf_int$age)
max_age <- max(scf_int$age)
span <- 0.1
with_progress({
    p <- progressor(steps = 99)
    for (i in 1:99) {
        for (age in min_age:max_age) {
            bd <- sort(abs(scf_int$age - age))[floor(span*nrow(scf_int))]
            kernel_weight <- dnorm(scf_int$age - age, sd = bd)

            fit <- rq(rank_wealth ~ 1, weights = weight*kernel_weight, data = scf_int, tau = i/100)

            scf_int[scf_int$age == age, paste0("pred", i)] <- predict(fit, scf_int[scf_int$age == age, ])
        }
        p()
    }
})

cond_rank_wealth <- scf_int %>% group_by(age) %>% group_split() %>% map_dfr(~ {
    x <- unlist(c(0, .x[1, paste0("pred", 1:99)], 1))
    y <- (0:100)/100

    return(bind_cols(
        cond_rank_wealth = approx(x, y, xout = .x$rank_wealth)$y,
        weight = .x$weight
    ))
})

pdf(here("graphs", "03-estimate-inheritance-process", "marginal-wealth-age.pdf"), height = 3.5, width = 3.5)
print(cond_rank_wealth %>%
    ggplot(aes(cond_rank_wealth, stat(density), weight = weight)) +
    scale_y_continuous(limits = c(0, 1.5)) +
    scale_x_continuous(limits = c(0, 1), labels = percent, breaks = seq(0, 1, 0.1)) +
    xlab("percentile in wealth distribution\n(conditional on age)") +
    geom_histogram(breaks = seq(0, 1, 0.05), na.rm = TRUE) +
    theme_bw())
dev.off()

pdf(here("graphs", "03-estimate-inheritance-process", "wealth-age.pdf"), height = 3.5, width = 3.5)
print(scf_int %>% ggplot() +
    geom_point(aes(x = age, y = rank_wealth, alpha = weight), na.rm = TRUE) +
    geom_line(aes(x = age, y = pred10), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred20), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred30), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred40), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred50), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred60), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred70), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred80), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred90), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    scale_y_continuous(labels = percent, limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(limits = c(20, 80), breaks = seq(20, 80, 10)) +
    ylab("percentile in the wealth distribution") +
    theme_bw() +
    theme(legend.position = "none"))
dev.off()

# ---------------------------------------------------------------------------- #
# Rank in the inheritance distribution, conditional on age
# ---------------------------------------------------------------------------- #

min_age <- min(scf_int$age)
max_age <- max(scf_int$age)
span <- 0.1
with_progress({
    p <- progressor(steps = 99)
    for (i in 1:99) {
        for (age in min_age:max_age) {
            bd <- sort(abs(scf_int$age - age))[floor(span*nrow(scf_int))]
            kernel_weight <- dnorm(scf_int$age - age, sd = bd)

            fit <- rq(rank_inheritance ~ 1, weights = weight*kernel_weight, data = scf_int, tau = i/100)

            scf_int[scf_int$age == age, paste0("pred", i)] <- predict(fit, scf_int[scf_int$age == age, ])
        }
        p()
    }
})

cond_rank_inheritance <- scf_int %>% group_by(age) %>% group_split() %>% map_dfr(~ {
    x <- unlist(c(0, .x[1, paste0("pred", 1:99)], 1))
    y <- (0:100)/100

    return(bind_cols(
        cond_rank_inheritance = approx(x, y, xout = .x$rank_inheritance)$y,
        weight = .x$weight
    ))
})

pdf(here("graphs", "03-estimate-inheritance-process", "marginal-inheritance-age.pdf"), height = 3.5, width = 3.5)
print(cond_rank_inheritance %>%
    ggplot(aes(cond_rank_inheritance, stat(density), weight = weight)) +
    scale_y_continuous(limits = c(0, 1.5)) +
    scale_x_continuous(limits = c(0, 1), labels = percent, breaks = seq(0, 1, 0.1)) +
    xlab("percentile in inheritance distribution\n(conditional on age)") +
    geom_histogram(breaks = seq(0, 1, 0.05)) +
    theme_bw())
dev.off()

pdf(here("graphs", "03-estimate-inheritance-process", "inheritance-age.pdf"), height = 3.5, width = 3.5)
print(scf_int %>% ggplot() +
    geom_point(aes(x = age, y = rank_inheritance, alpha = weight), na.rm = TRUE) +
    geom_line(aes(x = age, y = pred10), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred20), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred30), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred40), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred50), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred60), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred70), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred80), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    geom_line(aes(x = age, y = pred90), color = "#377eb8", size = 0.7, alpha = 0.8, na.rm = TRUE) +
    scale_y_continuous(labels = percent, limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(limits = c(20, 80), breaks = seq(20, 80, 10)) +
    ylab("percentile in the inheritance distribution") +
    theme_bw() +
    theme(legend.position = "none"))
dev.off()

# ---------------------------------------------------------------------------- #
# Conditional rank correlation
# ---------------------------------------------------------------------------- #

scf_int <- scf_int %>% select(-starts_with("pred"))
scf_int <- cbind(
    scf_int,
    cond_rank_wealth = cond_rank_wealth$cond_rank_wealth,
    cond_rank_inheritance = cond_rank_inheritance$cond_rank_inheritance
)

copula_inheritance <- BiCopSelect(
    u1 = scf_int$cond_rank_wealth,
    u2 = scf_int$cond_rank_inheritance,
    weight = scf_int$weight,
    method = "itau",
    # Only consider single-parameter families
    familyset = c(1, 3, 4, 5, 6, 13, 14, 16, 23, 24, 26, 33, 34, 36),
    se = TRUE
)

pdf(here("graphs", "03-estimate-inheritance-process", "inheritance-wealth-conditional.pdf"), height = 3.5, width = 3.5)
print(
    scf_int %>%
        ggplot(aes(x = cond_rank_wealth, y = cond_rank_inheritance, alpha = weight)) +
        scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
        scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
        xlab("percentile in the wealth distribution") +
        ylab("percentile in the inheritance distribution") +
        geom_point(na.rm = TRUE) +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.2), legend.position = "none",
            panel.border = element_blank(), panel.background = element_blank())
)
dev.off()

# ---------------------------------------------------------------------------- #
# Simulate the flow of inheritance and its distribution based on
# the demographic data and the parameters estimated above
# ---------------------------------------------------------------------------- #

# Determine the rank in the wealth distribution conditional on age, by year
dina_micro_inheritance <- dina_micro_children %>%
    as.data.frame() %>%
    group_by(year, age) %>%
    arrange(year, age, desc(wealth)) %>%
    mutate(cond_age_rank_wealth = 1 - cumsum(weight)/sum(weight)) %>%
    ungroup()

# Value of 'phi', the wealth-specific factor that determines the probability
# of receiving inheritance
dina_micro_inheritance <- dina_micro_inheritance %>% mutate(phi = value_phi(cond_age_rank_wealth))

# Determine rank on the wealth distribution conditional on age and having
# received inheritance
dina_micro_inheritance <- dina_micro_inheritance %>%
    group_by(year, age) %>%
    arrange(year, age, desc(wealth)) %>%
    mutate(cond_age_inh_rank_wealth = 1 - cumsum(phi*weight)/sum(phi*weight)) %>%
    ungroup()

# Store key individual parameters of the inheritance simulation for when
# we simulate the process
dina_micro_inheritance_params <- dina_micro_inheritance %>% transmute(
    id, year, sex, weight, wealth, phi, cond_age_rank_wealth, cond_age_inh_rank_wealth
)

dina_micro_inheritance <- dina_micro_inheritance %>% group_by(year) %>% group_split()
with_progress({
    set.seed(19920902)
    p <- progressor(along = dina_micro_inheritance)
    dina_micro_inheritance <- dina_micro_inheritance %>% map_dfr(~ {
        data_year <- .x$year[1]

        # Add the mortality rate of spouse, if any
        data_qx <- .x %>%
            select(id, sex, qx) %>%
            pivot_wider(id, names_from = sex, values_from = qx) %>%
            rename(qx_male = male, qx_female = female)
        .x <- .x %>% left_join(data_qx, by = "id") %>%
            mutate(qx_spouse = if_else(sex == "female", qx_male, qx_female))

        # Distribution of people who die with a surviving spouse: it is the sample
        # made up of couples, weighted by the probability of dying AND the
        # probability that their spouse does NOT die
        dead_with_surviving_spouse <- .x %>%
            filter(couple == 1) %>%
            mutate(weight = weight*qx*(1 - qx_spouse)) %>%
            mutate(status = "dead with surviving spouse")

        # Distribution of people who live with a dying spouse: it is the sample
        # made up of couples, weighted by the probability of NOT dying AND the
        # probability that their spouse die
        living_with_dead_spouse <- .x %>%
            filter(couple == 1) %>%
            mutate(weight = weight*qx_spouse*(1 - qx)) %>%
            mutate(status = "living with dead spouse")

        # Distribution of people who die without a surviving spouse: it combines
        # the sample of singles weighted by the probability of dying with the
        # samples of couples weighted by the probability of both spouses dying
        dead_without_surviving_spouse <- .x %>%
            mutate(weight = if_else(as.logical(couple), weight*qx*qx_spouse, weight*qx)) %>%
            mutate(status = "dying with no surviving spouse")

        # People with no change of status
        no_change <- .x %>%
            mutate(weight = if_else(as.logical(couple), weight*(1 - qx)*(1 - qx_spouse), weight*(1 - qx))) %>%
            mutate(status = "no change")

        # People living with a dead spouse receive the wealth of their spouse
        # without taxation
        living_with_dead_spouse <- living_with_dead_spouse %>% mutate(transfer_spouse = wealth)

        # For people who die without a surviving spouse: wealth goes to their
        # children in equal proportion, after payment of estate taxes
        transfer_children <- dead_without_surviving_spouse %>%
            mutate(estate_tax = estate_tax(wealth, data_year)) %>%
            select(id, weight, wealth, estate_tax, starts_with("age_child")) %>%
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
        living <- bind_rows(living_with_dead_spouse, no_change) %>% group_by(age, sex) %>% group_split() %>% map_dfr(~ {
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

        # Combine the dead and the living in a single sample
        data_inheritance <- bind_rows(
            living,
            dead_with_surviving_spouse %>% select(-starts_with("age_child")),
            dead_without_surviving_spouse %>% select(-starts_with("age_child"))
        )

        p()

        return(data_inheritance %>% select(year, weight, wealth, transfer_parent, transfer_spouse, transfer_estate_tax))
    })
})

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

write_rds(dina_micro_inheritance, here("work", "03-estimate-inheritance-process", "dina_micro_inheritance.rds"))
write_rds(copula_inheritance, here("work", "03-estimate-inheritance-process", "copula_inheritance.rds"))
write_rds(dina_micro_inheritance_params, here("work", "03-estimate-inheritance-process", "dina_micro_inheritance_params.rds"))
