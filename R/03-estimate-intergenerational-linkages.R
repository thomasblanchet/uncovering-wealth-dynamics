# ---------------------------------------------------------------------------- #
# Simulate intergenerational linkages between parents and their children
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(here)
library(progressr)
library(scales)
library(furrr)
library(zoo)

handlers(handler_progress(
    format = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta"
))
options(dplyr.summarise.inform = FALSE)

population <- read_rds(here("work", "02-import-population", "population.rds"))
life_table <- read_rds(here("work", "02-import-mortality", "life_table.rds"))
fertility_both <- read_rds(here("work", "03-estimate-male-fertility", "fertility_both.rds"))
dina_micro <- read_rds(here("work", "03-combine-calibrate-microdata", "dina_micro.rds"))

# First, calculate sex ratio at birth from the population data
sex_ratio_birth <- population %>%
    filter(variant == "Past" | variant == "Medium") %>%
    filter(age == 0) %>%
    pivot_wider(c(year, variant), names_from = "sex", values_from = "population") %>%
    mutate(frac_male = male/(male + female), frac_female = female/(male + female)) %>%
    select(year, frac_male, frac_female)

fertility <- fertility_both %>% filter(variant == "Past" | variant == "Medium") %>% select(-variant)

# Then, simulate children to all the people in the DINA microdata using
# real demographic data
dina_micro_children <- dina_micro %>%
    group_by(year, sex, age) %>%
    group_split()
# Execute in parallel
plan(sequential)
with_progress({
    set.seed(19920902)
    p <- progressor(along = dina_micro_children)

    dina_micro_children <- dina_micro_children %>% future_map_dfr(~ {
        data_year <- .x$year[1]
        data_sex <- .x$sex[1]
        data_age <- .x$age[1]
        n <- nrow(.x)

        # Keep the fertility rates associated to the gender of interest
        asfr <- fertility %>% filter(sex == data_sex)

        # For someone aged A in year Y, their children must have been born
        # between year Y - A and Y
        asfr <- asfr %>% filter(year >= (data_year - data_age) & (year <= data_year))
        # If their child is born in year Y*, the parent has to be aged (A - Y + Y*)
        asfr <- asfr %>% filter(age == data_age - data_year + year) %>% arrange(year)

        # Complete the ASFR to include years until present (including
        # past child-bearing age)
        asfr <- asfr %>% complete(
            year = min(year):data_year,
            fill = list(
                asfr = 0, asfr1 = 0, asfr2 = 0, asfr3 = 0, asfr4 = 0, asfr5 = 0,
                bc = 0, bc1 = 0, bc2 = 0, bc3 = 0, bc4 = 0, bc5 = 0,
                sex = data_sex
            )
        )
        asfr <- asfr %>%
            left_join(sex_ratio_birth, by = "year") %>%
            mutate(age = data_age - data_year + year)

        # Matrix with the age of children born in each year
        birth_male <- matrix(data = FALSE, nrow = n, ncol = nrow(asfr))
        birth_female <- matrix(data = FALSE, nrow = n, ncol = nrow(asfr))
        birth_order_male <- matrix(data = 0, nrow = n, ncol = nrow(asfr))
        birth_order_female <- matrix(data = 0, nrow = n, ncol = nrow(asfr))
        children_total <- matrix(data = 0, nrow = n, ncol = nrow(asfr))

        # Loop over years and have children be born randomly according
        # to demographic parameters
        for (i in 1:nrow(asfr)) {
            current_year <- asfr$year[i]

            # The number of birth of each person depends on the number of children
            # they already had.
            asfr_year_age <- unlist(asfr[i, paste0("asfr", 1:5)])
            # The fertility rate is computed based on the total number of persons
            # in the age class: to simulate the appropriate number of births by
            # birth order we adjust the rates to use the number of persons
            # with the relevant number of children already born
            frac_number_children <- c(
                sum(children_total[, i] == 0),
                sum(children_total[, i] == 1),
                sum(children_total[, i] == 2),
                sum(children_total[, i] == 3),
                sum(children_total[, i] >= 4)
            )/n
            asfr_year_age <- asfr_year_age/frac_number_children

            newborn <- (runif(n) <= asfr_year_age[pmin(children_total[, i] + 1, 5)])
            newborn_sex_male <- (runif(n) <= asfr$frac_male[i])
            newborn_male <- newborn & newborn_sex_male
            newborn_female <- newborn & !newborn_sex_male

            children_total[newborn, i:nrow(asfr)] <- children_total[newborn, i:nrow(asfr)] + 1

            birth_order_male[newborn_male, i:nrow(asfr)] <- birth_order_male[newborn_male, i:nrow(asfr)] + 1
            birth_order_female[newborn_female, i:nrow(asfr)] <- birth_order_female[newborn_female, i:nrow(asfr)] + 1

            birth_male[newborn_male, i] <- TRUE
            birth_female[newborn_female, i] <- TRUE
        }

        # Then have children dies randomly according to age/year/gender specific
        # mortality rates
        max_children_female <- max(rowSums(birth_female))
        max_children_male <- max(rowSums(birth_male))

        age_children_male <- matrix(data = NA, nrow = n, ncol = max_children_male)
        age_children_female <- matrix(data = NA, nrow = n, ncol = max_children_female)

        for (i in 1:nrow(asfr)) {
            current_year <- asfr$year[i]

            life_table_male <- life_table %>%
                filter(variant == "Medium" | variant == "Past") %>%
                filter(year == current_year & sex == "male") %>%
                arrange(age) %>%
                select(age, qx)
            life_table_female <- life_table %>%
                filter(variant == "Medium" | variant == "Past") %>%
                filter(year == current_year & sex == "female") %>%
                arrange(age)

            # Increment the age of formerly born children
            age_children_male <- 1 + age_children_male
            age_children_female <- 1 + age_children_female

            # Put an age zero to children born in corresponding year
            age_children_male[cbind(
                which(birth_male[, i]),
                birth_order_male[birth_male[, i], i]
            )] <- 0
            age_children_female[cbind(
                which(birth_female[, i]),
                birth_order_female[birth_female[, i], i]
            )] <- 0

            # Randomly kill the children based on their age/year/gender specific
            # mortality rates
            qx_males <- apply(age_children_male, 2, function(age) {
                qx <- tibble(age = age) %>% left_join(life_table_male, by = "age") %>% pull(qx)
                if (length(qx) == 0) {
                    return(rep(NA, length(age)))
                } else {
                    return(qx)
                }
            })

            if (length(dim(qx_males)) > 0) {
                death_males <- apply(qx_males, 2, function(qx) {
                    death <- rep(FALSE, length(qx))
                    nmiss <- !is.na(qx)
                    death[nmiss] <- (runif(sum(nmiss)) <= qx[nmiss])
                    return(death)
                })

                age_children_male[death_males] <- NA
            }

            qx_females <- apply(age_children_female, 2, function(age) {
                qx <- tibble(age = age) %>% left_join(life_table_female, by = "age") %>% pull(qx)
                if (length(qx) == 0) {
                    return(rep(NA, length(age)))
                } else {
                    return(qx)
                }
            })

            if (length(dim(qx_females)) > 0) {
                death_females <- apply(qx_females, 2, function(qx) {
                    death <- rep(FALSE, length(qx))
                    nmiss <- !is.na(qx)
                    death[nmiss] <- (runif(sum(nmiss)) <= qx[nmiss])
                    return(death)
                })

                age_children_female[death_females] <- NA
            }
        }

        if (ncol(age_children_male) > 0) {
            colnames(age_children_male) <- paste0("age_child_male", 1:ncol(age_children_male))
        }
        if (ncol(age_children_female) > 0) {
            colnames(age_children_female) <- paste0("age_child_female", 1:ncol(age_children_female))
        }

        .x <- cbind(.x, age_children_male, age_children_female)
        p()
        return(.x)
    })
})

# Define age groups
dina_micro_children <- dina_micro_children %>% mutate(
    age_group = cut(age, breaks = c(seq(20, 90, 5), +Inf), include.lowest = TRUE, labels = FALSE)
)

# ---------------------------------------------------------------------------- #
# Save
# ---------------------------------------------------------------------------- #

dir.create(here("work", "03-estimate-intergenerational-linkages"), showWarnings = FALSE)
write_rds(dina_micro_children, here("work", "03-estimate-intergenerational-linkages", "dina_micro_children.rds"))

# ---------------------------------------------------------------------------- #
# Graphs
# ---------------------------------------------------------------------------- #

dir.create(here("graphs", "03-estimate-intergenerational-linkages"), showWarnings = FALSE)

pdf(file = here("graphs", "03-estimate-intergenerational-linkages", "age-children.pdf"), height = 7, width = 7*(1 + sqrt(5))/2)
for (y in unique(dina_micro_children$year)) {
    cat(paste0("--> ", y, "\n"))

    data <- dina_micro_children %>%
        filter(year == y) %>%
        select(weight, sex_parent = sex, qx, starts_with("age_child_female"), starts_with("age_child_male")) %>%
        mutate(id = row_number()) %>%
        pivot_longer(-c(id, weight, sex_parent, qx), values_to = "age", names_to = "variable") %>%
        filter(!is.na(age)) %>%
        mutate(variable = as.character(variable)) %>%
        mutate(sex_child = if_else(startsWith(variable, "age_child_female"),
            "child: female",
            "child: male"
        )) %>%
        mutate(sex_parent = paste("parent:", sex_parent)) %>%
        mutate(birth_order = as.integer(str_match(variable, "(\\d+)")[, 2])) %>%
        mutate(birth_order = pmin(birth_order, 5)) %>%
        mutate(birth_order = case_when(
            birth_order == 1 ~ "1",
            birth_order == 2 ~ "2",
            birth_order == 3 ~ "3",
            birth_order == 4 ~ "4",
            birth_order == 5 ~ "5+"
        )) %>%
        group_by(sex_parent, sex_child, birth_order, age) %>%
        summarise(population = sum(weight)) %>%
        group_by(sex_parent, sex_child, birth_order) %>%
        arrange(sex_parent, sex_child, birth_order, age) %>%
        mutate(population = rollapply(population, width = 5, FUN = mean, align = "center", partial = TRUE, na.rm = TRUE)) %>%
        ungroup()

    p <- data %>%
        ggplot(aes(x = age, y = population, fill = birth_order)) +
        coord_flip() +
        facet_grid(sex_child ~ sex_parent) +
        scale_x_continuous(limits = c(-1, 91), breaks = seq(0, 90, 10)) +
        scale_y_continuous(limits = c(0, 3000000), breaks = seq(0, 3000000, 500000), labels = comma) +
        scale_fill_brewer(name = "birth order", type = "seq", palette = "PuBu") +
        geom_col(position = position_stack(reverse = TRUE), na.rm = TRUE) +
        ggtitle(paste("Children with a live parent in", y)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))

    print(p)

    remove(data)
}
dev.off()

pdf(file = here("graphs", "03-estimate-intergenerational-linkages", "age-children-parent-death.pdf"), height = 7, width = 7*(1 + sqrt(5))/2)
for (y in unique(dina_micro_children$year)) {
    cat(paste0("--> ", y, "\n"))

    data <- dina_micro_children %>%
        filter(year == y) %>%
        select(weight, sex_parent = sex, qx, starts_with("age_child_female"), starts_with("age_child_male")) %>%
        mutate(id = row_number()) %>%
        pivot_longer(-c(id, weight, sex_parent, qx), values_to = "age", names_to = "variable") %>%
        filter(!is.na(age)) %>%
        mutate(variable = as.character(variable)) %>%
        mutate(sex_child = if_else(startsWith(variable, "age_child_female"),
            "child: female",
            "child: male"
        )) %>%
        mutate(sex_parent = paste("parent:", sex_parent)) %>%
        mutate(birth_order = as.integer(str_match(variable, "(\\d+)")[, 2])) %>%
        mutate(birth_order = pmin(birth_order, 5)) %>%
        mutate(birth_order = case_when(
            birth_order == 1 ~ "1",
            birth_order == 2 ~ "2",
            birth_order == 3 ~ "3",
            birth_order == 4 ~ "4",
            birth_order == 5 ~ "5+"
        )) %>%
        group_by(sex_parent, sex_child, birth_order, age) %>%
        summarise(population = sum(weight*qx, na.rm = TRUE)) %>%
        group_by(sex_parent, sex_child, birth_order) %>%
        arrange(sex_parent, sex_child, birth_order, age) %>%
        mutate(population = rollapply(population, width = 5, FUN = mean, align = "center", partial = TRUE, na.rm = TRUE)) %>%
        ungroup()

    p <- data %>%
        ggplot(aes(x = age, y = population, fill = birth_order)) +
        coord_flip() +
        facet_grid(sex_child ~ sex_parent) +
        scale_x_continuous(limits = c(-1, 91), breaks = seq(0, 90, 10)) +
        scale_y_continuous(limits = c(0, 100000), breaks = seq(0, 100000, 20000), labels = comma) +
        scale_fill_brewer(name = "birth order", type = "seq", palette = "PuBu") +
        geom_col(position = position_stack(reverse = TRUE), na.rm = TRUE) +
        ggtitle(paste("Age distribution of children with a parent that died in", y)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))

    print(p)

    remove(data)
}
dev.off()

