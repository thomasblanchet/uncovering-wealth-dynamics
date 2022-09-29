# ---------------------------------------------------------------------------- #
# Set of useful utility functions to be used throughout the code
# ---------------------------------------------------------------------------- #

library(here)
library(readr)

# ---------------------------------------------------------------------------- #
# Weighted statistics
# ---------------------------------------------------------------------------- #

weighted_quantile <- function(y, weight, probs) {
    ord <- order(y)
    weight <- weight/sum(weight)
    p <- cumsum(weight[ord]) - weight[ord]/2
    q <- approx(p, y[ord], xout = probs, rule = 2)$y
    names(q) <- paste0(100*probs, "%")
    return(q)
}

weighted_var <- function(y, weight, na.rm = FALSE) {
    # Note max(., 0) used in case negative values in edge cases due to
    # numerical instability
    return(max(weighted.mean(y^2, weight, na.rm = na.rm) - weighted.mean(y, weight, na.rm = na.rm)^2, 0))
}

weighted_sd <- function(y, weight, na.rm = FALSE) {
    return(sqrt(weighted_var(y, weight, na.rm = na.rm)))
}

weighted_cov <- function(x, y, weight, na.rm = FALSE) {
    e_xy <- weighted.mean(x*y, weight, na.rm = na.rm)
    e_x <- weighted.mean(x, weight, na.rm = na.rm)
    e_y <- weighted.mean(y, weight, na.rm = na.rm)
    return(e_xy - e_x*e_y)
}

# ---------------------------------------------------------------------------- #
# Winsorizing
# ---------------------------------------------------------------------------- #

winsorize <- function(x, range) {
    x[x < min(range)] <- min(range)
    x[x > max(range)] <- max(range)
    return(x)
}

# ---------------------------------------------------------------------------- #
# Kernel functions
# ---------------------------------------------------------------------------- #

tricube <- function(u) {
    w <- (70/81)*(1 - abs(u)^3)^3
    w[w < 0] <- 0
    return(w)
}

rectangular <- function(u) {
    return(dunif(u, min = -1, max = +1))
}

# ---------------------------------------------------------------------------- #
# Weighted local polynomial regressions
# ---------------------------------------------------------------------------- #

nreg_drv0_grid <- function(x, y, grid, weight = NULL, alpha = NULL, bw = NULL) {
    if (is.null(weight)) {
        weight <- rep(1, length(x))
    }
    fitted_y <- sapply(grid, function(a) {
        if (is.null(bw)) {
            if (is.null(alpha)) {
                stop("specify either 'bw' or 'alpha'")
            }
            # Determine the bandwidth so that we use a fraction alpha of the data
            bw <- sort(unique(abs(x - a)))
            bw <- bw[ceiling(length(bw)*alpha)]
        }
        # Only use observations within the support of the kernel
        s <- (abs(x - a) < bw)
        kernel_weight <- rectangular((x[s] - a)/bw)/bw
        fitted <- weighted.mean(y[s], weight[s]*kernel_weight)
        return(fitted)
    })
    return(list(x = grid, y = fitted_y))
}

nreg_drv0 <- function(x, y, weight = NULL, alpha = NULL, bw = NULL) {
    grid <- seq(min(x), max(x), length.out = 200)
    nreg <- nreg_drv0_grid(x, y, weight = weight, alpha = alpha, bw = bw, grid = grid)
    if (sum(!is.na(nreg$x) & !is.na(nreg$y)) <= 2) {
        return(list(x = x, y = rep(NA_real_, length(y)), weight = weight))
    }
    fitted_y <- approx(nreg$x, nreg$y, xout = x, rule = 2)$y
    return(list(x = x, y = fitted_y, weight = weight))
}

nreg_drv0_cov_grid <- function(x, y, grid, weight = NULL, alpha = NULL, bw = NULL) {
    if (is.null(weight)) {
        weight <- rep(1, length(x))
    }
    fitted_y <- t(sapply(grid, function(a) {
        if (is.null(bw)) {
            if (is.null(alpha)) {
                stop("specify either 'bw' or 'alpha'")
            }
            # Determine the bandwidth so that we use a fraction alpha of the data
            bw <- sort(unique(abs(x - a)))
            bw <- bw[ceiling(length(bw)*alpha)]
        }
        # Only use observations within the support of the kernel
        s <- (abs(x - a) < bw)
        kernel_weight <- rectangular((x[s] - a)/bw)/bw

        norm_weight <- weight[s]*kernel_weight
        norm_weight <- norm_weight/sum(norm_weight)

        if (ncol(as.matrix(y)) > 1) {
            fitted_mean <- colSums(norm_weight*y[s, ])

            Y <- t(t(as.matrix(y[s, ])) - fitted_mean)
            fitted_cov <- as.vector(crossprod(Y, norm_weight*Y))
        } else {
            fitted_mean <- sum(norm_weight*y[s])

            Y <- y[s] - fitted_mean
            fitted_cov <- sum(norm_weight*Y^2)
        }

        return(cbind(x = a, t(fitted_mean), t(fitted_cov)))
    }))

    fitted_y <- as.data.frame(fitted_y)

    if (ncol(as.matrix(y)) > 1) {
        names_mean <- paste0("mean_", names(y))
        names_cov <- as.vector(outer(names(y), names(y), function(v1, v2) if_else(
            v1 == v2,
            paste0("var_", v1),
            paste0("cov_", v1, "_", v2)
        )))
        colnames(fitted_y) <- c("x", names_mean, names_cov)
    } else {
        colnames(fitted_y) <- c("x", "mean_y", "var_y")
    }

    return(fitted_y)
}

nreg_drv1_grid <- function(x, y, grid, weight = NULL, alpha = NULL, bw = NULL) {
    if (is.null(weight)) {
        weight <- rep(1, length(x))
    }
    fitted_y <- sapply(grid, function(a) {
        if (is.null(bw)) {
            if (is.null(alpha)) {
                stop("specify either 'bw' or 'alpha'")
            }
            # Determine the bandwidth so that we use a fraction alpha of the data
            bw <- sort(unique(abs(x - a)))
            bw <- bw[ceiling(length(bw)*alpha)]
        }
        # Only use observations within the support of the kernel
        s <- (abs(x - a) < bw)
        if (all(is.na(y[s]))) {
            return(NA)
        }
        kernel_weight <- rectangular((x[s] - a)/bw)/bw
        fitted <- lm(y[s] ~ x[s], weights = weight[s]*kernel_weight)
        return(coef(fitted)[2])
    })
    return(list(x = grid, y = fitted_y))
}

nreg_drv1 <- function(x, y, weight = NULL, alpha = NULL, bw = NULL) {
    grid <- seq(min(x), max(x), length.out = 200)
    nreg <- nreg_drv1_grid(x, y, weight = weight, alpha = alpha, bw = bw, grid = grid)
    if (sum(!is.na(nreg$x) & !is.na(nreg$y)) <= 2) {
        return(list(x = x, y = rep(NA_real_, length(y)), weight = weight))
    }
    fitted_y <- approx(nreg$x, nreg$y, xout = x, rule = 2)$y
    return(list(x = x, y = fitted_y, weight = weight))
}

# ---------------------------------------------------------------------------- #
# Function to calculate top income shares
# ---------------------------------------------------------------------------- #

top_shares <- function(y, weights = NULL, p, rank_by = NULL) {
    if (is.null(weights)) {
        weights <- rep(1, length(y))
    }
    if (is.null(rank_by)) {
        rank_by <- y
    }
    data <- cbind(y, weights, rank_by)
    data <- na.omit(data)
    data <- data[order(data[, 3]), ]
    n <- nrow(data)

    # Calculate ranks
    r <- cumsum(data[, 2])
    r <- c(0, r[-n])/r[n]

    # Calculate top shares
    return(sapply(p, function(p) {
        return(sum(data[r >= p, 1]*data[r >= p, 2])/sum(data[, 1]*data[, 2]))
    }))
}

# ---------------------------------------------------------------------------- #
# Estimate CDF from density object
# ---------------------------------------------------------------------------- #

cdf_from_density <- function(density, p0, p1) {
    x <- density$x
    y <- density$y
    n <- length(x)
    dx <- diff(x)

    cdf <- cumsum(c(p0, (y[1:(n - 1)] + y[2:n])/2*dx, p1))
    cdf <- cdf[1:n]/cdf[n + 1]

    return(cdf)
}

# ---------------------------------------------------------------------------- #
# Perform linear calibration of weighted data
# ---------------------------------------------------------------------------- #

linear_calibration <- function(d, X, M) {
    # Sample totals
    m <- as.vector(d %*% X)

    # Perform linear calibration
    h <- solve(t(X * d) %*% X, M - m)
    w <- d*(1 + as.vector(X %*% h))

    return(w)
}

# ---------------------------------------------------------------------------- #
# Match two datasets statistically based on a continuous variable
# ---------------------------------------------------------------------------- #

stat_match <- function(x, y, by, weight) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)

    n_x <- nrow(x)
    n_y <- nrow(y)

    by_x <- as.vector(x[, by])
    by_y <- as.vector(y[, by])

    # Break ties randomly in the sort
    x <- x[order(by_x, runif(n_x)), ]
    y <- y[order(by_y, runif(n_y)), ]

    wgt_x <- as.vector(x[, weight])
    wgt_y <- as.vector(y[, weight])

    wgt_y <- wgt_y*sum(wgt_x)/sum(wgt_y)

    i <- 1
    j <- 1
    k <- 1
    match_id_x <- vector(mode = "numeric", length = n_x + n_y - 1)
    match_id_y <- vector(mode = "numeric", length = n_x + n_y - 1)
    match_wgt <- vector(mode = "numeric", length = n_x + n_y - 1)

    while (i <= n_x && j <= n_y) {
        match_id_x[k] <- i
        match_id_y[k] <- j

        if (wgt_x[i] < wgt_y[j]) {
            match_wgt[k] <- wgt_x[i]
            wgt_y[j] <- wgt_y[j] - wgt_x[i]
            i <- i + 1
        } else if (wgt_x[i] > wgt_y[j]) {
            match_wgt[k] <- wgt_y[j]
            wgt_x[i] <- wgt_x[i] - wgt_y[j]
            j <- j + 1
        } else {
            match_wgt[k] <- wgt_y[j]
            i <- i + 1
            j <- j + 1
        }
        k <- k + 1
    }
    match_id_x <- match_id_x[1:(k - 1)]
    match_id_y <- match_id_y[1:(k - 1)]
    match_wgt <- match_wgt[1:(k - 1)]

    match_x <- x[match_id_x, !(colnames(x) %in% c(weight))]
    match_y <- y[match_id_y, !(colnames(y) %in% c(weight, by))]

    cnames <- c(
        colnames(x)[!(colnames(x) %in% c(weight))],
        colnames(y)[!(colnames(y) %in% c(weight, by))]
    )
    df <- cbind(match_x, match_y)
    colnames(df) <- cnames

    df[, weight] <- match_wgt

    return(df)
}

# ---------------------------------------------------------------------------- #
# Function estimating Deming regression
# ---------------------------------------------------------------------------- #

deming <- function(y, x, w = NULL, delta = 1) {
    if (delta < 0) {
        stop("delta must be nonnegative")
    }
    if (is.null(w)) {
        w <- rep(1, length(y))
    }

    # Relevant moments
    mx <- weighted.mean(x, w)
    my <- weighted.mean(y, w)
    sxx <- weighted.mean((x - mx)^2, w)
    syy <- weighted.mean((y - my)^2, w)
    sxy <- weighted.mean((x - mx)*(y - my), w)

    # Coefficients
    if (delta == 0) {
        slope <- sxy/sxx
    } else if (is.infinite(delta)) {
        slope <- syy/sxy
    } else {
        slope <- (syy - delta*sxx + sqrt((syy - delta*sxx)^2 + 4*delta*sxy^2))/(2*sxy)
    }

    # Constrain slope to be nonpositive
    if (slope > 0) {
        slope <- 0
    }

    intercept <- my - slope*mx

    # Fitted values
    xfit <- x + slope/(slope^2 + delta)*(y - intercept - slope*x)
    yfit <- slope*xfit + intercept

    return(list(slope = slope, intercept = intercept, xfit = xfit, yfit = yfit))
}

# ---------------------------------------------------------------------------- #
# Store functions
# ---------------------------------------------------------------------------- #

dir.create(here("work", "01-utils"), showWarnings = FALSE)

write_rds(weighted_quantile,  here("work", "01-utils", "weighted_quantile.rds"))
write_rds(weighted_var,       here("work", "01-utils", "weighted_var.rds"))
write_rds(weighted_cov,       here("work", "01-utils", "weighted_cov.rds"))
write_rds(weighted_sd,        here("work", "01-utils", "weighted_sd.rds"))
write_rds(winsorize,          here("work", "01-utils", "winsorize.rds"))
write_rds(tricube,            here("work", "01-utils", "tricube.rds"))
write_rds(rectangular,        here("work", "01-utils", "rectangular.rds"))
write_rds(nreg_drv0_grid,     here("work", "01-utils", "nreg_drv0_grid.rds"))
write_rds(nreg_drv0,          here("work", "01-utils", "nreg_drv0.rds"))
write_rds(nreg_drv0_cov_grid, here("work", "01-utils", "nreg_drv0_cov_grid.rds"))
write_rds(nreg_drv1_grid,     here("work", "01-utils", "nreg_drv1_grid.rds"))
write_rds(nreg_drv1,          here("work", "01-utils", "nreg_drv1.rds"))
write_rds(top_shares,         here("work", "01-utils", "top_shares.rds"))
write_rds(cdf_from_density,   here("work", "01-utils", "cdf_from_density.rds"))
write_rds(linear_calibration, here("work", "01-utils", "linear_calibration.rds"))
write_rds(stat_match,         here("work", "01-utils", "stat_match.rds"))
write_rds(deming,             here("work", "01-utils", "deming.rds"))
