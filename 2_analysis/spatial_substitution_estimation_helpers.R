# ============================================================================
# spatial_substitution_estimation_helpers.R
#
# All of the shared machinery the spatial-substitution SDEM estimation needs,
# in one place. Sourced by spatial_substitution_estimate_run.R (and by the
# plot). It is the merge of three files that used to live separately:
#
#   sdem_helpers.R      name cleaning, coef/vcov extraction, direct/indirect
#                       /total impacts, and the smoke-bin colour/label vectors
#   sdem_sparse_ml.R    the sparse maximum-likelihood engine for the panel SDEM
#   sdem_v4_design.R    the sample loader, the sparse design, and everything
#                       downstream of the fit (impacts, the typical realized
#                       episode, joint tests, the coefficient table)
#
# The only behavioural change from those three files is the sample loader:
# build_spatial_weight_matrix.R now writes ONE self-contained prepared-panel
# cache carrying the raw outcome and the FE factors alongside W and the raw
# smoke bins, so sdem_v4_load_panel() reads a single file instead of the old
# prepared-panel + appendix-base pair. Nothing else about the estimator moved.
# ============================================================================

library(tidyverse)
library(Matrix)

# The six own-county smoke bins (beta), their W-lagged Durbin terms (theta),
# and the four weather bin factors that go in X.
SMOKE_VARS   <- c("bin_d_001_015", "bin_d_015_030", "bin_d_030_060",
                  "bin_d_060_100", "bin_d_100_200", "bin_d_200_500")
W_SMOKE_VARS <- paste0("w_", SMOKE_VARS)
WEATHER_VARS <- c("binned_pr", "binned_tmmn", "binned_tmmx", "binned_rmax")

# ============================================================================
# Name cleaning, coef/vcov, and impacts
# ============================================================================

# splm and spatialreg decorate coefficient / vcov names with the expression
# that produced them ("as.matrix(xf)x1", "I(x - lambda * WX)x1") and give
# package-built spatial lags their own prefixes ("lag_x1" in spgm, "lag.x1" in
# errorsarlm). Normalize so estimates can be matched by variable name across
# estimators.
clean_names <- function(x) {
  nm <- if (is.null(dim(x))) names(x) else colnames(x)
  nm <- sub("^as\\.matrix\\(xf\\)", "", nm)
  nm <- sub("^I\\(x - lambda \\* WX\\)", "", nm)
  nm <- sub("^lag[_.]", "w_", nm)
  if (is.null(dim(x))) names(x) <- nm else dimnames(x) <- list(nm, nm)
  x
}

# spml returns an unnamed vcov whose rows follow coef() (spatial parameter
# first); spgm returns a named one with the "as.matrix(xf)" prefix; feols
# stores it separately. Return a coefficient vector and a vcov that agree,
# both cleanly named.
coef_vcov <- function(m) {
  cf <- clean_names(coef(m))
  V  <- if (inherits(m, "fixest")) vcov(m) else m$vcov
  if (is.null(dimnames(V)[[1]])) {
    stopifnot(ncol(V) == length(cf))
    dimnames(V) <- list(names(cf), names(cf))
  } else {
    V <- clean_names(V)
  }
  list(cf = cf, V = V)
}

# Direct / indirect / total effects for a Spatial Durbin ERROR Model.
# Because the SDEM has no W y term there is no (I - rho W)^-1 multiplier, so
# the impacts ARE the coefficients and their standard errors come straight
# from the vcov -- no simulation needed (contrast the SDM in
# spatial_sdm_impacts_with_ci.rds, which needed 120 draws):
#     direct_k   = beta_k
#     indirect_k = theta_k
#     total_k    = beta_k + theta_k,   Var = Var(b) + Var(t) + 2 Cov(b, t)
sdem_impacts <- function(m, bin_names, beta_names, theta_names) {
  if (is.null(m)) return(NULL)
  cv <- coef_vcov(m); cf <- cv$cf; V <- cv$V
  ok <- beta_names %in% names(cf) & theta_names %in% names(cf) &
        beta_names %in% rownames(V) & theta_names %in% rownames(V)
  if (!any(ok)) return(NULL)
  bn <- beta_names[ok]; tn <- theta_names[ok]
  b   <- as.numeric(cf[bn]);      th  <- as.numeric(cf[tn])
  vb  <- as.numeric(diag(V)[bn]); vt  <- as.numeric(diag(V)[tn])
  cbt <- as.numeric(V[cbind(bn, tn)])
  bind_rows(
    tibble(variable = bin_names[ok], effect_type = "direct",   estimate = b,  se = sqrt(vb)),
    tibble(variable = bin_names[ok], effect_type = "indirect", estimate = th, se = sqrt(vt)),
    tibble(variable = bin_names[ok], effect_type = "total",    estimate = b + th,
           se = sqrt(pmax(vb + vt + 2 * cbt, 0)))
  ) %>%
    mutate(ci_low = estimate - 1.96 * se, ci_high = estimate + 1.96 * se,
           t = estimate / se, p = 2 * pnorm(-abs(t)))
}

SMOKE_BIN_COLORS <- c(bin_d_001_015 = 'forestgreen', bin_d_015_030 = 'gold',
                      bin_d_030_060 = 'orange',      bin_d_060_100 = 'red',
                      bin_d_100_200 = 'purple',      bin_d_200_500 = 'maroon')
SMOKE_BIN_LABELS <- c(bin_d_001_015 = "Good", bin_d_015_030 = "Moderate",
                      bin_d_030_060 = "Unhealthy for \nSensitive Groups",
                      bin_d_060_100 = "Unhealthy", bin_d_100_200 = "Very Unhealthy",
                      bin_d_200_500 = "Hazardous")

# ============================================================================
# The sparse maximum-likelihood engine for the panel SDEM
# ============================================================================
#
# WHY THIS EXISTS
# ---------------
# The earlier splm route partials the state x year-month effect and the four
# weather bin factors out of y and X with fixest::demean (FWL) and hands splm
# only the 12 smoke regressors. That is a two-step estimator: the annihilator M
# is applied in LEVELS, so what spml then sees is M u, and M u is not a SAR
# error. lambda is the spatial correlation of the RESIDUALIZED error, and beta
# is a two-step estimate rather than the GLS one.
#
# The fix is to stop residualizing and put those controls in X, so the spatial
# filter B = I - lambda W hits them too:
#
#     y = X b + WX t + C g + u,   u = lambda W u + e
#
# with C = [state x year-month dummies, weather bin dummies, day dummies].
#
# splm cannot do this. spml() builds a DENSE model matrix, and the design here
# is NT = 5,457,537 rows by K = 4,731 columns -- 192 GiB for one copy, on a
# machine with 128 GB, before spml's internals take the several copies the
# within transform and the likelihood need. It is not a tuning problem, it is
# the wrong data structure: the design is 99.9% zeros, because every control is
# a dummy. So the likelihood is coded directly against a SPARSE design (34M
# nonzeros in X, 80M in WX -- about 1.4 GB).
#
# WHAT IS ABSORBED, AND WHY THAT IS EXACT
# ---------------------------------------
# The day-of-week x county x season effect (56,056 levels) stays absorbed, and
# absorbing it costs nothing in exactness. Write the cell c = (dow, season) and
# note that within a cell the effect is CONSTANT over that cell's dates while B
# acts only ACROSS counties within a date. Stack a cell as N x T_c. Then the
# filtered unit dummies are D* = 1_{T_c} (x) B, so
#
#     D*'D* = T_c B'B ,  D*'z* = B'B sum_t z_t
#     => coef = mean_t z_t ,  resid = B (z_t - mean_t z_t)
#
# i.e. regressing the FILTERED data on the FILTERED unit dummies returns
# exactly B applied to the DEMEANED data:  B M = (I - P*) B.  Demeaning first
# and filtering first give identical residuals (verified numerically to 8e-15).
#
# Two consequences. (i) Concentrating the 56,056 unit effects out of the
# likelihood is exact, not an approximation -- the profile log-likelihood below
# is the real one. (ii) The dow x county x season effect that the earlier route
# already demeans was never the part of the two-step that was approximate; the
# state x year-month and weather terms were.
#
# THE ESTIMATOR
# -------------
# With the unit effects concentrated out, for a balanced panel with a
# time-invariant W:
#
#     ll_c(lambda) = T log|I_N - lambda W| - (NT/2) log( RSS(lambda) / NT )
#     RSS(lambda)  = || B M (y - X beta_hat(lambda)) ||^2
#
# X*(lambda) = X - lambda WX is AFFINE in lambda, so every cross product needed
# is a QUADRATIC in lambda whose three coefficient matrices do not depend on it:
#
#     A(lambda) = A0 - lambda (A1 + A1') + lambda^2 A2
#         A0 = X'MX,  A1 = X'M(WX),  A2 = (WX)'M(WX)
#
# Build A0, A1, A2 once (sparse), and each likelihood evaluation is one dense
# K x K Cholesky plus one sparse log-determinant -- a second or two. The
# profile is then optimized over the single scalar lambda.
#
# M is a one-way demeaner, so it never has to be materialized either:
#
#     Z1' M Z2 = Z1'Z2 - (D'Z1)' diag(1/n_g) (D'Z2)
#
# with D the unit indicator. Both terms are sparse-by-sparse products.
# ============================================================================

# --- Sparse dummy blocks ----------------------------------------------------

# Indicator columns for a factor with `drop_ref` reference levels removed.
# Returns an n x (nlev - length(drop_ref)) sparse matrix with prefixed names.
dummy_block <- function(f, prefix, drop_ref = 1L) {
  f <- droplevels(as.factor(f))
  M <- Matrix::t(Matrix::fac2sparse(f))                    # n x nlev
  colnames(M) <- paste0(prefix, levels(f))
  if (length(drop_ref)) M <- M[, -drop_ref, drop = FALSE]
  M
}

# Column sums within groups: returns nlev(g) x ncol(Z).
group_sum <- function(Z, g) Matrix::fac2sparse(g) %*% Z

# Z1' M Z2 for the one-way demeaner M on groups `g`, WITHOUT forming M Z.
# `S1`/`S2` are the group sums (group_sum) and `n_g` the group sizes.
cross_demeaned <- function(Z1, Z2, S1, S2, n_g) {
  as.matrix(Matrix::crossprod(Z1, Z2)) -
    as.matrix(Matrix::crossprod(S1, S2 / n_g))
}

# --- Spatial lags -----------------------------------------------------------

# W applied within each day to a county x day panel column, returned in the
# panel's own row order. `cell_idx` is the (county row, date column) index
# matrix used throughout the SDEM scripts.
lag_dense_col <- function(x, W, cell_idx, N, TT) {
  X <- matrix(0, nrow = N, ncol = TT)
  X[cell_idx] <- x
  as.matrix(W %*% X)[cell_idx]
}

# A "county characteristic x period" block, and its spatial lag, sharing one
# column ordering. Column (v, p) is  value_v(county) * 1{period(row) = p},
# laid out characteristic-major so that column index = (v - 1) * n_period + p.
#
# This is what the state x year-month effect is: V is the 0/1 state indicator,
# so V x year-month reproduces the state_year_month factor exactly. Writing it
# this way rather than as a plain factor is what makes the spatial lag cheap --
# W hits only the county part, so W (V x period) = (W V) x period, and W V is a
# single N x n_state multiply giving the share of each county's neighbor weight
# sitting in each state.
county_by_period_block <- function(V, county_idx, period_idx, n_period,
                                   col_names, tol = 1e-12) {
  nz    <- which(abs(as.matrix(V)) > tol, arr.ind = TRUE)   # (county, v)
  nz    <- nz[order(nz[, 1]), , drop = FALSE]
  cnt   <- tabulate(nz[, 1], nbins = nrow(V))
  start <- c(1L, cumsum(cnt) + 1L)
  n_row <- length(county_idx)
  rep_i <- rep.int(seq_len(n_row), cnt[county_idx])
  pos   <- sequence(cnt[county_idx], from = start[county_idx])
  v_id  <- nz[pos, 2]
  sparseMatrix(
    i = rep_i,
    j = (v_id - 1L) * n_period + period_idx[rep_i],
    x = as.matrix(V)[cbind(nz[pos, 1], v_id)],
    dims = c(n_row, n_period * ncol(V)),
    dimnames = list(NULL, col_names)
  )
}

# --- The likelihood ---------------------------------------------------------

# Everything the profile needs, precomputed once.
#   X, WX : NT x K sparse design and its spatial lag
#   y, Wy : NT outcome and its spatial lag
#   g     : absorbed one-way grouping factor (NULL = nothing absorbed)
sdem_moments <- function(X, WX, y, Wy, g, verbose = TRUE) {
  say <- function(...) if (verbose) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")
  Y  <- cbind(y = y, Wy = Wy)
  if (is.null(g)) {
    n_g <- NULL; SX <- SWX <- SY <- NULL
    cd  <- function(Z1, Z2, S1, S2) as.matrix(Matrix::crossprod(Z1, Z2))
  } else {
    g   <- droplevels(as.factor(g))
    n_g <- as.numeric(table(g))
    say("Group sums over ", nlevels(g), " absorbed levels")
    SX  <- group_sum(X, g); SWX <- group_sum(WX, g); SY <- group_sum(Y, g)
    cd  <- function(Z1, Z2, S1, S2) cross_demeaned(Z1, Z2, S1, S2, n_g)
  }
  say("A0 = X'MX");        A0 <- cd(X,  X,  SX,  SX)
  say("A1 = X'M(WX)");     A1 <- cd(X,  WX, SX,  SWX)
  say("A2 = (WX)'M(WX)");  A2 <- cd(WX, WX, SWX, SWX)
  say("X'MY, WX'MY, Y'MY")
  BX <- cd(X,  Y,  SX,  SY)       # K x 2 : [X'My , X'M(Wy)]
  BW <- cd(WX, Y,  SWX, SY)       # K x 2 : [(WX)'My , (WX)'M(Wy)]
  CC <- cd(Y,  Y,  SY,  SY)       # 2 x 2 : [y'My, y'M(Wy); ., (Wy)'M(Wy)]
  list(A0 = A0, A1 = A1, A2 = A2,
       b0 = BX[, 1], b1 = BX[, 2], b2 = BW[, 1], b3 = BW[, 2],
       c0 = CC[1, 1], c1 = CC[1, 2], c2 = CC[2, 2])
}

# Drop structurally collinear columns. The null space of A0 = X'MX is also in
# the null space of A1 and A2 -- if M X b = 0 then X b is constant within each
# absorbed unit, W (being time-invariant) leaves it constant within the unit,
# so M W X b = 0 too -- which is why one pivoted QR at lambda = 0 settles the
# rank for every lambda.
sdem_full_rank_cols <- function(A0, tol = -1) {
  ch <- suppressWarnings(chol(A0, pivot = TRUE, tol = tol))
  r  <- attr(ch, "rank"); p <- attr(ch, "pivot")
  sort(p[seq_len(r)])
}

sdem_moments_subset <- function(m, keep) {
  m$A0 <- m$A0[keep, keep, drop = FALSE]
  m$A1 <- m$A1[keep, keep, drop = FALSE]
  m$A2 <- m$A2[keep, keep, drop = FALSE]
  m$b0 <- m$b0[keep]; m$b1 <- m$b1[keep]
  m$b2 <- m$b2[keep]; m$b3 <- m$b3[keep]
  m
}

# RSS and beta at a given lambda, from the precomputed quadratics.
sdem_solve_at <- function(m, lambda) {
  A <- m$A0 - lambda * (m$A1 + t(m$A1)) + lambda^2 * m$A2
  b <- m$b0 - lambda * (m$b1 + m$b2)    + lambda^2 * m$b3
  cc <- m$c0 - 2 * lambda * m$c1        + lambda^2 * m$c2
  R  <- chol(A)
  beta <- backsolve(R, backsolve(R, b, transpose = TRUE))
  list(beta = beta, rss = as.numeric(cc - sum(b * beta)), A = A, R = R)
}

# log|I_N - lambda W|, sparse LU. Memoized: the optimizer revisits values.
make_ldet <- function(W) {
  N <- nrow(W); I <- Diagonal(N); cache <- new.env(parent = emptyenv())
  function(lambda) {
    key <- format(lambda, digits = 15)
    if (!is.null(cache[[key]])) return(cache[[key]])
    v <- as.numeric(Matrix::determinant(I - lambda * W, logarithm = TRUE)$modulus)
    assign(key, v, envir = cache); v
  }
}

# Fit. Returns an spml-shaped object: coef() gives c(rho, beta...) and $vcov
# is the matching matrix, so coef_vcov()/sdem_impacts() and the plot work on it
# unchanged.
sdem_sparse_fit <- function(m, W, N, TT, col_names,
                            interval = c(-0.95, 0.99), tol = 1e-7,
                            verbose = TRUE) {
  say  <- function(...) if (verbose) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")
  NT   <- N * TT
  ldet <- make_ldet(W)

  nll <- function(lambda) {
    s <- sdem_solve_at(m, lambda)
    -(TT * ldet(lambda) - (NT / 2) * log(s$rss / NT))
  }

  # optimize() is golden section plus parabolic interpolation and will happily
  # settle into a local optimum. log|I - lambda W| is concave in lambda but
  # RSS(lambda) is a ratio of quadratics and need not be, so bracket the
  # optimum on a coarse grid first and refine inside the winning cell.
  say("Profiling the concentrated likelihood over lambda")
  t0   <- Sys.time()
  grid <- seq(interval[1], interval[2], length.out = 13)
  gval <- vapply(grid, nll, numeric(1))
  i    <- which.min(gval)
  say("grid minimum at lambda = ", round(grid[i], 3),
      " (", i, " of ", length(grid), ")")
  brk <- c(grid[max(1L, i - 1L)], grid[min(length(grid), i + 1L)])
  opt <- optimize(nll, interval = brk, tol = tol)
  lam <- opt$minimum
  say("lambda_hat = ", round(lam, 6), "  (",
      round(as.numeric(Sys.time() - t0, units = "mins"), 2), " min)")

  fit    <- sdem_solve_at(m, lam)
  sigma2 <- fit$rss / NT
  # For the SAR error model the information matrix is block diagonal between
  # beta and (lambda, sigma^2), so Var(beta) = sigma^2 (X*'X*)^-1 and lambda's
  # variance comes from the curvature of its own profile.
  Rinv  <- backsolve(fit$R, diag(nrow(fit$R)))
  V_b   <- sigma2 * (Rinv %*% t(Rinv))
  h     <- max(1e-4, 1e-3 * (1 - abs(lam)))
  d2    <- (nll(lam + h) - 2 * nll(lam) + nll(lam - h)) / h^2
  var_l <- if (is.finite(d2) && d2 > 0) 1 / d2 else NA_real_

  nm <- c("rho", col_names)
  cf <- c(lam, as.numeric(fit$beta)); names(cf) <- nm
  V  <- matrix(0, length(nm), length(nm), dimnames = list(nm, nm))
  V[1, 1] <- var_l
  V[-1, -1] <- V_b

  structure(list(coefficients = cf, vcov = V, lambda = lam, sigma2 = sigma2,
                 rss = fit$rss, logLik = -opt$objective, n = N, T = TT,
                 k = length(col_names), converged = TRUE),
            class = c("sdem_sparse", "list"))
}

summary.sdem_sparse <- function(object, which = NULL, ...) {
  cf <- coef(object); se <- sqrt(diag(object$vcov))
  keep <- if (is.null(which)) seq_along(cf) else c(1L, match(which, names(cf)))
  data.frame(estimate = cf[keep], se = se[keep],
             z = cf[keep] / se[keep],
             p = 2 * pnorm(-abs(cf[keep] / se[keep])))
}

# ============================================================================
# Sample, design, and reporting
# ============================================================================

# Load the balanced county/date panel and W written by
# build_spatial_weight_matrix.R. That cache is self-contained: its panel_df
# carries the raw smoke bins, the raw outcome `y`, and the FE factors
# (weather bins, day_of_week, season), so there is no second cache to read.
sdem_v4_load_panel <- function(say = cat) {
  prep_cache <- file.path(REGRESSION_RESULTS_DIR, "sdem_prepared_panel.rds")
  stopifnot(file.exists(prep_cache))

  say("Loading prepared panel: ", prep_cache)
  prep <- readRDS(prep_cache)
  needed <- c("county_geoid_str", "spend_date_range_start", SMOKE_VARS,
              "y", WEATHER_VARS, "day_of_week", "season")
  missing <- setdiff(needed, names(prep$panel_df))
  if (length(missing))
    stop("Prepared-panel cache ", prep_cache, " is missing columns: ",
         paste(missing, collapse = ", "),
         ". Rebuild it with build_spatial_weight_matrix.R.")

  panel <- prep$panel_df %>%
    select(all_of(needed)) %>%
    arrange(county_geoid_str, spend_date_range_start)
  county_ids <- prep$county_ids
  dates_bal  <- prep$dates_bal
  W          <- prep$W
  rm(prep); gc()

  N <- length(county_ids); TT <- length(dates_bal)
  stopifnot(nrow(panel) == N * TT, all(abs(Matrix::rowSums(W) - 1) < 1e-8))
  say("Panel: N = ", N, " counties x T = ", TT, " days = ", nrow(panel), " rows")

  county_idx <- match(panel$county_geoid_str, county_ids)
  date_idx   <- match(panel$spend_date_range_start, dates_bal)
  list(panel = panel, W = W, county_ids = county_ids, dates_bal = dates_bal,
       N = N, TT = TT, county_idx = county_idx, date_idx = date_idx,
       cell_idx = cbind(county_idx, date_idx))
}

# The sparse design and its spatial lag. Blocks, in order:
#   smoke      6   own-county smoke bins                     (beta)
#   w_smoke    6   W x smoke bins, the Durbin terms          (theta)
#   sym      ~3k   state x year-month
#   weather  ~19   binned_pr / tmmn / tmmx / rmax
#   day     T - 1  day effects
#
# WX is the same blocks with W applied within each day, which is what the GLS
# filter X*(lambda) = X - lambda WX needs. Three blocks have structure worth
# exploiting rather than lagging column by column:
#   * W x (day dummy) = day dummy exactly, because W is row-standardized;
#   * W x (state x year-month) = (W x state indicator) x year-month, one
#     N x n_state multiply instead of ~3,000 panel-length ones;
#   * only smoke and weather genuinely have to be lagged day by day.
#
# If `unit_dummies` is a factor, its indicator columns are appended to both X
# and WX instead of being absorbed -- that is the literal
# day-of-week x county x season unit design, used to verify the absorbed one.
sdem_v4_build_design <- function(dat, unit_dummies = NULL, say = cat) {
  panel <- dat$panel; W <- dat$W; N <- dat$N; TT <- dat$TT
  lag_col <- function(x) lag_dense_col(x, W, dat$cell_idx, N, TT)

  say("Building the sparse design")

  # --- smoke, its lag (the Durbin terms) and the lag of the lag -------------
  X_smoke  <- as.matrix(panel[, SMOKE_VARS])
  X_wsmoke <- sapply(SMOKE_VARS, function(v) lag_col(panel[[v]]))
  X_w2smk  <- apply(X_wsmoke, 2, lag_col)
  colnames(X_wsmoke) <- W_SMOKE_VARS
  colnames(X_w2smk)  <- paste0("w2_", SMOKE_VARS)
  panel[W_SMOKE_VARS] <- as.data.frame(X_wsmoke)

  # --- state x year-month ---------------------------------------------------
  state   <- substr(dat$county_ids, 1, 2)
  ym      <- format(as.Date(dat$dates_bal), "%Y_%m")
  ym_lev  <- sort(unique(ym)); ym_idx <- match(ym, ym_lev)[dat$date_idx]
  st_lev  <- sort(unique(state))
  V_state <- sparseMatrix(i = seq_len(N), j = match(state, st_lev), x = 1,
                          dims = c(N, length(st_lev)))
  sym_names <- paste0("sym_", rep(st_lev, each = length(ym_lev)), "_",
                      rep(ym_lev, times = length(st_lev)))
  X_sym <- county_by_period_block(V_state, dat$county_idx, ym_idx,
                                  length(ym_lev), sym_names)
  L_sym <- county_by_period_block(as.matrix(W %*% V_state), dat$county_idx,
                                  ym_idx, length(ym_lev), sym_names)
  # present combinations only, minus one reference state: the relation
  # sum_s sym[s, m] = sum_{t in m} day[t] is one dependency per year-month, and
  # dropping a whole state's columns removes exactly those
  sym_keep <- which(Matrix::colSums(abs(X_sym)) > 0 &
                    !startsWith(colnames(X_sym), paste0("sym_", st_lev[1], "_")))
  X_sym <- X_sym[, sym_keep, drop = FALSE]
  L_sym <- L_sym[, sym_keep, drop = FALSE]
  say("state x year-month columns: ", ncol(X_sym))
  panel$sym <- interaction(substr(panel$county_geoid_str, 1, 2),
                           format(panel$spend_date_range_start, "%Y_%m"),
                           drop = TRUE)

  # --- weather bins ---------------------------------------------------------
  wb <- lapply(WEATHER_VARS, function(v) {
    D <- dummy_block(panel[[v]], paste0(v, "_"), drop_ref = 1L)
    L <- Matrix::Matrix(apply(as.matrix(D), 2, lag_col), sparse = TRUE)
    dimnames(L) <- dimnames(D)
    list(X = D, L = L)
  })
  X_wthr <- do.call(cbind, lapply(wb, `[[`, "X"))
  L_wthr <- do.call(cbind, lapply(wb, `[[`, "L"))
  rm(wb); gc()
  say("weather bin columns: ", ncol(X_wthr))

  # --- day effects ----------------------------------------------------------
  X_day <- dummy_block(panel$spend_date_range_start, "day_", drop_ref = 1L)
  say("day columns: ", ncol(X_day))

  X_blocks <- list(Matrix::Matrix(X_smoke, sparse = TRUE),
                   Matrix::Matrix(X_wsmoke, sparse = TRUE), X_sym, X_wthr, X_day)
  L_blocks <- list(Matrix::Matrix(X_wsmoke, sparse = TRUE),
                   Matrix::Matrix(X_w2smk,  sparse = TRUE), L_sym, L_wthr, X_day)

  if (!is.null(unit_dummies)) {
    # The literal design: the unit effects are ordinary regressors, so the GLS
    # filter has to lag them too. W x (unit dummy) is one column per neighbor
    # sharing the unit's (dow, season) cell -- and on any given day every
    # county IS in that day's cell, so this is just W restricted to the cell.
    D_unit <- dummy_block(unit_dummies, "unit_", drop_ref = integer(0))
    L_unit <- Matrix::Matrix(
      apply(as.matrix(D_unit), 2, lag_col), sparse = TRUE)
    dimnames(L_unit) <- dimnames(D_unit)
    X_blocks <- c(X_blocks, list(D_unit)); L_blocks <- c(L_blocks, list(L_unit))
    say("unit (dow x county x season) columns carried in X: ", ncol(D_unit))
  }

  X  <- do.call(cbind, X_blocks)
  WX <- do.call(cbind, L_blocks)
  colnames(WX) <- colnames(X)
  say("Design: ", nrow(X), " x ", ncol(X), " (X nnz ", length(X@x),
      ", WX nnz ", length(WX@x), ")")

  panel$dcs <- interaction(panel$day_of_week, panel$county_geoid_str,
                           panel$season, drop = TRUE)
  list(X = X, WX = WX, y = panel$y, Wy = lag_col(panel$y),
       panel = panel, X_wsmoke = X_wsmoke, dcs = panel$dcs)
}

sdem_v4_report <- function(results, fit, bins, panel, X_wsmoke, say = cat) {
  x_r <- bins; wx_r <- paste0("w_", bins)

  results$impacts <- list(ml = sdem_impacts(fit, bins, x_r, wx_r))
  for (nm in names(results$impacts)) {
    say("Impacts (", nm, "):"); print(as.data.frame(results$impacts[[nm]]))
  }

  # "Direct" (own county exposed, neighbors clean) and "total" (everyone
  # exposed) are endpoints, and neither is what a real smoke episode looks
  # like. Evaluate the model instead at the neighbor exposure profile actually
  # observed on the days a county sits in bin k, as a linear combination
  # a'coef so the strongly negative beta-theta covariance is carried through.
  neighbor_profile <- t(sapply(bins, function(k)
    colMeans(X_wsmoke[panel[[k]] == 1, , drop = FALSE])))
  dimnames(neighbor_profile) <- list(bins, bins)
  say("Neighbor exposure profile, E[Wx_j | own county in bin k]:")
  print(round(neighbor_profile, 3))

  cv <- coef_vcov(fit)
  results$typical_episode <- purrr::map_dfr(seq_along(bins), function(k) {
    a <- setNames(rep(0, length(cv$cf)), names(cv$cf))
    a[x_r[k]] <- 1
    a[wx_r]   <- a[wx_r] + neighbor_profile[k, ]
    est <- sum(a * cv$cf); se <- sqrt(as.numeric(t(a) %*% cv$V %*% a))
    tibble(variable = bins[k], effect_type = "typical_episode",
           estimate = est, se = se, ci_low = est - 1.96 * se,
           ci_high = est + 1.96 * se, t = est / se,
           p = 2 * pnorm(-abs(est / se)))
  })
  say("Typical realized episode:"); print(as.data.frame(results$typical_episode))
  results$neighbor_profile <- neighbor_profile

  # Whether the WX block belongs at all is a JOINT question -- dropping a
  # coefficient because p > 0.05 is a pre-test estimator.
  wald <- function(nm) {
    nm <- nm[nm %in% names(cv$cf)]; b <- cv$cf[nm]; S <- cv$V[nm, nm, drop = FALSE]
    stat <- as.numeric(t(b) %*% solve(S) %*% b)
    tibble(chisq = stat, df = length(nm),
           p = pchisq(stat, length(nm), lower.tail = FALSE))
  }
  results$joint_tests <- bind_rows(
    wald(wx_r) %>% mutate(hypothesis = "all theta (neighbor smoke) = 0  [SDEM vs SEM]"),
    wald(x_r)  %>% mutate(hypothesis = "all beta (own smoke) = 0")
  ) %>% select(hypothesis, chisq, df, p)
  say("Joint Wald tests:"); print(as.data.frame(results$joint_tests))

  grab <- function(m, label) {
    c2 <- coef_vcov(m)
    tibble(estimator = label, variable = rep(bins, 2),
           part = rep(c("beta (own smoke)", "theta (neighbor smoke)"),
                      each = length(bins)),
           estimate = c(as.numeric(c2$cf[x_r]), as.numeric(c2$cf[wx_r])),
           se = c(sqrt(diag(c2$V)[x_r]), sqrt(diag(c2$V)[wx_r])))
  }
  results$comparison <- grab(fit, "SDEM ML (sparse, controls in X)")
  say("Coefficient table:"); print(as.data.frame(results$comparison))

  results$lambda_tab <- tibble(estimator = "SDEM ML (sparse, controls in X)",
                               lambda = fit$lambda, se = sqrt(fit$vcov[1, 1]))
  say("lambda:"); print(as.data.frame(results$lambda_tab))
  results
}
