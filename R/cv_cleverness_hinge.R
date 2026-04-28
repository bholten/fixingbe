###############################################################################
# Cross-validation of the cleverness hinge proposed in
# R/investigate_high_dps.R.
#
# Question 1: Does adding pmax(cleverness - K, 0) to M7 generalise out of
#   sample, or are we just fitting noise on the ~6 high-clev creatures?
# Question 2: How sensitive is the optimal K to which high-clev creatures
#   land in the held-out fold?
#
# Strategy:
#   (CV-A) Repeated stratified 10-fold CV. For each candidate K (and the
#          K=NA baseline = pure M7), report mean test-fold RMSE +/- SE
#          across 50 reps. Stratify on cleverness so each fold contains a
#          proportional share of high-clev creatures.
#   (CV-B) Leave-one-out on the 6 highest-cleverness creatures. For each
#          one, refit knot scan with that creature held out and record the
#          best K. Tells us if any single creature is driving the result.
#   (CV-C) Bootstrap the best-K distribution (200 reps).
###############################################################################
source("R/data.R")

suppressPackageStartupMessages({
  library(dplyr)
})

set.seed(20260427)

# Inline M7 (same as investigate_high_dps.R)
build_features <- function(d) {
  d$ke_floor <- pmax(pmin(d$kinetic, d$energy), 0)
  d$nk       <- (d$blast + d$heat + d$cold +
                 d$electricity + d$acid + d$stun) / 6
  d
}

m7_form <- level ~ hardiness + fortitude + dexterity + intellect +
                   cleverness + power + ke_floor + nk

hinge_form <- function(K) {
  if (is.na(K)) m7_form
  else update(m7_form, as.formula(sprintf("~ . + I(pmax(cleverness - %d, 0))", K)))
}

###############################################################################
# Data
###############################################################################
df <- normalized_df %>% filter(armor == 0) %>% build_features()
cat(sprintf("n unarmored = %d\n", nrow(df)))
cat(sprintf("creatures with cleverness >= 300: %d\n",
            sum(df$cleverness >= 300)))
cat(sprintf("creatures with cleverness >= 400: %d\n",
            sum(df$cleverness >= 400)))

###############################################################################
# (CV-A) Repeated stratified 10-fold CV
###############################################################################
cat("\n=== (CV-A) Repeated 10-fold CV ===\n")

# Stratify by cleverness quartiles (with extra weight to top decile to ensure
# high-clev creatures are spread across folds).
make_folds <- function(d, k = 10) {
  # Bucket by cleverness rank, then assign folds within buckets.
  n <- nrow(d)
  rk <- rank(d$cleverness, ties.method = "first")
  bucket <- ceiling(rk / (n / k))    # rough strata of size k each
  folds <- integer(n)
  for (b in unique(bucket)) {
    idx <- which(bucket == b)
    folds[idx] <- sample(rep(seq_len(k), length.out = length(idx)))
  }
  folds
}

cv_rmse <- function(d, K, k = 10, n_reps = 50) {
  rmses <- numeric(n_reps)
  for (r in seq_len(n_reps)) {
    folds <- make_folds(d, k)
    sse <- 0; cnt <- 0
    for (f in seq_len(k)) {
      tr <- d[folds != f, ]
      te <- d[folds == f, ]
      fit <- lm(hinge_form(K), data = tr)
      pred <- predict(fit, newdata = te)
      sse <- sse + sum((te$level - pred) ^ 2)
      cnt <- cnt + nrow(te)
    }
    rmses[r] <- sqrt(sse / cnt)
  }
  rmses
}

K_grid <- c(NA, 200, 250, 275, 300, 325, 350, 375, 400, 425, 450, 475)
n_reps <- 50

cv_results <- data.frame(
  K = K_grid,
  mean_rmse = NA_real_,
  se_rmse   = NA_real_
)

for (i in seq_along(K_grid)) {
  K <- K_grid[i]
  rmses <- cv_rmse(df, K, k = 10, n_reps = n_reps)
  cv_results$mean_rmse[i] <- mean(rmses)
  cv_results$se_rmse[i]   <- sd(rmses) / sqrt(n_reps)
  cat(sprintf("  K = %-4s  mean RMSE = %.4f  SE = %.4f\n",
              ifelse(is.na(K), "M7", as.character(K)),
              cv_results$mean_rmse[i],
              cv_results$se_rmse[i]))
}

baseline <- cv_results$mean_rmse[is.na(cv_results$K)]
cv_results$delta_vs_M7 <- cv_results$mean_rmse - baseline

cat("\n  Improvement over M7 (negative = better out-of-sample):\n")
print(cv_results %>%
        mutate(across(where(is.numeric), ~round(.x, 4))) %>%
        as.data.frame())

best_cv <- cv_results %>% filter(!is.na(K)) %>% arrange(mean_rmse) %>% head(1)
cat(sprintf("\n  Best K by CV: %s  (RMSE %.4f, %+.4f vs M7)\n",
            best_cv$K, best_cv$mean_rmse, best_cv$delta_vs_M7))

###############################################################################
# (CV-B) Leave-one-out on top-cleverness creatures
###############################################################################
cat("\n=== (CV-B) Leave-one-out on top-cleverness creatures ===\n")
cat("Refit knot scan with each high-clev creature held out; record best K.\n\n")

high_clev <- df %>% arrange(desc(cleverness)) %>% head(8)
print(high_clev %>%
        select(serial, skin, level, cleverness, power, fortitude) %>%
        as.data.frame())

eval_K_in <- function(d, K) {
  fit <- lm(hinge_form(K), data = d)
  AIC(fit)
}

K_fine <- seq(200, 475, by = 25)
loo_results <- data.frame(
  held_out = c("(none)", high_clev$serial),
  best_K   = NA_integer_,
  best_coef = NA_real_,
  delta_AIC = NA_real_
)

for (i in seq_len(nrow(loo_results))) {
  if (loo_results$held_out[i] == "(none)") {
    sub <- df
  } else {
    sub <- df %>% filter(serial != loo_results$held_out[i])
  }
  base_aic <- eval_K_in(sub, NA)
  scan <- sapply(K_fine, function(K) eval_K_in(sub, K))
  best_idx <- which.min(scan)
  best_K <- K_fine[best_idx]
  best_fit <- lm(hinge_form(best_K), data = sub)
  hinge_coef <- coef(best_fit)[grepl("pmax", names(coef(best_fit)))]
  loo_results$best_K[i]    <- best_K
  loo_results$best_coef[i] <- as.numeric(hinge_coef)
  loo_results$delta_AIC[i] <- base_aic - scan[best_idx]
}

print(loo_results %>%
        mutate(across(c(best_coef, delta_AIC), ~round(.x, 3))) %>%
        as.data.frame())

cat("\n  Range of best-K under LOO: [",
    min(loo_results$best_K), ",", max(loo_results$best_K), "]\n")
cat("  Range of hinge coef under LOO: [",
    round(min(loo_results$best_coef), 3), ",",
    round(max(loo_results$best_coef), 3), "]\n")

###############################################################################
# (CV-C) Bootstrap best-K
###############################################################################
cat("\n=== (CV-C) Bootstrap distribution of best K ===\n")

n_boot <- 200
boot_K <- integer(n_boot)
boot_coef <- numeric(n_boot)
for (b in seq_len(n_boot)) {
  idx <- sample(seq_len(nrow(df)), replace = TRUE)
  d_b <- df[idx, ]
  scan <- sapply(K_fine, function(K) eval_K_in(d_b, K))
  best_idx <- which.min(scan)
  boot_K[b] <- K_fine[best_idx]
  fit_b <- lm(hinge_form(K_fine[best_idx]), data = d_b)
  boot_coef[b] <- as.numeric(coef(fit_b)[grepl("pmax", names(coef(fit_b)))])
}

cat(sprintf("  Bootstrap n = %d\n", n_boot))
cat("  Distribution of best K:\n")
print(table(boot_K))
cat(sprintf("\n  Median best K: %d\n", median(boot_K)))
cat(sprintf("  IQR best K: [%d, %d]\n",
            quantile(boot_K, 0.25), quantile(boot_K, 0.75)))
cat(sprintf("  Median hinge coef: %.3f\n", median(boot_coef)))
cat(sprintf("  IQR hinge coef: [%.3f, %.3f]\n",
            quantile(boot_coef, 0.25), quantile(boot_coef, 0.75)))

###############################################################################
# (CV-D) For the operationally interesting K=400 / K=425 / K=350,
#   bootstrap the coefficient with K fixed (no knot-search noise).
###############################################################################
cat("\n=== (CV-D) Bootstrap coefficient with K fixed ===\n")
for (K_fixed in c(300, 325, 350, 400, 425)) {
  coefs <- numeric(n_boot)
  for (b in seq_len(n_boot)) {
    idx <- sample(seq_len(nrow(df)), replace = TRUE)
    fit_b <- lm(hinge_form(K_fixed), data = df[idx, ])
    coefs[b] <- as.numeric(coef(fit_b)[grepl("pmax", names(coef(fit_b)))])
  }
  ok <- coefs[!is.na(coefs)]
  cat(sprintf("  K=%d   n_valid=%3d  median coef = %.4f   95%% CI = [%.4f, %.4f]\n",
              K_fixed,
              length(ok),
              median(ok),
              quantile(ok, 0.025),
              quantile(ok, 0.975)))
}

###############################################################################
# (CV-E) Was the bootstrap bimodality (K=325 vs K=425) about
#   one specific creature?  Refit the knot scan with rancor 6j048r2a
#   removed (LOO-B identified it as the K=425 anchor).
###############################################################################
cat("\n=== (CV-E) Bootstrap with rancor 6j048r2a removed ===\n")

df_no_rancor48 <- df %>% filter(serial != "6j048r2a")
boot_K2 <- integer(n_boot)
boot_coef2 <- numeric(n_boot)
for (b in seq_len(n_boot)) {
  idx <- sample(seq_len(nrow(df_no_rancor48)), replace = TRUE)
  d_b <- df_no_rancor48[idx, ]
  scan <- sapply(K_fine, function(K) eval_K_in(d_b, K))
  best_idx <- which.min(scan)
  boot_K2[b] <- K_fine[best_idx]
  fit_b <- lm(hinge_form(K_fine[best_idx]), data = d_b)
  cf <- as.numeric(coef(fit_b)[grepl("pmax", names(coef(fit_b)))])
  boot_coef2[b] <- ifelse(length(cf) == 1, cf, NA_real_)
}

cat("  Distribution of best K (rancor 6j048r2a held out):\n")
print(table(boot_K2))
cat(sprintf("\n  Median best K (no 6j048r2a): %d (vs %d with full set)\n",
            median(boot_K2), median(boot_K)))
cat(sprintf("  IQR best K (no 6j048r2a): [%d, %d]\n",
            quantile(boot_K2, 0.25), quantile(boot_K2, 0.75)))
cat(sprintf("  Median hinge coef (no 6j048r2a): %.3f (vs %.3f with full set)\n",
            median(boot_coef2, na.rm = TRUE), median(boot_coef, na.rm = TRUE)))

cat("\n=== END ===\n")
