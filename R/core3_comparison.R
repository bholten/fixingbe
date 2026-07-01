###############################################################################
# core3_comparison.R - Head-to-head: Core3 (DNA Lab) vs simple model vs actual
#
# For every cleaned furrycat creature we produce three creature levels:
#   1. actual        - the empirically measured retail CL (creatures.csv$level)
#   2. model_level   - our reverse-engineered simple model (custom_model)
#   3. core3_level   - Core3's real crafting-time formula, via the dna-lab-level
#                      binary (built with -DUSE_CORE3_LIBS=ON / HAVE_CORE3)
#
# Core3 is fed the creature's REALIZED combat stats (HAM, damage, speed, hit,
# armor, resists) straight from creatures.csv; effectiveArmor is derived inside
# Core3 as sum(resists)*2. Our model is fed the genetic attributes. Both are
# judged purely by accuracy against the actual level.
#
# NOTE: our model was fit on this same data (in-sample); Core3 was not. See the
# Rmd for the honest-comparison caveat and cross-validation.
###############################################################################

source("R/data.R")
source("R/creature_level_formulas.R")

# --- Configuration ---------------------------------------------------------
# Where the dna-lab checkout lives (holds the built binary + Core3 submodule).
dnalab_root <- Sys.getenv("DNALAB_ROOT", unset = "/home/brennan/dev/dna-lab")
dnalab_bin  <- Sys.getenv("DNALAB_LEVEL_BIN",
                          unset = file.path(dnalab_root, "build", "dna-lab-level"))
core3_bin   <- Sys.getenv("CORE3_BIN",
                          unset = file.path(dnalab_root,
                                            "submodules/Core3/MMOCoreORB/bin"))

# --- Core3 prediction via the C++ binary -----------------------------------
# Returns a data frame: serial, core3_level. Errors loudly if the binary is
# missing or Core3 fails to initialize (we never silently fall back to the
# untrusted standalone replica).
core3_predict <- function(df,
                          bin = dnalab_bin,
                          core3_bin_dir = core3_bin) {
  if (!file.exists(bin)) {
    stop(sprintf(
      "dna-lab-level not found at %s\nBuild it with:\n  cd %s/build && cmake -DUSE_CORE3_LIBS=ON .. && make dna-lab-level",
      bin, dnalab_root))
  }

  # Map our normalized_df columns to the binary's expected header names.
  # Resists are the creature's REALIZED values (from creatures.csv).
  in_df <- data.frame(
    serial     = df$serial,
    health     = df$health,
    action     = df$action,
    mind       = df$mind,
    damage_min = df$damage_low,
    damage_max = df$damage_high,
    speed      = df$speed,
    chance_hit = df$to_hit,
    armor      = df$armor,
    kinetic    = df$kinetic,
    energy     = df$energy,
    blast      = df$blast,
    heat       = df$heat,
    cold       = df$cold,
    electric   = df$electricity,
    acid       = df$acid,
    stun       = df$stun,
    stringsAsFactors = FALSE
  )

  in_path  <- tempfile(fileext = ".csv")
  out_path <- tempfile(fileext = ".csv")
  on.exit(unlink(c(in_path, out_path)), add = TRUE)
  readr::write_csv(in_df, in_path)

  # Write results to a file, not stdout: Core3's DnaManager logging goes to
  # stdout and would otherwise contaminate the CSV.
  status <- system2(bin,
                    args = c("--core3-bin", shQuote(core3_bin_dir),
                             "--output", shQuote(out_path),
                             shQuote(in_path)),
                    stdout = FALSE, stderr = FALSE)

  if (!file.exists(out_path)) {
    stop(sprintf("dna-lab-level failed (exit %s); no output written.", status))
  }

  res <- utils::read.csv(out_path, stringsAsFactors = FALSE)
  if (!all(c("serial", "core3_level") %in% names(res))) {
    stop("dna-lab-level output missing expected columns; got: ",
         paste(names(res), collapse = ", "))
  }
  res
}

# Cached Core3 predictions (serial, core3_level) so the published Rmd can build
# in the r-lua-builder Docker image, which has neither the dna-lab binary nor
# the Core3 libraries. Regenerate with refresh_core3_cache() after any change to
# dna-lab-level, the Core3 submodule, or the cleaned data.
core3_cache_path <- "data/derived/core3_predictions.csv"

refresh_core3_cache <- function(df = normalized_df, path = core3_cache_path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  res <- core3_predict(df)
  readr::write_csv(res, path)
  message(sprintf("Wrote %d Core3 predictions to %s", nrow(res), path))
  invisible(res)
}

# Get Core3 predictions: prefer the live binary if available, else the cache.
core3_levels <- function(df = normalized_df,
                         prefer_binary = TRUE,
                         path = core3_cache_path) {
  if (prefer_binary && file.exists(dnalab_bin)) {
    return(core3_predict(df))
  }
  if (!file.exists(path)) {
    stop(sprintf(
      "No dna-lab-level binary and no cache at %s. Build the binary and run refresh_core3_cache().",
      path))
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

# --- Build the combined comparison frame -----------------------------------
build_comparison <- function(df = normalized_df, prefer_binary = TRUE) {
  df$model_level <- custom_model(df)

  core3 <- core3_levels(df, prefer_binary = prefer_binary)
  df <- dplyr::left_join(df, core3, by = "serial")

  cmp <- data.frame(
    serial      = df$serial,
    skin        = df$skin,
    armor       = df$armor,
    fortitude   = df$fortitude,
    is_armored  = df$fortitude >= 500,   # the model's own switch point
    actual      = df$level,
    model_level = df$model_level,
    core3_level = df$core3_level,
    stringsAsFactors = FALSE
  )
  cmp$model_resid <- cmp$model_level - cmp$actual
  cmp$core3_resid <- cmp$core3_level - cmp$actual
  cmp
}

# --- Metrics ---------------------------------------------------------------
level_metrics <- function(pred, actual) {
  resid <- pred - actual
  ok <- is.finite(resid)
  resid <- resid[ok]; actual_ok <- actual[ok]
  ss_res <- sum(resid^2)
  ss_tot <- sum((actual_ok - mean(actual_ok))^2)
  data.frame(
    n        = length(resid),
    rmse     = sqrt(mean(resid^2)),
    mae      = mean(abs(resid)),
    bias     = mean(resid),
    r2       = 1 - ss_res / ss_tot,
    within_1 = mean(abs(resid) <= 1),
    within_2 = mean(abs(resid) <= 2)
  )
}

# Metrics table for both predictors over a subset, tagged with a group label.
metrics_table <- function(cmp) {
  make <- function(sub, grp) {
    rbind(
      cbind(group = grp, model = "simple",
            level_metrics(sub$model_level, sub$actual)),
      cbind(group = grp, model = "core3",
            level_metrics(sub$core3_level, sub$actual))
    )
  }
  rbind(
    make(cmp, "all"),
    make(cmp[!cmp$is_armored, ], "unarmored (fort<500)"),
    make(cmp[cmp$is_armored, ],  "armored (fort>=500)")
  )
}

# Convenience: run everything and return both frames.
run_core3_comparison <- function(df = normalized_df, prefer_binary = TRUE) {
  cmp <- build_comparison(df, prefer_binary = prefer_binary)
  list(comparison = cmp, metrics = metrics_table(cmp))
}
