###############################################################################
# validate_dna_samples.R - fixingbe-side driver for the DNA-sample validator
#
# The authoritative validator lives in DNA Lab (`dna-lab-validate`), which uses
# DNA Lab's own sampling functions (DnaSimulator.h) + creature corpus. This file
# is a THIN DRIVER: it shells out to that binary and returns the flagged rows.
# Do NOT reimplement the sampling bounds in R — an earlier attempt miscalibrated
# them (dependability/power) because SWG's sampling math is easy to get subtly
# wrong. See docs/retail_divergences.md and docs/creature_level_investigation_review.md.
#
# Validator tiers (per row in the returned frame):
#   1 = physically-impossible DNA value (> 1000 cap)         -> real corruption
#   2 = wild sample outside its source's achievable range,
#       on stats with a trustworthy corpus mapping           -> candidate bad sample
#   3 = systematic corpus/Core3-vs-retail divergence
#       (power / fierceness / fortitude)                     -> NOT a sample error
###############################################################################

# --- Configuration ---------------------------------------------------------
dnalab_root      <- Sys.getenv("DNALAB_ROOT", unset = "/home/brennan/dev/dna-lab")
validate_bin     <- Sys.getenv("DNALAB_VALIDATE_BIN",
                     unset = file.path(dnalab_root, "build", "dna-lab-validate"))
creatures_json   <- Sys.getenv("DNALAB_CREATURES",
                     unset = file.path(dnalab_root, "optimizer", "creatures.json"))
overrides_json   <- Sys.getenv("DNALAB_CREATURE_OVERRIDES",
                     unset = file.path(dnalab_root, "optimizer", "creatures.overrides.json"))

# Run the validator over a samples CSV (defaults to the furrycat corpus).
# Returns a data frame: sample_id, creature_serial, source, tier, stat, value,
# lo, hi, note. Errors loudly if the binary is missing (build it first:
#   cd <dnalab_root>/build && cmake .. && make dna-lab-validate).
validate_dna_samples <- function(
    samples_csv = "data/clean/furrycat/samples.csv",
    bin = validate_bin, creatures = creatures_json, overrides = overrides_json,
    tol = 25) {

  if (!file.exists(bin)) {
    stop(sprintf(
      "dna-lab-validate not found at %s\nBuild it:\n  cd %s/build && cmake .. && make dna-lab-validate",
      bin, dnalab_root))
  }
  if (!file.exists(creatures)) stop("creatures.json not found at ", creatures)

  out_path <- tempfile(fileext = ".csv")
  on.exit(unlink(out_path), add = TRUE)

  args <- c("--creatures", shQuote(creatures),
            "--tol", tol,
            "--output", shQuote(out_path),
            shQuote(samples_csv))
  if (file.exists(overrides)) args <- c("--overrides", shQuote(overrides), args)

  status <- system2(bin, args = args, stdout = FALSE, stderr = FALSE)
  if (!file.exists(out_path)) {
    stop(sprintf("dna-lab-validate failed (exit %s); no output written.", status))
  }
  utils::read.csv(out_path, stringsAsFactors = FALSE)
}

# Convenience summaries.
validate_summary <- function(v = validate_dna_samples()) {
  cat("Violations by tier (1=impossible, 2=candidate bad sample, 3=corpus divergence):\n")
  print(table(tier = v$tier))
  cat("\nTier 1 (physically impossible):\n")
  print(v[v$tier == 1, c("source", "stat", "value")])
  cat("\nTier 2 by stat (candidate bad samples):\n")
  print(sort(table(v$stat[v$tier == 2]), decreasing = TRUE))
  invisible(v)
}
