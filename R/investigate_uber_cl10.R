###############################################################################
# investigate_uber_cl10.R
#
# The "uber CL10" puzzle: a cluster of CL10 unarmored creatures with very high
# health (~10k), kinetic resist clamped near 60, and energy resist heavily
# negative. The current `kinen = (kinetic + energy)/2` aggregate averages a
# near-cap positive with a heavily-negative number, so the model "sees" them
# as having ~5% kinen and over-predicts them by 2-5 levels.
#
# Hypothesis under test: kinetic and energy are mechanically the same in the
# game (both capped at 60% in-engine, hit by the same creature builds), so the
# CL formula should treat them symmetrically. The miss isn't kinetic vs energy
# weighting — it's that the aggregate `kinen` is credited even when it's
# dragged down by a vulnerability. The fix is a *vulnerability-aware* kinen
# that gives no level credit when the average is below zero.
#
# Models:
#   M0  baseline kinen (current canonical formula)
#   M5  kinen_pos + kinen_neg  (symmetric in k,e; positive vs negative kinen)
#   M6  pmax(kinen, 0)         (simplest: only positive kinen counts)
#
# Diagnostic-only models (NOT candidates — used to expose the identification
# problem when kinetic has no leverage in the dataset):
#   M1  split k + e separately (lets coefs differ — picks up artifact)
#   M3  fully asymmetric pos/neg per resist
#   M4  parsimonious asymmetric (energy_pos + kinetic_neg)
###############################################################################

source("R/data.R")

library(dplyr)
library(ggplot2)

no_armor_df <- normalized_df %>% filter(armor == 0) %>% mutate(
  kinen_pos    = pmax(kinen, 0),
  kinen_neg    = pmin(kinen, 0),
  kinen_capped = pmax(kinen, 0),  # alias used by M6 — same as kinen_pos
  kinetic_pos  = pmax(kinetic, 0),
  kinetic_neg  = pmin(kinetic, 0),
  energy_pos   = pmax(energy, 0),
  energy_neg   = pmin(energy, 0)
)

###############################################################################
# Candidate models (symmetric in kinetic and energy)
###############################################################################

# M0: baseline kinen (current canonical formula structure)
m_base <- lm(level ~ hardiness + fortitude + dexterity + intellect +
               cleverness + power + kinen + nonkinen,
             data = no_armor_df)

# M5: kinen split into positive and negative parts. Treats k,e symmetrically
# (uses the kinen aggregate) but lets above-zero and below-zero kinen contribute
# differently.
m_kinen_pn <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                   cleverness + power + kinen_pos + kinen_neg + nonkinen,
                 data = no_armor_df)

# M6: simplest — only the positive part of kinen contributes; negative kinen
# is ignored entirely. This is the natural "vulnerability gives no penalty,
# just no credit" hypothesis.
m_kinen_floor <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                      cleverness + power + kinen_capped + nonkinen,
                    data = no_armor_df)

###############################################################################
# Diagnostic models (not candidates — used to explain WHY symmetric is right)
###############################################################################

# M1: separate kinetic and energy coefficients. Looks like k and e differ, but
# only because kinetic is clamped at ~60 in the data and carries no leverage.
m_split <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                cleverness + power + kinetic + energy + nonkinen,
              data = no_armor_df)

# M3: fully asymmetric (pos/neg per resist, no k=e constraint)
m_asym <- lm(level ~ hardiness + fortitude + dexterity + intellect +
               cleverness + power +
               kinetic_pos + kinetic_neg + energy_pos + energy_neg + nonkinen,
             data = no_armor_df)

# M4: parsimonious asymmetric (energy_pos and kinetic_neg only)
m_pars <- lm(level ~ hardiness + fortitude + dexterity + intellect +
               cleverness + power + energy_pos + kinetic_neg + nonkinen,
             data = no_armor_df)

# Helper for one-line model summaries
summarize_model <- function(name, m, df) {
  pred <- predict(m, newdata = df)
  resid <- df$level - pred
  data.frame(
    model = name,
    R2 = round(summary(m)$r.squared, 4),
    resid_sd = round(sd(resid), 3),
    AIC = round(AIC(m), 1),
    n_params = length(coef(m))
  )
}

###############################################################################
# Headline comparison
###############################################################################

cat("=============================================================\n")
cat("  HEADLINE: SYMMETRIC CANDIDATE MODELS vs BASELINE\n")
cat("=============================================================\n\n")

cat("All unarmored creatures (n =", nrow(no_armor_df), "):\n")
print(bind_rows(
  summarize_model("M0 baseline kinen",          m_base,        no_armor_df),
  summarize_model("M5 kinen_pos + kinen_neg",   m_kinen_pn,    no_armor_df),
  summarize_model("M6 pmax(kinen, 0) only",     m_kinen_floor, no_armor_df)
))

cat("\nM5 coefficients:\n");          print(round(coef(m_kinen_pn), 4))
cat("\nM6 coefficients:\n");          print(round(coef(m_kinen_floor), 4))

cat("\nLR test: does kinen_neg add anything beyond pmax(kinen, 0)?\n")
print(anova(m_kinen_floor, m_kinen_pn))

cat("\nShapiro-Wilk on residuals:\n")
cat(sprintf("  M0 baseline:        p = %.4g\n", shapiro.test(resid(m_base))$p.value))
cat(sprintf("  M5 kinen pos/neg:   p = %.4g\n", shapiro.test(resid(m_kinen_pn))$p.value))
cat(sprintf("  M6 pmax(kinen, 0):  p = %.4g\n", shapiro.test(resid(m_kinen_floor))$p.value))

###############################################################################
# Identification diagnostic: WHY symmetric is the right constraint
###############################################################################

cat("\n\n=============================================================\n")
cat("  DIAGNOSTIC: kinetic has no leverage in the dataset\n")
cat("=============================================================\n\n")

cat("Distribution of kinetic resist:\n")
print(summary(no_armor_df$kinetic))
cat("\n  fraction at >= 50:    ", round(mean(no_armor_df$kinetic >= 50), 3), "\n")
cat("  fraction at exactly 60:", round(mean(no_armor_df$kinetic == 60), 3), "\n\n")

cat("Distribution of energy resist (for comparison):\n")
print(summary(no_armor_df$energy))

cat("\nWith kinetic that constant, free-coefficient k+e and asym pos/neg models\n")
cat("can't 'see' kinetic — they assign all the resist signal to energy.\n\n")

cat("Free-coefficient comparison (NOT candidates):\n")
print(bind_rows(
  summarize_model("M1 split k + e",  m_split, no_armor_df),
  summarize_model("M3 fully asym",   m_asym,  no_armor_df),
  summarize_model("M4 pars asym",    m_pars,  no_armor_df)
))

cat("\nM3 (fully asym) coefficients — note kinetic_pos near zero:\n")
print(round(coef(m_asym), 4))

# Hold-out: remove the cap-clamped tail. If kinetic_pos coefficient gets
# closer to energy_pos on the unclamped subset, the apparent asymmetry was
# driven by the clamp. (Not entirely conclusive — the unclamped subset still
# has positive kinetic that varies, but variance is squeezed.)
cat("\nHold-out (kinetic < 50, n =", sum(no_armor_df$kinetic < 50), "):\n")
m_asym_h <- update(m_asym, data = no_armor_df %>% filter(kinetic < 50))
print(round(coef(m_asym_h), 4))

###############################################################################
# Performance on the documented persistent-outlier list
###############################################################################

cat("\n\n=============================================================\n")
cat("  CANDIDATE MODELS ON DOCUMENTED PERSISTENT OUTLIERS\n")
cat("=============================================================\n\n")

outlier_serials <- c(
  # over-predicted in docs
  "dcnqk0gj", "lvn43jrf", "plu7hqkc", "10pq6ihd", "kslg1d70",
  # under-predicted in docs
  "pdefjush", "1lc95n55", "norqrpou", "66h9j346"
)

no_armor_df$pred_base    <- predict(m_base,        newdata = no_armor_df)
no_armor_df$pred_kpn     <- predict(m_kinen_pn,    newdata = no_armor_df)
no_armor_df$pred_kfloor  <- predict(m_kinen_floor, newdata = no_armor_df)

outlier_table <- no_armor_df %>%
  filter(serial %in% outlier_serials) %>%
  transmute(
    serial, skin, level,
    base   = round(pred_base, 1),   base_r = round(level - pred_base, 1),
    M5     = round(pred_kpn, 1),    M5_r   = round(level - pred_kpn, 1),
    M6     = round(pred_kfloor, 1), M6_r   = round(level - pred_kfloor, 1)
  ) %>%
  arrange(level)
cat("Per-outlier prediction comparison:\n")
print(as.data.frame(outlier_table))

outliers <- no_armor_df %>% filter(serial %in% outlier_serials)
cat("\nAggregate on outlier list:\n")
cat(sprintf("  M0 baseline:       mean |resid| = %.2f, max |resid| = %.2f\n",
            mean(abs(outliers$level - outliers$pred_base)),
            max(abs(outliers$level - outliers$pred_base))))
cat(sprintf("  M5 kinen pos/neg:  mean |resid| = %.2f, max |resid| = %.2f\n",
            mean(abs(outliers$level - outliers$pred_kpn)),
            max(abs(outliers$level - outliers$pred_kpn))))
cat(sprintf("  M6 pmax(kinen,0):  mean |resid| = %.2f, max |resid| = %.2f\n",
            mean(abs(outliers$level - outliers$pred_kfloor)),
            max(abs(outliers$level - outliers$pred_kfloor))))

# Uber-CL10 cluster
cat("\n--- Uber-CL10 cluster (CL10, kinetic >= 50, health >= 7000, fort 350-499) ---\n")
uber10 <- no_armor_df %>%
  filter(level == 10 & kinetic >= 50 & health >= 7000 & fortitude >= 350 & fortitude < 500)
cat("n =", nrow(uber10), "\n")
cat(sprintf("  M0 baseline:       mean resid = %+.2f, sd = %.2f\n",
            mean(uber10$level - uber10$pred_base),    sd(uber10$level - uber10$pred_base)))
cat(sprintf("  M5 kinen pos/neg:  mean resid = %+.2f, sd = %.2f\n",
            mean(uber10$level - uber10$pred_kpn),     sd(uber10$level - uber10$pred_kpn)))
cat(sprintf("  M6 pmax(kinen,0):  mean resid = %+.2f, sd = %.2f\n",
            mean(uber10$level - uber10$pred_kfloor),  sd(uber10$level - uber10$pred_kfloor)))

###############################################################################
# Low-stat negative-residual cluster (separate puzzle)
###############################################################################

cat("\n\n=============================================================\n")
cat("  PART (c): LOW-STAT NEGATIVE-RESIDUAL CLUSTER (DIFFERENT MODE)\n")
cat("=============================================================\n\n")

low_stat <- no_armor_df %>%
  mutate(resid_base = level - pred_base) %>%
  filter(resid_base <= -4 & health < 5000) %>%
  arrange(resid_base) %>%
  select(serial, skin, level, pred_base, pred_kpn, pred_kfloor,
         health, fortitude, hardiness, cleverness, power, kinetic, energy, nonkinen)

cat("Creatures with baseline residual <= -4 AND health < 5000:\n")
print(as.data.frame(low_stat))

cat(sprintf("\n  baseline:          mean resid = %.2f\n",
            mean(low_stat$level - low_stat$pred_base)))
cat(sprintf("  M5 kinen pos/neg:  mean resid = %.2f\n",
            mean(low_stat$level - low_stat$pred_kpn)))
cat(sprintf("  M6 pmax(kinen,0):  mean resid = %.2f\n",
            mean(low_stat$level - low_stat$pred_kfloor)))

cat("\nNote: these creatures have low health, fortitude near 0, and the\n")
cat("residual pattern doesn't track kinen — likely a separate mechanism\n")
cat("(possibly a HAM-floor or skin-tier effect).\n")

###############################################################################
# Visualizations
###############################################################################

cat("\n\n=== VISUALIZATIONS ===\n")

p1 <- ggplot(no_armor_df, aes(x = kinen, y = level - pred_base)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(title = "Baseline residual vs kinen",
       subtitle = "Slope change at kinen = 0 motivates the pmax(kinen, 0) form",
       y = "residual (actual - predicted)") +
  theme_minimal()
print(p1)

p2 <- ggplot(no_armor_df, aes(x = pred_base, y = pred_kpn)) +
  geom_point(aes(color = level - pred_base), alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_gradient2(low = "blue", mid = "gray80", high = "red", name = "baseline\nresidual") +
  labs(title = "M0 baseline vs M5 kinen pos/neg predictions") +
  theme_minimal()
print(p2)

p3 <- ggplot(no_armor_df, aes(x = level, y = level - pred_kpn)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "M5 (kinen pos/neg): residual vs actual level",
       y = "residual (actual - predicted)") +
  theme_minimal()
print(p3)
