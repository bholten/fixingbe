###############################################################################
# investigate_constrained_regression.R
#
# Test whether the negative fortitude coefficient in the unarmored M7 model
# is doing real predictive work, or whether it is collinearity bookkeeping
# that we can constrain away with negligible loss of fit.
#
# Approach: refit M7 on the unarmored subset with fortitude >= 0 enforced
# via quadratic programming (quadprog::solve.QP). Compare R^2, residual SD,
# and per-cluster fit (especially the uber-CL10 cluster, where the empirical
# fortitude penalty supposedly earns its keep) against the unconstrained
# model.
#
# By KKT: if the unconstrained estimate violates the constraint (fort < 0),
# the constrained optimum sets the violating coefficient to 0 and re-fits
# the others. So the QP solution should match what we'd get by dropping
# fortitude from the regression — we verify both ways and report the
# coefficient shifts that matter.
###############################################################################

source("R/data.R")
library(dplyr)
library(quadprog)

unarm <- normalized_df %>% dplyr::filter(armor == 0) %>%
  mutate(ke_floor = pmax(pmin(kinetic, energy), 0))

# Unconstrained M7 (current canonical)
m_unc <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                    cleverness + power + ke_floor + nonkinen,
            data = unarm)

# Constrained: fortitude >= 0
X <- model.matrix(m_unc)
y <- unarm$level
n <- length(y)
p <- ncol(X)
fort_idx <- which(colnames(X) == "fortitude")

Dmat <- t(X) %*% X
dvec <- as.vector(t(X) %*% y)
Amat <- matrix(0, nrow = p, ncol = 1)
Amat[fort_idx, 1] <- 1
bvec <- 0

# Tiny ridge for numerical PD safety; doesn't move solution meaningfully
Dmat_pd <- Dmat + diag(1e-8, p)
qp <- solve.QP(Dmat_pd, dvec, Amat, bvec, meq = 0)
b_con <- qp$solution
names(b_con) <- colnames(X)

yhat_con  <- as.vector(X %*% b_con)
resid_con <- y - yhat_con
sse_con   <- sum(resid_con^2)
sst       <- sum((y - mean(y))^2)
r2_con    <- 1 - sse_con / sst
sd_con    <- sd(resid_con)
sigma2_con <- sse_con / n
aic_con   <- n * log(2*pi*sigma2_con) + n + 2 * (p + 1)

cat("==============================================================\n")
cat(" Unarmored M7: unconstrained vs fortitude-constrained (>= 0)\n")
cat("==============================================================\n\n")

cat(sprintf("Sample size: n = %d\n\n", n))

cmp <- data.frame(
  Term = colnames(X),
  Unconstrained = round(coef(m_unc), 5),
  Constrained = round(b_con, 5),
  Shift = round(b_con - coef(m_unc), 5)
)
print(cmp, row.names = FALSE)

cat("\nFit metrics:\n")
cat(sprintf("  Unconstrained: R^2 = %.4f, resid SD = %.3f, AIC = %.1f\n",
            summary(m_unc)$r.squared, sd(resid(m_unc)), AIC(m_unc)))
cat(sprintf("  Constrained:   R^2 = %.4f, resid SD = %.3f, AIC = %.1f\n",
            r2_con, sd_con, aic_con))
cat(sprintf("  Delta R^2    = %+.4f, delta SD = %+.3f\n",
            r2_con - summary(m_unc)$r.squared,
            sd_con - sd(resid(m_unc))))

###############################################################################
# Sanity check: KKT implies dropping fortitude should give the same answer
###############################################################################
m_drop <- lm(level ~ hardiness + dexterity + intellect +
                     cleverness + power + ke_floor + nonkinen, data = unarm)

cat("\nKKT sanity check (drop-fortitude lm should match constrained QP):\n")
drop_coefs <- c(coef(m_drop)["(Intercept)"], 0,  # 0 for fortitude
                coef(m_drop)[c("hardiness","dexterity","intellect",
                               "cleverness","power","ke_floor","nonkinen")])
names(drop_coefs) <- colnames(X)[c(1, fort_idx,
                                   setdiff(seq_len(p), c(1, fort_idx)))]
# Reorder to match X column order
drop_coefs <- drop_coefs[colnames(X)]
cat(sprintf("  Max |QP - drop_lm| = %.2e\n",
            max(abs(b_con - drop_coefs))))

###############################################################################
# How does the constraint affect the uber-CL10 cluster specifically?
###############################################################################
cat("\nUber-CL10 cluster (level <= 12 & kinetic >= 50):\n")
uber_idx <- unarm$level <= 12 & unarm$kinetic >= 50
cat(sprintf("  n = %d\n", sum(uber_idx)))
cat(sprintf("  Unconstrained: mean resid %+.2f, SD %.2f\n",
            mean(resid(m_unc)[uber_idx]), sd(resid(m_unc)[uber_idx])))
cat(sprintf("  Constrained:   mean resid %+.2f, SD %.2f\n",
            mean(resid_con[uber_idx]), sd(resid_con[uber_idx])))

###############################################################################
# Per-fortitude-band breakdown — where does the constraint hurt most?
###############################################################################
cat("\nResidual SD by fortitude band (unarmored subset):\n")
unarm$band <- cut(unarm$fortitude,
                  breaks = c(-Inf, 0, 100, 200, 300, 400, 500),
                  labels = c("<=0","0-100","100-200","200-300","300-400","400-500"),
                  include.lowest = TRUE, right = FALSE)
band_summary <- unarm %>%
  mutate(resid_unc = resid(m_unc), resid_con = resid_con) %>%
  group_by(band) %>%
  summarise(n = n(),
            sd_unc = sd(resid_unc),
            sd_con = sd(resid_con),
            mean_unc = mean(resid_unc),
            mean_con = mean(resid_con),
            .groups = "drop")
print(as.data.frame(band_summary))

###############################################################################
# What if we also fit on the all-special subset (no fort -> resist algebraic
# link)? Does the constraint cost less there?
###############################################################################
cat("\n\nAll-special subset (k & e from DNA only, fort -> resist channel closed):\n")
all_special <- unarm %>%
  mutate(
    k_eff = !is.na(kinetic.effective) & kinetic.effective != 0,
    e_eff = !is.na(energy.effective)  & energy.effective  != 0,
    k_spc = !is.na(kinetic.special)   & kinetic.special   != 0,
    e_spc = !is.na(energy.special)    & energy.special    != 0
  ) %>%
  dplyr::filter(!k_eff & !e_eff & (k_spc | kinetic == 0) & (e_spc | energy == 0))

m_unc_sp <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                       cleverness + power + ke_floor + nonkinen,
               data = all_special)

X_sp <- model.matrix(m_unc_sp)
y_sp <- all_special$level
fort_idx_sp <- which(colnames(X_sp) == "fortitude")
Dmat_sp <- t(X_sp) %*% X_sp + diag(1e-8, ncol(X_sp))
dvec_sp <- as.vector(t(X_sp) %*% y_sp)
Amat_sp <- matrix(0, nrow = ncol(X_sp), ncol = 1)
Amat_sp[fort_idx_sp, 1] <- 1
qp_sp <- solve.QP(Dmat_sp, dvec_sp, Amat_sp, 0, meq = 0)
b_con_sp <- qp_sp$solution
resid_con_sp <- y_sp - as.vector(X_sp %*% b_con_sp)
r2_con_sp <- 1 - sum(resid_con_sp^2) /
  sum((y_sp - mean(y_sp))^2)

cat(sprintf("  n = %d\n", nrow(all_special)))
cat(sprintf("  Unconstrained: fort_coef = %+.5f, R^2 = %.4f, SD = %.3f\n",
            coef(m_unc_sp)["fortitude"], summary(m_unc_sp)$r.squared,
            sd(resid(m_unc_sp))))
cat(sprintf("  Constrained:   fort_coef = %+.5f, R^2 = %.4f, SD = %.3f\n",
            b_con_sp[fort_idx_sp], r2_con_sp, sd(resid_con_sp)))

###############################################################################
# Summary
###############################################################################
cat("\n\n==============================================================\n")
cat(" SUMMARY\n")
cat("==============================================================\n")
cat("The constrained model sets fortitude = 0 (boundary solution, as KKT\n")
cat("predicts when the unconstrained estimate violates the constraint).\n")
cat("The cost is the R^2 / SD shift reported above; the question is\n")
cat("whether that cost is worth eating for a more physically defensible\n")
cat("formula in the SWGEmu re-implementation.\n")
