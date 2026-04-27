###############################################################################
# investigate_elastic_net.R
#
# Apply elastic-net regression to the unarmored M7 model to see how
# regularization treats the negative fortitude coefficient under collinearity.
#
# Three sub-questions:
#  (1) Does cross-validated ridge / lasso / elastic-net keep fortitude
#      negative, or does it shrink it toward zero?
#  (2) Along the regularization path, in what order do predictors enter
#      the model? (Robust ones first, fragile ones last.)
#  (3) Are there elastic-net solutions that match OLS predictive performance
#      while having a more "physically defensible" coefficient profile?
#
# glmnet handles standardization internally and reports coefficients back
# on the original scale; we don't pre-standardize.
###############################################################################

source("R/data.R")
library(dplyr)
library(glmnet)

unarm <- normalized_df %>% dplyr::filter(armor == 0) %>%
  mutate(ke_floor = pmax(pmin(kinetic, energy), 0))

predictors <- c("hardiness","fortitude","dexterity","intellect",
                "cleverness","power","ke_floor","nonkinen")
X <- as.matrix(unarm[, predictors])
y <- unarm$level
n <- length(y)

# OLS baseline for comparison
m_ols <- lm(level ~ hardiness + fortitude + dexterity + intellect +
                    cleverness + power + ke_floor + nonkinen,
            data = unarm)
ols_coef <- coef(m_ols)

###############################################################################
# (1) Cross-validated fits at three alpha values
###############################################################################
set.seed(2026)

cv_ridge <- cv.glmnet(X, y, alpha = 0.0, nfolds = 10, standardize = TRUE)
cv_enet  <- cv.glmnet(X, y, alpha = 0.5, nfolds = 10, standardize = TRUE)
cv_lasso <- cv.glmnet(X, y, alpha = 1.0, nfolds = 10, standardize = TRUE)

# Pull lambda.min coefficients (best CV fit), back on original scale
get_coefs <- function(cv) as.vector(coef(cv, s = "lambda.min"))
get_se_coefs <- function(cv) as.vector(coef(cv, s = "lambda.1se"))

cv_table <- data.frame(
  Term = c("(Intercept)", predictors),
  OLS = round(as.vector(ols_coef), 5),
  Ridge_min = round(get_coefs(cv_ridge), 5),
  ElasticNet_min = round(get_coefs(cv_enet), 5),
  Lasso_min = round(get_coefs(cv_lasso), 5)
)

cat("==============================================================\n")
cat(" Cross-validated coefficients (lambda.min, original scale)\n")
cat("==============================================================\n\n")
print(cv_table, row.names = FALSE)

cat("\nLambda.1se (more parsimonious choice, within 1 SE of best CV MSE):\n")
cv_1se <- data.frame(
  Term = c("(Intercept)", predictors),
  Ridge_1se = round(get_se_coefs(cv_ridge), 5),
  ElasticNet_1se = round(get_se_coefs(cv_enet), 5),
  Lasso_1se = round(get_se_coefs(cv_lasso), 5)
)
print(cv_1se, row.names = FALSE)

###############################################################################
# (2) Fit metrics for each cross-validated solution
###############################################################################
fit_metrics <- function(beta, X, y, p) {
  yhat <- as.vector(cbind(1, X) %*% beta)
  resid <- y - yhat
  sse <- sum(resid^2)
  sst <- sum((y - mean(y))^2)
  list(r2 = 1 - sse/sst, sd = sd(resid), n_nonzero = sum(beta[-1] != 0))
}

ols_fit   <- fit_metrics(ols_coef, X, y)
ridge_fit <- fit_metrics(get_coefs(cv_ridge), X, y)
enet_fit  <- fit_metrics(get_coefs(cv_enet),  X, y)
lasso_fit <- fit_metrics(get_coefs(cv_lasso), X, y)

ridge_se  <- fit_metrics(get_se_coefs(cv_ridge), X, y)
enet_se   <- fit_metrics(get_se_coefs(cv_enet),  X, y)
lasso_se  <- fit_metrics(get_se_coefs(cv_lasso), X, y)

cat("\n==============================================================\n")
cat(" Fit metrics\n")
cat("==============================================================\n")
metrics_df <- data.frame(
  Model = c("OLS","Ridge (lambda.min)","ElasticNet (lambda.min)","Lasso (lambda.min)",
            "Ridge (lambda.1se)","ElasticNet (lambda.1se)","Lasso (lambda.1se)"),
  R2 = c(ols_fit$r2, ridge_fit$r2, enet_fit$r2, lasso_fit$r2,
         ridge_se$r2, enet_se$r2, lasso_se$r2),
  Resid_SD = c(ols_fit$sd, ridge_fit$sd, enet_fit$sd, lasso_fit$sd,
               ridge_se$sd, enet_se$sd, lasso_se$sd),
  NonZero_Coefs = c(ols_fit$n_nonzero, ridge_fit$n_nonzero, enet_fit$n_nonzero,
                    lasso_fit$n_nonzero, ridge_se$n_nonzero,
                    enet_se$n_nonzero, lasso_se$n_nonzero)
)
print(metrics_df, row.names = FALSE, digits = 4)

###############################################################################
# (3) Coefficient path: lasso entry order
#
# Walk lambda from large -> small. The order in which each predictor first
# becomes nonzero ranks the predictors by how much signal they each carry.
###############################################################################
cat("\n==============================================================\n")
cat(" Lasso entry order along the regularization path\n")
cat(" (predictors ranked by 'first to enter' = most signal)\n")
cat("==============================================================\n\n")

lasso_path <- glmnet(X, y, alpha = 1.0, standardize = TRUE)
beta_path <- as.matrix(lasso_path$beta)  # rows = predictors, cols = lambdas
# lambdas are sorted decreasing in glmnet, so col 1 is sparsest
entry_lambda <- apply(beta_path, 1, function(row) {
  nz <- which(row != 0)
  if (length(nz) == 0) NA else lasso_path$lambda[min(nz)]
})
entry_order <- data.frame(
  Predictor = names(entry_lambda),
  EnteredAtLambda = round(entry_lambda, 5),
  EnteredAtSign = sapply(seq_along(entry_lambda), function(i) {
    nz <- which(beta_path[i, ] != 0)
    if (length(nz) == 0) "—" else
      if (beta_path[i, min(nz)] > 0) "+" else "-"
  })
)
entry_order <- entry_order[order(-entry_order$EnteredAtLambda), ]
print(entry_order, row.names = FALSE)

###############################################################################
# (4) Stability of the fortitude coefficient along the path
#
# At what lambda does fortitude become nonzero, and is it always negative
# from that point downward to lambda=0?
###############################################################################
cat("\n==============================================================\n")
cat(" Fortitude coefficient stability along the lasso path\n")
cat("==============================================================\n\n")
fort_idx_p <- which(rownames(beta_path) == "fortitude")
fort_path <- beta_path[fort_idx_p, ]
fort_nonzero_idx <- which(fort_path != 0)
if (length(fort_nonzero_idx) == 0) {
  cat("fortitude never enters the lasso path (always zero across all lambdas).\n")
} else {
  cat(sprintf("fortitude first enters at lambda = %.5f (rank %d of %d steps)\n",
              lasso_path$lambda[min(fort_nonzero_idx)],
              min(fort_nonzero_idx), length(lasso_path$lambda)))
  cat(sprintf("fortitude sign at every nonzero lambda: %s\n",
              if (all(fort_path[fort_nonzero_idx] < 0)) "always negative"
              else if (all(fort_path[fort_nonzero_idx] > 0)) "always positive"
              else "mixed"))
  cat(sprintf("fortitude coef range across path: [%.5f, %.5f]\n",
              min(fort_path[fort_nonzero_idx]), max(fort_path[fort_nonzero_idx])))
}

###############################################################################
# (5) Where does the fortitude penalty go in elastic-net? (vs OLS)
###############################################################################
cat("\n==============================================================\n")
cat(" Coefficient comparison on the (hardiness, fortitude) collinear pair\n")
cat("==============================================================\n\n")
pairs_df <- data.frame(
  Model = c("OLS","Ridge.min","ElasticNet.min","Lasso.min",
            "Ridge.1se","ElasticNet.1se","Lasso.1se"),
  Hardiness = c(ols_coef["hardiness"],
                get_coefs(cv_ridge)[2], get_coefs(cv_enet)[2], get_coefs(cv_lasso)[2],
                get_se_coefs(cv_ridge)[2], get_se_coefs(cv_enet)[2], get_se_coefs(cv_lasso)[2]),
  Fortitude = c(ols_coef["fortitude"],
                get_coefs(cv_ridge)[3], get_coefs(cv_enet)[3], get_coefs(cv_lasso)[3],
                get_se_coefs(cv_ridge)[3], get_se_coefs(cv_enet)[3], get_se_coefs(cv_lasso)[3])
)
pairs_df$Sum_HF <- pairs_df$Hardiness + pairs_df$Fortitude
print(pairs_df, row.names = FALSE, digits = 4)

cat("\nNote: under collinearity, ridge tends to redistribute mass between\n")
cat("hardiness and fortitude; lasso tends to pick one and zero the other.\n")

