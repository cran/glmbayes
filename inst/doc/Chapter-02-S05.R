## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(glmbayes)

## ----ggamma-illustration-setup------------------------------------------------
alpha0 <- 2;  b0 <- 4         ## Gamma(2, 4) prior: mean = 0.5
k      <- 1                   ## known shape (exponential)
n_obs  <- 20L;  sum_y <- 35   ## data: 20 observations, sum = 35

post_shape <- alpha0 + n_obs * k
post_rate  <- b0    + sum_y

ggamma_analytic <- data.frame(
  Example = "Call inter-arrivals (illustration)",
  n = n_obs,
  `sum(y)` = sum_y,
  Posterior = sprintf("Gamma(%d, %d)", post_shape, post_rate),
  `Mean (rate)` = post_shape / post_rate,
  `SD (rate)`   = sqrt(post_shape) / post_rate,
  check.names = FALSE
)
knitr::kable(ggamma_analytic, digits = 4,
  caption = "Conjugate Gamma--Gamma posterior for rate beta")

## ----ggamma-illustration-plot, fig.width = 6, fig.height = 4------------------
beta_grid <- seq(0.01, 1.5, length.out = 500)

## Normalized likelihood: Gamma(n*k + 1, sum_y) shape, treated as density
lik_unnorm <- dgamma(beta_grid, shape = n_obs * k + 1, rate = sum_y)

## Scale likelihood to have same peak height as the prior (visual only)
prior_vals   <- dgamma(beta_grid, alpha0, b0)
post_vals    <- dgamma(beta_grid, post_shape, post_rate)
lik_scaled   <- lik_unnorm * (max(prior_vals) / max(lik_unnorm))

plot(beta_grid, prior_vals, type = "l", lwd = 2, col = "steelblue",
     xlab = expression(beta), ylab = "Density",
     main = "Gamma–Gamma update: prior, likelihood, posterior",
     ylim = c(0, max(post_vals) * 1.1))
lines(beta_grid, lik_scaled, lwd = 2, col = "darkgreen", lty = 2)
lines(beta_grid, post_vals,  lwd = 2, col = "tomato")
legend("topright",
       legend = c("Prior  Gamma(2, 4)",
                  "Likelihood (scaled)",
                  sprintf("Posterior  Gamma(%d, %d)", post_shape, post_rate)),
       col  = c("steelblue", "darkgreen", "tomato"),
       lty  = c(1, 2, 1), lwd = 2, bty = "n")
abline(v = alpha0 / b0,          lty = 3, col = "steelblue")
abline(v = post_shape / post_rate, lty = 3, col = "tomato")

## ----cb-data, eval = requireNamespace("bayesrules", quietly = TRUE)-----------
library(bayesrules)

## Use net finishing times (minutes); drop missing values
y_cb <- cherry_blossom_sample$net
y_cb <- y_cb[is.finite(y_cb)]
n_cb <- length(y_cb)

ybar_cb <- mean(y_cb)
s2_cb   <- var(y_cb)

## Method-of-moments estimate of shape k: k_hat = ybar^2 / s^2
k_hat_cb <- ybar_cb^2 / s2_cb

cat(sprintf("n = %d,  mean = %.2f min,  var = %.2f,  k_hat = %.3f\n",
            n_cb, ybar_cb, s2_cb, k_hat_cb))

## ----cb-prior, eval = requireNamespace("bayesrules", quietly = TRUE)----------
## Set prior centered at rate corresponding to ~85 min mean finish time
## E[mu] = k / beta_prior  =>  beta_prior = k_hat / 85
alpha0_cb <- 5
b0_cb     <- alpha0_cb / (k_hat_cb / ybar_cb)  ## prior mean rate = k_hat / ybar

cat(sprintf("Prior: Gamma(%.2f, %.4f),  prior mean rate = %.4f,  implied mean time = %.1f min\n",
            alpha0_cb, b0_cb,
            alpha0_cb / b0_cb,
            k_hat_cb / (alpha0_cb / b0_cb)))

## ----cb-posterior, eval = requireNamespace("bayesrules", quietly = TRUE)------
post_shape_cb <- alpha0_cb + n_cb * k_hat_cb
post_rate_cb  <- b0_cb     + sum(y_cb)

post_mean_rate_cb <- post_shape_cb / post_rate_cb
post_sd_rate_cb   <- sqrt(post_shape_cb) / post_rate_cb
post_mean_time_cb <- k_hat_cb / post_mean_rate_cb

cb_analytic <- data.frame(
  Dataset = "Cherry blossom finish times",
  n = n_cb,
  `k (fixed)` = k_hat_cb,
  Posterior = sprintf("Gamma(%.2f, %.4f)", post_shape_cb, post_rate_cb),
  `Mean (rate beta)` = post_mean_rate_cb,
  `SD (rate beta)`   = post_sd_rate_cb,
  `Mean time (min)`  = post_mean_time_cb,
  check.names = FALSE
)
knitr::kable(cb_analytic, digits = 4,
  caption = "Conjugate Gamma--Gamma posterior for rate beta (shape--rate)")

## ----cb-glmb, eval = requireNamespace("bayesrules", quietly = TRUE)-----------
df_cb <- data.frame(y = y_cb)

cb_beta <- matrix(alpha0_cb / b0_cb, nrow = 1, ncol = 1,
                  dimnames = list(NULL, "(Intercept)"))

cb_pf <- dGamma(
  shape          = alpha0_cb,
  rate           = b0_cb,
  beta           = cb_beta,
  Inv_Dispersion = FALSE,
  lik_shape      = k_hat_cb
)

set.seed(2026)
fit_cb <- glmb(
  n       = 20000,
  y ~ 1,
  data    = df_cb,
  family  = Gamma(link = "identity"),
  pfamily = cb_pf
)
print(fit_cb)

## ----cb-compare, eval = requireNamespace("bayesrules", quietly = TRUE)--------
cb_compare <- data.frame(
  Dataset = "Cherry blossom finish times",
  Posterior = cb_analytic$Posterior,
  `Analytic Mean (rate)` = cb_analytic$`Mean (rate beta)`,
  `Analytic SD (rate)`   = cb_analytic$`SD (rate beta)`,
  `glmb Post.Mean` = fit_cb$coef.means["(Intercept)"],
  `glmb Post.Sd`   = sd(fit_cb$coefficients[, "(Intercept)", drop = TRUE]),
  check.names = FALSE
)
knitr::kable(cb_compare, digits = 4,
  caption = "Analytic vs. glmb() posterior mean and SD for rate beta")

