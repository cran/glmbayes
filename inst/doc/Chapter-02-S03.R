## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(glmbayes)
if (requireNamespace("bayesrules", quietly = TRUE)) library(bayesrules)

## ----bb-data------------------------------------------------------------------
a0 <- 4;  b0 <- 6          ## Beta(4, 6) prior: mean = 0.4
y  <- 14; n  <- 30          ## data: 14/30 successes

## Analytic posterior
post_a <- a0 + y            ## 4 + 14 = 18
post_b <- b0 + (n - y)      ## 6 + 16 = 22

## ----bb-bayesrules-plot, eval = requireNamespace("bayesrules", quietly = TRUE)----
library(bayesrules)
## Overlay prior, likelihood (scaled), and posterior densities
plot_beta_binomial(
  alpha      = a0,
  beta       = b0,
  y          = y,
  n          = n,
  prior      = TRUE,
  likelihood = TRUE,
  posterior  = TRUE
)

## ----bb-bayesrules-summary, eval = requireNamespace("bayesrules", quietly = TRUE)----
## Tabular summary of prior, likelihood, and posterior
summarize_beta_binomial(alpha = a0, beta = b0, y = y, n = n)

## ----bb-analytic--------------------------------------------------------------
bb_analytic <- data.frame(
  Example = "Analgesic trial (BayesRules)",
  n = n,
  y = y,
  Posterior = sprintf("Beta(%d, %d)", post_a, post_b),
  Mean = post_a / (post_a + post_b),
  SD = sqrt(post_a * post_b / ((post_a + post_b)^2 * (post_a + post_b + 1))),
  check.names = FALSE
)
knitr::kable(bb_analytic, digits = 4,
  caption = "Conjugate Beta--Binomial posterior")

## ----bechdel-data, eval = requireNamespace("bayesrules", quietly = TRUE)------
library(bayesrules)

## Binary outcome: 1 = PASS, 0 = FAIL
pass   <- as.integer(bechdel[["binary"]] == "PASS")
n_bech <- length(pass)
y_bech <- sum(pass)

cat(sprintf("n = %d,  y (PASS) = %d,  proportion = %.3f\n",
            n_bech, y_bech, y_bech / n_bech))

## ----bechdel-posterior, eval = requireNamespace("bayesrules", quietly = TRUE)----
a0_b <- 9;  b0_b <- 11    ## Beta(9, 11) prior: mean = 0.45

post_a_b <- a0_b + y_bech
post_b_b <- b0_b + (n_bech - y_bech)

c(prior_mean  = a0_b / (a0_b + b0_b),
  data_freq   = round(y_bech / n_bech, 4),
  post_mean   = round(post_a_b / (post_a_b + post_b_b), 4))

## 95% credible interval
round(qbeta(c(0.025, 0.975), post_a_b, post_b_b), 4)

## ----bechdel-analytic, eval = requireNamespace("bayesrules", quietly = TRUE)----
analytic_mean_b <- post_a_b / (post_a_b + post_b_b)
analytic_sd_b   <- sqrt(post_a_b * post_b_b /
                        ((post_a_b + post_b_b)^2 * (post_a_b + post_b_b + 1)))

bech_analytic <- data.frame(
  Dataset = "Bechdel test",
  n = n_bech,
  y = y_bech,
  Posterior = sprintf("Beta(%d, %d)", post_a_b, post_b_b),
  Mean = analytic_mean_b,
  SD = analytic_sd_b,
  check.names = FALSE
)
knitr::kable(bech_analytic, digits = 4,
  caption = "Conjugate Beta--Binomial posterior")

## ----bechdel-glmb, eval = requireNamespace("bayesrules", quietly = TRUE)------
df_bech <- data.frame(y = pass)

bech_beta <- matrix(a0_b / (a0_b + b0_b), nrow = 1, ncol = 1,
                    dimnames = list(NULL, "(Intercept)"))

bech_pf <- dBeta(shape1 = a0_b, shape2 = b0_b, beta = bech_beta)

set.seed(2026)
fit_bech <- glmb(
  n       = 20000,
  y ~ 1,
  data    = df_bech,
  weights = rep(1L, n_bech),
  family  = binomial(link = "identity"),
  pfamily = bech_pf
)
print(fit_bech)

## ----bechdel-compare, eval = requireNamespace("bayesrules", quietly = TRUE)----
bech_compare <- data.frame(
  Dataset = "Bechdel test",
  Posterior = bech_analytic$Posterior,
  `Analytic Mean` = bech_analytic$Mean,
  `Analytic SD`   = bech_analytic$SD,
  `glmb Post.Mean` = fit_bech$coef.means["(Intercept)"],
  `glmb Post.Sd`   = sd(fit_bech$coefficients[, "(Intercept)", drop = TRUE]),
  check.names = FALSE
)
knitr::kable(bech_compare, digits = 4,
  caption = "Analytic vs. glmb() posterior mean and SD")

