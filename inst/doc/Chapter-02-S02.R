## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(glmbayes)
if (requireNamespace("bayesrules", quietly = TRUE)) library(bayesrules)

## ----nn-bayesrules-data-------------------------------------------------------
prior_mean <- 7;    prior_sd <- 1.5   ## prior: N(7, 1.5^2)
sigma      <- 1.8                     ## known population SD
y_bar      <- 6.1;  n_sleep  <- 22L   ## observed sample

## ----nn-bayesrules-plot, eval = requireNamespace("bayesrules", quietly = TRUE)----
library(bayesrules)
## Overlay prior, likelihood, and posterior
plot_normal_normal(
  mean       = prior_mean,
  sd         = prior_sd,
  sigma      = sigma,
  y_bar      = y_bar,
  n          = n_sleep,
  prior      = TRUE,
  likelihood = TRUE,
  posterior  = TRUE
)

## ----nn-bayesrules-summary, eval = requireNamespace("bayesrules", quietly = TRUE)----
## Tabular summary of prior, likelihood, and posterior parameters
summarize_normal_normal(
  mean  = prior_mean,
  sd    = prior_sd,
  sigma = sigma,
  y_bar = y_bar,
  n     = n_sleep
)

## ----nn-analytic--------------------------------------------------------------
prec_0 <- 1 / prior_sd^2
prec_L <- n_sleep / sigma^2
prec_n <- prec_0 + prec_L

post_mean <- (prec_0 * prior_mean + prec_L * y_bar) / prec_n
post_sd   <- sqrt(1 / prec_n)

nn_analytic <- data.frame(
  Example = "Sleep hours (BayesRules)",
  n = n_sleep,
  `y-bar` = y_bar,
  Posterior = sprintf("N(%.4f, %.4f^2)", post_mean, post_sd),
  Mean = post_mean,
  SD = post_sd,
  check.names = FALSE
)
knitr::kable(nn_analytic, digits = 4,
  caption = "Conjugate Normal--Normal posterior (known sigma)")

## ----faithful-setup-----------------------------------------------------------
y_faith <- faithful$waiting
n_faith <- length(y_faith)

ybar_faith  <- mean(y_faith)
sigma_faith <- sd(y_faith)           ## treat as known

prior_mean_f <- 72;  prior_sd_f <- 15   ## N(72, 15^2) prior

cat(sprintf("n = %d,  y-bar = %.2f,  sigma = %.2f\n",
            n_faith, ybar_faith, sigma_faith))

## ----faithful-posterior-------------------------------------------------------
prec_0f <- 1 / prior_sd_f^2
prec_Lf <- n_faith / sigma_faith^2
prec_nf <- prec_0f + prec_Lf

post_mean_f <- (prec_0f * prior_mean_f + prec_Lf * ybar_faith) / prec_nf
post_sd_f   <- sqrt(1 / prec_nf)

faith_analytic <- data.frame(
  Dataset = "Old Faithful waiting",
  n = n_faith,
  `y-bar` = ybar_faith,
  Posterior = sprintf("N(%.4f, %.4f^2)", post_mean_f, post_sd_f),
  Mean = post_mean_f,
  SD = post_sd_f,
  check.names = FALSE
)
knitr::kable(faith_analytic, digits = 4,
  caption = "Conjugate Normal--Normal posterior (known sigma)")

## ----faithful-lmb-------------------------------------------------------------
df_faith <- data.frame(y = y_faith)

mu_f    <- matrix(prior_mean_f, nrow = 1, ncol = 1,
                  dimnames = list(NULL, "(Intercept)"))
Sigma_f <- matrix(prior_sd_f^2, nrow = 1, ncol = 1,
                  dimnames = list("(Intercept)", "(Intercept)"))

pf_faith <- dNormal(mu = mu_f, Sigma = Sigma_f,
                    dispersion = sigma_faith^2)

set.seed(2026)
fit_faith <- lmb(
  n       = 20000,
  y ~ 1,
  data    = df_faith,
  pfamily = pf_faith
)
print(fit_faith)

## ----faithful-compare---------------------------------------------------------
faith_compare <- data.frame(
  Dataset = "Old Faithful waiting",
  Posterior = faith_analytic$Posterior,
  `Analytic Mean` = faith_analytic$Mean,
  `Analytic SD`   = faith_analytic$SD,
  `lmb Post.Mean` = fit_faith$coef.means["(Intercept)"],
  `lmb Post.Sd`   = sd(fit_faith$coefficients[, "(Intercept)", drop = TRUE]),
  check.names = FALSE
)
knitr::kable(faith_compare, digits = 4,
  caption = "Analytic vs. lmb() posterior mean and SD")

