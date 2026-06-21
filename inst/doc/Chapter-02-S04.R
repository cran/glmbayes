## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(glmbayes)
if (requireNamespace("bayesrules", quietly = TRUE)) library(bayesrules)

## ----gp-br-data---------------------------------------------------------------
br_y_df  <- data.frame(y = c(1, 1, 1, rep(0, 6)))  ## n = 9, sum(y) = 3
br_n     <- nrow(br_y_df)
br_shape <- 3
br_rate  <- 4

## Analytic posterior
post_shape_br <- br_shape + sum(br_y_df$y)   ## 3 + 3 = 6
post_rate_br  <- br_rate  + br_n              ## 4 + 9 = 13

## ----gp-br-plot, eval = requireNamespace("bayesrules", quietly = TRUE)--------
library(bayesrules)
plot_gamma_poisson(
  shape      = br_shape,
  rate       = br_rate,
  sum_y      = sum(br_y_df$y),
  n          = br_n,
  prior      = TRUE,
  likelihood = TRUE,
  posterior  = TRUE
)

## ----gp-br-summary, eval = requireNamespace("bayesrules", quietly = TRUE)-----
summarize_gamma_poisson(shape = br_shape, rate = br_rate,
                        sum_y = sum(br_y_df$y), n = br_n)

## ----gp-analytic--------------------------------------------------------------
gp_analytic <- data.frame(
  Example = "Bayes Rules! daily counts",
  n = br_n,
  `sum(y)` = sum(br_y_df$y),
  Posterior = sprintf("Gamma(%d, %d)", post_shape_br, post_rate_br),
  Mean = post_shape_br / post_rate_br,
  SD = sqrt(post_shape_br) / post_rate_br,
  check.names = FALSE
)
knitr::kable(gp_analytic, digits = 4,
  caption = "Conjugate Gamma--Poisson posterior (Bayes Rules! scenario)")

## ----gp-glmb------------------------------------------------------------------
gp_beta <- matrix(br_shape / br_rate, 1L, 1L, dimnames = list(NULL, "(Intercept)"))
gp_pf   <- dGamma(shape = br_shape, rate = br_rate,
                  beta = gp_beta, Inv_Dispersion = FALSE)

set.seed(2026)
fit_gp <- glmb(
  n       = 20000,
  y ~ 1,
  data    = br_y_df,
  weights = rep(1L, br_n),
  family  = poisson(link = "identity"),
  pfamily = gp_pf
)
print(fit_gp)

## ----gp-compare---------------------------------------------------------------
gp_compare <- data.frame(
  Example = "Bayes Rules! daily counts",
  Posterior = gp_analytic$Posterior,
  `Analytic Mean` = gp_analytic$Mean,
  `Analytic SD`   = gp_analytic$SD,
  `glmb Post.Mean` = fit_gp$coef.means["(Intercept)"],
  `glmb Post.Sd`   = sd(fit_gp$coefficients[, "(Intercept)", drop = TRUE]),
  check.names = FALSE
)
knitr::kable(gp_compare, digits = 4,
  caption = "Analytic vs. glmb() posterior mean and SD")

## ----ht-load, eval = requireNamespace("LearnBayes", quietly = TRUE)-----------
library(LearnBayes)
data("hearttransplants")
cat(sprintf("%d hospitals  |  sum(y) = %d  |  sum(e) = %.0f\n",
            nrow(hearttransplants), sum(hearttransplants$y), sum(hearttransplants$e)))

## ----ht-prior-----------------------------------------------------------------
alpha0_ht <- 16L;  beta0_ht <- 15174L
ex_A <- 66L;   yobs_A <- 1L
ex_B <- 1767L; yobs_B <- 4L

## ----ht-albert-analytic-------------------------------------------------------
ht_albert <- data.frame(
  Hospital = c("A", "B"),
  e = c(ex_A, ex_B),
  y = c(yobs_A, yobs_B),
  Posterior = c("Gamma(17, 15240)", "Gamma(20, 16941)"),
  Mean = c(
    (alpha0_ht + yobs_A) / (beta0_ht + ex_A),
    (alpha0_ht + yobs_B) / (beta0_ht + ex_B)
  ),
  SD = c(
    sqrt(alpha0_ht + yobs_A) / (beta0_ht + ex_A),
    sqrt(alpha0_ht + yobs_B) / (beta0_ht + ex_B)
  ),
  check.names = FALSE
)
knitr::kable(ht_albert, digits = 6,
  caption = "Albert Ch. 3.2: analytic posterior mean and SD (shape--rate Gamma)")

## ----ht-glmb-A, eval = requireNamespace("LearnBayes", quietly = TRUE)---------
ht_beta <- matrix(alpha0_ht / beta0_ht, 1L, 1L, dimnames = list(NULL, "(Intercept)"))
ht_pf   <- dGamma(shape = alpha0_ht, rate = beta0_ht,
                  beta = ht_beta, Inv_Dispersion = FALSE)

set.seed(2026)
fit_A <- glmb(n = 20000, y ~ 1, data = data.frame(y = yobs_A),
              weights = ex_A, family = poisson(link = "identity"), pfamily = ht_pf)
print(fit_A)

## ----ht-glmb-B, eval = requireNamespace("LearnBayes", quietly = TRUE)---------
set.seed(2026)
fit_B <- glmb(n = 20000, y ~ 1, data = data.frame(y = yobs_B),
              weights = ex_B, family = poisson(link = "identity"), pfamily = ht_pf)
print(fit_B)

## ----ht-glmb-compare, eval = requireNamespace("LearnBayes", quietly = TRUE)----
ht_compare <- data.frame(
  Hospital = c("A", "B"),
  e = c(ex_A, ex_B),
  y = c(yobs_A, yobs_B),
  Posterior = c("Gamma(17, 15240)", "Gamma(20, 16941)"),
  `Albert Mean` = ht_albert$Mean,
  `Albert SD`   = ht_albert$SD,
  `glmb Post.Mean` = c(fit_A$coef.means["(Intercept)"],
                       fit_B$coef.means["(Intercept)"]),
  `glmb Post.Sd`   = c(sd(fit_A$coefficients[, "(Intercept)", drop = TRUE]),
                       sd(fit_B$coefficients[, "(Intercept)", drop = TRUE])),
  check.names = FALSE
)
knitr::kable(ht_compare, digits = 6,
  caption = "Albert analytic vs. glmb() posterior mean and SD")

