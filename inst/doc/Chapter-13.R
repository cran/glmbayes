## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(glmbayes)
library(bayestestR)

## ----menarche-fit-------------------------------------------------------------
data(menarche, package = "MASS")

Age2 <- menarche$Age - 13
Menarche_Model_Data <- data.frame(
  Menarche  = menarche$Menarche,
  Total     = menarche$Total,
  Age2      = Age2
)

ps <- Prior_Setup(
  cbind(Menarche, Total - Menarche) ~ Age2,
  family = binomial(link = "logit"),
  data   = Menarche_Model_Data
)

fit_logit <- glmb(
  cbind(Menarche, Total - Menarche) ~ Age2,
  family  = binomial(link = "logit"),
  pfamily = dNormal(mu = ps$mu, Sigma = ps$Sigma),
  data    = Menarche_Model_Data,
  n             = 800,
  use_parallel  = FALSE
)

coef_draws <- as.data.frame(fit_logit$coefficients)

## ----describe-----------------------------------------------------------------
bayestestR::describe_posterior(coef_draws)
bayestestR::hdi(coef_draws, ci = 0.89)
bayestestR::rope(coef_draws, range = c(-0.02, 0.02))
bayestestR::p_direction(coef_draws)

