## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 6,
  fig.height = 4
)

## ----setup--------------------------------------------------------------------
library(glmbayes)

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
coef_draws <- as.matrix(fit_logit$coefficients)

## ----ppc-setup----------------------------------------------------------------
## Observed success proportions (aligned with simulate.glmb for binomial)
y_obs <- Menarche_Model_Data$Menarche / Menarche_Model_Data$Total

pred_resp <- predict(fit_logit, type = "response")
nd <- max(1L, min(50L, nrow(pred_resp)))
pred_sub <- pred_resp[seq_len(nd), , drop = FALSE]

y_rep <- stats::simulate(
  fit_logit,
  pred = pred_sub,
  prior.weights = fit_logit$prior.weights
)

stopifnot(nrow(y_rep) == nd, ncol(y_rep) == length(y_obs))

