## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(glmbayes)

## ----Plant_Data---------------------------------------------------------------
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)

## ----Plant_w_intercept--------------------------------------------------------
lm.D9 <- lm(weight ~ group)

## ----Plant_Prior--------------------------------------------------------------
ps=Prior_Setup(weight ~ group,family=gaussian())

## ----Plant_Prior_Spec---------------------------------------------------------
ps

## ----Plant_lmb_calls----------------------------------------------------------
lmb.D9=lmb(weight ~ group,dNormal(mu=ps$mu,Sigma  =ps$Sigma,dispersion=ps$dispersion))
##lmb.D9_v2=lmb(weight ~ group,dNormal_Gamma(mu=ps$mu,Sigma_0=ps$Sigma_0,shape=ps$shape,rate=ps$rate))
## lmb.D9_v3=lmb(weight ~ group,dIndependent_Normal_Gamma(mu=ps$mu,Sigma = ps$Sigma,shape=ps$shape_ING,rate=ps$rate))
summary(lmb.D9)

## ----Plant_summary------------------------------------------------------------
summary(lm.D9)

## ----Plant_lmb_summary--------------------------------------------------------
summary(lmb.D9)

## ----Plant_lmb_coefficients1--------------------------------------------------
sumlmb<-summary(lmb.D9)
sumlmb$coefficients1

## ----Plant_lmb_coefficients---------------------------------------------------
sumlmb<-summary(lmb.D9)
sumlmb$coefficients

## ----Plant_lmb_Percentiles----------------------------------------------------
sumlmb<-summary(lmb.D9)
sumlmb$Percentiles

## ----br03-setup, eval = requireNamespace("bayesrules", quietly = TRUE)--------
library(bayesrules)
bikes_br <- within(bayesrules::bikes, {
  temp_feel_c <- temp_feel - mean(temp_feel)
})

ps_bikes <- Prior_Setup(rides ~ temp_feel_c, family = gaussian(), data = bikes_br)
coef_br03 <- rownames(ps_bikes$mu)   ## Prior_Setup: names on rows, not colnames(mu)
mu_br <- matrix(c(5000, 100), nrow = 1)
colnames(mu_br) <- coef_br03
Sigma_br <- diag(c(1000^2, 40^2))
dimnames(Sigma_br) <- list(coef_br03, coef_br03)

## Posterior summaries reported in Bayes Rules! Ch. 9 (rides ~ temp_feel; stan_glm output)
book_br03 <- data.frame(
  parameter = c("(Intercept)", "temp_feel_c"),
  book_mean = c(-2194, 82.2),
  book_sd   = c(362, 5.15),
  note = c(
    "Book fits rides ~ temp_feel (uncentered); intercept not comparable to centered fit",
    "Slope matches book temp_feel coefficient (Ch. 9 tidy table)"
  ),
  check.names = FALSE
)

## ----br03-lmb, eval = requireNamespace("bayesrules", quietly = TRUE)----------
set.seed(2026)
lmb_bikes <- lmb(
  rides ~ temp_feel_c,
  data = bikes_br,
  pfamily = dNormal(mu = mu_br, Sigma = Sigma_br, dispersion = ps_bikes$dispersion),
  n = 2000
)
print(lmb_bikes)

## ----br03-compare, eval = requireNamespace("bayesrules", quietly = TRUE)------
br03_compare <- data.frame(
  parameter = book_br03$parameter,
  `Book mean` = book_br03$book_mean,
  `Book SD`   = book_br03$book_sd,
  `lmb Post.Mean` = as.numeric(lmb_bikes$coef.means[book_br03$parameter]),
  `lmb Post.Sd`   = sapply(book_br03$parameter, function(p)
    sd(lmb_bikes$coefficients[, p, drop = TRUE])),
  check.names = FALSE
)
knitr::kable(br03_compare, digits = 2,
  caption = "Bayes Rules! Ch. 9 posterior vs. lmb() (informative priors; fixed sigma)")

