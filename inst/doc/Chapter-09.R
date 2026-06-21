## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(glmbayes)

## ----Menarche-----------------------------------------------------------------
data(menarche,package="MASS")
Age2 <- menarche$Age - 13
Menarche_Model_Data <- data.frame(
  Age = menarche$Age,
  Total = menarche$Total,
  Menarche = menarche$Menarche,
  Age2 = Age2
)
Menarche_Model_Data

## ----Menarche_Prior_Logit-----------------------------------------------------
ps <- Prior_Setup(
  cbind(Menarche, Total - Menarche) ~ Age2,
  family = binomial(link = "logit"),
  data = Menarche_Model_Data
)
mu <- ps$mu
V  <- ps$Sigma

## ----Menarche_Model_Logit-----------------------------------------------------
glmb.logit <- glmb(
  cbind(Menarche, Total - Menarche) ~ Age2,
  family  = binomial(link = "logit"),
  pfamily = dNormal(mu = mu, Sigma = V),
  data    = Menarche_Model_Data,
  n       = 1000
)

## ----Menarche_Summary_Logit---------------------------------------------------
summary(glmb.logit)

## ----Menarche_Prior_Probit----------------------------------------------------
ps2 <- Prior_Setup(
  cbind(Menarche, Total - Menarche) ~ Age2,
  family = binomial(link = "probit"),
  data = Menarche_Model_Data
)
mu2 <- ps2$mu
V2  <- ps2$Sigma

## ----Menarche_Model_Probit----------------------------------------------------
glmb.probit <- glmb(
  cbind(Menarche, Total - Menarche) ~ Age2,
  family  = binomial(link = "probit"),
  pfamily = dNormal(mu = mu2, Sigma = V2),
  data    = Menarche_Model_Data,
  n       = 1000
)

## ----Menarche_Summary_Probit--------------------------------------------------
summary(glmb.probit)

## ----Menarche_Prior_cloglog---------------------------------------------------
ps3 <- Prior_Setup(
  cbind(Menarche, Total - Menarche) ~ Age2,
  family = binomial(link = "cloglog"),
  data = Menarche_Model_Data
)
mu3 <- ps3$mu
V3  <- ps3$Sigma

## ----Menarche_Model_Cloglog---------------------------------------------------
glmb.cloglog <- glmb(
  cbind(Menarche, Total - Menarche) ~ Age2,
  family  = binomial(link = "cloglog"),
  pfamily = dNormal(mu = mu3, Sigma = V3),
  data    = Menarche_Model_Data,
  n       = 1000
)

## ----Menarche_Summary_cloglog-------------------------------------------------
summary(glmb.cloglog)

## ----Menarche_DIC_Compare-----------------------------------------------------
DIC_comp<-rbind(
  extractAIC(glmb.logit),
  extractAIC(glmb.probit),
  extractAIC(glmb.cloglog))

rownames(DIC_comp)<-c("logit","probit","cloglog")
DIC_comp


## ----br09-setup, eval = requireNamespace("bayesrules", quietly = TRUE)--------
library(bayesrules)
weather <- bayesrules::weather_perth
weather$raintomorrow <- as.integer(weather$raintomorrow == "Yes")
mu_w <- matrix(c(-1.4, 0.07), nrow = 1)
colnames(mu_w) <- c("(Intercept)", "humidity9am")
Sigma_w <- diag(c(0.7^2, 0.035^2))
dimnames(Sigma_w) <- list(colnames(mu_w), colnames(mu_w))
book_br09 <- data.frame(
  parameter = c("(Intercept)", "humidity9am"),
  book_lo   = c(-5.08785, 0.04147),
  book_hi   = c(-4.13450, 0.05487),
  book_mid  = c(-4.611175, 0.04817),
  check.names = FALSE
)

## ----br09-fit, eval = requireNamespace("bayesrules", quietly = TRUE)----------
set.seed(2026)
glmb_rain <- glmb(
  raintomorrow ~ humidity9am,
  family  = binomial(),
  pfamily = dNormal(mu = mu_w, Sigma = Sigma_w),
  data    = weather,
  n       = 2000
)
print(glmb_rain)

## ----br09-compare, eval = requireNamespace("bayesrules", quietly = TRUE)------
br09_compare <- data.frame(
  parameter = book_br09$parameter,
  `Book 80% lo` = book_br09$book_lo,
  `Book 80% hi` = book_br09$book_hi,
  `Book midpoint` = book_br09$book_mid,
  `glmb Post.Mean` = as.numeric(glmb_rain$coef.means[book_br09$parameter]),
  `glmb Post.Sd`   = sapply(book_br09$parameter, function(p)
    sd(glmb_rain$coefficients[, p, drop = TRUE])),
  check.names = FALSE
)
knitr::kable(br09_compare, digits = 4,
  caption = "Bayes Rules! Ch. 13 vs. glmb() (informative priors)")

