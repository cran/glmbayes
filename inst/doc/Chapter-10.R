## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(glmbayes)

## ----dobson-------------------------------------------------------------------
    ## Dobson (1990) Page 93: Randomized Controlled Trial :
    set.seed(333)
    counts <- c(18,17,15,20,10,20,25,13,12)
    outcome <- gl(3,1,9)
    treatment <- gl(3,3)
    print(d.AD <- data.frame(treatment, outcome, counts))

## ----glm_call-----------------------------------------------------------------

    glm.D93 <- glm(counts ~ outcome + treatment,
                   family = poisson(link = "log"))
    summary(glm.D93)

## ----Prior_Setup--------------------------------------------------------------

    ps <- Prior_Setup(counts ~ outcome + treatment,
                      family = poisson())
    mu <- ps$mu
    V  <- ps$Sigma

## ----Call_glmb----------------------------------------------------------------

    glmb.D93 <- glmb(counts ~ outcome + treatment,
                     family = poisson(),
                     pfamily = dNormal(mu = mu, Sigma = V))

## ----summary_glmb-------------------------------------------------------------
    print(glmb.D93)
    summary(glmb.D93)

## ----br10-setup, eval = requireNamespace("bayesrules", quietly = TRUE)--------
library(bayesrules)
equality <- bayesrules::equality_index
equality <- equality[equality$laws < max(equality$laws), ]

ps_eq <- Prior_Setup(laws ~ percent_urban + historical,
                     family = poisson(), data = equality)

## Bayes Rules! Ch. 12 tidy(equality_model) posterior estimates (log link)
book_br10 <- data.frame(
  parameter = c("(Intercept)", "percent_urban", "historicalgop", "historicalswing"),
  book_mean = c(1.71, 0.0164, -1.52, -0.610),
  book_sd   = c(0.303, 0.00353, 0.134, 0.103),
  check.names = FALSE
)

## ----br10-glmb, eval = requireNamespace("bayesrules", quietly = TRUE)---------
set.seed(2026)
glmb_eq <- glmb(
  laws ~ percent_urban + historical,
  family  = poisson(),
  pfamily = dNormal(mu = ps_eq$mu, Sigma = ps_eq$Sigma),
  data    = equality,
  n       = 2000
)
print(glmb_eq)

## ----br10-compare, eval = requireNamespace("bayesrules", quietly = TRUE)------
br10_compare <- data.frame(
  parameter = book_br10$parameter,
  `Book mean` = book_br10$book_mean,
  `Book SD`   = book_br10$book_sd,
  `glmb Post.Mean` = as.numeric(glmb_eq$coef.means[book_br10$parameter]),
  `glmb Post.Sd`   = sapply(book_br10$parameter, function(p)
    sd(glmb_eq$coefficients[, p, drop = TRUE])),
  check.names = FALSE
)
knitr::kable(br10_compare, digits = 4,
  caption = "Bayes Rules! Ch. 12 (informative + weak book priors) vs. glmb() with Prior_Setup() defaults")

