## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library(glmbayes)
# data("Cleveland")
# 
# ps <- Prior_Setup(
#   hd ~ age + sex + cp + trestbps + chol +
#     fbs + restecg + thalach + exang + oldpeak + slope + ca + thal,
#   family = binomial(logit),
#   data = Cleveland
# )
# 
# t_cpu <- system.time({
#   fit_cpu <- glmb(
#     hd ~ age + sex + cp + trestbps + chol +
#       fbs + restecg + thalach + exang + oldpeak + slope + ca + thal,
#     family       = binomial(link = "logit"),
#     pfamily      = dNormal(mu = ps$mu, Sigma = ps$Sigma),
#     data         = Cleveland,
#     n            = 20000,
#     Gridtype     = 2,
#     use_parallel = TRUE,
#     use_opencl   = FALSE,
#     verbose      = FALSE
#   )
# })
# 
# t_gpu <- system.time({
#   fit_gpu <- glmb(
#     hd ~ age + sex + cp + trestbps + chol +
#       fbs + restecg + thalach + exang + oldpeak + slope + ca + thal,
#     family       = binomial(link = "logit"),
#     pfamily      = dNormal(mu = ps$mu, Sigma = ps$Sigma),
#     data         = Cleveland,
#     n            = 20000,
#     Gridtype     = 2,
#     use_parallel = TRUE,
#     use_opencl   = TRUE,
#     verbose      = FALSE
#   )
# })
# 
# t_cpu
# t_gpu

## ----echo=FALSE, out.width="100%"---------------------------------------------
knitr::include_graphics(
  system.file("extdata", "cleveland_non_opencl_output_01.png", package = "glmbayes")
)

## ----echo=FALSE, out.width="100%"---------------------------------------------
knitr::include_graphics(
  system.file("extdata", "cleveland_opencl_output_01.png", package = "glmbayes")
)

## ----eval=FALSE---------------------------------------------------------------
# summary(fit_gpu)

## ----echo=FALSE, out.width="100%"---------------------------------------------
knitr::include_graphics(
  system.file("extdata", "cleveland_summary_output_01.png", package = "glmbayes")
)
knitr::include_graphics(
  system.file("extdata", "cleveland_summary_output_02.png", package = "glmbayes")
)

