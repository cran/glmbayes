## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,echo = FALSE-------------------------------------------------------
library(glmbayes)

## ----menarche data------------------------------------------------------------
## Load menarche data
data(menarche,package="MASS")
head(menarche, 5)


## ----Analysis Setup-----------------------------------------------------------
## Number of variables in model
Age=menarche$Age
nvars=2
set.seed(333)

## Reference Ages for setting of priors and Age_Difference
ref_age1=13  # user can modify this
ref_age2=15  ## user can modify this

## Define variables used later in analysis
Age2=Age-ref_age1
Age_Diff=ref_age2-ref_age1


## ----Prior Info---------------------------------------------------------------

## Point estimates at reference ages
m1=0.5  
m2=0.9

## Lower bound of prior credible intervals for point estimates
m1_lower=0.3
m2_lower=0.7

## Assumed correlation between the two (on link scale)
m_corr=0.4

## ----Logit: set up link function info and initialize prior matrices-----------

## Set up link function and initialize prior mean and Variance-Covariance matrices
bi_logit <- binomial(link="logit")
mu1<-matrix(0,nrow=nvars,ncol=1)
rownames(mu1)=c("Intercept","Age2")
colnames(mu1)=c("Prior Mean")
V1<-1*diag(nvars)
rownames(V1)=c("Intercept","Age2")
colnames(V1)=c("Intercept","Age2")

## ----Logit:set prior means----------------------------------------------------
## Prior mean for intercept is set to point estimate 
## at reference age1 (on logit scale)
mu1[1,1]=bi_logit$linkfun(m1)

## Prior mean for slope is set to difference in point estimates
## on logit scale divided by Age_Diff

mu1[2,1]=(bi_logit$linkfun(m2) -bi_logit$linkfun(m1))/Age_Diff 
print(mu1)



## ----Logit:set prior Variance Covariance matrix-------------------------------
## Implied standard deviations for point estimates on logit scale

sd_m1= (bi_logit$linkfun(m1) -bi_logit$linkfun(m1_lower))/1.96
sd_m2= (bi_logit$linkfun(m2) -bi_logit$linkfun(m2_lower))/1.96

## Also compute implied estimate for upper bound of confidence intervals

m1_upper=bi_logit$linkinv(bi_logit$linkfun(m1)+sd_m1*1.96)
m2_upper=bi_logit$linkinv(bi_logit$linkfun(m2)+sd_m2*1.96)
print("m1_upper is:")
m1_upper
print("m2_upper is:")
m2_upper


## Implied Standard deviation for slope (using variance formula for difference between two variables)
a=(1/Age_Diff)
sd_slope=sqrt((a*sd_m1)^2+(a*sd_m2)^2-2*a*a*(sd_m1*sd_m2*m_corr))

#Cov(m1,slope)=cov(m1, a*(m2-m1)) =a*E[(m1-E[m1])((m2-m1)-E[m2-m1])]
#   =a*E[(m1-E[m1])(m2-E[m2])]- a* E[(m1-E[m1])(m1-E[m1])]
##   =a*Cov[m1,m2] - a*Var[m1]
##  =a*sd_m1*sd_m2*m_corr-a* sd_m1*sd_m1
cov_V1=a*sd_m1*sd_m2*m_corr-a* sd_m1*sd_m1

# Set covariance matrix
V1[1,1]=sd_m1^2
V1[2,2]=sd_slope^2
V1[1,2]=cov_V1
V1[2,1]=V1[1,2]
print("V1 is:")
print(V1)

## ----Run Logit,results = "hide"-----------------------------------------------
Menarche_Model_Data=data.frame(Age=menarche$Age,Total=as.integer(menarche$Total),
Menarche=as.integer(menarche$Menarche),Age2)

glmb.out1<-glmb(n=1000,cbind(Menarche, as.integer(Total-Menarche)) ~Age2,family=binomial(logit),
pfamily=dNormal(mu=mu1,Sigma=V1),data=Menarche_Model_Data)


## ----Print Logit--------------------------------------------------------------

# Print model output
print(glmb.out1)

# Print prior mean as comparison
print(t(mu1))


## ----Summary Logit------------------------------------------------------------
summary(glmb.out1)

## ----br08-setup, eval = requireNamespace("bayesrules", quietly = TRUE)--------
library(bayesrules)
weather <- bayesrules::weather_perth
weather$raintomorrow <- as.integer(weather$raintomorrow == "Yes")

mu_w <- matrix(c(-1.4, 0.07), nrow = 1)
colnames(mu_w) <- c("(Intercept)", "humidity9am")
Sigma_w <- diag(c(0.7^2, 0.035^2))
dimnames(Sigma_w) <- list(colnames(mu_w), colnames(mu_w))

## Bayes Rules! Ch. 13: 80% posterior intervals on log-odds scale (rain_model_1)
book_br08 <- data.frame(
  parameter = c("(Intercept)", "humidity9am"),
  book_lo   = c(-5.08785, 0.04147),
  book_hi   = c(-4.13450, 0.05487),
  book_mid  = c(-4.611175, 0.04817),
  check.names = FALSE
)

## ----br08-glmb, eval = requireNamespace("bayesrules", quietly = TRUE)---------
set.seed(2026)
glmb_w <- glmb(
  raintomorrow ~ humidity9am,
  family  = binomial(),
  pfamily = dNormal(mu = mu_w, Sigma = Sigma_w),
  data    = weather,
  n       = 2000
)
print(glmb_w)

## ----br08-compare, eval = requireNamespace("bayesrules", quietly = TRUE)------
br08_compare <- data.frame(
  parameter = book_br08$parameter,
  `Book 80% lo` = book_br08$book_lo,
  `Book 80% hi` = book_br08$book_hi,
  `Book midpoint` = book_br08$book_mid,
  `glmb Post.Mean` = as.numeric(glmb_w$coef.means[book_br08$parameter]),
  `glmb Post.Sd`   = sapply(book_br08$parameter, function(p)
    sd(glmb_w$coefficients[, p, drop = TRUE])),
  check.names = FALSE
)
knitr::kable(br08_compare, digits = 4,
  caption = "Bayes Rules! Ch. 13 (80% intervals) vs. glmb() with informative priors")

