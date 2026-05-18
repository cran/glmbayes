## ----include = FALSE----------------------------------------------------------
# Prevent knitr from generating tangled .R files
knitr::opts_knit$set(tangle = FALSE)

# Prevent rmarkdown/knitr from keeping .md or .Rmd intermediates
knitr::opts_knit$set(keep_md = FALSE, keep_rmd = FALSE)

# Your existing chunk options
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup,echo = FALSE-------------------------------------------------------
library(glmbayes)

## ----dobson-------------------------------------------------------------------
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))

## ----glm_call,results = "hide"------------------------------------------------
## Call to glm
glm.D93 <- glm(counts ~ outcome + treatment, 
              family = poisson())

## ----Prior_Setup,results = "hide"---------------------------------------------
## Using glmb
## Step 1: Set up Default Prior 
ps=Prior_Setup(counts ~ outcome + treatment,family=poisson())
mu=ps$mu
V=ps$Sigma

## ----Call_glmb,results = "hide"-----------------------------------------------
# Step 3: Call the glmb function
glmb.D93<-glmb(counts ~ outcome + treatment, family=poisson(), pfamily=dNormal(mu=mu,Sigma=V))

## ----Printed_glm_Views--------------------------------------------------------
## Printed view of the output from the glm function 
print(glm.D93)

## ----Printed_glmb_Views-------------------------------------------------------

## Printed view of the output from the glmb function 
print(glmb.D93)

