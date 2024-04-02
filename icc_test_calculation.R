#############################
## Testing ICC calculation
## Author; Zoe Haskell-Craig
###############################

# packages --------------
library(tidyverse)
library(lme4)
library(performance)
library(sjstats)

# synthetic dataset ------------------
n <- 1000
id <- c(1:n)
hosp_id <- rep(c(1:20), 50)
  
beta_0 <- rep(0.5, n) #fixed effect intercept
zeta_hosp <- rnorm(n=20, mean = 0, sd = 0.25) #random intercepts for hospital
zeta_0 <- rep(zeta_hosp, 50)

data <- tibble(id, hosp_id, beta_0, zeta_0)

data$Y_p <- data$beta_0 + data$zeta_0
data$Y <- rbinom(n, size = 1, prob = data$Y_p)


# unconditional mean model -----------
m_null <- glmer(Y ~ 1 + (1|hosp_id), data, family = binomial)
summary(m_null)

# performance calculation
performance::icc(m_null)

# hand calculation
sigma2_0 <- as.data.frame(VarCorr(m_null),comp="Variance")$vcov[1]
total_var <- sigma2_0 + (pi^2)/3
icc_hand <- sigma2_0/total_var
icc_hand

# CONCLUSION-------------
# package for icc for logistic mixed-effects model calculates icc = sigma2_0/(sigma2_0 + pi^2/3)

