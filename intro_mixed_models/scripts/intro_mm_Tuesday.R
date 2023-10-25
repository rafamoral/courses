library(tidyverse)

# Random Effects Models ---------------------------------------------------

## loading the cbpp dataset
library(lme4)

cbpp_df <- as_tibble(cbpp)
?cbpp

cbpp_df %>%
  ggplot(aes(x = herd, y = incidence/size * 100)) +
  theme_bw() +
  geom_boxplot()

## looking at herd 4 only
cbpp_df %>%
  filter(herd == 4)

fit1 <- glm(cbind(incidence, size - incidence) ~ 1,
            family = binomial,
            data = cbpp_df)

## estimated value of beta0, i.e. the log-odds of disease incidence in herd 4
coef(fit1)
## estimated value of pi
plogis(coef(fit1))
## 95% confidence interval for the prob. of disease incidence in herd 4
plogis(confint.default(fit1))

cbpp_df %>%
  filter(herd == 4) %>%
  summarise(pi_hat = mean(incidence/size))

## a single model of all herds
fit2 <- glm(cbind(incidence, size - incidence) ~ 0 + herd,
            family = binomial,
            data = cbpp_df)
coef(fit2)

round(plogis(coef(fit2)), 2)

logits_glm <- coef(fit2)
pi_glm <- plogis(coef(fit2))

## multilevel / mixed version of the multi-herd model
library(lme4)
fit3 <- glmer(cbind(incidence, size - incidence) ~ 1 + (1 | herd),
              family = binomial,
              data = cbpp_df)
summary(fit3)

fixef(fit3) ## fixed intercept
ranef(fit3) ## random deviations for each herd

VarCorr(fit3)$herd[1] ## estimate of sigma^2

## let's look at the model for the random intercepts
curve(dnorm(x, fixef(fit3), sd = sqrt(VarCorr(fit3)$herd[1])),
      xlim = c(-6,3))

plogis(fixef(fit3) + c(-1, 1) * 1.96 * (VarCorr(fit3)$herd[1]))
## if we repeated this study with an infinite number of herds,
## we expect that 95% of the time the incidence will be between these
## probabilities

## 95% CI for the fixed intercept
-2.0457 + c(-1, 1) * 1.96 * 0.2411
## this has a completely different interpretation
## if we repeat the study many times and construct the 95% CI for
## the fixed intercept, we expect that 95% of them will include
## the true value of the fixed intercept

fixef(fit3) + ranef(fit3)$herd$`(Intercept)`
coef(fit2)

# Linear Mixed Effects Models ---------------------------------------------

alcohol_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_mixed_models/data/alcohol.csv")
alcohol_df

alcohol_df %>%
  ggplot(aes(x = reorder(country, alcohol), y = alcohol)) +
  theme_bw() +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

fit4 <- lmer(alcohol ~ 1 + (1 | country),
             data = alcohol_df)
summary(fit4)  
  
## estimate of beta_0
fixef(fit4)
## ranef gives the predicted "b" values (BLUPs)
ranef(fit4)

## the mu's are beta_0 + b (or coef(fit4))
coef(fit4)

alcohol_df %>%
  filter(country == "Brazil")

## Intraclass correlation coefficient (ICC)
VarCorr(fit4) %>% as.data.frame
sig2_b <- 22.207828
sig2 <- 1.107824
ICC <- sig2_b / (sig2 + sig2_b)

## displaying shrinkage
comp <- tibble("country" = unique(alcohol_df$country),
               "flat" = coef(lm(alcohol ~ 0 + country, data = alcohol_df)),
               "mixed" = coef(fit4)$country$`(Intercept)`)

comp %>%
  pivot_longer(2:3,
               names_to = "model type",
               values_to = "mu") %>%
  ggplot(aes(x = `model type`, y = mu, group = country)) +
  theme_bw() +
  geom_point() +
  geom_line(alpha = .3) +
  ylab(expression(mu))

## shrinkage when all variation is due to random noise
beta0 <- 5
sigma2 <- 4

set.seed(2023)
simulated_df <- tibble(y = rnorm(100, mean = beta0, sd = sqrt(sigma2)),
                       country = gl(20, 5))

fit5 <- lmer(y ~ 1 + (1 | country),
             data = simulated_df)
summary(fit5)
ranef(fit5)

VarCorr(fit5) %>% as.data.frame
sig2_b <- 0.02071003
sig2 <- 3.92442152
ICC <- sig2_b / (sig2 + sig2_b)

comp <- tibble("country" = unique(simulated_df$country),
               "flat" = coef(lm(y ~ 0 + country, data = simulated_df)),
               "mixed" = coef(fit5)$country$`(Intercept)`)

comp %>%
  pivot_longer(2:3,
               names_to = "model type",
               values_to = "mu") %>%
  ggplot(aes(x = `model type`, y = mu, group = country)) +
  theme_bw() +
  geom_point() +
  geom_line(alpha = .3) +
  ylab(expression(mu))

# Linear Mixed Effects Models ---------------------------------------------

## loading the data
library(lme4)
data("sleepstudy")
sleepstudy_df <- as_tibble(sleepstudy)
sleepstudy_df

## "spaghetti" plot
sleepstudy_df %>%
  ggplot(aes(x = Days, y = Reaction, group = Subject)) +
  theme_bw() +
  geom_point() +
  geom_line()

## looking at each individual
sleepstudy_df %>%
  ggplot(aes(x = Days, y = Reaction)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  facet_wrap(~ Subject)

## predicting from linear models fitted separately to each subject
sleepstudy_df %>%
  ggplot(aes(x = Days, y = Reaction)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~ Subject) +
  geom_smooth(method = "lm", se = FALSE)

## a model for subject 334
sleepstudy_334 <- sleepstudy_df %>%
  filter(Subject == 334)

fit6 <- lm(Reaction ~ Days,
           data = sleepstudy_334)
coef(fit6) ## the beta coefficients
sigma(fit6) ## the estimate for sigma

## model includes intercept automatically, same as...
fit6a <- lm(Reaction ~ 1 + Days,
            data = sleepstudy_334)
coef(fit6a)

## to remove intercept use "0 +" or "-1 +"
fit6b <- lm(Reaction ~ 0 + Days,
            data = sleepstudy_334)
coef(fit6b)
fit6c <- lm(Reaction ~ - 1 + Days,
            data = sleepstudy_334)
coef(fit6c)

## non-multilevel model for all subjects in the data
## 18 * 2 + 1 = 37 parameters (18 intercepts, 18 slopes, 1 variance)
fit7 <- lm(Reaction ~ 0 + Subject + Subject : Days,
           data = sleepstudy_df)
coef(fit7)
