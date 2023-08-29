library(tidyverse)

## Random Effect Models --------------------------------------------------------

## loading the cbpp dataset
library(lme4)

cbpp_df <- as_tibble(lme4::cbpp)
cbpp_df

## look at batch 4

cbpp_df_4 <- filter(cbpp_df, herd == "4")

fit1 <- glm(cbind(incidence, size - incidence) ~ 1,
            family = binomial,
            data = cbpp_df_4)

## estimated value of beta, i.e. the log odds of disease incidence in herd 4
coef(fit1)
## estimated value of pi, i.e. the probability of disease incidence in herd 4
plogis(coef(fit1))
## 95% confidence interval on the probability of disease incidence in herd 4
plogis(confint.default(fit1))

## a single model of all herds
fit2 <- glm(cbind(incidence, size - incidence) ~ 0 + herd,
            family = binomial,
            data = cbpp_df)
round(plogis(coef(fit2)), 2)

logits_glm <- coef(fit2)
pi_glm <- plogis(coef(fit2))

# multilevel version of the multi herd model
library(lme4)
fit3 <- glmer(cbind(incidence, size - incidence) ~ 1 + (1 | herd),
              family = binomial,
              data = cbpp_df)
summary(fit3)

fixef(fit3)
VarCorr(fit3)$herd[1]

fixef(fit3) + c(-1, 1) * 1.96 * VarCorr(fit3)$herd[1]
plogis(fixef(fit3) + c(-1, 1) * 1.96 * VarCorr(fit3)$herd[1])
## if we repeated this study with an infinite number of herds, we expect
## that 95% of the time the incidence will be between these probabilities

## random effect, or random differences, from the average log odds
ranef(fit3)
coef(fit2)

## shrinkage
comp <- tibble("herd" = 1:15,
               "flat" = plogis(coef(fit2)),
               "mixed" = plogis(fixef(fit3) + ranef(fit3)$herd$`(Intercept)`))

comp %>%
  pivot_longer(2:3,
               names_to = "model type",
               values_to = "pi") %>%
  ggplot(aes(x = `model type`, y = pi, group = herd)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  ylab(expression(pi))

## Linear Mixed Effect Models --------------------------------------------------

alcohol_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_mixed_models/data/alcohol.csv")
alcohol_df

