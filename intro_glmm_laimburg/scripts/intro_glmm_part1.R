library(tidyverse)
library(modelr)
library(lme4)

theme_set(theme_bw())

# Random effects models ---------------------------------------------------

## load the `cbpp` dataset
cbpp_df <- as_tibble(cbpp)

cbpp_df %>%
  ggplot(aes(x = herd, y = incidence / size)) +
  geom_boxplot()

## as an example, let's look at the data from herd 4
cbpp_df_4 <- cbpp_df %>%
  filter(herd == "4")

fit16 <- glm(cbind(incidence, size - incidence) ~ 1,
             family = binomial,
             data = cbpp_df_4)

## estimated value of beta (log-odds of disease incidence)
coef(fit16)
## estimated value of pi (prob. of disease incidence)
plogis(coef(fit16))

## we can fit a single model of all herds
fit17 <- glm(cbind(incidence, size - incidence) ~ 0 + herd,
             family = binomial,
             data = cbpp_df)
plogis(coef(fit17)) %>%
  round(2)

## multilevel version of the multi-herd model
library(lme4)
fit18 <- glmer(cbind(incidence, size - incidence) ~ (1 | herd),
               family = binomial,
               data = cbpp_df)
summary(fit18)

fixef(fit18) ## estimate of the overall intercept beta0 (mean of the population of log-odds)
VarCorr(fit18)$herd[1] ## estimate of the variance of the population of log-odds

plogis(fixef(fit18))

## the "model of models" (distribution of log-odds for an infinite population of herds)
curve(dnorm(x, mean = fixef(fit18), sd = sqrt(VarCorr(fit18)$herd[1])),
      xlim = c(-5,1))

## expect 95% of the herds in this population to have
## a log-odds of disease incidence in between
fixef(fit18) + c(-1,1) * 1.96 * sqrt(VarCorr(fit18)$herd[1])
## a prob. of disease incidence in between
plogis(fixef(fit18) + c(-1,1) * 1.96 * sqrt(VarCorr(fit18)$herd[1]))

## random effects
## random differences from the average log-odds
ranef(fit18)

## gives the log-odds for each herd
coef(fit18)


# Normal Random Effects Model ---------------------------------------------

alcohol_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/alcohol.csv")

alcohol_df %>%
  ggplot(aes(x = year, y = alcohol, col = country)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none")

alcohol_df %>%
  ggplot(aes(x = reorder(country, alcohol), y = alcohol)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   size = 5))

unique(alcohol_df$country) %>% length
## if we fitted a flat model, we'd need to estimate
## 189 beta coefficients + 1 sigma^2 for the error

## fit the mixed model
fit19 <- lmer(alcohol ~ (1 | country),
              data = alcohol_df)
summary(fit19)

## ranef gives the "b" values (BLUPs)
ranef(fit19)

## Intraclass correlation
22.2 / (22.2 + 1.1)

## an example when all variation is due to random noise
beta0 <- 5
sigma2 <- 4

set.seed(123)
simulated_df <- tibble(y = rnorm(100,
                                 mean = beta0,
                                 sd = sqrt(sigma2)),
                       country = gl(20, 5))

simulated_df %>%
  ggplot(aes(x = reorder(country, y), y = y)) +
  geom_boxplot()

fit20 <- lmer(y ~ (1 | country),
              data = simulated_df)
summary(fit20)

ranef(fit20)

## fit20 is overly complex (boundary estimation issues)
## fitting the true model
fit21 <- lm(y ~ 1,
            data = simulated_df)
summary(fit21)
sigma(fit21)^2