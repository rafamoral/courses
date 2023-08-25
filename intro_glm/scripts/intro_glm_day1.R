library(tidyverse)

## loading the weight dataset
weight_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/weight.csv")
weight_df

## exploratory plots
weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  theme_bw() +
  geom_point() +
  geom_smooth(se = FALSE)

weight_df %>%
  ggplot(aes(x = age, y = weight)) +
  theme_bw() +
  geom_point() +
  geom_smooth(se = FALSE)

## modelling weight as a function of height
fit1 <- lm(weight ~ height, data = weight_df)
summary(fit1)
summary(fit1)$coefficients

confint(fit1)
confint(fit1, parm = "height")
confint(fit1, parm = "height", level = 0.9)

## modelling weight as a function of height and age
fit2 <- lm(weight ~ height + age, data = weight_df)
summary(fit2)
summary(fit2)$coefficients

confint(fit2)

## The Bernoulli GLM -----------------------------------------------------------

## loading the affairs data
affairs_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/affairs.csv")
affairs_df

affairs_df <- affairs_df %>%
  mutate(had_affair = affairs > 0)

## probability of having an affair as a function of number of years married
affairs_df %>%
  ggplot(aes(x = yearsmarried, y = had_affair %>% as.numeric)) +
  theme_bw() +
  geom_jitter(width = .2, height = .1, alpha = .5) +
  geom_smooth(se = FALSE)

## natural logarithm and Euler's constant
log(c(1,2,4,8,16), base = 2)
log(c(1,10,100,1000,10000), base = 10)
exp(1) # Euler's constant
log(exp(2))

## the logit link function
p <- 0.75
odds <- p / (1 - p)
logit_p <- log(odds)
qlogis(p)

x <- tibble(Probability = seq(0, 1, length = 200),
            `Log odds` = qlogis(Probability))
x %>%
  ggplot(aes(x = Probability, y = `Log odds`)) +
  theme_bw() +
  geom_line()

## the inverse logit
my_logit <- 0
my_ilogit <- exp(my_logit) / (1 + exp(my_logit))
my_ilogit2 <- 1 / (1 + exp(- my_logit))
plogis(my_logit)

x %>%
  ggplot(aes(y = Probability, x = `Log odds`)) +
  theme_bw() +
  geom_line()

## fitting a Bernoulli model
fit3 <- glm(had_affair ~ yearsmarried,
            family = binomial(link = "logit"),
            data = affairs_df)
summary(fit3)$coefficients

estimates <- coef(fit3)

## what is the log odds of having an affair for a person with 10 years of marriage
estimates[1] + estimates[2] * 10

## what is the log odds of having an affair for a person with 11 years of marriage
estimates[1] + estimates[2] * 11

## odds ratio for the yearsmarried predictor
## the factor by which the odds increases for a unit change in yearsmarried
log_odds_10 <- estimates[1] + estimates[2] * 10
odds_10 <- exp(log_odds_10)

log_odds_11 <- estimates[1] + estimates[2] * 11
odds_11 <- exp(log_odds_11)

odds_11 / odds_10

exp(estimates[2])

confint.default(fit3)

## 95% confidence interval on the odds ratio for `yearsmarried`
exp(confint.default(fit3, parm = 'yearsmarried'))

## prediction
## what is the probability of having an affair for a person with 10 years of marriage
plogis(estimates[1] + estimates[2] * 10)

## what is the probability of having an affair for a person with 20 years of marriage
plogis(estimates[1] + estimates[2] * 20)

library(modelr)
newdata <- tibble(yearsmarried = seq(0, 15, length = 200))

add_predictions(data = newdata, model = fit3, type = "response")

add_predictions(data = newdata, model = fit3, type = "response") %>%
  ggplot(aes(x = yearsmarried, y = pred)) +
  theme_bw() +
  geom_line() +
  ylim(0, 1) +
  geom_jitter(data = affairs_df,
              aes(y = had_affair %>% as.numeric),
              width = .2, height = .05, alpha = .3)

## deviance
summary(fit3)

deviance(fit3)
- 2 * logLik(fit3)

fit4 <- glm(had_affair ~ 1,
            family = binomial,
            data = affairs_df)
deviance(fit4)

## difference of deviances
deviance(fit4) - deviance(fit3)

1 - pchisq(deviance(fit4) - deviance(fit3), df = 1)
pchisq(deviance(fit4) - deviance(fit3), df = 1, lower.tail = FALSE)

## Alternatively
anova(fit4, fit3, test = 'Chisq')

## full model
fit5 <- glm(had_affair ~ age + yearsmarried + religiousness + children + education,
            family = binomial,
            data = affairs_df)

## reduced model
fit6 <- glm(had_affair ~ age + yearsmarried + religiousness,
            family = binomial,
            data = affairs_df)

## testing full vs. reduced
anova(fit6, fit5, test = "Chisq")

## a series of tests dropping one predictor at a time
drop1(fit5, test = 'Chisq')

summary(fit5)

## The Binomial GLM ------------------------------------------------------------