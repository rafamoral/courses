library(tidyverse)

## The Normal Model: A Recap ---------------------------------------------------

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
affairs_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_glmm_laimburg/data/affairs.csv")
affairs_df

## affairs: number of affairs in the past year
## religiousness: 1 (not religious) to 5 (very religious)
## rating: self-rating of marriage (1 = very unhappy; 5 = very happy)

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

estimates <- coef(fit6)

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
anova(fit4, fit3, test = "Chisq")

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
drop1(fit5, test = "Chisq")

summary(fit5)

## The Binomial GLM ------------------------------------------------------------

## loading the citrus psyllid mortality data
library(hnp)
data(fungi)

fungi$species <- relevel(fungi$species, "beauveria")

fungi %>%
  ggplot(aes(x = lconc, y = y/20, colour = species)) +
  theme_bw() +
  geom_point() +
  ylab("proportion of dead insects") +
  xlab("log10(concentration)")

## fitting binomial GLM
fit7 <- glm(cbind(y, 20 - y) ~ lconc + species,
            family = binomial,
            data = fungi)
summary(fit7)
estimates <- coef(fit7)

drop1(fit7, test = "Chisq")

## what is the log odds of killing the citrus psyllid when using a
## log10(concentration) of 5 for B. bassiana?
estimates[1] + estimates[2] * 5

## what is the average probability of killing the citrus psyllid when using a
## log10(concentration) of 5 for B. bassiana?
plogis(estimates[1] + estimates[2] * 5)

## how many citrus psyllids would die out of a batch of 20, on average, when using a
## log10(concentration) of 5 for B. bassiana?
plogis(estimates[1] + estimates[2] * 5) * 20

## odds ratio for an increase in one unit in log10(concentration)
exp(estimates[2])
## an 84% increase in the odds of dying for every extra unit in log10(concentration)

## what is the odds ratio of a citrus psyllid dying when using
## I. fumosorosea rather than B. bassiana?
exp(estimates[3])

## what is the % increase in the odds of a citrus psyllid dying when using
## I. fumosorosea rather than B. bassiana?
exp(estimates[3]) * 100

## 95% confidence interval for the odds ratio
exp(confint.default(fit7, parm = "speciesisaria"))
## the odds of the insect dying when infected with isaria is 2 to 4 times
## the odds of dying when infected with beauveria, with 95% confidence

## prediction
newdata <- expand.grid(lconc = seq(4, 8, length = 200),
                       species = c("beauveria","isaria"))

add_predictions(data = newdata, model = fit7, type = "response") %>%
  ggplot(aes(x = lconc, y = pred, colour = species)) +
  theme_bw() +
  geom_point(aes(y = y/20),
             data = fungi) +
  geom_line()

## LD50 for beauveria  
dose.p(fit7, p = 0.5, cf = 1:2)

## LD50 for isaria (a bit fiddly due to how the function dose.p works)
dose.p(update(fit7, . ~ . - 1), p = 0.5, cf = c(3,1))

## using alternative link functions

x_logistic <- tibble(x = seq(-5, 5, length = 200),
                     `f(x)` = dlogis(x),
                     `F(x)` = plogis(x),
                     dist = "logistic")

x_logistic %>%
  pivot_longer(2:3,
               names_to = "type",
               values_to = "y") %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~ type, scales = "free_y") +
  ylab("")

x_normal <- tibble(x = seq(-5, 5, length = 200),
                   `f(x)` = dnorm(x),
                   `F(x)` = pnorm(x),
                   dist = "normal")

x_normal %>%
  pivot_longer(2:3,
               names_to = "type",
               values_to = "y") %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~ type, scales = "free_y") +
  ylab("")

library(ordinal)
x_gumbel <- tibble(x = seq(-5, 5, length = 200),
                   `f(x)` = dgumbel(x, max = FALSE),
                   `F(x)` = pgumbel(x, max = FALSE),
                   dist = "gumbel")

x_gumbel %>%
  pivot_longer(2:3,
               names_to = "type",
               values_to = "y") %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~ type, scales = "free_y") +
  ylab("")

x_dist <- rbind(x_logistic, x_normal, x_gumbel)

x_dist %>%
  ggplot(aes(x = x, y = `f(x)`, col = dist)) +
  theme_bw() +
  geom_line()

x_dist %>%
  ggplot(aes(x = x, y = `F(x)`, col = dist)) +
  theme_bw() +
  geom_line()

## loading the golf dataset
golf_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/golf_putts.csv")
golf_df

## exploratory plot
golf_df %>%
  ggplot(aes(x = distance, y = success/attempts)) +
  theme_bw() +
  geom_point()

## fitting binomial models with 3 different links and checking predicted curves
fit8 <- glm(cbind(success, attempts - success) ~ distance,
            family = binomial(link = "logit"), ## default link
            data = golf_df)

fit9 <- glm(cbind(success, attempts - success) ~ distance,
            family = binomial(link = "probit"),
            data = golf_df)

fit10 <- glm(cbind(success, attempts - success) ~ distance,
             family = binomial(link = "cloglog"),
             data = golf_df)

newdata <- tibble(distance = seq(2, 20, length = 200))
newdata <- add_predictions(data = newdata, model = fit8, var = "logit", type = "response")
newdata <- add_predictions(data = newdata, model = fit9, var = "probit", type = "response")
newdata <- add_predictions(data = newdata, model = fit10, var = "cloglog", type = "response")

newdata %>%
  pivot_longer(cols = 2:4,
               names_to = "link",
               values_to = "pred") %>%
  ggplot(aes(x = distance, y = pred, col = link)) +
  theme_bw() +
  geom_point(data = golf_df,
             aes(y = success/attempts, col = NA)) +
  geom_line()

## using quadratic trends
fit11 <- glm(cbind(success, attempts - success) ~ poly(distance, 2),
             family = binomial(link = "cloglog"),
             data = golf_df)

newdata <- add_predictions(data = newdata, model = fit11, var = "cloglog quad", type = "response")

newdata %>%
  pivot_longer(cols = 2:5,
               names_to = "link",
               values_to = "pred") %>%
  ggplot(aes(x = distance, y = pred, col = link)) +
  theme_bw() +
  geom_point(data = golf_df,
             aes(y = success/attempts, col = NA)) +
  geom_line()
## careful extrapolating!