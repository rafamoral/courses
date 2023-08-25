library(tidyverse)

## The Multinomial GLM ---------------------------------------------------------

## loading the grazing dataset
grazing_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/grazing.csv")
grazing_df

library(nnet)

fit1 <- multinom(resp ~ months, data = grazing_df)
summary(fit1)
estimates <- coef(fit1)

## logit 2: log(pi2/pi1) =   0.55 - 0.015 * month
## logit 3: log(pi3/pi1) = - 1.22 - 0.047 * month

## for month 3
den <- 1 + exp(estimates[1,1] + estimates[1,2] * 3) + exp(estimates[2,1] + estimates[2,2] * 3)
pi1 <- 1 / den
pi2 <- pi1 * exp(estimates[1,1] + estimates[1,2] * 3)
pi3 <- pi1 * exp(estimates[2,1] + estimates[2,2] * 3)
pi1 + pi2 + pi3

predict(fit1, data.frame(months = 3), type = "prob")
c(pi1, pi2, pi3)

## predictions
library(modelr)

newdata <- tibble(months = 3:16)
add_predictions(data = newdata, model = fit1, type = "prob")

newdata <- tibble(months = seq(3, 16, length = 200))
newdata <- tibble(newdata, 
                  add_predictions(data = newdata, model = fit1, type = "prob")$pred %>%
                    as.data.frame)
names(newdata)[2:4] <- c("bare ground", "tussocks", "weeds")

newdata %>%
  pivot_longer(2:4,
               names_to = "category",
               values_to = "prob") %>%
  ggplot(aes(x = months, y = prob, col = category)) +
  theme_bw() +
  geom_line() +
  ylim(0, 1)

## The Poisson GLM -------------------------------------------------------------

x <- tibble(x = 0:20,
            `P(x)` = dpois(x, 3.5))

x %>%
  ggplot(aes(x = x, y = `P(x)`)) +
  theme_bw() +
  geom_point(size = 1) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = `P(x)`), lwd = .3) +
  ggtitle("Y ~ Poisson(3.5)")

## binomial approximation
x <- tibble(x = 0:25,
            Poisson = dpois(x, 0.0075 * 1000),
            Binomial = dbinom(x, 1000, 0.0075))

x %>%
  pivot_longer(2:3,
               names_to = "distribution",
               values_to = "P(x)") %>%
  ggplot(aes(x = x, y = `P(x)`)) +
  theme_bw() +
  geom_point(size = 1) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = `P(x)`), lwd = .3) +
  facet_wrap(~ distribution)

## loading number of GP visits dataset
doctor_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/doctor_visits.csv")
doctor_df$sex <- factor(doctor_df$sex, labels = c("male","female"))
doctor_df$age <- doctor_df$age * 100

## exploratory plot
doctor_df %>%
  ggplot(aes(x = age, y = gp_visits)) +
  theme_bw() +
  geom_jitter(height = .15, width = .005, alpha = .1) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ sex) +
  ylab("Number of GP visits") +
  xlab("Age")

## fitting Poisson models
fit2 <- glm(gp_visits ~ sex + age,
            family = poisson,
            data = doctor_df)
summary(fit2)
estimates <- coef(fit2)

## What is the rate (aka lambda, aka mean) of the Poisson distribution for a female aged 50?

## log of the rate for a female aged 50 is....
log_rate_female_50 <- estimates[1] + estimates[2] * 1 + estimates[3] * 50

## the rate for a female aged 50 is....
rate_female_50 <- exp(log_rate_female_50)

## draw samples from the Poisson dist whose rate is rate_female_50
rpois(1000, lambda = rate_female_50)
table(rpois(1000, lambda = rate_female_50))
round(dpois(0:5, lambda = rate_female_50), 3)

## What is the rate (aka lambda, aka mean) of the Poisson distribution
## for a *male* aged 50?

## log of the rate for a male aged 50 is....
log_rate_male_50 <- estimates[1] + estimates[2] * 0 + estimates[3] * 50

## the rate for a male aged 50 is....
rate_male_50 <- exp(log_rate_male_50)

## draw samples from the Poisson dist whose rate is rate_male_50
rpois(1000, lambda = rate_male_50)
table(rpois(1000, lambda = rate_male_50))
round(dpois(0:5, lambda = rate_male_50), 3)

log_rate_female_50 - log_rate_male_50

c(rate_female_50, rate_male_50)
rate_female_50 / rate_male_50

exp(estimates[2])

summary(fit2)
confint.default(fit2)

## 95% CI on the factor by which the rate change as we change from men to women
exp(confint.default(fit2, parm = "sexfemale"))

## predicted curves
newdata <- expand.grid(age = seq(19, 72, length = 200),
                       sex = c("male","female"))
add_predictions(data = newdata, model = fit2, type = "response", var = "gp_visits") %>%
  ggplot(aes(x = age, y = gp_visits)) +
  theme_bw() +
  geom_jitter(data = doctor_df,
              height = .15, width = .005, alpha = .1) +
  geom_line(col = 2, lwd = 1) +
  facet_wrap(~ sex) +
  ylab("Number of GP visits") +
  xlab("Age")

## model comparison
fit3 <- glm(gp_visits ~ sex + age + insurance,
            family = poisson,
            data = doctor_df)
deviance(fit2)
deviance(fit3)

deviance(fit2) - deviance(fit3)

pchisq(deviance(fit2) - deviance(fit3), df = 3, lower.tail = FALSE)

anova(fit2, fit3, test = 'Chisq')

## loading the xxxx dataset (offset example)