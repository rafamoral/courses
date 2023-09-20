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

## note
## the deviance is calculated as
## 2 * (logLik(saturated model) - logLik(current model))
2*(sum(dpois(doctor_df$gp_visits, doctor_df$gp_visits, log = TRUE)) - logLik(fit3))
deviance(fit3)
- 2 * logLik(fit3)
## for the Bernoulli model, logLik(saturated model) = 0, therefore D = - 2 * logLik(current model)
## some packages use deviance = - 2 * logLik(current model) for all models
## although inconsistent, this doesn't change tests based on differences of deviances
## because the logLik of the saturated model is a constant that is cancelled out

## loading the insurance dataset (offset example)
insurance_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/insurance.csv")
insurance_df$District <- as.factor(insurance_df$District)
insurance_df$Age <- factor(insurance_df$Age, levels = c("<25", "25-29", "30-35", ">35"))

## exploratory plots
insurance_df %>%
  ggplot(aes(x = Holders, y = Claims)) +
  theme_bw() +
  geom_point()

insurance_df %>%
  ggplot(aes(x = Age, y = Claims)) +
  theme_bw() +
  geom_boxplot()

insurance_df %>%
  ggplot(aes(x = Age, y = Claims/Holders)) +
  theme_bw() +
  geom_boxplot()

## fitting the Poisson model without offset
fit4 <- glm(Claims ~ Age,
            family = poisson,
            data = insurance_df)
summary(fit4)

exp(2.2) # according to this misspecified model, people older than 35 make
         # 9 times more claims than people younger than 25

## fitting the Poisson model with offset
fit5 <- glm(Claims ~ Age + offset(log(Holders)),
            family = poisson,
            data = insurance_df)
summary(fit5)

exp(-0.5) # now this model estimates a 40% reduction in claims for people
          # older than 35 compared to people younger than 25

newdata <- tibble(Age = unique(insurance_df$Age),
                  Holders = 1000)

add_predictions(data = newdata, model = fit5, type = 'response')

## Goodness-of-fit of Poisson models

## loading the progeny data
library(hnp)
data(progeny)
progeny

fit6 <- glm(y ~ extract,
            family = poisson,
            data = progeny)
summary(fit6)

curve(dchisq(x, fit6$df.residual), xlim = c(0, 100))
abline(v = deviance(fit6))
pchisq(deviance(fit6), fit6$df.residual, lower.tail = FALSE)

qchisq(0.95, fit6$df.residual) # critical value

## half-normal plot with a simulated envelope
hnp(fit6)