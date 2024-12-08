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

## contamination dataset
contamination_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glmm_laimburg/data/05_milk_contamination_data.csv")
contamination_df <- contamination_df %>%
  mutate(obs_ID = as.factor(obs_ID),
         farmcode = as.factor(farmcode))

## exploratory plot
contamination_df %>%
  ggplot(aes(x = doy, y = contamination)) +
  theme_bw() +
  geom_point(alpha = .5) +
  facet_grid(overseed ~ rain)

## fitting Poisson models
fit2 <- glm(contamination ~ doy + overseed,
            family = poisson,
            data = contamination_df)
summary(fit2)
estimates <- coef(fit2)

## What is the rate (aka lambda, aka mean) of the Poisson distribution in an
## overseeded plot for day 100?

## log of the rate for an overseeded plot for day 100 is...
log_rate_overseeded_100 <- estimates[1] + estimates[2] * 100 + estimates[3]

## the rate for an overseeded plot for day 100 is...
rate_overseeded_100 <- exp(log_rate_overseeded_100)

## What is the rate (aka lambda, aka mean) of the Poisson distribution
## for a non-overseeded plot on day 100?

## log of the rate
log_rate_nonoverseeded_100 <- estimates[1] + estimates[2] * 100

## the rate
rate_nonoverseeded_100 <- exp(log_rate_nonoverseeded_100)

log_rate_overseeded_100 - log_rate_nonoverseeded_100 ## same as estimates[3]

rate_overseeded_100 / rate_nonoverseeded_100

exp(estimates[3])

summary(fit2)
confint.default(fit2)

## 95% CI on the factor by which the rate change as we change from nonoverseeded to overseeded
exp(confint.default(fit2, parm = "overseed"))

## model comparison
fit3 <- glm(contamination ~ doy + overseed + rain,
            family = poisson,
            data = contamination_df)
deviance(fit2)
deviance(fit3)

deviance(fit2) - deviance(fit3)

pchisq(deviance(fit2) - deviance(fit3), df = 1, lower.tail = FALSE)

anova(fit2, fit3, test = 'Chisq')

## note
## the deviance is calculated as
## 2 * (logLik(saturated model) - logLik(current model))
2*(sum(dpois(contamination_df$contamination, contamination_df$contamination, log = TRUE)) - logLik(fit3))
deviance(fit3)
- 2 * logLik(fit3)
## for the Bernoulli model, logLik(saturated model) = 0, therefore D = - 2 * logLik(current model)
## some packages use deviance = - 2 * logLik(current model) for all models
## although inconsistent, this doesn't change tests based on differences of deviances
## because the logLik of the saturated model is a constant that is cancelled out

## extra: example of a Poisson GLM analysis using an offset term
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