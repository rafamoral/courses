library(tidyverse)

# The Multinomial GLM -----------------------------------------------------

## loading the grazing dataset
grazing_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/grazing.csv")
grazing_df

library(nnet)

fit1 <- multinom(resp ~ months,
                 data = grazing_df)

logLik(fit1) ## objective function reported before "convergence"
- 2 * logLik(fit1) ## deviance reported

summary(fit1)
estimates <- coef(fit1)

## logit 2: log(pi2 / pi1) =   0.5475287 - 0.01535834 * month = eta2
## logit 3: log(pi3 / pi1) = - 1.2238692 - 0.04701409 * month = eta3

## for month 3
denominator <- 1 + exp(0.5475287 - 0.01535834 * 3) + exp(- 1.2238692 - 0.04701409 * 3)
pi1 <- 1 / denominator
pi2 <- pi1 * exp(0.5475287 - 0.01535834 * 3)
pi3 <- pi1 * exp(- 1.2238692 - 0.04701409 * 3)
pi1 + pi2 + pi3

predict(fit1, data.frame(months = 3), type = "prob")
c(pi1, pi2, pi3)

## getting the predictions over time
library(modelr)

newdata <- tibble(months = 3:16)
add_predictions(data = newdata, model = fit1, type = "prob")

newdata <- tibble(months = seq(3, 16, length = 200))
newdata <- tibble(newdata, add_predictions(data = newdata, model = fit1, type = "prob")$pred %>%
  as.data.frame)
names(newdata)

newdata %>%
  pivot_longer(2:4,
               names_to = "category",
               values_to = "prob") %>%
  ggplot(aes(x = months, y = prob, colour = category)) +
  theme_bw() +
  geom_line() +
  ylim(0, 1)

## assessing the effect of the months covariate
## full model
fit1 <- multinom(resp ~ months,
                 data = grazing_df)
## reduced model
fit2 <- multinom(resp ~ 1,
                 data = grazing_df)

## comparing using anova()
anova(fit1, fit2)

# The Poisson GLM ---------------------------------------------------------

## loading the number of GP visits dataset
doctor_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/doctor_visits.csv")
doctor_df$sex <- factor(doctor_df$sex, labels = c("male", "female"))
doctor_df$age <- doctor_df$age * 100

## exploratory plot
doctor_df %>%
  ggplot(aes(x = age, y = gp_visits)) +
  theme_bw() +
  geom_jitter(width = .5, height = .2, alpha = .1) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ sex, nrow = 2) +
  ylab("Number of GP visits") +
  xlab("Age")

## fitting Poisson models
fit3 <- glm(gp_visits ~ sex + age,
            family = poisson,
            data = doctor_df)
summary(fit3)
estimates <- coef(fit3)

## What is the rate (aka the mean) of the Poisson distribution
## for a female aged 50?
log_rate_female_50 <- estimates[1] + estimates[2] + estimates[3] * 50
##             intercept + effect of female on intercept + slope * 50

## the rate for a female aged 50 is
rate_female_50 <- exp(log_rate_female_50)

## draw samples from the Poisson distribution whose rate is rate_female_50
rpois(1000, lambda = rate_female_50)
table(rpois(1000, lambda = rate_female_50))
round(dpois(0:5, lambda = rate_female_50), 3)

## What is the rate (aka the mean) of the Poisson distribution
## for a *male* aged 50?
log_rate_male_50 <- estimates[1] + estimates[3] * 50

## the rate for a *male* aged 50 is
rate_male_50 <- exp(log_rate_male_50)

## draw samples from the Poisson distribution whose rate is rate_male_50
table(rpois(1000, lambda = rate_male_50))
round(dpois(0:5, lambda = rate_male_50), 3)

log_rate_female_50 - log_rate_male_50

rate_female_50 / rate_male_50
exp(estimates[2])
## females aged 50 have a 30% increase on average of GP visits when
## compared to males aged 50

## effects of age
exp(estimates[3])
## for every extra year of life, the mean number of GP visits
## increases by ~ 1.4%

## 95% CI on the factor by which the rate changes as we change
## from men to women
exp(confint.default(fit3, parm = "sexfemale"))

## predicted curves
newdata <- expand.grid(age = seq(19, 72, length = 200),
                       sex = c("male", "female"))

add_predictions(data = newdata,
                model = fit3,
                type = "response",
                var = "gp_visits") %>%
  ggplot(aes(x = age, y = gp_visits)) +
  theme_bw() +
  geom_jitter(data = doctor_df,
              width = .5, height = .2, alpha = .1) +
  geom_line(col = 2) +
  facet_wrap(~ sex, nrow = 2) +
  ylab("Number of GP visits") +
  xlab("Age") +
  ylim(0, 9)

add_predictions(data = newdata,
                model = fit3,
                type = "response",
                var = "gp_visits") %>%
  ggplot(aes(x = age, y = gp_visits, colour = sex)) +
  theme_bw() +
  geom_line() +
  ylab("Number of GP visits") +
  xlab("Age") +
  ylim(0, 9)

## model comparison
drop1(fit3, test = "Chisq")

## let's include more predictors
fit4 <- glm(gp_visits ~ sex + age + insurance,
            family = poisson,
            data = doctor_df)
deviance(fit3)
deviance(fit4)

delta_D <- deviance(fit3) - deviance(fit4)
pchisq(delta_D, df = 3, lower.tail = FALSE)

anova(fit3, fit4, test = "Chisq")
