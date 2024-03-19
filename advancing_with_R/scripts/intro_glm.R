# part 1

library(tidyverse)

## The Normal Model: A Recap ---------------------------------------------------

## loading the weight dataset
weight_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/weight_glm.csv")
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

################################################################################
# part 2

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

log_rate_female_50 - log_rate_male_50 ## same as estimates[2]

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

################################################################################
# part 3

library(tidyverse)

## Overdispersion --------------------------------------------------------------

## Mean-variance relationship

pois_meanvar <- function(mu) mu
qpois_meanvar <- function(mu, phi = 2) phi * mu
negbin_meanvar <- function(mu, theta = 1 / 2) mu + mu^2 / theta

bin_meanvar <- function(mu, m = 10) mu / m * (m - mu)
qbin_meanvar <- function(mu, m = 10, phi = 2) phi * mu / m * (m - mu)
betabin_meanvar <- function(mu, m = 10, phi = 2) mu / m * (m - mu) * (1 + (m - 1) * phi)

x <- tibble(Mean = seq(0, 10, length = 200),
            Poisson = pois_meanvar(Mean),
            `Quasi-Poisson` = qpois_meanvar(Mean),
            NegBin = negbin_meanvar(Mean),
            Binomial = bin_meanvar(Mean),
            `Quasi-binomial` = qbin_meanvar(Mean),
            `Beta-binomial` = betabin_meanvar(Mean))

x %>%
  pivot_longer(c(2,5),
               names_to = "Model",
               values_to = "Variance") %>%
  ggplot(aes(x = Mean, y = Variance, col = Model)) +
  theme_bw() +
  geom_line()

## Poisson mean-variance relationship
sim_counts <- rpois(100, lambda = 5)
mean(sim_counts); var(sim_counts)
var(sim_counts) / mean(sim_counts)

sim_counts <- rpois(1000, lambda = 5)
mean(sim_counts); var(sim_counts)
var(sim_counts) / mean(sim_counts)

sim_counts <- rpois(1000, lambda = 10)
mean(sim_counts); var(sim_counts)
var(sim_counts) / mean(sim_counts)

## Binomial mean-variance relationship
sim_bin <- rbinom(1000, size = 10, prob = 0.2)
mean(sim_bin); var(sim_bin)
var(sim_bin) / mean(sim_bin)
1 - 0.2

## Example: Maize weevil progeny dataset
library(hnp)
data(progeny)

progeny %>%
  group_by(extract) %>%
  summarise(mean = mean(y),
            var = var(y))

progeny %>%
  group_by(extract) %>%
  summarise(mean = mean(y),
            var = var(y)) %>%
  ggplot(aes(x = mean, y = var)) +
  theme_bw() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2)

fit1 <- glm(y ~ extract,
            family = poisson,
            data = progeny)
summary(fit1)

anova(fit1, test = "Chisq")

pchisq(deviance(fit1), fit1$df.residual, lower.tail = FALSE)

hnp(fit1)

## Quasi-Poisson model

fit2 <- glm(y ~ extract,
            family = quasipoisson,
            data = progeny)
summary(fit2)
summary(fit2)$dispersion

sum(((progeny$y - fitted(fit1))^2 / fitted(fit1))) / fit1$df.residual
sum(residuals(fit1, type = "pearson")^2) / fit1$df.residual

anova(fit2, test = "F")
(444.68 / 3) / summary(fit2)$dispersion

hnp(fit2)

## Negative binomial model

p1 <- x %>%
  pivot_longer(2:4,
               names_to = "Model",
               values_to = "Variance") %>%
  ggplot(aes(x = Mean, y = Variance, col = Model)) +
  theme_bw() +
  geom_line()

p2 <- x %>%
  pivot_longer(2:4,
               names_to = "Model",
               values_to = "Variance") %>%
  ggplot(aes(x = Mean, y = Variance, col = Model)) +
  theme_bw() +
  geom_line() +
  xlim(0, 1) +
  ylim(0, 3)

library(ggpubr)
ggarrange(p1, p2, ncol = 2, common.legend = TRUE)

## Example: effects of agricultural oils on D. citri oviposition
data(oil, package = "hnp")

oil_meanvar <- oil %>%
  group_by(treat) %>%
  summarise(mean = mean(y),
            var = var(y))

oil_meanvar %>%
  ggplot(aes(x = mean, y = var)) +
  theme_bw() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2)

mv1 <- lm(var ~ 0 + mean,
          data = oil_meanvar)
mv2 <- lm(var ~ 0 + offset(mean) + I(mean^2),
          data = oil_meanvar)

library(modelr)
newdata <- tibble(mean = seq(5.7, 76.3, length = 200))

newdata <- add_predictions(data = newdata, model = mv1, var = "quasi")
newdata <- add_predictions(data = newdata, model = mv2, var = "negbin")

oil_meanvar %>%
  ggplot(aes(x = mean, y = var)) +
  theme_bw() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_line(data = newdata %>%
              pivot_longer(2:3,
                           names_to = "model",
                           values_to = "var"),
            aes(col = model))

fit3 <- glm(y ~ treat,
            family = poisson,
            data = oil)
summary(fit3)
anova(fit3, test = "Chisq")
hnp(fit3)

fit4 <- glm(y ~ treat,
            family = quasipoisson,
            data = oil)
summary(fit4)
anova(fit4, test = "F")
hnp(fit4)

fit5 <- glm.nb(y ~ treat,
               data = oil)
summary(fit5)
anova(fit5, test = "F")
hnp(fit5)

## Example: Biochemists dataset
biochem_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/biochemist.csv")
biochem_df

library(pscl)
?bioChemists

biochem_df %>%
  mutate(prestige_binned = cut(prestige, breaks = c(0,2,4,6)),
         mentor_binned = cut(mentor, breaks = c(-1,20,40,80))) %>%
  group_by(gender, married, prestige_binned, mentor_binned) %>%
  summarise(mean = mean(publications),
            var = var(publications)) %>%
  ggplot(aes(x = mean, y = var)) +
  theme_bw() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2)

fit6 <- glm(publications ~ gender + married + children + prestige + mentor,
            family = poisson,
            data = biochem_df)
summary(fit6)
drop1(fit6, test = "Chisq")
hnp(fit6)

fit7 <- glm(publications ~ gender + married + children + prestige + mentor,
            family = quasipoisson,
            data = biochem_df)
summary(fit7)
drop1(fit7, test = "F")
hnp(fit7, paint = TRUE)

fit8 <- glm.nb(publications ~ gender + married + children + prestige + mentor,
               data = biochem_df)
summary(fit8)
drop1(fit8, test = "F")
hnp(fit8, paint = TRUE)

estimates <- coef(fit8)

## predicted log of the mean of the negbin for values of prestige from 1 to 5
estimates[1] + estimates[5] * seq(5)

## predicted mean of the negbin for values of prestige from 1 to 5
exp(estimates[1] + estimates[5] * seq(5))

# the factor by which the mean increases for every unit change of the predictor
exp(estimates[5])

## draw samples from a negative binomial
rnegbin(10000, mu = 1.62, theta = 2.26) %>% var()
1.62 + 1.62^2 / 2.26

tibble(x = rnegbin(10000, mu = 1.62, theta = 2.26)) %>%
  ggplot(aes(x = x)) +
  theme_bw() +
  geom_bar()

## mean-variance relationship: models for discrete proportions
x %>%
  pivot_longer(5:7,
               names_to = "Model",
               values_to = "Variance") %>%
  ggplot(aes(x = Mean, y = Variance, col = Model)) +
  theme_bw() +
  geom_line()

## Example revisited: Diaphorina citri mortality data
data(fungi)

fungi$species <- relevel(fungi$species, "beauveria")

fungi %>%
  ggplot(aes(x = lconc, y = y/20, colour = species)) +
  theme_bw() +
  geom_point() +
  ylab("proportion of dead insects") +
  xlab("log10(concentration)")

fit9 <- glm(cbind(y, 20 - y) ~ lconc + species,
            family = binomial,
            data = fungi)
summary(fit9)
drop1(fit9, test = "Chisq")
hnp(fit9)

fit10 <- glm(cbind(y, 20 - y) ~ lconc + species,
             family = quasibinomial,
             data = fungi)
summary(fit10)
drop1(fit10, test = "F")
hnp(fit10)

## mean mortality probability when using beauveria at a log concentration of 5
pi_beauveria <- plogis(- 4.503 + 0.609 * 5)

## mean mortality probability when using isaria at a log concentration of 5
pi_isaria <- plogis(- 4.503 + 1.037 + 0.609 * 5)

library(gamlss)
fit11 <- gamlss(cbind(y, 20 - y) ~ lconc + species,
                family = BB,
                data = fungi)
summary(fit11)
hnp(fit11)

pi_beauveria_bb <- plogis(- 4.5930 + 0.6226 * 5)
pi_isaria_bb <- plogis(- 4.5930 + 1.0795 + 0.6226 * 5)

## estimated distribution based on the binomial model
p1 <- tibble(x = 0:20,
             beauveria = dbinom(x, size = 20, prob = pi_beauveria),
             isaria = dbinom(x, size = 20, prob = pi_isaria)) %>%
  pivot_longer(2:3,
               names_to = "species",
               values_to = "P(x)") %>%
  ggplot(aes(x = x, y = `P(x)`, fill = species)) +
  theme_bw() +
  geom_bar(stat = "identity",
           position = "dodge") +
  xlab("Number of dead out of 20") +
  ylim(0, 0.23)

## estimated distribution based on the beta-binomial model
p2 <- tibble(x = 0:20,
             beauveria = dBB(x, bd = 20, mu = pi_beauveria_bb, sigma = exp(-2.595)),
             isaria = dBB(x, bd = 20, mu = pi_isaria_bb, sigma = exp(-2.595))) %>%
  pivot_longer(2:3,
               names_to = "species",
               values_to = "P(x)") %>%
  ggplot(aes(x = x, y = `P(x)`, fill = species)) +
  theme_bw() +
  geom_bar(stat = "identity",
           position = "dodge") +
  xlab("Number of dead out of 20") +
  ylim(0, 0.23)

ggarrange(p1, p2, ncol = 1, common.legend = TRUE)

## Zero-Inflation --------------------------------------------------------------

## ZIP distribution
tibble(x = 0:15,
       `P(x)` = dZIP(x, mu = 5, sigma = 0.2)) %>%
  ggplot(aes(x = x, y = `P(x)`)) +
  theme_bw() +
  geom_bar(stat = "identity")

## Example: hunting spider abundance
spider_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/spider.csv")
spider_df

## exploratory plot
spider_df %>%
  ggplot(aes(x = soil_dry_mass, y = count)) +
  theme_bw() +
  geom_point()

## ZIP model
library(pscl)
fit12 <- zeroinfl(count ~ soil_dry_mass,
                  dist = "poisson",
                  data = spider_df)
summary(fit12)

estimates <- coef(fit12)

## what is the log odds that the latent variable takes the value of 1, if soil_dry_mass = 3?
estimates[3] + estimates[4] * 3

## what is the probability that the latent variable takes the value of 1, if soil_dry_mass = 3?
## what is the probability there are no hunting spiders in environments with soil_dry_mass = 3?
plogis(estimates[3] + estimates[4] * 3)

## what is the probability that there are no hunting spiders in environments
## with soil_dry_mass = 1, 2, 3, 4?
plogis(estimates[3] + estimates[4] * 1:4)

## what is the log of the mean abundance of hunting spiders in environments
## with soil_dry_mass = 3?
estimates[1] + estimates[2] * 3

## what is the mean abundance of hunting spiders in environments
## with soil_dry_mass = 3?
exp(estimates[1] + estimates[2] * 3)

## what is the mean abundance of hunting spiders in environments
## with soil_dry_mass = 1, 2, 3, 4?
exp(estimates[1] + estimates[2] * 1:4)

spider_pred <- tibble(soil_dry_mass = 1:4)
spider_pred <- add_predictions(data = spider_pred, fit12, type = "response", var = "response")
spider_pred <- add_predictions(data = spider_pred, fit12, type = "zero", var = "zero")
spider_pred <- add_predictions(data = spider_pred, fit12, type = "count", var = "count")
spider_pred

spider_pred$response
(1 - spider_pred$zero) * spider_pred$count

## fitted curve
newdata <- tibble(soil_dry_mass = seq(1, 3.5, length = 200))
newdata <- add_predictions(data = newdata, model = fit12, type = "response", var = "count")

spider_df %>%
  ggplot(aes(x = soil_dry_mass, y = count)) +
  theme_bw() +
  geom_point() +
  geom_line(data = newdata)

## goodness-of-fit
hnp(fit12)

## ZINB distribution
tibble(x = 0:15,
       ZIP = dZIP(x, mu = 5, sigma = 0.2),
       ZINB = dZINBI(x, mu = 5, sigma = 1, nu = 0.2)) %>%
  pivot_longer(2:3,
               names_to = "model",
               values_to = "P(x)") %>%
  ggplot(aes(x = x, y = `P(x)`, fill = model)) +
  theme_bw() +
  geom_bar(stat = "identity",
           position = "dodge")

## Fitting ZINB model
fit13 <- zeroinfl(count ~ soil_dry_mass,
                  dist = "negbin",
                  data = spider_df)
summary(fit13)
hnp(fit13)

estimates_zip <- estimates ## current estimates object
estimates_zinb <- coef(fit13)

mu_zip <- exp(estimates_zip[1] + estimates_zip[2] * 3)
omega_zip <- plogis(estimates_zip[3] + estimates_zip[4] * 3)

mu_zinb <- exp(estimates_zinb[1] + estimates_zinb[2] * 3)
omega_zinb <- plogis(estimates_zinb[3] + estimates_zinb[4] * 3)
theta_zinb <- fit13$theta

## fitted distribution of spider abundance when soil_dry_mass = 3
tibble(x = 0:25,
       ZIP = dZIP(x, mu = mu_zip, sigma = omega_zip),
       ZINB = dZINBI(x, mu = mu_zinb, sigma = theta_zinb, nu = omega_zinb)) %>%
  pivot_longer(2:3,
               names_to = "model",
               values_to = "P(x)") %>%
  ggplot(aes(x = x, y = `P(x)`, fill = model)) +
  theme_bw() +
  geom_bar(stat = "identity",
           position = "dodge") +
  xlab("Hunting spider abundance")