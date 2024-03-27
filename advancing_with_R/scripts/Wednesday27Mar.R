library(tidyverse)

# The Normal Model: A Recap -----------------------------------------------

## load the weight_glm dataset
weight_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/weight_glm.csv")

## exploratory plot
theme_set(theme_bw())

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(alpha = .2, size = .8) +
  geom_smooth(se = FALSE)

weight_df %>%
  ggplot(aes(x = age, y = weight)) +
  geom_point(alpha = .2, size = .8) +
  geom_smooth(se = FALSE)

## simple LR: model the weight as a function of height
## syntax: response ~ covariate1 + covariate2 + ...
## weight = beta0 + beta1 * height
fit1 <- lm(weight ~ height,
           data = weight_df)
summary(fit1)
summary(fit1)$coefficients

confint(fit1, parm = "height")
## with 95% confidence, we can say that every extra cm of height
## is associated to an increase of 1.11kg to 1.18kg in weight,
## on average

confint(fit1, parm = "height", level = .99)

## multiple LR: model weight as a function of height and age
fit2 <- lm(weight ~ height + age,
           data = weight_df)
summary(fit2)$coefficients
confint(fit2)
## with 95% confidence, for a fixed age, 1 extra cm of height
## is associated with an increase of between 1.10kg and 1.16kg
## on average


# The Bernoulli GLM -------------------------------------------------------

## load affairs dataset
affairs_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/affairs.csv")

## affairs = no. of affairs they had in the past year
## religiousness = self-rating of how religious they are (1 = not religious, 5 = very religious)
## rating = self-rating of how happy they are in the marriage (1 = very unhappy, 5 = very happy)

## creating the binary variable that will be analysed
## had_affair = TRUE / FALSE (yes / no)
affairs_df <- affairs_df %>%
  mutate(had_affair = affairs > 0)

## exploratory plot
affairs_df %>%
  ggplot(aes(x = yearsmarried, y = as.numeric(had_affair))) +
  geom_jitter(height = .1,
              alpha = .2) +
  geom_smooth(se = FALSE)

## natural log and Euler's constant e
log(c(1, 2, 4, 8, 16), base = 2)
log(c(1, 2, 4, 8, 16), base = 10)
log(c(1, 10, 100, 1000, 10000), base = 10)

exp(1) ## Euler's constant
print(exp(1), digits = 20)

log(exp(2))

log(c(1, 2, 4, 8, 16))

## probabilities vs. odds
p <- 0.75
odds <- p / (1 - p)

## logits = log-odds (I have p, want to get the logit)
logit_p <- log(odds)
log(p / (1 - p))
qlogis(p)

qlogis(.000000000001)
qlogis(0)
qlogis(1)
qlogis(.999999999999)

## inverse logits (I have a logit, want to get back p)
my_logit <- 0
exp(my_logit) / (1 + exp(my_logit))
1 / (1 + exp(- my_logit))
plogis(my_logit)

## qlogis(p) = calculates the logit of p = gives back a logit
## plogis(l) = calculates the inverse logit of l = gives back a probability

## fit the Bernoulli model to the had_affair variable
## as a function of number of years married
fit3 <- glm(had_affair ~ yearsmarried,
            family = binomial(link = "logit"),
            data = affairs_df)
summary(fit3)

estimates <- coef(fit3)

## what is the log-odds of having an affair for
## a person with 10 years of marriage?
estimates[1] + estimates[2] * 10

## what is the log-odds of having an affair for
## a person with 11 years of marriage?
estimates[1] + estimates[2] * 11

eta1 <- estimates[1] + estimates[2] * 10
eta2 <- estimates[1] + estimates[2] * 11

eta2 - eta1
estimates[2]

## odds-ratio for the yearsmarried variable
## the factor by which the odds increase for a unit change
## in yearsmarried
exp(estimates[2]) ## = 1.06
## interpretation:
## for every extra year of marriage, the odds of having
## an affair increase by 6%, on average

## 95% confidence interval for the odds ratio
## (for the predictor yearsmarried)
exp(confint.default(fit3, parm = "yearsmarried"))
## interpretation: with 95% confidence,
## for every extra year of marriage, the odds of having
## an affair increase by between 2.5% and 9.7%, on average

## predicted probabilities

## what is the estimated probability of having an affair for
## a person with 10 years of marriage?
plogis(estimates[1] + estimates[2] * 10)

## what is the estimated probability of having an affair for
## a person with 11 years of marriage?
plogis(estimates[1] + estimates[2] * 11)

## predict() function
predict(fit3, data.frame(yearsmarried = 10), type = "link")
predict(fit3, data.frame(yearsmarried = 10), type = "response")

## overlay estimated curve to the data
library(modelr)

affairs_df %>%
  add_predictions(model = fit3,
                  type = "response") %>%
  ggplot(aes(x = yearsmarried, y = as.numeric(had_affair))) +
  geom_jitter(height = .1,
              alpha = .2) +
  geom_line(aes(y = pred))

## set up a grid for predictions to obtain a smooth curve
newdata <- tibble(yearsmarried = seq(0, 15, length = 200))

add_predictions(data = newdata,
                model = fit3,
                type = "response") %>%
  ggplot(aes(x = yearsmarried, y = pred)) +
  geom_line() +
  geom_jitter(data = affairs_df,
              aes(y = as.numeric(had_affair)),
              height = .1,
              alpha = .2) +
  xlab("Number of years married") +
  ylab("Estimated probability of affair")

## deviances and likelihoods
logLik(fit3) ## log-likelihood of fit3
-2 * logLik(fit3)
deviance(fit3)

## the larger the (log-)likelihood is, the better our model is
## the smaller the deviance, the better our fit
## the deviance is a measure of discrepancy

## fit a reduced model
fit4 <- glm(had_affair ~ 1,
            family = binomial(link = "logit"),
            data = affairs_df)
coef(fit4) ## only an intercept

deviance(fit3)
deviance(fit4)

## difference between deviances
delta_D <- deviance(fit4) - deviance(fit3)

## null hypothesis: fit3 and fit4 are equivalent
## alternative: fit3 (full model) has a better fit (by including the extra term yearsmarried)

## under the null, delta_D ~ chi-squared with 1 d.f.
curve(dchisq(x, df = 1), xlim = c(0, 15))
abline(v = delta_D, lty = 2)

pchisq(delta_D, df = 1, lower.tail = FALSE) ## p-value
## we reject the null in favour of the alternative
## fit3 has better goodness-of-fit
## yearsmarried is an important predictor
## beta1 is not zero

anova(fit4, fit3, test = "Chisq")

## complete model
fit5 <- glm(had_affair ~ gender + age + yearsmarried + children +
              religiousness + education + factor(occupation) + rating,
            family = binomial,
            data = affairs_df)
summary(fit5)

## reduced model
fit6 <- glm(had_affair ~ age + yearsmarried + religiousness + rating,
            family = binomial,
            data = affairs_df)

anova(fit6, fit5, test = "Chisq")
curve(dchisq(x, df = 9), xlim = c(0, 30))
abline(v = 12.005, lty = 2)
## no (weak) evidence against the null hypothesis
## therefore the reduced model can be used
## the variables gender, children, education and occupation can be dropped
## as they (in combination) do not improve the fit of the model significantly

## this is called a "global" test
## we can do "local", or single-variable tests too

## a series of tests dropping one predictor at a time
drop1(fit5, test = "Chisq")


# Binomial GLM ------------------------------------------------------------

# install.packages("hnp")

## load the citrys psyllid mortality dataset
library(hnp)
?fungi

data(fungi)
fungi

glimpse(fungi)
fungi$species

fungi$species <- relevel(fungi$species, "beauveria")

## exploratory plot
fungi %>%
  ggplot(aes(x = lconc, y = y / m,
             col = species)) +
  geom_point()

## fit the binomial GLM to this data
## response is a two-culumn matrix
## first column = number of successes
## second column = number of failures
fit7 <- glm(cbind(y, m - y) ~ lconc,
            family = binomial(link = "logit"),
            data = fungi)
summary(fit7)

estimates <- coef(fit7)
exp(estimates[2]) ## odds-ratio
## for every unit increase in the log(concentration)
## we have a 2.4-fold increase in the odds of an insect
## dying, on average

exp(confint.default(fit7, parm = "lconc"))

## check whether inclusion of lconc is significant
drop1(fit7, test = "Chisq")

## let's also include species in the model
fit8 <- glm(cbind(y, m - y) ~ lconc + species,
            family = binomial(link = "logit"),
            data = fungi)
estimates <- coef(fit8)

drop1(fit8, test = "Chisq")

## what is the log-odds of an insect dying when exposed
## to log10(concentration) of 5 using beauveria?
estimates[1] + estimates[2] * 5

## what is the estimated probability of an insect dying when exposed
## to log10(concentration) of 5 using beauveria?
plogis(estimates[1] + estimates[2] * 5)

## how many insects would die, on average, out of a total of
## 20 insects, when exposed to beauveria at a
## log10(concentration) of 5?
20 * plogis(estimates[1] + estimates[2] * 5)

## we can look at the full estimated distribution
plot(0:20,
     dbinom(0:20, size = 20, prob = .21),
     type = "h")

## probability that 0/20 die
dbinom(0, size = 20, prob = .21)

## odds-ratio for an increase in one unit in
## log10(concentration), keeping species fixed
exp(estimates[2])
## a 270% increase (a 2.7-fold increase) in the odds
## of dying for every extra unit of lconc

## what about the odds-ratio of an insect dying
## if we use isaria compared to using beauveria
exp(estimates[3])
## using isaria yields a 4.1-fold increase in mortality
## of the pest compared to beauveria

exp(confint.default(fit8, parm = "speciesisaria"))
## with 95% confidence, the odds of the insect dying
## when exposed to isaria is 2.5 to 6.7 times the
## odds of dying when exposed to beauveria

## predicted curves
newdata <- expand.grid(lconc = seq(4, 8, length = 200),
                       species = c("isaria","beauveria"))

add_predictions(data = newdata,
                model = fit8,
                type = "response") %>%
  ggplot(aes(x = lconc, y = pred, col = species)) +
  geom_line() +
  geom_point(data = fungi,
             aes(y = y / m))

## "lethal dose" (LD)
## LD50: the dose that kills 50% of the insects
## LD95: the dose that kills 95% of the insects
dose.p(fit7)
dose.p(fit7, p = .95)

## comparing link functions

## load the golf dataset
golf_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/golf_putts.csv")

## exploratory plot
golf_df %>%
  ggplot(aes(x = distance, y = success / attempts)) +
  geom_point()

## fit the binomial model with the 3 different links
fit9 <- glm(cbind(success, attempts - success) ~ poly(distance, 2),
            family = binomial(link = "logit"),
            data = golf_df)

fit10 <- glm(cbind(success, attempts - success) ~ poly(distance, 2),
             family = binomial(link = "probit"),
             data = golf_df)

fit11 <- glm(cbind(success, attempts - success) ~ poly(distance, 2),
             family = binomial(link = "cloglog"),
             data = golf_df)

newdata <- tibble(distance = seq(2, 20, length = 200))

add_predictions(data = newdata,
                model = fit9,
                type = "response",
                var = "logit") %>%
  add_predictions(model = fit10,
                  type = "response",
                  var = "probit") %>%
  add_predictions(model = fit11,
                  type = "response",
                  var = "cloglog") %>%
  ggplot(aes(x = distance)) +
  geom_point(data = golf_df,
             aes(y = success / attempts)) +
  geom_line(aes(y = logit), col = "red") +
  geom_line(aes(y = probit), col = "blue") +
  geom_line(aes(y = cloglog), col = "darkgreen")

## these three models are NOT nested
## therefore: we cannot do a LRT (or difference of deviances)
## however, we can use information criteria

## Akaike information criterion (AIC)
## AIC = 2*no. parameters + Deviance
## smaller = better
AIC(fit9)  # logit
AIC(fit10) # probit
AIC(fit11) # cloglog

## model averaging based on Akaike weights
source("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/scripts/helper_functions.R")

my_aics <- c(AIC(fit9), AIC(fit10), AIC(fit11))
round(akaike_weights(my_aics), 10) ## rounding to 10 decimal places
my_weights <- akaike_weights(my_aics)

golf_pred <-
  add_predictions(data = newdata,
                model = fit9,
                type = "response",
                var = "logit") %>%
  add_predictions(model = fit10,
                  type = "response",
                  var = "probit") %>%
  add_predictions(model = fit11,
                  type = "response",
                  var = "cloglog") %>%
  mutate(weighted_logit = logit * my_weights[1],
         weighted_probit = probit * my_weights[2],
         weighted_cloglog = cloglog * my_weights[3],
         avg_pred = weighted_logit +
                    weighted_probit +
                    weighted_cloglog)

BIC(fit9)
BIC(fit10)
BIC(fit11)


# Multinomial GLMs --------------------------------------------------------

## loading the grazing dataset
grazing_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/grazing.csv")

## fit the multinomial model
library(nnet)
fit12 <- multinom(resp ~ months,
                  data = grazing_df)

logLik(fit12) ## objective function reported at convergence

summary(fit12)

coef(fit12)

## first logit: log(pi2 / pi1) =   0.5475287 - 0.01535834 * month
## secnd logit: log(pi3 / pi1) = - 1.2238692 - 0.04701409 * month

predict(fit12, data.frame(months = 3), type = "class")
predict(fit12, data.frame(months = 3), type = "probs")

## getting predictions over time
newdata <- tibble(months = 3:16)
add_predictions(data = newdata,
                model = fit12,
                type = "prob")$pred

newdata <- tibble(months = seq(3, 16, length = 200))

pred_data <- 
  add_predictions(data = newdata,
                model = fit12,
                type = "prob")$pred %>%
  as_tibble %>%
  mutate(months = seq(3, 16, length = 200))

pred_data %>%
  pivot_longer(1:3,
               names_to = "category",
               values_to = "prob") %>%
  ggplot(aes(x = months, y = prob, col = category)) +
  geom_line() +
  ylim(0, 1)

## significance of the `months` covariate
## full model
fit12 <- multinom(resp ~ months,
                  data = grazing_df)
## reduced model
fit13 <- multinom(resp ~ 1,
                  data = grazing_df)

## likelihood-ratio test
anova(fit12, fit13, test = "Chisq")


# Poisson GLMs ------------------------------------------------------------

## load the GP visits dataset
doctor_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/doctor_visits.csv")
doctor_df$sex <- factor(doctor_df$sex, labels = c("male", "female"))
doctor_df$age <- doctor_df$age * 100

## exploratory plot
doctor_df %>%
  ggplot(aes(x = age, y = gp_visits)) +
  geom_jitter(width = .75,
              height = .2,
              alpha = .1) +
  facet_wrap(~ sex) +
  geom_smooth(se = FALSE)

## fit Poisson GLMs
fit14 <- glm(gp_visits ~ sex + age,
             family = poisson,
             data = doctor_df)
summary(fit14)
estimates <- coef(fit14)

## what is the rate (aka the mean) of the Poisson distribution
## for a female aged 50?
log_rate_female_50 <- estimates[1] + estimates[2] + estimates[3] * 50
##                    intercept + eff. of female + slope * 50

## the rate is
rate_female_50 <- exp(log_rate_female_50)

dpois(0:5, lambda = rate_female_50) %>%
  round(3)

library(magrittr)

dpois(0:5, lambda = rate_female_50) %>%
  round(3) %>%
  multiply_by(1000)

rpois(1000, lambda = rate_female_50) %>%
  table

## what is the rate (mean) for a male aged 50?
log_rate_male_50 <- estimates[1] + estimates[3] * 50
rate_male_50 <- exp(log_rate_male_50)

dpois(0:5, lambda = rate_male_50) %>%
  round(3) %>%
  multiply_by(1000)

plot(0:5,
     dpois(0:5, lambda = rate_male_50),
     type = "h")
