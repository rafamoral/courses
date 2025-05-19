library(tidyverse)

# A recap on the normal model ---------------------------------------------

## let's load the weight dataset
weight_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_glm/data/weight.csv")
weight_df

## pipes
# "%>%" is a pipe from the magrittr package (pronounced with a sophisticated French accent, as per René Magritte)
# "|>" this is the native R pipe

sum(log(sqrt(1:10)))

(1:10) %>% sqrt %>% log %>% sum
## the pipe is read as "and then"
## the pipe is used for readability purposes
## the native R pipe needs parentheses, the magrittr one doesn't

x <- 1:10

x %>%
  sum %>%
  log %>%
  sqrt

x |>
  sum() |>
  log() |>
  sqrt()

ggplot(data = weight_df, aes(x = height, y = weight)) +
  theme_bw() +
  geom_point(alpha = .2) +
  geom_smooth(se = FALSE)

## do some exploratory plots
weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  theme_bw() +
  geom_point(alpha = .2) +
  geom_smooth(se = FALSE)

weight_df %>%
  ggplot(aes(x = age, y = weight)) +
  theme_bw() +
  geom_point(alpha = .2) +
  geom_smooth(se = FALSE)

## let's model weight as a function of height
fit1 <- lm(weight ~ height,
           data = weight_df)
summary(fit1)
summary(fit1)$coefficients

## do inference about the model parameters (here the slope is what we are interested in)
confint(fit1, parm = "height")

## we could fit a multiple regression model as well
fit2 <- lm(weight ~ height + age,
           data = weight_df)
summary(fit2)$coefficients
## interpretation: for a 1-cm increase in height, we expect an increase of
##                 1.1259618 kg, on average, for a fixed age
##                 for a 1-year increase in age, we expect an increase of
##                 368 grams, on average, for a fixed height

confint(fit2)


# The Bernoulli GLM -------------------------------------------------------

## loading the affairs dataset
affairs_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_glm/data/affairs.csv")
affairs_df

## let's convert the response (affairs) to a binary variable
## "have you had an affair in the past year: yes/no"

affairs_df <- affairs_df %>%
  mutate(had_affair = affairs > 0)

## let's estimate the probability of having an affair as a function
## of how long the person was married (the yearsmarried variable)
affairs_df %>%
  ggplot(aes(x = yearsmarried, y = as.numeric(had_affair))) +
  theme_bw() +
  geom_jitter(width = .2, height = .1, alpha = .2) +
  geom_smooth(se = FALSE)

## natural logs and Euler's constant
exp(1) ## Euler's constant
?log
log(c(1,2,4,8,16), base = 2)
log(c(1,2,4,8,16)) ## base e
log(exp(10))

## log-odds and logits
p <- 0.75
odds <- p / (1 - p)
logit_p <- log(odds)
log(p / (1 - p)) ## logit function
qlogis(p)

?qlogis ## this is the inverse of the cdf of the logistic distribution (the "quantile" function)

## inverse logits
my_logit <- qlogis(p)
my_invlogit <- exp(my_logit) / (1 + exp(my_logit))
my_invlogit2 <- 1 / (1 + exp(- my_logit))
plogis(my_logit)

?plogis ## this is the cdf of the logistic distribution
plogis(0)
qlogis(0.5)

## fitting a Bernoulli GLM (Binomial GLM / logistic regression = because of logit link)
## the model is: Y_i ~ Bernoulli(pi_i)
##               logit(pi_i) = beta0 + beta1 * yearsmarried_i
fit3 <- glm(had_affair ~ yearsmarried,
            family = binomial(link = "logit"),
            data = affairs_df)
summary(fit3)$coefficients

estimates <- coef(fit3)

## what is the log-odds of having an affair for a person with 10 years of marriage?
estimates[1] + estimates[2] * 10

## what is the log-odds of having an affair for a person with 11 years of marriage?
estimates[1] + estimates[2] * 11

## odds ratio for the yearsmarried predictor
## the factor by which the odds increases for a unit change in yearsmarried
logodds10 <- estimates[1] + estimates[2] * 10
odds10 <- exp(logodds10)

logodds11 <- estimates[1] + estimates[2] * 11
odds11 <- exp(logodds11)

odds_ratio <- odds11 / odds10

exp(estimates[2]) ## exp(slope) = odds-ratio
exp(confint.default(fit3, parm = "yearsmarried"))
## with 95% confidence, for a 1-year increase in yearsmarried,
## the odds of having an affair increase from between 2.5% and 9.7%,
## on average

## let's have a look at the rating predictor
fit4 <- glm(had_affair ~ relevel(factor(rating), "5") + yearsmarried,
            family = binomial,
            data = affairs_df)
summary(fit4)

## let's do prediction (calculate the predicted pi's)

## what is the probability of having an affair for a person with 10 years of marriage?
plogis(estimates[1] + estimates[2] * 10)

## what is the probability of having an affair for a person with 11 years of marriage?
plogis(estimates[1] + estimates[2] * 11)

library(modelr)
newdata <- tibble(yearsmarried = seq(0.125, 15, length = 200))  

add_predictions(data = newdata, model = fit3) # by default, predictions are at the linear predictor scale
add_predictions(data = newdata, model = fit3, type = "response") # type = "response" gives predictions at the probability scale

add_predictions(data = newdata, model = fit3, type = "response") %>%
  ggplot(aes(x = yearsmarried, y = pred)) +
  theme_bw() +
  geom_line() +
  geom_jitter(data = affairs_df,
              aes(y = as.numeric(had_affair)),
              width = .2, height = .1, alpha = .2) +
  scale_y_continuous(breaks = seq(0, 1, by = .2)) +
  ylab("Predicted probabilities of having an affair") +
  xlab("Number of years married")

## deviances
summary(fit3)
deviance(fit3)
- 2 * logLik(fit3) ## this works for Bernoulli

## differences of deviances
fit_full <- glm(had_affair ~ yearsmarried + factor(rating),
                family = binomial,
                data = affairs_df)
fit_reduced <- glm(had_affair ~ yearsmarried,
                   family = binomial,
                   data = affairs_df)
fit_null <- glm(had_affair ~ 1,
                family = binomial,
                data = affairs_df)

deviance(fit_full)
deviance(fit_reduced)
deviance(fit_null)

delta_D <- deviance(fit_null) - deviance(fit_reduced)
pchisq(delta_D, df = 1, lower.tail = FALSE)

curve(dchisq(x, 1), xlim = c(0, 20))
abline(v = delta_D, lty = 2, col = 2)

anova(fit_null, fit_reduced, test = "Chisq")

AIC(fit_null)
AIC(fit_reduced)

## let's do a "drop1" type of analysis
fit5 <- glm(had_affair ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating,
            family = binomial,
            data = affairs_df)
summary(fit5)

anova(fit5) ## sequential analysis of deviance
drop1(fit5, test = "Chisq")

fit6 <- glm(had_affair ~ age + yearsmarried + religiousness + rating,
            family = binomial,
            data = affairs_df)
summary(fit6)

## a global test
anova(fit6, fit5)

drop1(fit6, test = "Chisq")


# Binomial GLMs -----------------------------------------------------------

library(hnp)
data(fungi)

## m = total number of insects exposed to the fungus
## y = number of insects that died

fungi %>%
  ggplot(aes(x = lconc, y = y / m, color = species)) +
  theme_bw() +
  geom_point()

## let's fit a binomial GLM to the fungi data
## for the binomial GLM we need the response to be a matrix
## first column = number of successes
## second column = number of failures
fit7 <- glm(cbind(y, m - y) ~ lconc + species,
            family = binomial,
            data = fungi)
summary(fit7)

exp(1.0006)
## for a 10-fold increase in concentration, the odds of the insect dying increases
## by a factor of 2.7, on average, for a fixed fungal species

exp(-1.4168)
## when using the fungal species beauveria, the odds of the insect dying reduces
## by ~ 75%, on average, for a fixed concentration of conidia

drop1(fit7, test = "Chisq")

## what is the log-odds of the insect dying when using a log10(concentration) of 5
## and the fungal species B. bassiana?
estimates <- coef(fit7)
estimates[1] + estimates[2] * 5 + estimates[3]

## what is the probability of the insect dying when using a log10(concentration) of 5
## and the fungal species B. bassiana?
plogis(estimates[1] + estimates[2] * 5 + estimates[3])

## how many insects would be expected to die, out of a batch of 20, on average,
## when using a log10(concentration) of 5 and the fungal species B. bassiana?
plogis(estimates[1] + estimates[2] * 5 + estimates[3]) * 20

## let's generate some predictions
newdata <- expand.grid(lconc = seq(4, 8, length = 200),
                       species = c("beauveria","isaria"))

newdata %>%
  add_predictions(model = fit7, type = "response") %>%
  ggplot(aes(x = lconc, y = pred, color = species)) +
  theme_bw() +
  geom_line() +
  geom_point(data = fungi,
             aes(y = y / m)) +
  xlab(expression(log[10](concentration))) +
  ylab(expression(hat(pi)))

## check ?plotmath for more

## lethal doses: the dose at which X% of insects die
library(MASS)
dose.p(fit7, p = .95) # for isaria

fit7_alt <- glm(cbind(y, m - y) ~ lconc + relevel(species, "beauveria"),
                family = binomial,
                data = fungi)
dose.p(fit7_alt, p = .95) # for beauveria
## but be careful because here we are extrapolating!
## better to look at other percentages

dose.p(fit7)
dose.p(fit7_alt)

## alternative link functions
## loading the golf dataset
golf_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_glm/data/golf_putts.csv")
golf_df

golf_df %>%
  ggplot(aes(x = distance, y = success / attempts)) +
  theme_bw() +
  geom_point()

## fit the binomial GLM with the 3 different links
fit8 <- glm(cbind(success, attempts - success) ~ poly(distance, 2),
            family = binomial(link = "logit"),
            data = golf_df)

fit9 <- glm(cbind(success, attempts - success) ~ poly(distance, 2),
            family = binomial(link = "probit"),
            data = golf_df)

fit10 <- glm(cbind(success, attempts - success) ~ poly(distance, 2),
             family = binomial(link = "cloglog"),
             data = golf_df)

AIC(fit8)
AIC(fit9)
AIC(fit10)

newdata <- tibble(distance = seq(2, 20, length = 200)) %>%
  add_predictions(model = fit8, type = "response", var = "logit") %>%
  add_predictions(model = fit9, type = "response", var = "probit") %>%
  add_predictions(model = fit10, type = "response", var = "cloglog")

newdata %>%
  pivot_longer(2:4,
               names_to = "link_function",
               values_to = "pred") %>%
  ggplot(aes(x = distance, y = pred)) +
  theme_bw() +
  geom_line(aes(color = link_function)) +
  geom_point(data = golf_df,
             aes(y = success / attempts))