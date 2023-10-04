library(tidyverse)

# The Normal Model: A Recap -----------------------------------------------

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

## modelling the weight as a function of height
fit1 <- lm(weight ~ height, data = weight_df)
summary(fit1)
summary(fit1)$coefficients

confint(fit1, parm = "height")
confint(fit1, parm = "height", level = 0.8)

## modelling the weight as a function of height and age
fit2 <- lm(weight ~ height + age, data = weight_df)
summary(fit2)
summary(fit2)$coefficients

confint(fit2)

# The Bernoulli GLM -------------------------------------------------------

## load the affairs dataset
affairs_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/affairs.csv")
affairs_df

## affairs: number of affairs in the past year
## religiousness: self-rating of how religious they are
##                (1 = not religious; 5 = very religious)
## rating: self-rating of how happy they are in the marriage
##         (1 = very unhappy; 5 = very happy)

## creating binary variable to be analysed ("had_affair" = yes / no)
affairs_df <- affairs_df %>%
  mutate(had_affair = affairs > 0)

## exploratory plot
## proportion of individuals who had affairs as a function of years married
affairs_df %>%
  ggplot(aes(x = yearsmarried, y = had_affair %>% as.numeric)) +
  theme_bw() +
  geom_jitter(width = .25, height = .1) +
  geom_smooth(se = FALSE)

## natural log and Euler's constant e (exp(1))
log(c(1, 2, 4, 8, 16), base = 2)
log(c(1, 2, 4, 8, 16), base = 10)
log(c(1, 10, 100, 1000, 10000), base = 10)
exp(1) ## Euler's constant
print(exp(1), digits = 22)
?log
log(exp(2))

## probabilities vs. odds
p <- 0.75
odds <- p / (1 - p)

## logits
logit_p <- log(odds)
qlogis(p)

x <- tibble(Probability = seq(0, 1, length = 200),
            `Log odds` = qlogis(Probability))
x %>%
  ggplot(aes(x = Probability, y = `Log odds`)) +
  theme_bw() +
  geom_line()

## inverse logits
my_logit <- 0
my_ilogit <- exp(my_logit) / (1 + exp(my_logit))
my_ilogit2 <- 1 / (1 + exp(- my_logit))
plogis(my_logit)

plogis(10)
plogis(-10)

## fit the Bernoulli model to the "had_affair" variable as a function of
## number of years married
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

eta1 <- estimates[1] + estimates[2] * 11
eta2 <- estimates[1] + estimates[2] * 10

eta1 - eta2
estimates[2]

## odds-ratio for the yearsmarried factor
## the factor by which the odds increases for a unit change in yearsmarried
exp(estimates[2]) ## = 1.06
## interpretation: for every extra year of marriage, the odds of having
##                 an affair increase by 6%, on average

log_odds_10 <- estimates[1] + estimates[2] * 10
odds_10 <- exp(log_odds_10)

log_odds_11 <- estimates[1] + estimates[2] * 11
odds_11 <- exp(log_odds_11)

odds_11 / odds_10
exp(estimates[2])

## 95% confidence interval for the odds ratio
## (for the predictor variable `yearsmarried`)
exp(confint.default(fit3, parm = "yearsmarried"))

## predicted probabilities
## what is the estimated probability of having an affair for
## a person with 10 years of marriage?
plogis(estimates[1] + estimates[2] * 10)

## what is the estimated probability of having an affair for
## a person with 11 years of marriage?
plogis(estimates[1] + estimates[2] * 11)

## differences change due to non-linearity
plogis(estimates[1] + estimates[2] * 11) - plogis(estimates[1] + estimates[2] * 10)
plogis(estimates[1] + estimates[2] * 15) - plogis(estimates[1] + estimates[2] * 14)

## ... but they are the same in the log-odds scale (*linear* predictor)
(estimates[1] + estimates[2] * 11) - (estimates[1] + estimates[2] * 10)
(estimates[1] + estimates[2] * 15) - (estimates[1] + estimates[2] * 14)

## what is the estimated probability of having an affair for
## a person with 11 years of marriage?
plogis(estimates[1] + estimates[2] * 20) ## be careful drawing conclusions
                                         ## from extrapolations

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
              width = .25, height = .1, alpha = .2) +
  xlab("Number of years married") +
  ylab("Estimated probability of affair")

## deviances and likelihoods
deviance(fit3)
- 2 * logLik(fit3)

## fitting a reduced model
fit4 <- glm(had_affair ~ 1,
            family = binomial(link = "logit"),
            data = affairs_df)
deviance(fit4)

## difference of deviances
delta_D <- deviance(fit4) - deviance(fit3)

## null hypothesis: fit3 and fit4 are equivalent
## under the null hypothesis, delta_D ~ chi-squared with 1 d.f.
curve(dchisq(x, 1), xlim = c(0, 15))
abline(v = delta_D, lty = 2)

pchisq(delta_D, df = 1, lower.tail = FALSE) ## p-value
## we reject the null in favour of the alternative that fit3 has
## better goodness-of-fit / yearsmarried is an important predictor
## beta1 != zero

anova(fit4, fit3, test = "Chisq")

## full model
fit5 <- glm(had_affair ~ age + yearsmarried + children + religiousness + education,
            family = binomial,
            data = affairs_df)
summary(fit5)$coefficients

## reduced model
fit6 <- glm(had_affair ~ age + yearsmarried + religiousness,
            family = binomial,
            data = affairs_df)

anova(fit6, fit5, test = "Chisq")

## a series of tests dropping one predictor at a time
drop1(fit5, test = "Chisq")

# The Binomial GLM --------------------------------------------------------

install.packages("hnp") ## if you don't have the hnp package installed

## loading the citru psyllid mortality dataset
library(hnp)
data(fungi)

fungi$species <- relevel(fungi$species, "beauveria")

## exploratory plot
fungi %>%
  ggplot(aes(x = lconc, y = y/m, colour = species)) +
  theme_bw() +
  geom_point() +
  ylab("Proportion of dead insects") +
  xlab("log10(concentration)")

## fitting the binomial GLM to the psyllid mortality data
fit7 <- glm(cbind(y, m - y) ~ lconc,
            family = binomial(link = "logit"),
            data = fungi)
summary(fit7)
estimates <- coef(fit7)
exp(estimates[2]) ## odds-ratio
## for every 1-unit increase in the log(concentration), we
## have a 2.4-fold increase in the odds of an insect dying,
## on average

drop1(fit7, test = "Chisq")

## we want to assess the effects of using different control agents
fit8 <- glm(cbind(y, m - y) ~ lconc + species,
            family = binomial,
            data = fungi)
summary(fit8)$coefficients
estimates <- coefficients(fit8)

drop1(fit8, test = "Chisq")

## what is the log-odds of killing the insect when using a
## log10(concentration) of 5 for beauveria?
estimates[1] + estimates[2] * 5

## what is the probability of killing the insect when using a
## log10(concentration) of 5 for beauveria?
plogis(estimates[1] + estimates[2] * 5)

## how many insects would die (on average) out of 20 when using
## log10(concentration) of 5 for beauveria?
20 * plogis(estimates[1] + estimates[2] * 5)

plot(0:20,
     dbinom(0:20, size = 20, prob = plogis(estimates[1] + estimates[2] * 5)),
     type = "h")

## odds ratio for an increase in one unit in log10(concentration)
exp(estimates[2])
## a 270% increase in the odds of dying for every extra unit of lconc

## what is the odds ratio of an insect dying when using
## isaria rather than beauveria?
exp(estimates[3])

## what is the % increase in the odds of an insect dying when using
## isaria rather than beauveria?
exp(estimates[3]) * 100

## 95% confidence interval for the odds ratio
exp(confint.default(fit8, parm = "speciesisaria"))
## the odds of the insect dying when infected with isaria is 2.5 to
## 6.7 times the odds of dying when infected with beauveria, with
## 95% confidence

## prediction
newdata <- expand.grid(lconc = seq(4, 8, length = 200),
                       species = c("beauveria", "isaria"))

library(modelr)
add_predictions(data = newdata, model = fit8, type = "response") %>%
  ggplot(aes(x = lconc, y = pred, colour = species)) +
  theme_bw() +
  geom_line() +
  geom_point(data = fungi,
             aes(y = y/m))

## "lethal dose" (LD)
## LD50: the dose that kills 50% of the insects
## LD95: the dose that kills 95% of the insects
dose.p(fit7)
dose.p(fit8, cf = c(1,2)) ## LD50 for beauveria

fit8.2 <- glm(cbind(y, m - y) ~ lconc + species - 1,
              family = binomial,
              data = fungi)
logLik(fit8)
logLik(fit8.2) ## the models are equivalent, fit8.2 is a reparameterisation

dose.p(fit8.2, cf = c(3,1)) ## LD50 for isaria
## a bit fiddly because of the way dose.p works

## comparing link functions
plogis(0)
pnorm(0)
library(ordinal)
pgumbel(0, max = FALSE)

## loading the golf dataset
golf_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/golf_putts.csv")
golf_df

## exploratory plot
golf_df %>%
  ggplot(aes(x = distance, y = success/attempts)) +
  theme_bw() +
  geom_point() +
  ylim(0, 1)

## let's fit the binomial model with 3 different links and compare
## the predicted curves
fit9 <- glm(cbind(success, attempts - success) ~ 
              distance,
            family = binomial(link = "logit"), ## default link
            data = golf_df)

fit10 <- glm(cbind(success, attempts - success) ~ distance,
             family = binomial(link = "probit"),
             data = golf_df)

fit11 <- glm(cbind(success, attempts - success) ~ distance,
             family = binomial(link = "cloglog"),
             data = golf_df)

newdata <- tibble(distance = seq(2, 20, length = 200))
newdata <- add_predictions(data = newdata,
                           model = fit9,
                           type = "response",
                           var = "logit")
newdata <- add_predictions(data = newdata,
                           model = fit10,
                           type = "response",
                           var = "probit")
newdata <- add_predictions(data = newdata,
                           model = fit11,
                           type = "response",
                           var = "cloglog")

newdata %>%
  pivot_longer(cols = 2:4,
               names_to = "link",
               values_to = "pred") %>%
  ggplot(aes(x = distance, y = pred, colour = link)) +
  theme_bw() +
  geom_line() +
  geom_point(data = golf_df,
             aes(y = success/attempts, colour = NA)) +
  ylim(0, 1)

## using a quadratic trend
fit12 <- glm(cbind(success, attempts - success) ~ poly(distance, 2),
             family = binomial(link = "cloglog"),
             data = golf_df)

newdata <- add_predictions(data = newdata,
                           model = fit12,
                           type = "response",
                           var = "cloglog quad")

newdata %>%
  pivot_longer(cols = 2:5,
               names_to = "link",
               values_to = "pred") %>%
  ggplot(aes(x = distance, y = pred, colour = link)) +
  theme_bw() +
  geom_line() +
  geom_point(data = golf_df,
             aes(y = success/attempts, colour = NA)) +
  ylim(0, 1)

## careful extrapolating!!
predict(fit12, data.frame(distance = 30), type = "response")