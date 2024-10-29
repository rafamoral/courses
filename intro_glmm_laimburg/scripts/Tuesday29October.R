library(tidyverse)


# Poisson GLMs ------------------------------------------------------------

## reading the milk contamination data
contamination_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_glmm_laimburg/data/05_milk_contamination_data.csv")

# label: whether sileage was fed or not to the animals

contamination_df <- contamination_df %>%
  mutate(obs_ID = as.factor(obs_ID),
         farmcode = as.factor(farmcode),
         label = as.factor(label),
         rain = as.factor(rain),
         stubbleheight = as.factor(stubbleheight),
         springfym = as.factor(springfym),
         overseed = as.factor(overseed))

## exploratory plots
contamination_df %>%
  ggplot(aes(x = doy, y = log(contamination + 1))) +
  theme_bw() +
  geom_point(size = .7,
             alpha = .3) +
  facet_grid(label ~ rain) +
  geom_smooth(se = FALSE)

contamination_df %>%
  ggplot(aes(x = doy, y = contamination)) +
  theme_bw() +
  geom_point(size = .7,
             alpha = .3) +
  facet_grid(label ~ rain) +
  geom_smooth(se = FALSE)

## fitting Poisson GLMs
fit1 <- glm(contamination ~ doy,
            family = poisson,
            data = contamination_df)
summary(fit1)

exp(coef(fit1)[2])
(1 - exp(coef(fit1)[2])) * 100
## the contamination level is decreasing by 0.05% for every extra day of the year

(1 - exp(confint.default(fit1, parm = "doy"))) * 100

## check goodness-of-fit (adequacy)
library(hnp)
hnp(fit1, paint = TRUE, print = TRUE)
## model fit is inadequate, DO NOT proceed with inference

anova(fit1, test = "Chisq") # can't trust this result, since model does not fit the data well

## let's include more predictors
## study the interaction between 'label' and 'rain'

fit2 <- glm(contamination ~ doy + label * rain,
            family = poisson,
            data = contamination_df)

anova(fit2, test = "Chisq")

## include all possible interactions
## some 4- and 5- way interaction effects are non-estimable, so we stop at 3

fit3 <- glm(contamination ~ (doy + label + rain + stubbleheight + springfym + overseed)^3,
            family = poisson,
            data = contamination_df)

anova(fit3, test = "Chisq")

## The Multinomial GLM ---------------------------------------------------------

## loading the grazing dataset
grazing_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/grazing.csv")
grazing_df

library(nnet)

fit1 <- multinom(resp ~ months, data = grazing_df)
fit2 <- multinom(resp ~ 1, data = grazing_df)

pchisq(2*(logLik(fit1) - logLik(fit2)), df = 2, lower.tail = FALSE)

## the effects of month are significant (LR = 9.73, d.f. = 2, p = 0.008)

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


# Overdispersion ----------------------------------------------------------

library(hnp)
data(progeny)

progeny_df <- as_tibble(progeny)

progeny_df %>%
  ggplot(aes(x = extract, y = y)) +
  theme_bw() +
  geom_boxplot()

## comparing means and variances
progeny_df %>%
  group_by(extract) %>%
  summarise(mean = mean(y),
            var = var(y))

## fitting the Poisson GLM
fit1 <- glm(y ~ extract,
            family = poisson,
            data = progeny_df)
hnp(fit1)

## for a well-fitted GLM (Poisson or binomial), we would expect
## the residual deviance and the number of residual d.f. to be similar
summary(fit1)

## compare the residual deviance with a chi-squared distribution with d.f. = resid d.f.
curve(dchisq(x, df = 36), xlim = c(0, 100))
abline(v = 89.768, lty = 2)

pchisq(89.768, df = 36, lower.tail = FALSE)

qchisq(c(.025,.975), df = 36)

anova(fit1, test = "Chisq") ## inference is unreliable until we accommodate the overdispersion

## fitting the quasi-Poisson model
fit2 <- glm(y ~ extract,
            family = quasipoisson,
            data = progeny_df)
summary(fit2)

hnp(fit2) ## model fits well, we can trust inferences

anova(fit2, test = "F") ## test is now F, because dispersion was estimated

## Negative binomial model

## reading the 'oil' dataset
data(oil)
oil_df <- as_tibble(oil)

oil_df %>%
  ggplot(aes(x = treat, y = y)) +
  theme_bw() +
  geom_boxplot()

oil_df %>%
  group_by(treat) %>%
  summarise(mean = mean(y),
            var = var(y))

fit1 <- glm(y ~ treat,
            family = poisson,
            data = oil_df)
summary(fit1)
hnp(fit1)

fit2 <- glm(y ~ treat,
            family = quasipoisson,
            data = oil_df)
hnp(fit2, paint = TRUE, print = TRUE)

library(MASS)
fit3 <- glm.nb(y ~ treat,
               data = oil_df)
hnp(fit3)

summary(fit3)
anova(fit3, test = "F")

# install.packages("coefplot")
library(coefplot)
coefplot(fit3)
coefplot(update(fit3, . ~ 0 + .))

## grouping all treatments apart from Nortox at 1.0%
oil_df$treat2 <- oil_df$treat ## create a copy of treatment factor
levels(oil_df$treat2) <- c(1,1,1,1,1,1,2) ## group together the treatments you believe are equal

fit_full <- glm.nb(y ~ treat,
                   data = oil_df)
fit_reduced <- glm.nb(y ~ treat2,
                      data = oil_df)

coef(fit_full) ## one coefficient per treatment
coef(fit_reduced) ## one coefficient per group; intercept = mean (log scale) of group 1; treat22 = difference between Nortox 1.0% and group 1

## likelihood-ratio test between nested models
anova(fit_full, fit_reduced) ## not significant = I can group the treatments the way I did

oil_df %>%
  group_by(treat) %>%
  summarise(mean = mean(y)) %>%
  mutate(letters = c("a","a","a","a","a","a","b")) %>%
  ggplot(aes(x = treat, y = mean)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  geom_text(aes(label = letters), nudge_y = 2)

## if you want to change the order of the treatments
oil_df$treat3 <- oil_df$treat
oil_df$treat3 <- as.character(oil_df$treat3)
oil_df$treat3 <- factor(oil_df$treat3,
                        levels = c("Nortox0.5","Nortox1.0",
                                   "Oppa0.5","Oppa1.0",
                                   "Iharol0.5","Iharol1.0",
                                   "Control"))

oil_df %>%
  group_by(treat3) %>%
  summarise(mean = mean(y)) %>%
  mutate(letters = c("a","b","a","a","a","a","a")) %>%
  ggplot(aes(x = treat3, y = mean)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  geom_text(aes(label = letters), nudge_y = 2)


# Mixed models ------------------------------------------------------------

library(lme4)

cbpp_df <- as_tibble(cbpp)

cbpp_df %>%
  ggplot(aes(x = herd, y = incidence / size)) +
  theme_bw() +
  geom_boxplot()

## let's create a "baby" model for herd 4
fit1 <- glm(cbind(incidence, size - incidence) ~ 1,
            family = binomial,
            data = cbpp_df %>%
              filter(herd == "4"))
summary(fit1)$coefficients
plogis(coef(fit1))

## let's estimate the average incidence of CBPP for all 15 herds in one go
fit2 <- glm(cbind(incidence, size - incidence) ~ 0 + herd,
            family = binomial,
            data = cbpp_df)
summary(fit2)$coefficients
plogis(coef(fit2))

## here we are spending 15 d.f. to describe the data
## but what can we say about the distribution of the beta0's?
## we can fit a mixed model to have a model of models (a model for the beta0's)

fit3 <- glmer(cbind(incidence, size - incidence) ~ (1 | herd),
              family = binomial,
              data = cbpp_df)
summary(fit3)
ranef(fit3) ## b_i

cbind(coef(fit2), fixef(fit3) + ranef(fit3)$herd[,1])

plot(coef(fit2), ranef(fit3)$herd[,1])

predict(fit3, type = "response")

plogis(-2.0457)

## Normal mixed-effects model

## Example: alcohol dataset
alcohol_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_mixed_models/data/alcohol.csv")

alcohol_df %>% 
  ggplot(aes(x = reorder(country, alcohol), y = alcohol)) +
  theme_bw() +
  geom_boxplot() +
  coord_flip()

alcohol_df %>% 
  ggplot(aes(x = reorder(country, alcohol), y = alcohol)) +
  theme_bw() +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## we can estimate the mean per capita alcohol consumption per country,
## assuming homogeneity of variances
fit1 <- lm(alcohol ~ country, data = alcohol_df)
length(coef(fit1))
## 189 beta's + 1 variance = 190 parameters

## fitting the mixed model
fit2 <- lmer(alcohol ~ (1 | country),
             data = alcohol_df)
summary(fit2)

## intraclass correlation coefficient (ICC)
22.208 / (22.208 + 1.108)

## checking for shrinkage
cbind(coef(fit1), fixef(fit2) + ranef(fit2)$country)
