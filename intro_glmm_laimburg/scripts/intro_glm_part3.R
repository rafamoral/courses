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