library(tidyverse)

## Example: Maize weevil progeny dataset
library(hnp)
data(progeny)

progeny %>%
  group_by(extract) %>%
  summarise(mean = mean(y),
            variance = var(y),
            `var/mean` = variance / mean)

progeny %>%
  group_by(extract) %>%
  summarise(mean = mean(y),
            variance = var(y)) %>%
  ggplot(aes(x = mean, y = variance)) +
  theme_bw() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2)

# Mean-Variance relationships ---------------------------------------------

## Poisson mean-variance relationship
sim_counts <- rpois(100, lambda = 5)
mean(sim_counts)
var(sim_counts)
var(sim_counts) / mean(sim_counts)

sim_counts <- rpois(1000, lambda = 5)
var(sim_counts) / mean(sim_counts)

sim_counts <- rpois(10000, lambda = 100)
var(sim_counts) / mean(sim_counts)

## binomial mean-variance relationship
sim_bin <- rbinom(1000, size = 20, p = 0.4)
var(sim_bin) / mean(sim_bin)
1 - 0.4

## back to the progeny data
fit1 <- glm(y ~ extract,
            family = poisson,
            data = progeny)
summary(fit1)
hnp(fit1)

anova(fit1, test = "Chisq")

## let's fit the quasi-Poisson model
fit2 <- glm(y ~ extract,
            family = quasipoisson,
            data = progeny)
summary(fit2)

phi_hat <- summary(fit2)$dispersion
summary(fit1)$coefficients[,2] * sqrt(phi_hat)
summary(fit2)$coefficients[,2]

anova(fit1, test = "Chisq")
anova(fit2, test = "F")

y <- progeny$y
mu_hat <- fitted(fit1)
v_mu_hat <- mu_hat

pearson_X2 <- sum((y - mu_hat)^2 / v_mu_hat)
phi_hat2 <- pearson_X2 / fit1$df.residual

(534.44 / phi_hat - 89.77 / phi_hat) / 3

hnp(fit1)
hnp(fit2)

# The negative binomial model ---------------------------------------------

## loading the "oil" dataset
library(hnp)
data(oil)

oil %>%
  ggplot(aes(x = treat, y = y)) +
  theme_bw() +
  geom_boxplot()

oil_meanvar <- oil %>%
  group_by(treat) %>%
  summarise(mean = mean(y),
            var = var(y),
            `var/mean` = var / mean)

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

## fitting the Poisson, quasi-Poisson, and negbin models
fit3 <- glm(y ~ treat,
            family = poisson,
            data = oil)

fit4 <- glm(y ~ treat,
            family = quasipoisson,
            data = oil)

library(MASS)
fit5 <- glm.nb(y ~ treat,
               data = oil)

anova(fit3, test = "Chisq")
anova(fit4, test = "F")
anova(fit5, test = "F")

hnp(fit3)
hnp(fit4)
hnp(fit5)

summary(fit5)

## Example: the biochemists dataset
biochem_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/biochemist.csv")
biochem_df

library(pscl)
?bioChemists

fit6 <- glm(publications ~ gender + married + children + prestige + mentor,
            family = poisson,
            data = biochem_df)
summary(fit6)
drop1(fit6, test = "Chisq")
hnp(fit6) ## cannot trust the p-values from drop1

fit7 <- glm(publications ~ gender + married + children + prestige + mentor,
            family = quasipoisson,
            data = biochem_df)
summary(fit7)
drop1(fit7, test = "F")
hnp(fit7, paint.out = TRUE, print.on = TRUE)

fit8 <- glm.nb(publications ~ gender + married + children + prestige + mentor,
               data = biochem_df)
summary(fit8)
drop1(fit8, test = "F")
hnp(fit8, paint.out = TRUE, print.on = TRUE)

# Overdispersed extensions of the binomial model --------------------------

## example revisited: insect mortality data
data(fungi)

fungi$species <- relevel(fungi$species, "beauveria")

fungi %>%
  ggplot(aes(x = lconc, y = y/m, colour = species)) +
  theme_bw() +
  geom_point() +
  ylab("proportion of dead insects") +
  xlab("log10(concentration)")

fit9 <- glm(cbind(y, m - y) ~ lconc + species,
            family = binomial,
            data = fungi)
summary(fit9)
hnp(fit9)
drop1(fit9, test = "Chisq") ## but cannot be trusted because the
                            ## binomial model underestimates the
                            ## variability in the data

fit10 <- glm(cbind(y, m - y) ~ lconc + species,
             family = quasibinomial,
             data = fungi)
summary(fit10)
hnp(fit10)
drop1(fit10, test = "Chisq")

library(gamlss)
fit11 <- gamlss(cbind(y, m - y) ~ lconc + species,
                family = BB,
                data = fungi)
summary(fit11)
hnp(fit11)

pi_beauveria <- plogis(- 4.503 + 0.609 * 5)
pi_isaria <- plogis(- 4.503 + 1.037 + 0.609 * 5)

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

library(ggpubr)
ggarrange(p1, p2, ncol = 1, common.legend = TRUE)

# Zero-Inflation ----------------------------------------------------------

## Example: hunting spider abundance
spider_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/spider.csv")
spider_df

sum(spider_df$count == 0)
mean(spider_df$count == 0)

## exploratory plot
spider_df %>%
  ggplot(aes(x = soil_dry_mass, y = count)) +
  theme_bw() +
  geom_point()

library(pscl)
fit12 <- zeroinfl(count ~ soil_dry_mass,
                  dist = "poisson",
                  data = spider_df)
summary(fit12)
hnp(fit12) ## better than Poisson, but still extra variability left to accommodate

estimates <- coef(fit12)

## what is the log-odds that the latent variable (z) takes the value
## of 1, if soil_dry_mass = 3?
## (i.e. what is the log-odds of a structural zero if soil_dry_mass = 3?)
estimates[3] + estimates[4] * 3

## what is the probability that the latent variable (z) takes the value
## of 1, if soil_dry_mass = 3?
## (i.e. what is the probability of a structural zero if soil_dry_mass = 3?)
plogis(estimates[3] + estimates[4] * 3)

## what is the probability that the latent variable (z) takes the value
## of 1, if soil_dry_mass = 1, 2, 3, 4?
## (i.e. what is the probability of a structural zero if soil_dry_mass = 1, 2, 3, 4?)
plogis(estimates[3] + estimates[4] * 1:4)

## what is the mean abundance of hunting spiders in environments
## with soil_dry_mass = 3?
exp(estimates[1] + estimates[2] * 3)

## predicted abundance is (1 - omega) * lambda
(1 - plogis(estimates[3] + estimates[4] * 3)) * exp(estimates[1] + estimates[2] * 3)

## what is the mean abundance of hunting spiders in environments
## with soil_dry_mass = 1, 2, 3, 4?
exp(estimates[1] + estimates[2] * 1:4)

spider_pred <- tibble(soil_dry_mass = 1:4)
spider_pred <- add_predictions(data = spider_pred,
                               model = fit12,
                               type = "response",
                               var = "response")
spider_pred <- add_predictions(data = spider_pred,
                               model = fit12,
                               type = "zero",
                               var = "zero")
spider_pred <- add_predictions(data = spider_pred,
                               model = fit12,
                               type = "count",
                               var = "count")
spider_pred

spider_pred$response
(1 - spider_pred$zero) * spider_pred$count

## fitted curve
newdata <- tibble(soil_dry_mass = seq(1, 3.5, length = 200))
newdata <- add_predictions(data = newdata,
                           model = fit12,
                           type = "response",
                           var = "count")

spider_df %>%
  ggplot(aes(x = soil_dry_mass, y = count)) +
  theme_bw() +
  geom_point() +
  geom_line(data = newdata, col = 2)

## fitting the ZINB model to the spider data
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