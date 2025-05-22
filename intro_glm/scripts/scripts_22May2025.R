library(tidyverse)

# Overdispersion ----------------------------------------------------------

## recap -- progeny data and goodness-of-fit / adequacy assessment
library(hnp)
data(progeny)

progeny %>%
  ggplot(aes(x = extract, y = y)) +
  theme_bw() +
  geom_boxplot()

fit1 <- glm(y ~ extract,
            family = poisson,
            data = progeny)
summary(fit1)

hnp(fit1)
## the hnp gives empirical evidence that the observed progeny data
## is not a plausible realisation of the fitted Poisson model
## therefore, this model is inadequate and should not be relied on
## for inferential purposes

progeny %>%
  group_by(extract) %>%
  summarise(mean = mean(y),
            var = var(y)) %>%
  mutate(var_over_mean = var / mean)
## the equidispersion assumption has been violated and we need a model
## to accommodate this extra-variability not described by the Poisson model

## exploring mean-variance relationships with simulation

## the normal distribution
set.seed(2025)
y1 <- rnorm(20, 5, 2)
y2 <- rnorm(20, 15, 2)
y3 <- rnorm(20, 25, 2)
y4 <- rnorm(20, 35, 2)

mean(y1); var(y1)
mean(y2); var(y2)
mean(y3); var(y3)
mean(y4); var(y4)

y <- c(y1, y2, y3, y4)
treatment <- gl(4, 20)

boxplot(y ~ treatment)

## the Poisson distribution
set.seed(2025)
y1 <- rpois(20, 5)
y2 <- rpois(20, 15)
y3 <- rpois(20, 25)
y4 <- rpois(20, 35)

mean(y1); var(y1)
mean(y2); var(y2)
mean(y3); var(y3)
mean(y4); var(y4)

y <- c(y1, y2, y3, y4)

boxplot(y ~ treatment)

## back to the progeny dataset
progeny %>%
  group_by(extract) %>%
  summarise(mean = mean(y),
            var = var(y)) %>%
  ggplot(aes(x = mean, y = var)) +
  theme_bw() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2)

## fitting the quasi-Poisson model

fit2 <- glm(y ~ extract,
            family = quasipoisson,
            data = progeny)
summary(fit2)

summary(fit2)$dispersion
sum((progeny$y - fitted(fit1))^2 / fitted(fit1)) / fit1$df.residual
sum(residuals(fit1, type = "pearson")^2) / fit1$df.residual

anova(fit1, test = "Chisq") # Chisq tests for Poisson models
anova(fit2, test = "F")     # F tests for quasi-Poisson models

summary(fit1)$coefficients
summary(fit2)$coefficients

summary(fit1)$coefficients[,2] * sqrt(summary(fit2)$dispersion)
# the standard errors (uncertainty) are inflated by a factor of sqrt(phi)

hnp(fit2)

## Negative binomial GLMs

## dataset: effects of agricultural oils on the oviposition of the citrus psyllid
data(oil)
oil

oil %>%
  ggplot(aes(x = treat, y = y)) +
  theme_bw() +
  geom_boxplot()

oil_meanvar <- oil %>%
  group_by(treat) %>%
  summarise(mean = mean(y),
            var = var(y))

oil_meanvar %>%
  ggplot(aes(x = mean, y = var)) +
  theme_bw() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_smooth(se = FALSE, span = 2)

mv_qp <- lm(var ~ 0 + mean,
            data = oil_meanvar)
mv_nb <- lm(var ~ 0 + offset(mean) + I(mean^2),
            data = oil_meanvar)

library(modelr)
newdata <- tibble(mean = seq(0, 80, length = 200))
newdata %>%
  add_predictions(model = mv_qp, var = "quasi_Poisson") %>%
  add_predictions(model = mv_nb, var = "negative_binomial") %>%
  pivot_longer(2:3,
               names_to = "type",
               values_to = "var") %>%
  ggplot(aes(x = mean, y = var)) +
  theme_bw() +
  geom_line(aes(color = type)) +
  geom_point(data = oil_meanvar) +
  geom_abline(intercept = 0, slope = 1, lty = 2)

fit3 <- glm(y ~ treat,
            family = poisson,
            data = oil)
summary(fit3)
hnp(fit3)

fit4 <- glm(y ~ treat,
            family = quasipoisson,
            data = oil)
summary(fit4)
hnp(fit4)

fit5 <- glm.nb(y ~ treat,
               data = oil)
summary(fit5)
hnp(fit5)

anova(fit5, test = "F")
anova(fit3, test = "Chisq")

AIC(fit3) # Poisson model
AIC(fit5) # Negative binomial model

AIC(fit4) # not available for the quasi-Poisson model
logLik(fit4) # not available for the quasi-Poisson model

## example: biochemists dataset
biochem_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_glm/data/biochemist.csv")

library(pscl)
?bioChemists

biochem_df %>%
  mutate(prestige_binned = cut(prestige, breaks = c(0,2,4,6)),
         mentor_binned = cut(prestige, breaks = c(-Inf,20,40,80))) %>%
  group_by(gender, married, prestige_binned, mentor_binned) %>%
  summarise(mean = mean(publications),
            var = var(publications)) %>%
  ggplot(aes(x = mean, y = var)) +
  theme_bw() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2)

## use as predictors: gender, married, children, prestige, mentor

fit6 <- glm(publications ~ gender + married + children + prestige + mentor,
            family = poisson,
            data = biochem_df)
summary(fit6)
hnp(fit6, paint = TRUE)

fit7 <- glm(publications ~ gender + married + children + prestige + mentor,
            family = quasipoisson,
            data = biochem_df)
summary(fit7)
hnp(fit7, paint = TRUE)

fit8 <- glm.nb(publications ~ gender + married + children + prestige + mentor,
               data = biochem_df)
summary(fit8)
hnp(fit8, paint = TRUE)

drop1(fit6, test = "Chisq")
drop1(fit7, test = "F")

## take a male, married, no children, mentor has 9 publications
## how does prestige influence the predictions?
estimates <- coef(fit8)

## log rate for prestige 1, 2, 3, 4, 5
estimates[1] + estimates[5] * (1:5) + estimates[6] * 9

## rate for prestige 1, 2, 3, 4, 5
exp(estimates[1] + estimates[5] * (1:5) + estimates[6] * 9)

## in a population of 1000 people who fall into this category
rnegbin(1000, mu = 1.981087, theta = 2.264) %>% table
rnegbin(1000, mu = 2.105873, theta = 2.264) %>% table

## Overdispersion in discrete proportions
data(fungi)

fungi %>%
  ggplot(aes(x = lconc, y = y / m, color = species)) +
  theme_bw() +
  geom_point()

fit9 <- glm(cbind(y, m - y) ~ lconc + species,
            family = binomial,
            data = fungi)
summary(fit9)
hnp(fit9)

fit10 <- glm(cbind(y, m - y) ~ lconc + species,
             family = quasibinomial,
             data = fungi)
summary(fit10)
hnp(fit10)

drop1(fit9, test = "Chisq") # binomial model
drop1(fit9, test = "F")     # quasi-binomial model

## we can use different packages to fit the beta-binomial model
## e.g. VGAM, aods3, glmmTMB, gamlss

library(gamlss)
fit11 <- gamlss(cbind(y, m - y) ~ lconc + species,
                family = BB,
                data = fungi)
summary(fit11)
hnp(fit11)
## as an alternative to hnp for gamlss models...
wp(fit11) # a worm-plot (a detrended hnp based on a different type of residual)

## let's have a look at the fitted distributions (comparing binomial vs. beta-binomial)
## take a log-concentration of 5 (10,000 conidia/mL)
coef_binomial <- coef(fit9)
coef_betabinomial <- coef(fit11)

## mean mortality probability when using the fungus isaria at lconc 5
pi_isaria_bin <- plogis(coef_binomial[1] + coef_binomial[2] * 5)
pi_isaria_betabin <- plogis(coef_betabinomial[1] + coef_betabinomial[2] * 5)

## mean mortality probability when using the fungus beauveria at lconc 5
pi_beauveria_bin <- plogis(coef_binomial[1] + coef_binomial[2] * 5 + coef_binomial[3])
pi_beauveria_betabin <- plogis(coef_betabinomial[1] + coef_betabinomial[2] * 5 + coef_betabinomial[3])

## estimated dispersion parameter for the beta-binomial
disp_betabin <- fitted(fit11, what = "sigma")[1]

## estimated distribution based on the binomial model (underestimating uncertainty)
plot_bin <- tibble(x = 0:20,
                   beauveria = dbinom(x, size = 20, prob = pi_beauveria_bin),
                   isaria = dbinom(x, size = 20, prob = pi_isaria_bin)) %>%
  pivot_longer(2:3,
               names_to = "species",
               values_to = "prob") %>%
  ggplot(aes(x = x, y = prob, fill = species)) +
  theme_bw() +
  geom_bar(stat = "identity", position = "identity",
           alpha = .6) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),) +
  xlab("Number of dead insects out of 20") +
  ylab("Probability")

plot_betabin <- tibble(x = 0:20,
                       beauveria = dBB(x, bd = 20, mu = pi_beauveria_bin, sigma = disp_betabin),
                       isaria = dBB(x, bd = 20, mu = pi_isaria_bin, sigma = disp_betabin)) %>%
  pivot_longer(2:3,
               names_to = "species",
               values_to = "prob") %>%
  ggplot(aes(x = x, y = prob, fill = species)) +
  theme_bw() +
  geom_bar(stat = "identity", position = "identity",
           alpha = .6) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),) +
  xlab("Number of dead insects out of 20") +
  ylab("Probability")

library(ggpubr)
final_plot <- ggarrange(plot_bin, plot_betabin, ncol = 1, common.legend = TRUE)
ggsave(filename = "my_plot.png", plot = final_plot, dpi = 800,
       width = 12, height = 10)

## alternatively...
binomial_probs <- tibble(x = 0:20,
                         beauveria = dbinom(x, size = 20, prob = pi_beauveria_bin),
                         isaria = dbinom(x, size = 20, prob = pi_isaria_bin)) %>%
  pivot_longer(2:3,
               names_to = "species",
               values_to = "prob")

betabinomial_probs <- tibble(x = 0:20,
                             beauveria = dBB(x, bd = 20, mu = pi_beauveria_bin, sigma = disp_betabin),
                             isaria = dBB(x, bd = 20, mu = pi_isaria_bin, sigma = disp_betabin)) %>%
  pivot_longer(2:3,
               names_to = "species",
               values_to = "prob")

binomial_probs <- binomial_probs %>%
  mutate(model = rep("binomial", n()))

betabinomial_probs <- betabinomial_probs %>%
  mutate(model = rep("beta-binomial", n()))

all_probs <- bind_rows(binomial_probs, betabinomial_probs)

all_probs %>%
  ggplot(aes(x = x, y = prob, fill = species)) +
  theme_bw() +
  geom_bar(stat = "identity", position = "identity",
           alpha = .6) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),) +
  xlab("Number of dead insects out of 20") +
  ylab("Probability") +
  facet_wrap(~ model, ncol = 1)

table(rBB(1000, bd = 20, mu = pi_beauveria_bin, sigma = disp_betabin))
table(rbinom(1000, size = 20, prob = pi_isaria_bin))


# Zero-Inflated models ----------------------------------------------------

## example: hunting spider abundance data
spider_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_glm/data/spider.csv")

spider_df %>%
  ggplot(aes(x = soil_dry_mass, y = count)) +
  egg::theme_article() +
  geom_point()

## let's fit the ZIP model
library(pscl)
fit12 <- zeroinfl(count ~ soil_dry_mass,
                  dist = "poisson",
                  data = spider_df)
summary(fit12)
estimates <- coef(fit12)

## what is the log odds that the latent variable Z takes value 1 (we get
## a structural zero), if soil dry mass = 3?
estimates[3] + estimates[4] * 3

## what is the probability that the latent variable Z takes value 1 (we get
## a structural zero), if soil dry mass = 3?
## what is the probability we have no hunting spiders in environments
## with soil dry mass = 3 (structural zero)
plogis(estimates[3] + estimates[4] * 3)

## what is the log of the mean abundance of hunting spiders in
## environments with soil dry mass = 3
estimates[1] + estimates[2] * 3

## what is the mean abundance of hunting spiders in
## environments with soil dry mass = 3
exp(estimates[1] + estimates[2] * 3)

## prob structural zero for soil dry mass = 1, 2, 3, 4
plogis(estimates[3] + estimates[4] * (1:4))

## mean abundance for soil dry mass = 1, 2, 3, 4
exp(estimates[1] + estimates[2] * (1:4))

spider_pred <- tibble(soil_dry_mass = 1:4) %>%
  add_predictions(model = fit12, type = "response", var = "response") %>%
  add_predictions(model = fit12, type = "zero", var = "zero") %>%
  add_predictions(model = fit12, type = "count", var = "count")
spider_pred

(1 - spider_pred$zero) * spider_pred$count
spider_pred$response

## generating a fitted curve
newdata <- tibble(soil_dry_mass = seq(1, 3.5, length = 200)) %>%
  add_predictions(model = fit12, type = "response")

spider_df %>%
  ggplot(aes(x = soil_dry_mass, y = count)) +
  egg::theme_article() +
  geom_point() +
  geom_line(data = newdata,
            aes(y = pred))

## checking adequacy
hnp(fit12)

## say we want to check whether soil dry mass affects the prob of a
## structural zero
fit13 <- zeroinfl(count ~ soil_dry_mass | 1,
                  dist = "poisson",
                  data = spider_df)
summary(fit13)

## doing a likelihood-ratio test by hand
lrt_stat <- 2*(logLik(fit12) - logLik(fit13))
pchisq(lrt_stat, df = 1, lower.tail = FALSE) %>% as.numeric
## careful: these tests aren't reliable when the model isn't adequate!

## let's fit a ZINB model
fit14 <- zeroinfl(count ~ soil_dry_mass,
                  dist = "negbin",
                  data = spider_df)
summary(fit14)

hnp(fit14)
AIC(fit12) # ZIP
AIC(fit14) # ZINB

estimates_zip <- estimates ## current estimates object
estimates_zinb <- coef(fit14)

mu_zip <- exp(estimates_zip[1] + estimates_zip[2] * 3)
omega_zip <- plogis(estimates_zip[3] + estimates_zip[4] * 3)

mu_zinb <- exp(estimates_zinb[1] + estimates_zinb[2] * 3)
omega_zinb <- plogis(estimates_zinb[3] + estimates_zinb[4] * 3)
theta_zinb <- fit14$theta

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