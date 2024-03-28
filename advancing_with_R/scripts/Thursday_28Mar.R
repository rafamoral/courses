library(tidyverse)
library(modelr)

theme_set(theme_bw())

# Poisson GLMs with an offset term ----------------------------------------

## load the insurance data
insurance_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/insurance.csv")
insurance_df$Age <- factor(insurance_df$Age,
                           levels = c("<25", "25-29", "30-35", ">35"))

## exploratory plots
insurance_df %>%
  ggplot(aes(x = Age, y = Claims)) +
  geom_boxplot()

insurance_df %>%
  ggplot(aes(x = Age, y = Claims / Holders)) +
  geom_boxplot()

## fit the Poisson model without an offset
fit1 <- glm(Claims ~ 0 + Age,
            family = poisson,
            data = insurance_df)
coef(fit1)
exp(coef(fit1))

fit2 <- glm(Claims ~ Age,
            family = poisson,
            data = insurance_df)
coef(fit2)
exp(coef(fit2))
## the Poisson model without the offset term estimates
## that people older than 35 will have a 9-fold increase
## in the number of claims made, compared to people
## younger than 25

## let's now include the offset term (which is the correct approach!)
fit3 <- glm(Claims ~ Age + offset(log(Holders)),
            family = poisson,
            data = insurance_df)
exp(coef(fit3)[2:4])
## the Poisson model *with* the offset term estimates
## that people older than 35 have a *40% reduction* in
## the number of claims made when compared to people
## younger than 25


# Assessing adequacy for Poisson models -----------------------------------

## load the Sitophilus zeamais progeny data
library(hnp)
data(progeny)

progeny %>%
  ggplot(aes(x = extract, y = y)) +
  geom_boxplot()

fit4 <- glm(y ~ extract,
            family = poisson,
            data = progeny)

summary(fit4)

curve(dchisq(x, df = 36), xlim = c(0,100))
abline(v = 89.768, lty = 2)
pchisq(89.768, df = 36, lower.tail = FALSE)

qchisq(.95, df = 36) ## critical value

progeny %>%
  group_by(extract) %>%
  summarise(avg = mean(y),
            var = var(y))

## model doesn't fit well, so we can't rely on the inference below
anova(fit4, test = "Chisq")

## half-normal plot with a simulated envelope
set.seed(2024)
hnp(fit4)
hnp(fit4, how.many.out = TRUE)
hnp(fit4, print.on = TRUE)
hnp(fit4, paint.out = TRUE)


# Overdispersion ----------------------------------------------------------

progeny %>%
  group_by(extract) %>%
  summarise(avg = mean(y),
            var = var(y)) %>%
  ggplot(aes(x = avg, y = var)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2)

## standard Poisson fit
fit4 <- glm(y ~ extract,
            family = poisson,
            data = progeny)
hnp(fit4)

summary(fit4)
anova(fit4, test = "Chisq")

## let's fit an alternative model that takes into account
## extra-variability (or over-dispersion)
## Quasi-Poisson model
fit5 <- glm(y ~ extract,
            family = quasipoisson,
            data = progeny)
hnp(fit5)
summary(fit5)

## estimated coefficients are exactly the same
coef(fit4)
coef(fit5)

## however, uncertainty about the estimates (standard errors)
## are now inflated
summary(fit4)$coefficients[,2]
summary(fit5)$coefficients[,2]

## in fact, they are inflated by a factor of sqrt(phi)
phi_hat <- summary(fit5)$dispersion
summary(fit4)$coefficients[,2] * sqrt(phi_hat)
summary(fit5)$coefficients[,2]

## to test for significance of effects, now we use the F test
anova(fit4, test = "Chisq") ## Poisson, with phi = 1, fixed
anova(fit5, test = "F")     ## Quasi-Poisson, scaling deviances by estimated phi

## grouping treatments we believe are the same
## and comparing using an F test
progeny <- progeny %>%
  mutate(extract_groups = extract)

## we want to test whether (leaf = branch = control) differs from (seed)
## first, create the grouped levels with a separate factor variable
levels(progeny$extract_groups) <- c("group","group","seed","group")
## then, fit the reduced model
fit6 <- glm(y ~ extract_groups,
            family = quasipoisson,
            data = progeny)
## finally, compare the two models
anova(fit6, fit5, test = "F") ## do not reject the null
## null: the two models are equivalent
## which also means that we CAN group the factor levels that we grouped
## or, they are not significantly different

## conclusion: seed extract provides better protection against
## the maize weevil; whereas leaf and branch extracts are no better
## than the non-active control

progeny %>%
  group_by(extract) %>%
  summarise(avg = mean(y),
            var = var(y)) %>%
  mutate(letters = c("a","a","b","a"))


fit5b <- glm(y ~ 0 + extract,
             family = quasipoisson,
             data = progeny)

library(coefplot)
coefplot(fit5b)
summary(fit5b)


# Negative binomial GLMs --------------------------------------------------

## load the oil dataset
library(hnp)
data(oil)

oil %>%
  group_by(treat) %>%
  summarise(avg = mean(y),
            var = var(y)) %>%
  ggplot(aes(x = avg, y = var)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_smooth(span = 10, se = FALSE) +
  geom_smooth(method = lm, se = FALSE, col = 2)

## fit the Poisson, Quasi-Poisson and negative binomial models
fit7 <- glm(y ~ treat,
            family = poisson,
            data = oil)

fit8 <- glm(y ~ treat,
            family = quasipoisson,
            data = oil)

library(MASS)
fit9 <- glm.nb(y ~ treat,
               data = oil)

anova(fit7, test = "Chisq")
anova(fit8, test = "F")
anova(fit9, test = "F")

hnp(fit7, paint = TRUE, print = TRUE)
hnp(fit8, paint = TRUE, print = TRUE)
hnp(fit9, paint = TRUE, print = TRUE)

oil %>%
  ggplot(aes(x = treat, y = y)) +
  geom_boxplot()

fit9b <- glm.nb(y ~ 0 + treat,
                data = oil)
coefplot(fit9b)

estimates <- coef(fit9b)
std_errors <- summary(fit9b)$coefficients[,2]

lower_bound <- estimates - 1.96 * std_errors
upper_bound <- estimates + 1.96 * std_errors

CI95 <- cbind(lower_bound, upper_bound)

## 95% of the area under the standard normal curve is between -1.96 and 1.96
qnorm(.975)
curve(dnorm(x), xlim = c(-3,3))
abline(v = c(-1.96,1.96), lty = 2)

## although the data is not normal, we use results from asymptotic
## inferential theory to derive a CI for the beta parameters
## asymptotically, the maximum likelihood estimators have a
## normal distribution

## aside: testing data for normality -- not recommended!
## 1. if the nature of the data is discrete, normal models don't apply
## 2. even if the data is normal, if you test the response itself you can
##    have problems (you need to test the residuals instead)

y1 <- rnorm(20, 5, 2)
y2 <- rnorm(20, 20, 2)
y <- c(y1, y2)
condition <- gl(2, 20)

hist(y, breaks = 20)
qqnorm(y); qqline(y)
shapiro.test(y) ## rejects normality

reg_res <- rstudent(lm(y ~ condition))
hist(reg_res)
qqnorm(reg_res); qqline(reg_res)
shapiro.test(reg_res)


# Overdispersion extensions for discrete proportion data ------------------

## example revisited: insect mortality data
library(hnp)
data(fungi)

fungi %>%
  ggplot(aes(x = lconc, y = y / m, col = species)) +
  geom_point()

## fit the standard binomial model
fit10 <- glm(cbind(y, m - y) ~ lconc + species,
             family = binomial,
             data = fungi)
summary(fit10)
hnp(fit10) ## poor fit
drop1(fit10, test = "Chisq") ## cannot be trusted because the binomial
                             ## model underestimates the uncertainty

## fit the quasi-binomial model
fit11 <- glm(cbind(y, m - y) ~ lconc + species,
             family = quasibinomial,
             data = fungi)
summary(fit11)
hnp(fit11)
drop1(fit11, test = "F")

## fit the beta-binomial model
library(gamlss)
fit12 <- gamlss(cbind(y, m - y) ~ lconc + species,
                family = BB,
                data = fungi)
summary(fit12)
hnp(fit12)

coef(fit10)
coef(fit12)

## estimated probability distribution based on the binomial model
## for both fungal species and assuming lconc = 5
prob_beauveria <-  plogis(- 4.911957 + 1.000563 * 5 - 1.416791)
prob_isaria <- plogis(- 4.911957 + 1.000563 * 5)

plot_binomial <- tibble(x = 0:20,
       beauveria = dbinom(x, size = 20, prob = prob_beauveria),
       isaria = dbinom(x, size = 20, prob = prob_isaria)) %>%
  pivot_longer(2:3,
               names_to = "species",
               values_to = "prob") %>%
  ggplot(aes(x = x, y = prob, fill = species)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  xlab("Number of dead insects out of a total of 20") +
  ylab("Estimated probability") +
  ggtitle("Binomial model")

## estimated probability distribution based on the binomial model
## for both fungal species and assuming lconc = 5
prob_beauveria_bb <- plogis(-4.7814210 + 0.9774934 * 5 -1.4164149)
prob_isaria_bb <- plogis(-4.7814210 + 0.9774934 * 5)

plot_betabinomial <- tibble(x = 0:20,
                            beauveria = dBB(x, bd = 20, mu = prob_beauveria_bb, sigma = exp(-2.604)),
                            isaria = dBB(x, bd = 20, mu = prob_isaria_bb, sigma = exp(-2.604))) %>%
  pivot_longer(2:3,
               names_to = "species",
               values_to = "prob") %>%
  ggplot(aes(x = x, y = prob, fill = species)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  xlab("Number of dead insects out of a total of 20") +
  ylab("Estimated probability") +
  ggtitle("Beta-binomial model")

library(ggpubr)
ggarrange(plot_binomial, plot_betabinomial,
          ncol = 1, common.legend = TRUE,
          legend = "right")


# Zero-inflated models ----------------------------------------------------

## hunting spider abundance data
spider_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/spider.csv")

mean(spider_df$count == 0) ## ~ 40% of the data consists of zeros

## exploratory plot
spider_df %>%
  ggplot(aes(x = soil_dry_mass, y = count)) +
  geom_point()

## fit the Zero-Inflated Poisson model
library(pscl)
fit13 <- zeroinfl(count ~ soil_dry_mass,
                  dist = "poisson",
                  data = spider_df)
summary(fit13)
hnp(fit13) ## still extra variability unaccounted for

## fit the Zero-Inflated negative binomial model
fit14 <- zeroinfl(count ~ soil_dry_mass,
                  dist = "negbin",
                  data = spider_df)
summary(fit14)
hnp(fit14)

lambda_pred <- predict(fit14, data.frame(soil_dry_mass = 2), type = "count")
omega_pred <- predict(fit14, data.frame(soil_dry_mass = 2), type = "zero")
y_pred <- predict(fit14, data.frame(soil_dry_mass = 2), type = "response")

(1 - omega_pred) * lambda_pred

## fitted curve
spider_df %>%
  add_predictions(model = fit14,
                  type = "response") %>%
  ggplot(aes(x = soil_dry_mass, y = count)) +
  geom_point() +
  geom_line(aes(y = pred))

AIC(fit13)
AIC(fit14)

## soil_dry_mass as predictor for count process
## only intercept for the zero-inflation process
fit15 <- zeroinfl(count ~ soil_dry_mass | 1,
                  dist = "negbin",
                  data = spider_df)
summary(fit15)

AIC(fit14)
AIC(fit15)

## likelihood-ratio test for nested models
delta_D <- as.numeric(2 * (logLik(fit14) - logLik(fit15)))
pchisq(delta_D, df = 1, lower.tail = FALSE)

## for non-nested models there is a test called the "Vuong test"


# Random effects models ---------------------------------------------------

## load the `cbpp` dataset
library(lme4)

cbpp_df <- as_tibble(cbpp)

cbpp_df %>%
  ggplot(aes(x = herd, y = incidence / size)) +
  geom_boxplot()

## as an example, let's look at the data from herd 4
cbpp_df_4 <- cbpp_df %>%
  filter(herd == "4")

fit16 <- glm(cbind(incidence, size - incidence) ~ 1,
             family = binomial,
             data = cbpp_df_4)

## estimated value of beta (log-odds of disease incidence)
coef(fit16)
## estimated value of pi (prob. of disease incidence)
plogis(coef(fit16))

## we can fit a single model of all herds
fit17 <- glm(cbind(incidence, size - incidence) ~ 0 + herd,
             family = binomial,
             data = cbpp_df)
plogis(coef(fit17)) %>%
  round(2)

## multilevel version of the multi-herd model
library(lme4)
fit18 <- glmer(cbind(incidence, size - incidence) ~ (1 | herd),
               family = binomial,
               data = cbpp_df)
summary(fit18)

fixef(fit18) ## estimate of the overall intercept beta0 (mean of the population of log-odds)
VarCorr(fit18)$herd[1] ## estimate of the variance of the population of log-odds

plogis(fixef(fit18))

## the "model of models" (distribution of log-odds for an infinite population of herds)
curve(dnorm(x, mean = fixef(fit18), sd = sqrt(VarCorr(fit18)$herd[1])),
      xlim = c(-5,1))

## expect 95% of the herds in this population to have
## a log-odds of disease incidence in between
fixef(fit18) + c(-1,1) * 1.96 * sqrt(VarCorr(fit18)$herd[1])
## a prob. of disease incidence in between
plogis(fixef(fit18) + c(-1,1) * 1.96 * sqrt(VarCorr(fit18)$herd[1]))

## random effects
## random differences from the average log-odds
ranef(fit18)

## gives the log-odds for each herd
coef(fit18)


# Normal Random Effects Model ---------------------------------------------

alcohol_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/alcohol.csv")

alcohol_df %>%
  ggplot(aes(x = year, y = alcohol, col = country)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none")

alcohol_df %>%
  ggplot(aes(x = reorder(country, alcohol), y = alcohol)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   size = 5))

unique(alcohol_df$country) %>% length
## if we fitted a flat model, we'd need to estimate
## 189 beta coefficients + 1 sigma^2 for the error

## fit the mixed model
fit19 <- lmer(alcohol ~ (1 | country),
              data = alcohol_df)
summary(fit19)

## ranef gives the "b" values (BLUPs)
ranef(fit19)

## Intraclass correlation
22.2 / (22.2 + 1.1)

## an example when all variation is due to random noise
beta0 <- 5
sigma2 <- 4

set.seed(123)
simulated_df <- tibble(y = rnorm(100,
                                 mean = beta0,
                                 sd = sqrt(sigma2)),
                       country = gl(20, 5))

simulated_df %>%
  ggplot(aes(x = reorder(country, y), y = y)) +
  geom_boxplot()

fit20 <- lmer(y ~ (1 | country),
              data = simulated_df)
summary(fit20)

ranef(fit20)

## fit20 is overly complex (boundary estimation issues)
## fitting the true model
fit21 <- lm(y ~ 1,
            data = simulated_df)
summary(fit21)
sigma(fit21)^2