library(tidyverse)

# Random effects models ---------------------------------------------------
# cmd/ctrl + shift + R

## load the cbpp dataset
library(lme4)
?cbpp

cbpp_df <- as_tibble(cbpp)
cbpp_df

## exploratory plot
cbpp_df %>%
  ggplot(aes(x = herd, y = incidence / size * 100)) +
  geom_boxplot()

## let's look at herd 4 only
cbpp_df_herd4 <- cbpp_df %>%
  filter(herd == 4)

fit1 <- glm(cbind(incidence, size - incidence) ~ 1, ## intercept-only model (no covariates)
            family = binomial,
            data = cbpp_df_herd4)

## estimated value of beta0 (the log-odds of disease incidence in herd 4)
coef(fit1)
## to get the estimated value of pi (the probability of disease incidence in herd 4)
## we need to back transform from the log-odds scale
## to do so, we can use the plogis() function (the inverse logit)
plogis(coef(fit1))

## we can calculate a 95% CI for the true prob. of disease incidence in herd 4
plogis(confint.default(fit1))

## we can also fit a single model for all herds
fit2 <- glm(cbind(incidence, size - incidence) ~ 0 + herd,
            family = binomial,
            data = cbpp_df)
coef(fit2)

round(plogis(coef(fit2)), 3)

## fit a mixed version of the multi-herd model
library(lme4)
fit3 <- glmer(cbind(incidence, size - incidence) ~ 1 + (1 | herd),
              family = binomial,
              data = cbpp_df)
## we are fitting a fixed intercept "1" and a random intercept per herd "(1 | herd)"
summary(fit3)

fixef(fit3) ## estimated fixed effects (only an intercept)
ranef(fit3) ## predicted random effects (one per herd)

VarCorr(fit3)$herd[1] ## estimate of sigma^2

## let's look at the "model of models", i.e. the normal curve we fitted to
## the population of disease incidence rates (in the log-odds scale)
curve(dnorm(x, mean = fixef(fit3), sd = sqrt(VarCorr(fit3)$herd[1])),
      xlim = c(-6,3))

## drawing a probability of disease incidence for a randomly
## chosen new herd from this population of herds
plogis(rnorm(1, mean = fixef(fit3), sd = sqrt(VarCorr(fit3)$herd[1])))

## the best guess for the prob. of disease incidence for
## a new herd we haven't looked at before
plogis(fixef(fit3))

## we expect 95% of the herds in this population to have
## a disease incidence prob. between
plogis(fixef(fit3) + c(-1, 1) * 1.96 * sqrt(VarCorr(fit3)$herd[1]))
## bear in mind these limits are not a confidence interval
## they are merely reflecting characteristics of the normal
## model that governs random effects
## a confidence interval for the true value of beta0 can be obtained via
confint(fit3)

## aside: checking normality of random effects
my_ranef <- ranef(fit3)$herd[,1]
hist(my_ranef)
qqnorm(my_ranef); qqline(my_ranef)
library(hnp)
hnp(my_ranef, scale = TRUE, half = FALSE)
shapiro.test(my_ranef)


# The normal random effects model -----------------------------------------

alcohol_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_mixed_models/data/alcohol.csv")
alcohol_df

alcohol_df %>%
  ggplot(aes(x = reorder(country, alcohol), y = alcohol)) +
  theme_bw() +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                   vjust = 0, size = 4))

fit4 <- lmer(alcohol ~ 1 + (1 | country),
             data = alcohol_df)
summary(fit4)

## estimate of beta0
fixef(fit4)
## ranef gives the predicted "b" values
ranef(fit4)

## as an aside, not a very well-fitted model
hist(ranef(fit4)$country[,1])
hnp(ranef(fit4)$country[,1], scale = TRUE, half = FALSE, paint = TRUE)
shapiro.test(ranef(fit4)$country[,1])
## note - full adequacy checks go beyond only assessing normality of random effects

coef(fit4) ## coef gives us the mu hats
## same as this
fixef(fit4) + ranef(fit4)$country

## Intraclass correlation coefficient (ICC)
VarCorr(fit4) %>% as.data.frame
sig2_b <- 22.207828
sig2 <- 1.107824
ICC <- sig2_b / (sig2 + sig2_b)

## displaying shrinkage
comp <- tibble(country = unique(alcohol_df$country),
               flat = coef(lm(alcohol ~ 0 + country,
                              data = alcohol_df)),
               mixed = coef(fit4)$country[,1])

comp %>%
  pivot_longer(2:3,
               names_to = "model_type",
               values_to = "prediction") %>%
  ggplot(aes(x = model_type, y = prediction, group = country)) +
  theme_bw() +
  geom_point(alpha = .4) +
  geom_line(alpha = .4) +
  ylab(expression(hat(mu)))

## the above is an example of almost no shrinkage, since
## most variation is accounted for by the between-group variance

## let's see an example of shrinkage when all variation
## is due to random noise
beta0 <- 5
sigma2 <- 4

set.seed(2025)
simulated_df <- tibble(y = rnorm(100, mean = beta0, sd = sqrt(sigma2)),
                       country = gl(20, 5))

fit5 <- lmer(y ~ 1 + (1 | country),
             data = simulated_df)
summary(fit5)
ranef(fit5)

VarCorr(fit5) %>% as.data.frame
sig2_b <- 0.02071003
sig2 <- 3.92442152
ICC <- sig2_b / (sig2 + sig2_b)

comp <- tibble(country = unique(simulated_df$country),
               flat = coef(lm(y ~ 0 + country,
                              data = simulated_df)),
               mixed = coef(fit5)$country[,1])

comp %>%
  pivot_longer(2:3,
               names_to = "model_type",
               values_to = "prediction") %>%
  ggplot(aes(x = model_type, y = prediction, group = country)) +
  theme_bw() +
  geom_point(alpha = .4) +
  geom_line(alpha = .4) +
  ylab(expression(hat(mu)))


true_fit <- lm(y ~ 1, data = simulated_df)
flat_fit <- lm(y ~ country, data = simulated_df)
mixed_fit <- lmer(y ~ 1 + (1 | country), data = simulated_df, REML = FALSE)

## comparing two fixed-effects models is simple via F tests (if they are normal)
anova(true_fit, flat_fit)

## comparing mixed vs fixed is a different story...
## some advocate for the use of information criteria
## but bear in mind their calculation for mixed models is an approximation
AIC(true_fit)
AIC(mixed_fit)

## we can, instead, carry out a likelihood-ratio test
## but we need to construct it by hand
LRT_stat <- as.numeric(2 * (logLik(true_fit) - logLik(mixed_fit)))
p_value <- pchisq(q = LRT_stat, df = 1, lower.tail = FALSE) / 2 ## need to halve the p-value
## p-value > 0.05, means our simpler (more parsimonious) model fits just as well as the more complex one
## therefore, we reject the more complex one in favour of the simpler one
## in this case, the test correctly identifies that we don't need
## the conutry-level random intercepts