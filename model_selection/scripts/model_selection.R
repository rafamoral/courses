library(tidyverse)

## Measuring Model Fit ---------------------------------------------------------

## height data example
y <- c(180, 191, 162, 171)

mu_hat <- mean(y)
sigma_hat <- sd(y)

dnorm(y, mean = mu_hat, sd = sigma_hat)
log(dnorm(y, mean = mu_hat, sd = sigma_hat))

prod(dnorm(y, mean = mu_hat, sd = sigma_hat))
sum(log(dnorm(y, mean = mu_hat, sd = sigma_hat)))

## what is the likelihood that the data came from a normal distribution with mu = 190?
prod(dnorm(y, mean = 190, sd = sigma_hat))
sum(log(dnorm(y, mean = 190, sd = sigma_hat)))

## using the MLE for sigma
n <- length(y)
sigma_hat_mle <- sd(y) * sqrt((n - 1) / n)

prod(dnorm(y, mean = mu_hat, sd = sigma_hat))
prod(dnorm(y, mean = mu_hat, sd = sigma_hat_mle))

sum(log(dnorm(y, mean = mu_hat, sd = sigma_hat)))
sum(log(dnorm(y, mean = mu_hat, sd = sigma_hat_mle)))

loglik <- numeric(300)
i <- 1
for(sigma in seq(8, 14, length = 300)) {
  loglik[i] <- sum(log(dnorm(y, mean = mu_hat, sd = sigma)))
  i <- i + 1
}

tibble(sigma = seq(8, 14, length = 300),
       ll = loglik) %>%
  ggplot(aes(x = sigma, y = ll)) +
  theme_bw() +
  geom_line() +
  ylab("log-likelihood") +
  geom_vline(xintercept = sigma_hat, lty = 2, col = 2) +
  geom_vline(xintercept = sigma_hat_mle, lty = 2, col = 4)

## the cars dataset

cars %>%
  ggplot(aes(x = dist)) +
  theme_bw() +
  geom_histogram(col = "white")

## fitting a normal model

fit1 <- lm(dist ~ 1, data = cars)
mu_hat <- coef(fit1)
mean(cars$dist)

sigma(fit1)
sd(cars$dist)

sigma_hat <- sqrt(sum(mean(residuals(fit1)^2)))

sum(dnorm(cars$dist, mean = mu_hat, sd = sigma_hat, log = TRUE))
logLik(fit1)

## a normal regression model

fit2 <- lm(dist ~ speed, data = cars)

mu_hat <- predict(fit2)
sigma_mle <- sqrt(mean(residuals(fit2)^2))

sum(dnorm(cars$dist, mean = mu_hat, sd = sigma_mle, log = TRUE))
logLik(fit2)

## a likelihood ratio

logLik(fit2) - logLik(fit1)
exp(logLik(fit2) - logLik(fit1))

## residual sums of squares

residuals(fit2)
cars$dist - predict(fit2)

sum(residuals(fit1)^2)
sum(residuals(fit2)^2) ## smaller, part of the variation explained by predictor

RSS_fit1 <- sum(residuals(fit1)^2)
RSS_fit2 <- sum(residuals(fit2)^2)

n <- nrow(cars)
-(n/2) * (log(2*pi) - log(n) + log(RSS_fit1) + 1)
logLik(fit1)

-(n/2) * (log(2*pi) - log(n) + log(RSS_fit2) + 1)
logLik(fit2)

logLik(fit2) - logLik(fit1)
(log(RSS_fit2) - log(RSS_fit1)) * (- n / 2)

## RMSE

sqrt(mean(residuals(fit1)^2))
sqrt(mean(residuals(fit2)^2))

## MAE

mean(abs(residuals(fit1)))
mean(abs(residuals(fit2)))

## Deviance and logistic regression

cars_df <- cars %>%
  mutate(z = dist > median(dist))

## constant probability of being above average distance
fit3 <- glm(z ~ 1, 
            family = binomial,
            data = cars_df)

## prob of being above average dist is function of speed
fit4 <- glm(z ~ speed, 
            family = binomial,
            data = cars_df)

logLik(fit3) # the probability of observing outcome given M3 (and mle's of unknowns)
logLik(fit4) # the probability of observing outcome given M4 (and mle's of unknowns)

logLik(fit4) - logLik(fit3) # log of the likelihod ratio

- 2 * logLik(fit3)
- 2 * logLik(fit4)
(deviance(fit3) - deviance(fit4)) / 2

exp(16.88) ## approx. 21 million times more likely

sum(residuals(fit3, type = "deviance")^2)
deviance(fit3)
- 2 * logLik(fit3)

residuals(fit3, type = "deviance")
y <- cars_df$z
p <- predict(fit3, type = "response")
sign(y - p) * sqrt(- 2 * (y * log(p) + (1 - y) * log(1 - p)))

## Nested Model Comparisons ----------------------------------------------------

fit5 <- lm(Fertility ~ Agriculture + Education, data = swiss)
fit6 <- lm(Fertility ~ Agriculture + Education + Catholic, data = swiss)

RSS_5 <- sum(residuals(fit5)^2)
RSS_6 <- sum(residuals(fit6)^2)

RSS_5
RSS_6

(RSS_5 - RSS_6) / RSS_6

## Null hypothesis test on model fits of nested models
anova(fit5, fit6)

## F statistic
(RSS_5 - RSS_6) / (RSS_6/fit6$df.residual)

# likelihood ratio
logLik(fit6) - logLik(fit5)

## drop1 nested model comparison
drop1(fit6, test = "F")

sum(residuals(lm(Fertility ~ Education + Catholic, data = swiss))^2)
sum(residuals(lm(Fertility ~ Agriculture + Catholic, data = swiss))^2)
sum(residuals(lm(Fertility ~ Agriculture + Education, data = swiss))^2)

## Sequential anova: Type I sums of squares
anova(fit6)
sum(residuals(lm(Fertility ~ 1, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture, data = swiss))^2)
sum(residuals(lm(Fertility ~ Agriculture, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture + Education, data = swiss))^2)
sum(residuals(lm(Fertility ~ Education + Agriculture, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture + Education + Catholic, data = swiss))^2)
sum(residuals(lm(Fertility ~ Agriculture + Education + Catholic, data = swiss))^2)

fit6 <- lm(Fertility ~ Agriculture + Education + Catholic, data = swiss)
fit6a <- lm(Fertility ~ Catholic + Agriculture + Education, data = swiss)
anova(fit6a)

## proportion decrease in error
(RSS_5 - RSS_6) / RSS_5
fit7 <- lm(Fertility ~ 1, data = swiss) # null model
RSS_7 <- sum(residuals(fit7)^2)

(RSS_7 - RSS_6) / RSS_7 # R^2 of fit6

anova(fit7, fit6) # global test

## Nested model comparison in GLMs

swiss_df <- swiss %>%
  mutate(z = Fertility > median(Fertility))

fit8 <- glm(z ~ Agriculture + Education, 
            family = binomial,
            data = swiss_df)

fit9 <- glm(z ~ Agriculture + Education + Catholic, 
            family = binomial,
            data = swiss_df)

logLik(fit8)
logLik(fit9)
logLik(fit9) - logLik(fit8)

deviance(fit8)
deviance(fit9)
deviance(fit8) - deviance(fit9)

summary(fit9)

## deviance comparison in glm's using a chi-squared test
anova(fit8, fit9, test = "Chisq")

## p-value manually calculated
pchisq(deviance(fit8) - deviance(fit9), df = 1, lower.tail = F)

## one continuous and one categorical predictor

samara_df <- read_table("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/data/samara.txt")
samara_df <- samara_df %>%
  mutate(Tree = factor(Tree))

samara_df %>%
  ggplot(aes(x = Load, y = Velocity, col = Tree)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## null model
fit10 <- lm(Velocity ~ 1, data = samara_df)
## lines parallel to the x-axis
fit11 <- lm(Velocity ~ Tree, data = samara_df)
## equal lines model
fit12 <- lm(Velocity ~ Load, data = samara_df)
## parallel lines model
fit13 <- lm(Velocity ~ Tree + Load, data = samara_df)
## common intercepts model
fit14 <- lm(Velocity ~ Tree : Load, data = samara_df)
## separate lines model
fit15 <- lm(Velocity ~ Tree * Load, data = samara_df)

library(modelr)
samara_df %>%
  add_predictions(model = fit10, var = "M0") %>%
  add_predictions(model = fit11, var = "M1") %>%
  add_predictions(model = fit12, var = "M2") %>%
  add_predictions(model = fit13, var = "M3") %>%
  add_predictions(model = fit14, var = "M4") %>%
  add_predictions(model = fit15, var = "M5") %>%
  pivot_longer(cols = 4:9,
               names_to = "model",
               values_to = "pred") %>%
  ggplot(aes(x = Load, y = Velocity, col = Tree)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~ model)

anova(fit10, fit11, fit13, fit15)
anova(fit10, fit12, fit13, fit15)
anova(fit10, fit12, fit14, fit15)

## Out-of-Sample Predictive Performance ----------------------------------------

source("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/scripts/helper_functions.R")

## leave-one-out cross validation

## read in housing data
housing_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/data/housing.csv")
housing_df <- housing_df %>%
  mutate(logprice = log(price))

housing_df %>%
  ggplot(aes(x = price)) +
  theme_bw() +
  geom_histogram(bins = 50, col = "white", lwd = .2)

housing_df %>%
  ggplot(aes(x = logprice)) +
  theme_bw() +
  geom_histogram(bins = 50, col = "white", lwd = .2)

## normal model
fit16 <- lm(price ~ 1, data = housing_df)
## lognormal model
fit17 <- lm(logprice ~ 1, data = housing_df)

lm_loo_cv(fit16)
lm_loo_cv(fit17)

# deviance scale
lm_loo_cv(fit16, deviance_scale = TRUE) # -2 * elpd of fit16
lm_loo_cv(fit17, deviance_scale = TRUE) # -2 * elpd of fit17

lm_loo_cv(fit16, deviance_scale = TRUE) - lm_loo_cv(fit17, deviance_scale = TRUE)

## AIC
## AIC \approx. -2 * elpd, however the approx doesn't hold well for more complex models

logLik(fit16)      # log likelihood 
-2 * logLik(fit16) # deviance (-2 x log likelihood)
2 * 2 - 2 * logLik(fit16) # AIC: 2K + deviance
AIC(fit16) # using built in AIC

AIC(fit17)

## estimate standard errors for elpd
lm_loo_cv(fit16, deviance_scale = TRUE, se = TRUE)
lm_loo_cv(fit17, deviance_scale = TRUE, se = TRUE)
## can do the same for AIC using bootstrap, for instance
## WAIC will give you se's as well
## important to take into account uncertainty when using threshold rules

## Small sample correction

AIC(fit16)
AICc(fit16)

AIC(fit17)
AICc(fit17)

## Nonlinear regression with splines
library(splines)
library(modelr)

vocab_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/data/vocab.csv")
vocab_df

vocab_df %>%
  ggplot(aes(x = age, y = vocab)) +
  theme_bw() +
  geom_point()

stats_vocab <- tibble(df = 1:20,
                      RSS = rep(NA, 20),
                      R2 = rep(NA, 20),
                      AICc = rep(NA, 20))
                      
for(i in 1:20) {
  model <- lm(vocab ~ poly(age, i),
              data = vocab_df)
  stats_vocab$RSS[i] <- sum(residuals(model)^2)
  stats_vocab$R2[i] <- summary(model)$r.squared
  stats_vocab$AICc[i] <- AICc(model)
}

stats_vocab %>%
  pivot_longer(2:4,
               names_to = "gof_measure",
               values_to = "value") %>%
  ggplot(aes(x = df, y = value)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~ gof_measure,
             scales = "free_y")

vocab_df %>%
  add_predictions(model = lm(vocab ~ poly(age, 3),
                             data = vocab_df)) %>%
  ggplot(aes(x = age, y = vocab)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred))
## replace poly with ns
## show that with df = 71 it is a saturated model

stats_vocab$AICc
akaike_weights(stats_vocab$AICc) %>% round(2)
akaike_weights(stats_vocab$AICc) %>% sum

vocab_df_pred <- vocab_df

for(i in 1:20) {
  vocab_df_pred <- vocab_df_pred %>%
    add_predictions(model = lm(vocab ~ poly(age, i),
                               data = vocab_df),
                    var = paste("df =", i))
}

all_pred <- vocab_df_pred %>%
  dplyr::select(- age, - vocab) %>%
  as.matrix

vocab_df$wpred <- (t(all_pred) * akaike_weights(stats_vocab$AICc)) %>%
  colSums

vocab_df %>%
  add_predictions(model = lm(vocab ~ ns(age, 6),
                             data = vocab_df)) %>%
  ggplot(aes(x = age, y = vocab)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred)) +
  geom_line(aes(y = wpred), col = 2)

## Variable Selection ----------------------------------------------------------

## reading student dataset
student_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/data/student.csv")
student_df

fit18 <- lm(math ~ ., data = student_df)

fit18_step_back <- step(fit18,
                        direction = "backward")
AIC(fit18) # notice how this is different to the reported AIC in step (dropping constant terms)

fit19 <- lm(math ~ 1, data = student_df)
fit19_step_forward <- step(fit19,
                           direction = "forward",
                           scope = formula(fit18))

fit20_step_both <- step(fit18,
                        direction = "both")
fit20_step_both

fit21_step_both <- step(fit19,
                        direction = "both",
                        scope = formula(fit18))

2^27

## all subsets regression
library(MuMIn)

fit22 <- lm(Fertility ~ .,
            data = swiss,
            na.action = "na.fail")
fit22_all_subsets <- dredge(fit22)

conf_set <- get.models(fit22_all_subsets, cumsum(weight) < 0.95)
conf_set

## regularisation

## avoid overfitting of noise, e.g.
tibble(age = seq(18, 89, length = 200)) %>%
  add_predictions(model = lm(vocab ~ ns(age, 27),
                             data = vocab_df),
                  var = "vocab") %>%
  ggplot(aes(x = age, y = vocab)) +
  theme_bw() +
  geom_point(data = vocab_df) +
  geom_line()

library(glmnet)

## LASSO
y <- student_df$math
X <- as.matrix(dplyr::select(student_df, - math))

fit23_lasso <- glmnet(X, y, alpha = 1)
plot(fit23_lasso, xvar = "lambda", label = TRUE)

fit23_lasso_cv <- cv.glmnet(X, y, alpha = 1)
plot(fit23_lasso_cv)

## 2nd = more regularised that is within 1 se of smallest MSE
coef(fit23_lasso, 
     s = c(fit23_lasso_cv$lambda.min,
           fit23_lasso_cv$lambda.1se))

## comparing with stepwise
formula(fit20_step_both)

## Ridge regression
fit24_ridge <- glmnet(X, y, alpha = 0)
plot(fit24_ridge, xvar = "lambda", label = TRUE)

fit24_ridge_cv <- cv.glmnet(X, y, alpha = 0)
plot(fit24_ridge_cv)

coef(fit24_ridge, 
     s = c(fit24_ridge_cv$lambda.min,
           fit24_ridge_cv$lambda.1se))

## careful with scaling vs. non-scaling
apply(student_df, 2, range)

fit25_lasso <- glmnet(scale(X), y, alpha = 1)
fit25_lasso_cv <- cv.glmnet(scale(X), y, alpha = 1)
plot(fit25_lasso_cv)

cbind(
  coef(fit23_lasso, 
       s = c(fit23_lasso_cv$lambda.1se)),
  coef(fit25_lasso, 
       s = c(fit25_lasso_cv$lambda.1se))
  )

## Bayesian regularised variable selection -------------------------------------

student_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/data/student_scaled.csv")

library(brms)
fit26 <- brm(math ~ .,
             data = student_df,
             prior = set_prior("horseshoe(df=3)"))
fit26

library(bayesplot)
mcmc_areas(fit26)
mcmc_areas(fit26,
           regex_pars = "^b_[safPMFtpnhirg]",
           prob = 0.95)

## another example
sleep_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/data/sleepstudy.csv")

sleep_df %>%
  ggplot(aes(x = Days, y = Reaction)) + 
  geom_point() + 
  stat_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~ Subject)


# Bayesian Linear Regression
fit27 <- brm(Reaction ~ Days, data = sleep_df)

# normal linear mixed effects
fit28 <- brm(Reaction ~ Days + (Days | Subject), data = sleep_df)

# robust linear mixed effects
fit29 <- brm(Reaction ~ Days + (Days | Subject), 
             family = student(),
             data = sleep_df)

waic(M27)
waic(M28)
waic(M29)

loo_compare(waic(M27), waic(M28), waic(M29))