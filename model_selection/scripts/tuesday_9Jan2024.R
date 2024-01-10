library(tidyverse)

# Measuring model fit -----------------------------------------------------

## height data
y <- c(180, 191, 162, 171)

mu_hat <- mean(y)
sigma_hat <- sd(y)

## assumption: Y ~ N(176, 12.4^2)

dnorm(y, mean = mu_hat, sd = sigma_hat)
prod(dnorm(y, mean = mu_hat, sd = sigma_hat))

log(dnorm(y, mean = mu_hat, sd = sigma_hat))
sum(log(dnorm(y, mean = mu_hat, sd = sigma_hat)))

## what is the likelihood that the data came from a normal distr.
## with mean mu = 190
prod(dnorm(y, mean = 190, sd = sigma_hat))
sum(log(dnorm(y, mean = 190, sd = sigma_hat)))

## Note: dnorm() is the prob. density function of the normal distr.
curve(dnorm(x), xlim = c(-3,3))
dnorm(-2)

## using the MLE for sigma
n <- length(y)
sigma_hat_mle <- sd(y) * sqrt((n - 1) / n)

sum(log(dnorm(y, mean = mu_hat, sd = sigma_hat)))
sum(log(dnorm(y, mean = mu_hat, sd = sigma_hat_mle)))

## the cars dataset

cars %>%
  ggplot(aes(x = dist)) +
  theme_bw() +
  geom_histogram(col = "white")

## fitting a normal model without covariates (same as we did before,
## but using the lm function now)

fit1 <- lm(dist ~ 1, data = cars)
coef(fit1)
mean(cars$dist)

sigma(fit1)
sd(cars$dist)

sigma_mle <- sqrt(mean(residuals(fit1)^2))
sum(log(dnorm(cars$dist, mean = coef(fit1), sd = sigma_mle)))
logLik(fit1)

## a normal regression model

cars %>%
  ggplot(aes(y = dist, x = speed)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = coef(fit1), col = 2)

fit2 <- lm(dist ~ speed, data = cars)
coef(fit2)

mu_hat <- predict(fit2) ## fitted(fit2)
sigma_mle <- sqrt(mean(residuals(fit2)^2))

sum(log(dnorm(cars$dist, mean = mu_hat, sd = sigma_mle)))
logLik(fit2)

## a likelihood ratio

logLik(fit2) - logLik(fit1)
exp(logLik(fit2) - logLik(fit1))

## residual sums of squares

residuals(fit2)
cars$dist - predict(fit2)

RSS_fit1 <- sum(residuals(fit1)^2)
RSS_fit2 <- sum(residuals(fit2)^2)

n <- nrow(cars)
-(n/2) * (log(2*pi) - log(n) + log(RSS_fit1) + 1)
logLik(fit1)

-(n/2) * (log(2*pi) - log(n) + log(RSS_fit2) + 1)
logLik(fit2)

logLik(fit2) - logLik(fit1)
(log(RSS_fit2) - log(RSS_fit1)) * (-n/2)

## RMSE (this measure translates as the uncertainty)
## (square root brings it to the same scale as the response)

sqrt(mean(residuals(fit1)^2))
sqrt(mean(residuals(fit2)^2))

## MAE (same scale as the response)

mean(abs(residuals(fit1)))
mean(abs(residuals(fit2)))

## Deviances and an example using logistic regression

cars_df <- cars %>%
  mutate(z = dist > median(dist))

## constant probability of being above the median distance

fit3 <- glm(z ~ 1,
            family = binomial(link = "logit"),
            data = cars_df)

## prob of being above median distance is a function of speed

fit4 <- glm(z ~ speed,
            family = binomial(link = "logit"),
            data = cars_df)

logLik(fit3) # prob. of observing the outcome given fit3
logLik(fit4) # prob. of observing the outcome given fit4

logLik(fit4) - logLik(fit3)
exp(16.88)
## approx. 21 million times more likely

-2 * logLik(fit3)
-2 * logLik(fit4)
(deviance(fit3) - deviance(fit4)) / 2 # same as logLik ratio

sum(residuals(fit3, type = "deviance")^2)
deviance(fit3)
- 2 * logLik(fit3)

# Nested model comparisons ------------------------------------------------

fit5 <- lm(Fertility ~ Agriculture + Education, data = swiss)
fit6 <- lm(Fertility ~ Agriculture + Education + Catholic, data = swiss)

RSS_5 <- sum(residuals(fit5)^2)
RSS_6 <- sum(residuals(fit6)^2)

RSS_5
RSS_6

(RSS_5 - RSS_6) / RSS_6

## we can refer to this as an "effect size", which multiplied
## by df1/(df0 - df1) (which is a "sample effect"), gives the F statistic

## F statistic
F_stat <- ((RSS_5 - RSS_6) / 1) / (RSS_6 / fit6$df.residual)

## reference distribution: F(1, 43)
curve(df(x, df1 = 1, df2 = fit6$df.residual), xlim = c(0, 40))
abline(v = F_stat, lty = 2)

anova(fit5, fit6)
pf(F_stat, df1 = 1, df2 = fit6$df.residual, lower.tail = FALSE)

## likelihood ratio
logLik(fit6) - logLik(fit5)
exp(10.14)

## drop1 nested model comparison (akin to type II)
drop1(fit6, test = "F") ## start with maximal model, dropping one at a time

## add1 model comparisons
## start with null model, add one at a time
add1(lm(Fertility ~ 1, data = swiss),
     scope = ~ Agriculture + Education + Catholic, test = "F")

## sequential ANOVA (type I)
anova(fit6)
sum(residuals(lm(Fertility ~ 1, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture, data = swiss))^2)
sum(residuals(lm(Fertility ~ Agriculture, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture + Education, data = swiss))^2)
sum(residuals(lm(Fertility ~ Agriculture + Education, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture + Education + Catholic, data = swiss))^2)
sum(residuals(lm(Fertility ~ Agriculture + Education + Catholic, data = swiss))^2)

fit6 <- lm(Fertility ~ Agriculture + Education + Catholic, data = swiss)
fit6a <- lm(Fertility ~ Catholic + Agriculture + Education, data = swiss)

logLik(fit6)
logLik(fit6a)

coef(fit6)
coef(fit6a)

anova(fit6)
anova(fit6a)

drop1(fit6, test = "F")
drop1(fit6a, test = "F")

## R^2: proportional decrease in error
## fitting the NULL model
fit7 <- lm(Fertility ~ 1, data = swiss)
RSS_7 <- sum(residuals(fit7)^2)

(RSS_7 - RSS_6) / RSS_7
## R^2 is a measure that compares the model you are fitting ("current" model)
## to the null model (i.e. how much variability have we explained when
## we add the covariates?)
## if we continue to add covariates, R^2 will continue to increase

## Adjusted R^2
y <- swiss$Fertility
ybar <- mean(swiss$Fertility)
TSS <- sum((y - ybar)^2)

1 - RSS_6/TSS * 46 / 43

summary(fit6)

## R^2 as the correlation between observed and fitted values
yhat <- predict(fit6)
cor(y, yhat)^2 ## this is the R^2

## global test
fit7 ## null model
fit6 ## model including agriculture, education and catholic
anova(fit7, fit6)