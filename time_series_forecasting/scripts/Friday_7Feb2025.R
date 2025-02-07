library(tidyverse)
library(fpp3)
library(mgcv)


# Generalized additive modelling ('ctd) -----------------------------------

## example: seasonal time series -- Queensland bulls
queensland_bulls <- aus_livestock %>%
  filter(Animal == "Bulls, bullocks and steers",
         State == "Queensland") %>%
  select(- Animal, - State) %>%
  mutate(time = 1:n(),
         month = month(Month))

queensland_bulls %>%
  gg_season(period = "year")

queensland_bulls_train <- queensland_bulls %>%
  filter(year(Month) < 2018)
queensland_bulls_test <- queensland_bulls %>%
  filter(year(Month) == 2018)

fit <- gam(Count ~ s(time, k = 30) + s(month, bs = "cc"),
           family = nb,
           data = queensland_bulls_train)
summary(fit)

library(gratia)
draw(fit)

queensland_bulls %>%
  mutate(pred = predict(fit, newdata = queensland_bulls, type = "response")) %>%
  autoplot(Count) +
  theme_minimal() +
  geom_line(data = queensland_bulls_test, col = 4) +
  geom_line(aes(y = pred), col = 2)

## if we wanted to calculate, say, RMSE by hand
queensland_bulls %>%
  mutate(pred = predict(fit, newdata = queensland_bulls, type = "response")) %>%
  filter(year(Month) == 2018) %>%
  mutate(squared_diff = (Count - pred)^2) %>%
  pull(squared_diff) %>%
  mean() %>%
  sqrt()

## fit a GAM with a 2-d spline (or tensor product/smooth)
fit2 <- gam(Count ~ te(time, month),
            family = nb,
            data = queensland_bulls_train)
summary(fit2)

draw(fit2)

queensland_bulls %>%
  mutate(pred = predict(fit2, newdata = queensland_bulls, type = "response")) %>%
  autoplot(Count) +
  theme_minimal() +
  geom_line(data = queensland_bulls_test, col = 4) +
  geom_line(aes(y = pred), col = 2)

queensland_bulls %>%
  mutate(pred = predict(fit2, newdata = queensland_bulls, type = "response")) %>%
  filter(year(Month) == 2018) %>%
  mutate(squared_diff = (Count - pred)^2) %>%
  pull(squared_diff) %>%
  mean() %>%
  sqrt()

AIC(fit) ## it seems like the first model has better predictive ability / fit
AIC(fit2) ## the 2d tensor smooth is not an improvement over separate TPRS and cyclic splines

appraise(fit, method = "simulate")

## let's attempt to incorporate **some** autocorrelation
fit3 <- gamm(Count ~ s(time, k = 30) + s(month, bs = "cc"),
             family = nb,
             data = queensland_bulls_train %>%
               mutate(year = factor(year(Month))),
             correlation = corARMA(form = ~ 1 | year, p = 1))
summary(fit3$gam)
summary(fit3$lme)

queensland_bulls %>%
  mutate(pred = predict(fit3$gam, newdata = queensland_bulls, type = "response")) %>%
  autoplot(Count) +
  theme_minimal() +
  geom_line(data = queensland_bulls_test, col = 4) +
  geom_line(aes(y = pred), col = 2)

queensland_bulls %>%
  mutate(pred = predict(fit3$gam, newdata = queensland_bulls, type = "response")) %>%
  filter(year(Month) == 2018) %>%
  mutate(squared_diff = (Count - pred)^2) %>%
  pull(squared_diff) %>%
  mean() %>%
  sqrt()


# Bayesian modelling ------------------------------------------------------

curve(dnorm(x, 10, 2), xlim = c(0, 20))

hist(my_sample <- rnorm(5000, 10, 2), prob = TRUE)

mean(my_sample)
quantile(my_sample, c(.025, .975))
qnorm(c(.025, .975), 10, 2)

library(R2jags)
library(rstan)

## Bayesian modelling using JAGS

sealevel_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/time_series_forecasting/data/tide_gauge.csv")

sealevel_df %>%
  ggplot(aes(x = year_AD, y = sea_level_m)) +
  theme_bw() +
  geom_line()

jags_code <- "
model {
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(mu[i], sigma^-2)
    mu[i] = alpha + beta * x[i]
  }
  # Priors
  alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 100^-2)
  sigma ~ dunif(0, 100)
}
"

jags_data <- list(N = nrow(sealevel_df),
                  y = sealevel_df$sea_level_m,
                  x = sealevel_df$year_AD/1000)

jags_run <- jags(data = jags_data,
                 parameters.to.save = c("alpha","beta","sigma"),
                 model.file = textConnection(jags_code))

print(jags_run)
plot(jags_run)

## we also have access to the posterior distributions of the parameters
post <- jags_run$BUGSoutput$sims.matrix
head(post)

hist(post[,"alpha"])
quantile(post[,"alpha"], c(.025, .975))

hist(post[,"beta"])
## what is the (posterior) probability that the slope is > 1.5?
sum(post[,"beta"] > 1.5) / 3000

## what is the (posterior) probability that the slope is < 1.5?
sum(post[,"beta"] < 1.5) / 3000

plot(sealevel_df$year_AD/1000, sealevel_df$sea_level_m)
lines(sealevel_df$year_AD/1000,
      mean(post[,"alpha"]) + mean(post[,"beta"]) * sealevel_df$year_AD/1000,
      col = 2, lwd = 3)

## trace plot
plot.ts(post[,"alpha"]) # what you want to see: "hairy caterpillar" pattern
plot.ts(post[,"beta"])
plot.ts(post[,"sigma"])

## Bayesian modelling using STAN

stan_code <- "
data {
  int<lower=0> N;
  vector[N] y;
  vector[N] x;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  y ~ normal(alpha + beta * x, sigma);
  alpha ~ normal(0, 100);
  beta ~ normal(0, 100);
  sigma ~ uniform(0, 100);
}
"

stan_data <- list(N = nrow(sealevel_df),
                  y = sealevel_df$sea_level_m,
                  x = sealevel_df$year_AD/1000)

stan_run <- stan(data = stan_data,
                 model_code = stan_code)

print(stan_run)
plot(stan_run)
traceplot(stan_run)

## AR(1), MA(1), random walk models; ARCH and GARCH models
## found in pre-coded examples from the scripts in intro_ts_part5.R

# Continuous-time models --------------------------------------------------

## take the random walk in discrete time
rw <- function() {
  y <- numeric(100)
  y[1] <- 0
  for(i in 2:100) y[i] <- y[i-1] + rnorm(1)
  return(y)
}

plot.ts(rw(), ylim = c(-20, 20))
lines(rw())

## Brownian motion and Ornstein Uhlenbeck models in JAGS
## found in pre-coded examples from the scripts in intro_ts_part6.R
