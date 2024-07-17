library(tidyverse)
library(R2jags)
library(rstan)


# Bayesian modelling with JAGS and Stan -----------------------------------

## Bayesian modelling with JAGS

sealevel_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/time_series_forecasting/data/tide_gauge.csv")

sealevel_df %>%
  ggplot(aes(x = year_AD, y = sea_level_m)) +
  theme_bw() +
  geom_line()

jags_code <- "
model {
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(alpha + beta*x[i], sigma^-2)
  }
  # Priors
  alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 100^-2)
  sigma ~ dunif(0, 100)
}"

jags_run <- jags(data = list(N = nrow(sealevel_df), 
                             y = sealevel_df$sea_level_m,
                             x = sealevel_df$year_AD/1000),
                 parameters.to.save = c("alpha", "beta",
                                        "sigma"),
                 model.file = textConnection(jags_code))

print(jags_run)
plot(jags_run)

## we now have access to the posterior distribution of the parameters
post <- jags_run$BUGSoutput$sims.matrix
head(post)

alpha_mean <- mean(post[,'alpha'])
beta_mean <- mean(post[,'beta'])
plot(sealevel_df$year_AD/1000, sealevel_df$sea_level_m)
lines(sealevel_df$year_AD/1000,
      alpha_mean + beta_mean * sealevel_df$year_AD/1000, col = "red")

## Bayesian modelling with Stan

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

stan_run <- stan(data = list(N = nrow(sealevel_df), 
                             y = sealevel_df$sea_level_m,
                             x = sealevel_df$year_AD/1000),
                 model_code = stan_code)

print(stan_run)
plot(stan_run)

## An AR(1) model in JAGS

## reading the sheep data (in millions)
sheep_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/time_series_forecasting/data/sheep.csv")

sheep_df %>%
  ggplot(aes(x = year, y = sheep)) +
  theme_bw() +
  geom_line()

jags_code <- "
model {
  # Likelihood
  for (t in 2:N) {
    y[t] ~ dnorm(alpha + beta * y[t-1], sigma^-2)
  }

  # Priors
  alpha ~ dnorm(0, 100^-2)
  beta ~ dunif(-1, 1)
  sigma ~ dunif(0, 100)
}
"

jags_run <- jags(data = list(N = nrow(sheep_df), 
                             y = sheep_df$sheep),
                 parameters.to.save = c("alpha", "beta", "sigma"),
                 model.file = textConnection(jags_code))

print(jags_run)
plot(jags_run)

## fitted values

post <- jags_run$BUGSoutput$sims.matrix
alpha_mean <- mean(post[,'alpha'])
beta_mean <- mean(post[,'beta'])
sheep_df <- sheep_df %>%
  mutate(sheep_forecast = c(NA, alpha_mean + beta_mean * sheep[1:(n() - 1)]))

sheep_df %>%
  ggplot(aes(x = year, y = sheep)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = sheep_forecast),
            col = 4)

## one-step ahead forecast
alpha_mean + beta_mean * sheep_df$sheep[nrow(sheep_df)]

## plotting the iterations
R2jags::traceplot(jags_run)

## creating credible intervals (Bayesian confidence intervals):
apply(post, 2, quantile, probs = c(0.025, 0.975))
hist(post[,'beta'], breaks = 30)

## random walk in JAGS

jags_code <- "
model {
  # Likelihood
  for (t in 2:N) {
    y[t] ~ dnorm(alpha + y[t-1], sigma^-2)
    y_pred[t] ~ dnorm(alpha + y[t-1], sigma^-2)
  }

  # Priors
  alpha ~ dnorm(0, 100^-2)
  sigma ~ dunif(0, 100)
}
"

jags_run <- jags(data = list(N = nrow(sheep_df), 
                             y = sheep_df$sheep),
                 parameters.to.save = c("alpha", "sigma", "y_pred"),
                 model.file = textConnection(jags_code))

print(jags_run)
plot(jags_run)

post <- jags_run$BUGSoutput$sims.matrix

sheep_df <- sheep_df %>%
  mutate(sheep_rw = c(NA, colMeans(post[,4:49])),
         sheep_rw_lower = c(NA, apply(post[,4:49], 2, quantile, .025)),
         sheep_rw_upper = c(NA, apply(post[,4:49], 2, quantile, .975)))

sheep_df %>%
  ggplot(aes(x = year, y = sheep)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = sheep_rw),
            col = 4) +
  geom_ribbon(aes(ymin = sheep_rw_lower,
                  ymax = sheep_rw_upper),
              fill = 4,
              alpha = .2)

## an MA(1) model in JAGS

jags_code <- "
model {
  # Likelihood
  e[1] ~ dnorm(0, sigma^-2)
  y[1] ~ dnorm(alpha, sigma^-2)
  y_pred[1] <- alpha + e[1]
  for (t in 2:N) {
    y[t] ~ dnorm(alpha + theta * e[t-1], sigma^-2)
    y_pred[t] ~ dnorm(alpha + theta * e[t-1], sigma^-2)
    e[t] ~ dnorm(0, sigma^-2)
  }

  # Priors
  alpha ~ dnorm(0, 100^-2)
  theta ~ dunif(-1, 1)
  sigma ~ dunif(0, 100)
}
"

e <- rnorm(100, 0, .2)
y_sim <- 2 + e[1]
for(t in 2:100) y_sim[t] <- 2 + .5 * e[t-1] + e[t]

plot.ts(y_sim)

jags_run <- jags(data = list(N = 100, 
                             y = y_sim),
                 parameters.to.save = c("alpha", "theta", "sigma", "y_pred"),
                 model.file = textConnection(jags_code),
                 n.iter = 10000,
                 n.burnin = 2000,
                 n.thin = 5)

print(jags_run)
plot(jags_run)

post <- jags_run$BUGSoutput$sims.matrix

sim_df <- tibble(time = 1:100,
                 y = y_sim,
                 yhat = colMeans(post[,5:104]),
                 lower = apply(post[,5:104], 2, quantile, .025),
                 upper = apply(post[,5:104], 2, quantile, .975))
  
sim_df %>%
  ggplot(aes(x = time, y = y)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = yhat),
            col = 4) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              fill = 4,
              alpha = .2)


# ARCH, GARCH, Stochastic volatility --------------------------------------

## reading forest fires data

