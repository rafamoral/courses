library(tidyverse)
library(R2jags)
library(rstan)

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
sheep_df <- read.csv("https://raw.githubusercontent.com/rafamoral/courses/main/time_series_forecasting/data/sheep.csv")