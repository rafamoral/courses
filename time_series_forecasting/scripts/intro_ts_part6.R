library(tidyverse)
library(R2jags)
library(rstan)

# Models for continuous time ----------------------------------------------

## Brownian motion in JAGS

## reading ice core data
icecore_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/time_series_forecasting/data/GISP2_20yr.csv")

icecore_df %>%
  ggplot(aes(x = Age, y = Del_18O)) +
  theme_bw() +
  geom_line()

model_code = "
model
{
  # Likelihood
  for (i in 2:T) {
    y[i] ~ dnorm( alpha * (t[i] - t[i-1]) + y[i-1], 
                  sigma_rw[i]^-2 )
    sigma_rw[i] <- sigma * sqrt(t[i] - t[i-1])
  }
  
  # Priors
  alpha ~ dnorm(0, 10^-2)
  sigma ~ dunif(0, 10)
}
"

bm_run <- jags(data = list(y = icecore_df$Del_18O,
                           t = icecore_df$Age,
                           T = nrow(icecore_df)),
               parameters.to.save = c("alpha", "sigma"),
               model.file = textConnection(model_code))

hist(bm_run$BUGSoutput$sims.list$alpha, breaks = 30)
hist(bm_run$BUGSoutput$sims.list$sigma, breaks = 30)

## Interpolation
## We can use the NA trick to create a new set of times at
## which we wish to predict
## We need to be careful that we don't give JAGS any time values
## which have 0 differences as this will cause it to crash

t_ideal <- seq(.01, max(icecore_df$Age) + .01, by = 100) # 100-point grid
y_ideal <- rep(NA, length(t_ideal))
t_all <- c(icecore_df$Age, t_ideal)
y_all <- c(icecore_df$Del_18O, y_ideal)

## checking
t_all[order(t_all)][1:10]
y_all[order(t_all)][1:10]

bm_run_2 <- jags(data = list(y = y_all[order(t_all)],
                             t = t_all[order(t_all)],
                             T = length(y_all)),
                 parameters.to.save = "y",
                 model.file = textConnection(model_code))

post_y <- bm_run_2$BUGSoutput$sims.list$y
pick_out <- which(is.na(y_all[order(t_all)]))

pred_df <- tibble(time = t_ideal,
                  y_hat = colMeans(post_y[, pick_out]))

icecore_df %>%
  ggplot(aes(x = Age, y = Del_18O)) +
  theme_bw() +
  geom_point(size = .2) +
  geom_line(data = pred_df,
            aes(x = t_ideal, y = y_hat),
            col = 4) + 
  xlim(6e4, 8e4)

icecore_df %>%
  ggplot(aes(x = Age, y = Del_18O)) +
  theme_bw() +
  geom_point(size = .2) +
  geom_line(data = pred_df,
            aes(x = t_ideal, y = y_hat),
            col = 4)

## fitting Ornstein-Uhlenbeck model

model_code = "
model
{
  # Likelihood
  for (i in 2:T) {
    y[i] ~ dnorm( theta * (alpha - y[i-1]) * 
                  (t[i] - t[i-1]) + y[i-1], 
                  sigma_ou[i]^-2 )
    sigma_ou[i] <- sigma * sqrt(t[i] - t[i-1])
  }

  # Priors
  alpha ~ dnorm(0, 100^-2)
  theta ~ dunif(0, 1)
  sigma ~ dunif(0, 100)
}
"

ou_run <- jags(data = list(y = icecore_df$Del_18O,
                           t = icecore_df$Age,
                           T = nrow(icecore_df)),
               parameters.to.save = c("alpha", "theta", "sigma"),
               model.file=textConnection(model_code))

print(ou_run)
R2jags::traceplot(ou_run)

hist(ou_run$BUGSoutput$sims.list$alpha, breaks = 30)
hist(ou_run$BUGSoutput$sims.list$theta, breaks = 30)
hist(ou_run$BUGSoutput$sims.list$sigma, breaks = 30)

bm_dic <- print(bm_run)
ou_dic <- print(ou_run)

with(bm_dic, print(c(DIC, pD)))
with(ou_dic, print(c(DIC, pD)))

# Multivariate time series ------------------------------------------------

## vector autoregression

## reading the temperature and sea level datasets

# HadCRUT is the dataset of worldwide monthly instrumental temperature
# records formed by combining the sea surface temperature records compiled
# by the Hadley Centre of the UK Met Office and the land surface air
# temperature records compiled by the Climatic Research Unit (CRU) of
# the University of East Anglia.

temp_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/time_series_forecasting/data/hadcrut.csv")

temp_df %>%
  ggplot(aes(x = Year, y = Anomaly)) +
  theme_bw() +
  geom_line()

sealevel_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/time_series_forecasting/data/tide_gauge.csv")
## correcting sea level ages
sealevel_df <- sealevel_df %>%
  mutate(Year2 = year_AD - 0.5)

## merge them together
bivariate_df <- merge(temp_df, sealevel_df, by.x='Year', by.y='Year2') %>%
  dplyr::select(Year, Anomaly, SeaLevel = sea_level_m)

## fitting VAR(1) model

model_code = "
model
{
  # Likelihood
  for (t in 2:T) {
    y[t, ] ~ dmnorm(mu[t, ], Sigma.Inv[,])
    mu[t, 1:k] <- A + Phi %*% y[t-1,]
  }
  Sigma <- inverse(Sigma.Inv[,]) 

  # Priors
  Sigma.Inv ~ dwish(I, k+1)
  for(i in 1:k) {
    A[i] ~ dnorm(0, 20^-2)
    for(j in 1:k) {
      Phi[i,j] ~ dunif(-1,1)
    }
  }
}
"

n_forecast <- 10

## we will difference the data for VAR
jags_data <- with(bivariate_df,
                  list(T = nrow(bivariate_df) + n_forecast - 1,
                       y = rbind(as.matrix(apply(bivariate_df[,c("Anomaly", "SeaLevel")], 2, "diff")),
                                 matrix(NA, ncol = 2, nrow = n_forecast)),
                       k = 2, ## number of time series
                       I = diag(2)))

var_run <- jags(data = jags_data,
                parameters.to.save = c("A","Phi","y"),
                model.file = textConnection(model_code))

plot(var_run)

y_future_post <- var_run$BUGSoutput$sims.list$y
dim(y_future_post) ## a 3-d array, MCMC samples x N x K
y_future_med <- apply(y_future_post, c(2,3), "median")
year_all <- c(bivariate_df$Year[-1], 2010:(2010 + n_forecast))

bivariate_df_pred <- tibble(Year = year_all[-1],
                            Anomaly = bivariate_df$Anomaly[1] + cumsum(y_future_med[,1]),
                            SeaLevel = bivariate_df$SeaLevel[1] + cumsum(y_future_med[,2]))

bivariate_df %>%
  pivot_longer(2:3,
               names_to = "variable",
               values_to = "y") %>%
  ggplot(aes(x = Year, y = y)) +
  theme_bw() +
  geom_point(size = .2) +
  facet_wrap(~ variable) +
  geom_line(data = bivariate_df_pred %>%
              pivot_longer(2:3,
                           names_to = "variable",
                           values_to = "y"),
            col = 4)