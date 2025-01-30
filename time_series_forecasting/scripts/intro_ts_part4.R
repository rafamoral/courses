library(fpp3)
library(tidyverse)

## Salary data
sal <- read.table("https://raw.githubusercontent.com/rafamoral/courses/main/time_series_forecasting/data/salary.txt",
                  header = TRUE)

sal %>%
  ggplot(aes(x = pexp, y = salary)) +
  theme_bw() +
  geom_point() +
  geom_smooth(se = FALSE)

fit1 <- lm(salary ~ pexp, data = sal)
summary(fit1)

library(splines)
fit2 <- lm(salary ~ bs(pexp, df = 3), data = sal)
summary(fit2)

fit3 <- lm(salary ~ ns(pexp, df = 3), data = sal)
summary(fit3)

library(modelr)
sal_pred <- tibble(pexp = 0:400) %>%
  add_predictions(model = fit1, var = "linear") %>%
  add_predictions(model = fit2, var = "cubic") %>%
  add_predictions(model = fit3, var = "natural")

sal %>%
  ggplot(aes(x = pexp, y = salary)) +
  theme_bw() +
  geom_point() +
  geom_line(data = sal_pred %>%
              pivot_longer(2:4,
                           names_to = "model",
                           values_to = "salary"),
            aes(col = model))

library(mgcv)
fit4 <- gam(salary ~ s(pexp), data = sal)
summary(fit4)
plot(fit4)

library(gratia)
draw(fit4)
appraise(fit4, method = "simulate")

sal_pred <- sal_pred %>%
  add_predictions(model = fit4, var = "thin_plate")
  
sal %>%
  ggplot(aes(x = pexp, y = salary)) +
  theme_bw() +
  geom_point() +
  geom_line(data = sal_pred %>%
              pivot_longer(2:5,
                           names_to = "model",
                           values_to = "salary"),
            aes(col = model))

## campy time series data
source("https://raw.githubusercontent.com/rafamoral/courses/main/time_series_forecasting/scripts/helper_functions.R")
library(tscount)
plot.ts(campy)
?campy

campy_df <- data.frame(count = as.vector(campy)) %>%
  mutate(time = 1:n())

fit5 <- gam(count ~ s(time, k = 6),
            data = campy_df,
            family = poisson,
            method = "REML")
small_check(fit5)
library(marginaleffects)
plot_fcs(fit5, data = campy_df) + theme_bw()

fit6 <- gam(count ~ s(time, k = 20),
            data = campy_df,
            family = poisson,
            method = "REML")
small_check(fit6)
plot_fcs(fit6, data = campy_df) + theme_bw()

fit7 <- gam(count ~ s(time, k = 50),
            data = campy_df,
            family = poisson,
            method = "REML")
small_check(fit7)
plot_fcs(fit7, data = campy_df) + theme_bw()

## forecasting
plot_fcs(fit5, n_ahead = 10, data = campy_df) + theme_bw()
plot_fcs(fit6, n_ahead = 10, data = campy_df) + theme_bw()
plot_fcs(fit7, n_ahead = 10, data = campy_df) + theme_bw()

par(mfrow = c(1,2))
acf(residuals(fit5)); pacf(residuals(fit5))
acf(residuals(fit6)); pacf(residuals(fit6))
acf(residuals(fit7)); pacf(residuals(fit7))

## including a fixed ("known") AR parameter through bam
fit8 <- bam(count ~ s(time, k = 50),
            data = campy_df,
            family = poisson,
            method = "fREML",
            rho = 0.6,
            discrete = TRUE)
small_check(fit8)
plot_fcs(fit8, n_ahead = 10, data = campy_df) + theme_bw()
par(mfrow = c(1,2))
acf(residuals(fit8)); pacf(residuals(fit8))

## dynamic GAMs using mvgam
library(mvgam)

fit9 <- mvgam(count ~ 1,
              trend_formula = ~ gp(time, k = 10),
              trend_model = AR(),
              priors = prior_string(prior ='normal(16, 4)', 
                                    class = 'rho_gp_trend(time)'),
              data = campy_df,
              family = poisson())

## the correlations between timepoints decay smoothly as we move further apart in time
gp_ests <- as.data.frame(fit9, variable = c('rho_gp',
                                            'alpha_gp'),
                         regex = TRUE)
plot_kernels(gp_ests = gp_ests, max_time = 30)

mcmc_plot(fit9, variable = c('ar1', 'sigma'), regex = TRUE, type = 'areas')

plot(hindcast(fit9))

fc <- forecast(fit9,
               newdata = data.frame(time = (max(campy_df$time) + 1):(max(campy_df$time) + 10)))
plot(fc)

## looking at derivatives to ask when the trend was changing most rapidly
## (i.e. at which time points was the derivative furthest from zero?)
plot_slopes(fit9, variables = 'time', condition = 'time',
            type = 'link') +
  labs(y = 'First derivative (slope) of linear predictor') +
  geom_hline(yintercept = 0, linetype = 'dashed')

## Queensland bulls time series
queensland_bulls <- aus_livestock %>%
  filter(Animal == "Bulls, bullocks and steers",
         State == "Queensland") %>%
  select(- Animal, - State) %>%
  mutate(year = year(Month),
         month  = month(Month),
         time = 1:n())

queensland_bulls %>%
  gg_season(Count, period = "year")

queensland_bulls_train <- queensland_bulls %>%
  filter(year(Month) < 2018)
queensland_bulls_test <- queensland_bulls %>%
  filter(year(Month) == 2018)

queensland_bulls %>% autoplot()

fit <- gam(Count ~ s(time, k = 30) + s(month, bs = "cc"),
           family = poisson,
           data = queensland_bulls_train)
summary(fit)

draw(fit)

queensland_bulls %>%
  mutate(pred = predict(fit, newdata = queensland_bulls, type = "response")) %>%
  autoplot(Count) +
  geom_line(data = queensland_bulls_test, col = 4) +
  geom_line(aes(y = pred), col = 2)

fit2 <- gam(Count ~ te(time, month),
            family = poisson,
            data = queensland_bulls_train)
summary(fit2)

draw(fit2)

queensland_bulls %>%
  mutate(pred = predict(fit2, newdata = queensland_bulls, type = "response")) %>%
  autoplot(Count) +
  geom_line(data = queensland_bulls_test, col = 4) +
  geom_line(aes(y = pred), col = 2)

AIC(fit)
AIC(fit2) ## tensor-type model is not a superior fit

fit3 <- gamm(Count ~ s(time, k = 30) + s(month, bs = "cc"),
             family = poisson,
             data = queensland_bulls_train,
             correlation = corARMA(form = ~ 1 | year, p = 1))
summary(fit3$lme)

queensland_bulls %>%
  mutate(pred = predict(fit3$gam, newdata = queensland_bulls, type = "response")) %>%
  autoplot(Count) +
  geom_line(data = queensland_bulls_test, col = 4) +
  geom_line(aes(y = pred), col = 2)

AIC(fit)
AIC(fit3$lme)

fit4 <- gamm(Count ~ s(time, k = 30) + s(month, bs = "cc"),
             family = poisson,
             data = queensland_bulls_train,
             correlation = corARMA(form = ~ 1 | year, p = 2))

queensland_bulls %>%
  mutate(pred = predict(fit4$gam, newdata = queensland_bulls, type = "response")) %>%
  autoplot(Count) +
  geom_line(data = queensland_bulls_test, col = 4) +
  geom_line(aes(y = pred), col = 2)

AIC(fit3$lme)
AIC(fit4$lme) ## improvement with AR(2) structure

fit5 <- gamm(Count ~ s(time, k = 30) + s(month, bs = "cc"),
             family = poisson,
             data = queensland_bulls_train,
             correlation = corARMA(form = ~ 1 | year, p = 2, q = 2))

queensland_bulls %>%
  mutate(pred = predict(fit5$gam, newdata = queensland_bulls, type = "response")) %>%
  autoplot(Count) +
  geom_line(data = queensland_bulls_test, col = 4) +
  geom_line(aes(y = pred), col = 2)

AIC(fit4$lme)
AIC(fit5$lme) ## improvement with ARMA(2,2) structure, but convergence issue

## comparing performances on test set

queensland_bulls_test <- queensland_bulls_test %>%
  mutate(no_corr = predict(fit, queensland_bulls_test, type = "response"),
         ar1 = predict(fit3$gam, queensland_bulls_test, type = "response"),
         ar2 = predict(fit4$gam, queensland_bulls_test, type = "response"),
         arma22 = predict(fit5$gam, queensland_bulls_test, type = "response"))

queensland_bulls_test %>%
  pivot_longer(6:9,
               names_to = "model",
               values_to = "pred") %>%
  ggplot(aes(x = time, y = Count)) +
  theme_bw() +
  geom_line() +
  geom_line(aes(y = pred), col = 4, lty = 2) +
  facet_wrap(~ model)

## forecast error
queensland_bulls_test %>%
  as_tibble() %>%
  pivot_longer(6:9,
               names_to = "model",
               values_to = "pred") %>%
  group_by(model) %>%
  summarise(RMSE = sqrt(sum(pred - Count)^2)) %>%
  arrange(RMSE)

## conclusion: ARMA(2,2) model shows superior forecast performance
##             in the validation set