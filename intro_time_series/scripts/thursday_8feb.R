library(tidyverse)
library(fpp3)

# ARIMA modelling ---------------------------------------------------------

## Differencing a time series
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  dplyr::mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)

google_2015 <- google_stock %>% filter(year(Date) == 2015)

google_2015 %>%
  features(Close, ljung_box, lag = 10) ## original series

autoplot(ACF(google_2015, Close))

google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, ljung_box, lag = 10) ## differenced series

google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  ACF(diff_close) %>%
  autoplot()

## testing whether the series are stationary via the KPSS test
google_2015 %>%
  features(Close, unitroot_kpss) ## original series

google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, unitroot_kpss) ## differenced series

## selecting the order of an ARIMA model automatically
egypt_economy <- global_economy %>%
  filter(Code == "EGY")

autoplot(egypt_economy, Exports)

fit <- egypt_economy %>%
  model(ARIMA(Exports)) ## selects the order automatically

report(fit)
tidy(fit)

## y_t = 2.5623 + 1.6764*y_t-1 - 0.8034*y_t-2 + e_t - 0.6896*e_t-1
## e_t ~ N(0, 8.046)

fit %>%
  forecast(h = 10) %>%
  autoplot(egypt_economy)

fit %>%
  gg_tsresiduals()

## ACFs and pACFs
eps <- rnorm(1000)
y <- numeric(1000)

## AR(1)
y[1] <- eps[1]
for(t in 2:1000) y[t] <- 5 + .7 * y[t-1] + eps[t]

plot.ts(y)
par(mfrow = c(2,1))
acf(y)
pacf(y)

## AR(2)
y[1:2] <- eps[1:2]
for(t in 3:1000) y[t] <- 5 + .3 * y[t-1] + .5 * y[t-2]+ eps[t]

plot.ts(y)
par(mfrow = c(2,1))
acf(y)
pacf(y)

## MA(1)
y[1] <- eps[1]
for(t in 2:1000) y[t] <- 5 + eps[t] + .8 * eps[t-1]

plot.ts(y)
par(mfrow = c(2,1))
acf(y)
pacf(y)

## Example: Irish exports
irish_economy <- global_economy %>%
  filter(Code == "IRL")

autoplot(irish_economy, Exports)

irish_economy %>%
  gg_tsdisplay(difference(Exports), plot_type = "partial")

arima_fit <- irish_economy %>%
  model(arima011 = ARIMA(Exports ~ 0 + pdq(0,1,1)),
        arima110 = ARIMA(Exports ~ 0 + pdq(1,1,0)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise = FALSE))

arima_fit

glance(arima_fit)

arima_fit %>%
  select(search) %>%
  gg_tsresiduals()

arima_fit %>%
  forecast(h = 5) %>%
  autoplot(irish_economy, level = NULL)

arima_fit %>%
  forecast(h = 5) %>%
  filter(.model == "search") %>%
  autoplot(irish_economy)

## Seasonal ARIMA

## Example: corticosteroid drug sales
h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6)

h02 %>%
  autoplot(log(Cost))

h02 %>%
  gg_tsdisplay(difference(log(Cost), lag = 12),
               plot_type = "partial")

h02 %>%
  gg_tsdisplay(difference(log(Cost), lag = 12) %>%
                 difference(),
               plot_type = "partial",
               lag_max = 40)
## ACF suggests q = 4 or 5, Q = 2
## pACF suggests p = 2, P = 0 or 1

fit <- h02 %>%
  model(arima014012 = ARIMA(log(Cost) ~ 0 + pdq(0,1,4) + PDQ(0,1,2)),
        auto = ARIMA(log(Cost)),
        search = ARIMA(log(Cost), stepwise = FALSE))
fit

## AICc selection
glance(fit) %>%
  arrange(AICc)

## RMSE selection
h02 %>%
  stretch_tsibble(.init = 180) %>%
  model(arima014012 = ARIMA(log(Cost) ~ 0 + pdq(0,1,4) + PDQ(0,1,2)),
        arima210011 = ARIMA(log(Cost) ~ 0 + pdq(2,1,0) + PDQ(0,1,1)),
        arima213011 = ARIMA(log(Cost) ~ 0 + pdq(2,1,3) + PDQ(0,1,1))) %>%
  forecast(h = 1) %>%
  accuracy(h02) %>%
  arrange(RMSE)

fit %>%
  select(auto) %>%
  gg_tsresiduals(lag_max = 40)

fit %>%
  select(auto) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 36, dof = 6)

fit %>%
  select(auto) %>%
  forecast(h = "3 years") %>%
  autoplot(h02)

# Dynamic regression ------------------------------------------------------

## Example: US consumption and income

us_change %>%
  autoplot(Consumption)

us_change %>%
  autoplot(Income)

fit <- us_change %>%
  model(ARIMA(Consumption ~ Income))

report(fit)

## consumption_t = 0.5949 + 0.1976 * income_t + eta_t
## eta_t = 0.7070 * eta_t-1 + epsilon_t - 0.6172 * epsilon_t-1 + 0.2066 * epsilon_t-2
## epsilon_t ~ N(0, 0.3113)

lm(Consumption ~ Income, data = us_change) ## this model ignores the serial dependence

fit %>%
  gg_tsresiduals()

augment(fit) %>%
  features(.innov, ljung_box, lag = 10, dof = 6)

## assuming average of the historic income
us_change_future <- new_data(us_change, 8) %>%
  mutate(Income = mean(us_change$Income))

fit %>%
  forecast(new_data = us_change_future) %>%
  autoplot(us_change)

# Generalized additive models ---------------------------------------------

library(mgcv) ## to work with GAMs
library(gratia) ## to visualise

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
AIC(fit5$lme) ## improvement with ARMA(2,2) structure

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