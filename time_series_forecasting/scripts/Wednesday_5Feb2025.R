library(tidyverse)
library(fpp3)


# Exponential Smoothing ('ctd) --------------------------------------------

## additive trend (Holt's linear method)
ire_pop <- global_economy %>%
  filter(Country == "Ireland") %>%
  mutate(Population = Population / 1e6) %>%
  select(Year, Population)

ire_pop %>%
  autoplot()

fit <- ire_pop %>%
  model(Holt = ETS(Population ~ error("A") + trend("A") + season("N")))

fit %>%
  tidy()

fit %>%
  forecast(h = 10) %>%
  autoplot(ire_pop)

## comparing with a drift forecast
ire_pop %>%
  model(Holt = ETS(Population ~ error("A") + trend("A") + season("N")),
        Drift = RW(Population ~ drift())) %>%
  forecast(h = 10) %>%
  autoplot(ire_pop, level = NULL)

## let's include a damped trend = trend("Ad")
fit <- ire_pop %>%
  model(Holt = ETS(Population ~ error("A") + trend("A") + season("N")),
        Damped_Holt = ETS(Population ~ error("A") + trend("Ad") + season("N")))

fit %>%
  tidy()

fit %>%
  forecast(h = 20) %>%
  autoplot(ire_pop, level = NULL)

## which one is best for 1-step ahead forecasting?
## we can answer this question by carrying out a cross-validation study

## stretch_tsibble creates sub-time series and stack them all together
## we can then fit a model that will generate h-step ahead forecasts
## for each sub-time series, therefore constituting a cross-validation study

ire_pop %>%
  stretch_tsibble(.init = 20) %>%
  model(simple = ETS(Population ~ error("A") + trend("N") + season("N")),
        holt = ETS(Population ~ error("A") + trend("A") + season("N")),
        damped_holt = ETS(Population ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 1) %>%
  accuracy(ire_pop) %>%
  arrange(RMSE)

## the CV study indicates the Holt model to be the best-performing in terms
## of 1-step ahead forecasts

## assessing model adequacy
ire_pop %>%
  model(holt = ETS(Population ~ error("A") + trend("A") + season("N"))) %>%
  gg_tsresiduals()

ire_pop %>%
  model(holt = ETS(Population ~ error("A") + trend("A") + season("N"))) %>%
  augment() %>%
  features(.innov, box_pierce, lag = 10, dof = 4)

## a second example for the CV study: WWWusage
www_usage <- as_tsibble(WWWusage)

www_usage %>%
  autoplot()

WWWusage %>%
  as_tsibble() %>%
  stretch_tsibble(.init = 24) %>%
  model(naive = NAIVE(value),
        drift = NAIVE(value ~ drift()),
        Mean = MEAN(value),
        simple = ETS(value ~ error("A") + trend("N") + season("N")),
        holt = ETS(value ~ error("A") + trend("A") + season("N")),
        damped_holt_phi.8 = ETS(value ~ error("A") + trend("Ad", phi = .8) + season("N")),
        damped_holt_phi_estimated = ETS(value ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 1) %>%
  accuracy(www_usage) %>%
  arrange(RMSE)

www_usage %>%
  model(damped_holt_phi_estimated = ETS(value ~ error("A") + trend("Ad") + season("N"))) %>%
  gg_tsresiduals()

www_usage %>%
  model(damped_holt_phi_estimated = ETS(value ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 20) %>%
  autoplot(www_usage)

www_usage %>%
  model(damped_holt_phi_estimated = ETS(value ~ error("A") + trend("Ad") + season("N"))) %>%
  augment() %>%
  pull(.innov) %>%
  hnp::hnp(., scale = TRUE, half = FALSE, print = TRUE)

www_usage %>%
  model(damped_holt_phi_estimated = ETS(value ~ error("A") + trend("Ad") + season("N"))) %>%
  augment() %>%
  pull(.innov) %>%
  shapiro.test()

## including seasonality (Holt-Winter's seasonal method)
aus_holiday <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Total_Trips = sum(Trips) / 1000) ## total no. trips in millions

aus_holiday %>%
  autoplot()

fit <- aus_holiday %>%
  model(hw_add = ETS(Total_Trips ~ error("A") + trend("A") + season("A")),
        hw_mult = ETS(Total_Trips ~ error("M") + trend("A") + season("M")))

fit %>%
  tidy

fit %>%
  forecast(h = "3 years") %>%
  autoplot(aus_holiday, level = NULL)


# ARIMA modelling ---------------------------------------------------------

## differencing and unit-root tests
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2015) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)

google_stock %>%
  autoplot(Close)

google_stock %>%
  mutate(diff_close = difference(Close)) %>%
  autoplot(diff_close)

## KPSS test for stationarity
## H0: the series is stationary
google_stock %>%
  features(Close, unitroot_kpss) ## not stationary

google_stock %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, unitroot_kpss) ## stationary (does not reject null)

## choosing the order of an ARIMA(p,d,q) model

## example: Egyptian exports
egypt_economy <- global_economy %>%
  filter(Country == "Egypt, Arab Rep.")

egypt_economy %>%
  autoplot(Exports)

fit <- egypt_economy %>%
  model(ARIMA(Exports))

fit %>%
  report()

fit %>%
  gg_tsresiduals()

fit %>%
  forecast(h = 10) %>%
  autoplot(egypt_economy)

## ACF and PACF
epsilon <- rnorm(1000)
y <- numeric(1000)

## AR(1)
y[1] <- 5 / (1 - .7)
for(i in 2:1000) y[i] <- 5 + .7 * y[i-1] + epsilon[i]

plot.ts(y)

acf(y) # notable pattern of an MA(infinity) series, with spikes decaying "exponentially"
pacf(y) # significant spike at lag 1 indicates AR(1) model

## AR(2)
y[1:2] <- epsilon[1:2]
for(i in 3:1000) y[i] <- 5 + .3 * y[i-1] + .5 * y[i-2] + epsilon[i]

acf(y) # notable pattern of an MA(infinity) series, with spikes decaying "exponentially"
pacf(y) # significant spike at lag 2 indicates AR(2) model

## MA(1)
y[1] <- epsilon[1]
for(i in 2:1000) y[i] <- 5 + epsilon[i] + .8 * epsilon[i-1]

acf(y) # significant spike at lag 1 indicates MA(1) model
pacf(y) # notable pattern of an AR(infinity) series with spikes decaying exponentially in absolute value

## MA(1)
y[1:2] <- epsilon[1:2]
for(i in 3:1000) y[i] <- 5 + epsilon[i] + .8 * epsilon[i-1] + .5 * epsilon[i-2]

acf(y) # significant spike at lag 2 indicates MA(2) model
pacf(y) # notable pattern of an AR(infinity) series with spikes decaying exponentially in absolute value

## I(1) = random walk
y[1] <- epsilon[1]
for(i in 2:1000) y[i] <- y[i-1] + epsilon[i]

plot.ts(y)

acf(y) # the dependency on the first observation is carried through many more lags in the series
pacf(y) # discounting the intermediate dependencies, we have only lag 1 as significant for the partial acf

## example: Irish exports
ire_exports <- global_economy %>%
  filter(Country == "Ireland")

ire_exports %>%
  autoplot(Exports)

## we must differentiate the series until it looks stationary before
## we can rely on the ACF and PACF to help us decide on the order
## of ARIMA models

ire_exports %>%
  autoplot(difference(log(Exports)))

## we are (1) log-transforming to stabilise the variance
## (2) doing first-order differencing to de-trend the series
## it now looks stationary, let's have a look at the ACF/PACF plots

ire_exports %>%
  gg_tsdisplay(difference(Exports), plot_type = "partial")
## ACF suggests ARIMA(0,1,1)
## PACF suggests ARIMA(1,1,0)
## if we use ARIMA(Exports) fable will automatically choose the order based on an internal algorithm
## so let's compare the three models

fit <- ire_exports %>%
  model(arima011 = ARIMA(Exports ~ 0 + pdq(0,1,1)),
        arima110 = ARIMA(Exports ~ 0 + pdq(1,1,0)),
        automatic = ARIMA(Exports))

glance(fit)

fit %>%
  select(automatic) %>%
  gg_tsresiduals()

fit %>%
  forecast(h = 5) %>%
  autoplot(ire_exports, level = NULL)

## Seasonal ARIMA
leisure <- us_employment %>%
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) %>%
  mutate(Employed = Employed / 1000) %>%
  select(Month, Employed)

leisure %>%
  autoplot()

## no need for transformation, variance looks stable
## trying seasonal differencing first
leisure %>%
  gg_tsdisplay(difference(Employed, 12),
               plot_type = "partial")
## still not stationary, now doing first-order differencing to detrend
leisure %>%
  gg_tsdisplay(difference(Employed, 12) %>% difference(),
               plot_type = "partial", lag = 50)

## ACF suggests ARIMA(0,1,2)(0,1,1)_12
## PACF suggests ARIMA(2,1,0)(3,1,0)_12

## can we mix and match? definitely!
## we may also try...
## ARIMA(0,1,2)(3,1,0)_12
## ARIMA(2,1,0)(0,1,1)_12
## ... and many more

fit <- leisure %>%
  model(arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
        arima210310 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(3,1,0)),
        arima012310 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(3,1,0)),
        arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
        automatic = ARIMA(Employed))

fit %>%
  select(automatic) %>%
  report()

glance(fit) %>%
  arrange(AICc)

fit %>%
  select(automatic) %>%
  gg_tsresiduals()

fit %>%
  select(automatic) %>%
  augment() %>%
  features(.innov, box_pierce, lag = 24, dof = 6)

fit %>%
  select(automatic) %>%
  forecast(h = "3 years") %>%
  autoplot(leisure)


# Dynamic regression ------------------------------------------------------

## example: US consumption and disposable income
us_change %>%
  autoplot(Consumption) +
  autolayer(us_change, Income, col = 2) +
  theme_minimal()

us_change %>%
  gg_tsdisplay(difference(Consumption), plot_type = "partial")

us_change %>%
  gg_tsdisplay(difference(Income), plot_type = "partial")

fit <- us_change %>%
  model(ARIMA(Consumption ~ Income + lag(Income, 1)))

fit %>%
  report

lm(Consumption ~ Income, data = us_change)

fit %>%
  gg_tsresiduals()

residuals(fit, type = "innovation") ## this is the epsilon <- this is the one we want to check for normality etc.
residuals(fit, type = "regression") ## this is the eta

plot.ts(residuals(fit, type = "regression")$.resid)

fit %>%
  augment() %>%
  pull(.innov) %>%
  na.omit %>%
  hnp::hnp(., scale = TRUE, half = FALSE, paint = TRUE)

## forecasting with covariates = we need a forecast for the covariate first!
us_change_future <- new_data(us_change, 8) %>%
  mutate(Income = seq(.6, 1.2, length = 8))

fit %>%
  forecast(new_data = us_change_future) %>%
  autoplot(us_change)


# Generalized additive modelling ------------------------------------------

## example: salary data
sal <- read.table("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/time_series_forecasting/data/salary.txt",
                  header = TRUE)

sal %>%
  ggplot(aes(x = pexp, y = salary)) +
  geom_point() +
  geom_smooth(se = FALSE)

fit1 <- lm(salary ~ pexp, data = sal)

library(splines)
fit2 <- lm(salary ~ bs(pexp, df = 3), data = sal) # B-splines
fit3 <- lm(salary ~ ns(pexp, df = 3), data = sal) # natural splines

library(modelr)
sal_pred <- tibble(pexp = 0:400) %>%
  add_predictions(model = fit1, var = "linear") %>%
  add_predictions(model = fit2, var = "b_splines") %>%
  add_predictions(model = fit3, var = "natural_splines")

sal %>%
  ggplot(aes(x = pexp, y = salary)) +
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
  geom_point() +
  geom_line(data = sal_pred %>%
              pivot_longer(2:5,
                           names_to = "model",
                           values_to = "salary"),
            aes(col = model))

## example 1 (non seasonal): campy time series data
library(tscount)
data(campy)

## load some helper functions
source("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/time_series_forecasting/scripts/helper_functions.R")

campy_df <- data.frame(count = as.numeric(campy)) %>%
  mutate(time = 1:n())

fit_campy1 <- gam(count ~ s(time),
                  data = campy_df,
                  family = poisson,
                  method = "REML")
summary(fit_campy1)
small_check(fit_campy1)

library(marginaleffects)
plot_fcs(fit_campy1, data = campy_df) +
  theme_bw()

fit_campy2 <- gam(count ~ s(time, k = 20),
                  data = campy_df,
                  family = poisson,
                  method = "REML")
small_check(fit_campy2)
plot_fcs(fit_campy2, data = campy_df) +
  theme_bw()

fit_campy3 <- gam(count ~ s(time, k = 50),
                  data = campy_df,
                  family = poisson,
                  method = "REML")
small_check(fit_campy3)
plot_fcs(fit_campy3, data = campy_df) +
  theme_bw()

## forecasting
plot_fcs(fit_campy1, n_ahead = 10, data = campy_df) +
  theme_bw()
plot_fcs(fit_campy2, n_ahead = 10, data = campy_df) +
  theme_bw()
plot_fcs(fit_campy3, n_ahead = 10, data = campy_df) +
  theme_bw()

AIC(fit_campy1)
AIC(fit_campy2)
AIC(fit_campy3)

par(mfrow = c(1,2))
acf(residuals(fit_campy1)); pacf(residuals(fit_campy1))
acf(residuals(fit_campy2)); pacf(residuals(fit_campy2))
acf(residuals(fit_campy3)); pacf(residuals(fit_campy3))

plot.ts(residuals(fit_campy1))
plot.ts(residuals(fit_campy2))
plot.ts(residuals(fit_campy3))

plot.ts(campy)
plot.ts(diff(campy))

acf(diff(campy))
pacf(diff(campy))

## include a fixed ("known") AR parameter through bam
fit_campy4 <- bam(count ~ s(time, k = 50),
                  data = campy_df,
                  family = poisson,
                  method = "fREML",
                  discrete = TRUE,
                  rho = .5)
summary(fit_campy4)
small_check(fit_campy4)
plot_fcs(fit_campy4, n_ahead = 10, data = campy_df) +
  theme_bw()

par(mfrow = c(2,2))
acf(residuals(fit_campy3)); pacf(residuals(fit_campy3))
acf(residuals(fit_campy4)); pacf(residuals(fit_campy4))

## dynamic GAMs using the mvgam package
library(mvgam)

fit_campy5 <- mvgam(count ~ 1,
                    trend_formula = ~ gp(time, k = 10),
                    trend_model = AR(),
                    priors = prior_string(prior = "normal(16, 4)",
                                          class = "rho_gp_trend(time)"),
                    data = campy_df,
                    family = poisson())

## in Gaussian processes, the correlations between observations decay
## smoothly as the time gap in between them increases
gp_ests <- as.data.frame(fit_campy5,
                         variable = c("rho_gp",
                                      "alpha_gp"),
                         regex = TRUE)
plot_kernels(gp_ests, max_time = 10)
mcmc_plot(fit_campy5, variable = c("ar1","sigma"), regex = TRUE, type = "areas")

plot(hindcast(fit_campy5))

fit5_forecasts <- forecast(fit_campy5,
                           newdata = data.frame(time = 141:150))
plot(fit5_forecasts)

plot_slopes(fit_campy5, variables = "time", condition = "time",
            type = "link") +
  geom_hline(yintercept = 0, linetype = "dashed")