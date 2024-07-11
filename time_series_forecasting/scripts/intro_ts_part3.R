library(tidyverse)
library(fpp3)

# ARIMA modelling ---------------------------------------------------------

## Differencing
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  dplyr::mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)

google_2015 <- google_stock %>% filter(year(Date) == 2015)

google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, ljung_box, lag = 10)

## KPSS test
google_2015 %>%
  features(Close, unitroot_kpss)

google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, unitroot_kpss)

## order of an ARIMA model
global_economy %>%
  filter(Code == "EGY") %>%
  autoplot(Exports) + theme_bw() +
  labs(y = "% of GDP")

egypt_economy <- global_economy %>%
  filter(Country == "Egypt, Arab Rep.")

fit <- egypt_economy %>%
  model(ARIMA(Exports)) ## selects order automatically

report(fit)
## write down ARIMA equation

fit %>% forecast(h = 10) %>%
  autoplot(global_economy) + theme_bw() +
  labs(y = "% of GDP")

## ACF and pACF
eps <- rnorm(1000)
y <- numeric(1000)

## AR(1)
y[1] <- eps[1]
for(i in 2:1000) y[i] <- 5 + .7*y[i-1] + eps[i]

par(mfrow=c(2,1))
acf(y, lag.max = 20)
pacf(y, lag.max = 20)

## AR(2)
y[1:2] <- eps[1:2]
for(i in 3:1000) y[i] <- 5 + .3*y[i-1] + .5*y[i-2] + eps[i]

par(mfrow=c(2,1))
acf(y, lag.max = 20)
pacf(y, lag.max = 20)

## MA(1)
y[1] <- eps[1]
for(i in 2:1000) y[i] <- 5 + eps[i] + .8 * eps[i-1]

par(mfrow=c(2,1))
acf(y, lag.max = 20)
pacf(y, lag.max = 20)

## MA(2)
y[1:2] <- eps[1:2]
for(i in 3:1000) y[i] <- 5 + eps[i] + .8 * eps[i-1] +.5 * eps[i-2]

par(mfrow=c(2,1))
acf(y, lag.max = 20)
pacf(y, lag.max = 20)

## I(1) = random walk
y[1] <- eps[1]
for(i in 2:1000) y[i] <- y[i-1] + eps[i]

par(mfrow=c(2,1))
acf(y, lag.max = 20)
pacf(y, lag.max = 20)

## Example: Irish exports
global_economy %>%
  filter(Code == "IRL") %>%
  autoplot(Exports) +
  theme_bw() +
  labs(title = "Irish exports",
       y = "% of GDP")

global_economy %>%
  filter(Code == "IRL") %>%
  gg_tsdisplay(difference(Exports), plot_type = "partial")

caf_fit <- global_economy %>%
  filter(Code == "IRL") %>%
  model(arima110 = ARIMA(Exports ~ 0 + pdq(1,1,0)),
        arima011 = ARIMA(Exports ~ 0 + pdq(0,1,1)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise = FALSE))

glance(caf_fit) %>%
  arrange(AICc)

caf_fit %>%
  dplyr::select(search) %>%
  gg_tsresiduals()

augment(caf_fit) %>%
  filter(.model== "search") %>%
  features(.innov, ljung_box, lag = 10, dof = 1)

caf_fit %>%
  forecast(h = 5) %>%
  filter(.model == "search") %>%
  autoplot(global_economy)

## Seasonal ARIMA
leisure <- us_employment %>%
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) %>%
  mutate(Employed = Employed/1000) %>%
  dplyr::select(Month, Employed)

autoplot(leisure, Employed) + theme_bw() +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

leisure %>%
  gg_tsdisplay(difference(Employed, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")

leisure %>%
  gg_tsdisplay(difference(Employed, 12) %>% difference(),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")
## Looking at the ACF only we might try the ARIMA(0,1,2)(0,1,1)_12 model
## Looking at the PACF only we might try the ARIMA(2,1,0)(1,1,0)_{12}
## We could merge and look at the PACF to model the non-seasonal part and the
## ACF to model the seasonal part and try the ARIMA(2,1,0)(0,1,1)_{12} model

fit <- leisure %>%
  model(arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
        arima210110 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(1,1,0)),
        arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
        auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE))

fit %>%
  pivot_longer(everything(),
               names_to = "Model name",
               values_to = "Orders")

glance(fit) %>%
  arrange(AICc) %>%
  select(.model:BIC)

fit %>%
  select(auto) %>%
  gg_tsresiduals(lag = 36)

augment(fit) %>%
  filter(.model == "auto") %>%
  features(.innov, ljung_box, lag = 24, dof = 4)

forecast(fit, h = 36) %>%
  filter(.model == "auto") %>%
  autoplot(leisure) + theme_bw() +
  labs(title = "US employment: leisure and hospitality",
       y = "Number of people (millions)")

## Example: corticosteroid drug sales
h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6)

h02 %>%
  mutate(log(Cost)) %>%
  pivot_longer(-Month) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") + theme_bw() +
  labs(y="", title="Corticosteroid drug scripts (H02)")

h02 %>%
  gg_tsdisplay(difference(log(Cost), 12),
               plot_type = 'partial', lag_max = 24)

h02 %>%
  gg_tsdisplay(difference(log(Cost), 12) %>% difference,
               plot_type = 'partial', lag_max = 24)


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
  gg_tsresiduals(lag_max = 36)

fit %>%
  select(auto) %>%
  augment %>%
  features(.innov, ljung_box, lag = 36, dof = 6)

fit %>%
  select(auto) %>%
  forecast() %>%
  autoplot(h02) +
  theme_bw() +
  labs(y = "$AU (millions)",
       title = "Corticosteroid drug scripts (H02) sales")

# Dynamic regression ------------------------------------------------------

## Example: US consumption and income
us_change %>%
  pivot_longer(c(Consumption, Income),
               names_to = "var", values_to = "value") %>%
  ggplot(aes(x = Quarter, y = value)) +
  theme_bw() +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(y = "Quarterly % change")

fit <- us_change %>%
  model(ARIMA(Consumption ~ Income))

report(fit)

## write down model equations
## y_t = 0.595 + 0.198 * x_t + eta_t
## eta_t = 0.707 * eta_{t-1} + epsilon_t - 0.617 * epsilon_{t-1} + 0.207 * epsilon_{t-2}
## epsilon_t ~ N(0, 0.311)

bind_rows(`Regression residuals (eta)` =
            as_tibble(residuals(fit, type = "regression")),
          `ARIMA residuals (epsilon)` =
            as_tibble(residuals(fit, type = "innovation")),
          .id = "type") %>%
  mutate(type = factor(type, levels = c("Regression residuals (eta)",
                                        "ARIMA residuals (epsilon)"))) %>%
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type))

fit %>%
  gg_tsresiduals()

augment(fit) %>%
  features(.innov, ljung_box, dof = 5, lag = 10)

## Forecasting

## We will calculate forecasts for the next eight quarters assuming that
## the future percentage changes in personal disposable income will be
## equal to the mean percentage change from the last forty years.

us_change_future <- new_data(us_change, 8) %>%
  mutate(Income = mean(us_change$Income))

forecast(fit, new_data = us_change_future) %>%
  autoplot(us_change) + theme_bw() +
  labs(y = "Percentage change")

## The prediction intervals for this model are narrower than if we had
## fitted an ARIMA model without covariates, because we are now able to
## explain some of the variation in the data using the income predictor.

## Forecasting electricity demand
vic_elec_daily <- vic_elec %>%
  filter(year(Time) == 2014) %>%
  index_by(Date = date(Time)) %>%
  summarise(Demand = sum(Demand) / 1e3,
            Temperature = max(Temperature),
            Holiday = any(Holiday)) %>%
  mutate(Day_Type = case_when(Holiday ~ "Holiday",
                              wday(Date) %in% 2:6 ~ "Weekday",
                              TRUE ~ "Weekend"))

vic_elec_daily %>%
  ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
  theme_bw() +
  geom_point() +
  labs(y = "Electricity demand (GW)",
       x = "Maximum daily temperature")

vic_elec_daily %>%
  pivot_longer(c(Demand, Temperature)) %>%
  ggplot(aes(x = Date, y = value)) +
  theme_bw() +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  ylab("")

fit <- vic_elec_daily %>%
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) + (Day_Type == "Weekday")))

report(fit)

fit %>%
  gg_tsresiduals()

augment(fit) %>%
  features(.innov, ljung_box, dof = 9, lag = 14)

vic_elec_future <- new_data(vic_elec_daily, 14) %>%
  mutate(Temperature = 26,
         Holiday = c(TRUE, rep(FALSE, 13)),
         Day_Type = case_when(Holiday ~ "Holiday",
                              wday(Date) %in% 2:6 ~ "Weekday",
                              TRUE ~ "Weekend"))

forecast(fit, vic_elec_future) %>%
  autoplot(vic_elec_daily) +
  theme_bw() +
  labs(title = "Daily electricity demand: Victoria",
       y = "GW")

vic_elec_future <- new_data(vic_elec_daily, 14) %>%
  mutate(Temperature = 35,
         Holiday = c(TRUE, rep(FALSE, 13)),
         Day_Type = case_when(Holiday ~ "Holiday",
                              wday(Date) %in% 2:6 ~ "Weekday",
                              TRUE ~ "Weekend"))

forecast(fit, vic_elec_future) %>%
  autoplot(vic_elec_daily) +
  theme_bw() +
  labs(title = "Daily electricity demand: Victoria",
       y = "GW")