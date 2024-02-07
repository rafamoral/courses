library(tidyverse)
library(fpp3)

# Benchmark forecasting methods -------------------------------------------

## tidying data
## example: GDP per capita
gdppc <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population)

# base R equivalent
# global_economy$GDP_per_capita <- global_economy$GDP / global_economy$Population

## Plot the date (visualise)
gdppc %>%
  filter(Country == "Ireland") %>%
  autoplot(GDP_per_capita) +
  theme_bw() +
  labs(y = "$US", title = "GDP per capita for Ireland")

## Define a model (specify)
fit <- gdppc %>%
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

fit
glance(fit)

report(fit)

fit %>%
  filter(Country == "Ireland") %>%
  report

fit %>%
  filter(Country == "Ireland") %>%
  tidy

## Forecast
fit %>% forecast(h = "3 years")

fit %>%
  filter(Country == "Ireland") %>%
  forecast(h = "3 years") %>%
  autoplot(gdppc) +
  theme_bw() +
  labs(y = "$US", title = "GDP per capita for Ireland")

## House registrations data
house_reg <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_time_series/data/house_reg_dublin.csv")

house_reg <- house_reg %>%
  mutate(YearQuarter = yearquarter(YearQuarter)) %>%
  as_tsibble(index = YearQuarter)

house_reg %>%
  autoplot(Registrations) +
  theme_bw()

## Mean method
house_reg %>%
  model(mean_model = MEAN(Registrations)) %>%
  forecast(h = "3 years") %>%
  autoplot(house_reg, level = NULL) + theme_bw()

## Naive method
house_reg %>%
  model(NAIVE(Registrations)) %>%
  forecast(h = "3 years") %>%
  autoplot(house_reg) + theme_bw()

house_reg %>%
  model(RW(Registrations)) %>% ## or RW() function
  forecast(h = "3 years") %>%
  autoplot(house_reg) + theme_bw()

## Seasonal naive method
house_reg %>%
  model(SNAIVE(Registrations)) %>%
  forecast(h = "3 years") %>%
  autoplot(house_reg, level = NULL) + theme_bw()

## Drift method
house_reg %>%
  model(RW(Registrations ~ drift())) %>%
  forecast(h = "3 years") %>%
  autoplot(house_reg, level = NULL) + theme_bw()

## Example: Google stock price
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)

google_2015 <- google_stock %>%
  filter(year(Date) == 2015)

## fit different time series method/models
google_fit <- google_2015 %>%
  model(mean = MEAN(Close),
        naive = NAIVE(Close),
        drift = NAIVE(Close ~ drift()))

## produce forecasts for January 2016
google_jan_2016 <- google_stock %>%
  filter(yearmonth(Date) == yearmonth("2016 Jan"))

google_fc <- google_fit %>%
  forecast(new_data = google_jan_2016)

## plot the forecasts
google_fc %>%
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close)

## augment() method
aug <- google_2015 %>%
  model(NAIVE(Close)) %>%
  augment()

autoplot(aug, .innov)

ACF(aug, .innov) %>%
  autoplot()

google_2015 %>%
  model(NAIVE(Close)) %>%
  gg_tsresiduals()

library(hnp)
hnp(na.omit(aug$.innov), half = FALSE, scale = TRUE,
    paint = TRUE, print = TRUE)

qqnorm(aug$.innov)
qqline(aug$.innov)

## Portmanteau tests
aug %>% features(.innov, box_pierce, lag = 10, dof = 0)
aug %>% features(.innov, ljung_box, lag = 10, dof = 0)
## these two tests agree
## null: series is not autocorrelated

## Hyndman et al. (2021) recommends
## use lag = 10 for non-seasonal data
## use lag = 2 * m (m = seasonal period) for seasonal data
## tests are questionable if lag > T / 5

## prediction intervals
google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  autoplot(google_2015)

## Bootstrapped invervals
fit <- google_2015 %>%
  model(NAIVE(Close))

sim <- fit %>% generate(h = 30, times = 5, bootstrap = TRUE)
## bootstrap = TRUE to sample from residuals
## bootstrap = FALSE to sample from assumed normal distribution

google_2015 %>%
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
            data = sim) +
  labs(title="Google daily closing stock price", y="$US") +
  theme_bw() +
  guides(colour = "none")

fc <- fit %>% forecast(h = 30, times = 5000, bootstrap = TRUE)

autoplot(fc, google_2015) +
  labs(title="Google daily closing stock price", y="$US") +
  theme_bw()

## Forecasting with decomposition
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade")

dcmp <- us_retail_employment %>%
  model(STL(Employed ~ trend(window = 7), robust = TRUE)) %>%
  components() %>%
  dplyr::select(-.model)

dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) +
  theme_bw() +
  labs(y = "Number of people",
       title = "US retail employment")

## the `decomposition_model()` function does this
## seasonal components will be forecast using `SNAIVE()` automatically
fit_dcmp <- us_retail_employment %>%
  model(stlf = decomposition_model(STL(Employed),
                                   NAIVE(season_adjust)))

fit_dcmp %>%
  forecast() %>%
  autoplot(us_retail_employment) +
  theme_bw() +
  labs(y = "Number of people",
       title = "US retail employment")

fit_dcmp %>% gg_tsresiduals()

## Evaluating forecast accuracy
train <- aus_production %>%
  filter_index("1992 Q1" ~ "2006 Q4") ## filter_index is very useful! (start ~ end)

beer_fit <- train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit %>% forecast(h = 14)

recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

beer_fc %>%
  autoplot(aus_production %>% filter(year(Quarter) >= 1992),
           level = NULL) +
  labs(y = "Megalitres",
       title = "Forecasts for quarterly beer production") +
  theme_bw() +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(beer_fc, recent_production)
## ME = mean error
## MPE = mean percentage error

# Exponential Smoothing ---------------------------------------------------

## Simple exponential smoothing
algeria_economy <- global_economy %>%
  filter(Country == "Algeria")

algeria_economy %>%
  autoplot(Exports) +
  theme_bw() +
  labs(y = "% of GDP", title = "Exports: Algeria")

alpha <- .5
f <- Vectorize(function(alpha = .5, h) {
  alpha * (1 - alpha) ^ h
}, "h")

f(h = 1:10)

## Estimate parameters and forecast
fit <- algeria_economy %>%
  model(ANN = ETS(Exports ~ error("A") + trend("N") + season("N")))
## additive error, no trend, no seasonal component

fc <- fit %>%
  forecast(h = 5)

tidy(fit)

autoplot(fc, algeria_economy) + theme_bw()

fc %>%
  autoplot(algeria_economy) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit)) +
  theme_bw() +
  labs(y="% of GDP", title="Exports: Algeria") +
  guides(colour = "none")

## Holt's linear method
ire_economy <- global_economy %>%
  filter(Country == "Ireland") %>%
  mutate(Pop = Population / 1e6)

autoplot(ire_economy, Pop) +
  theme_bw() +
  labs(y = "Millions", title = "Irish population")

fit <- ire_economy %>%
  model(AAN = ETS(Pop ~ error("A") + trend("A") + season("N")))
## additive error, additive trend, no seasonality

fc <- fit %>% forecast(h = 10)

tidy(fit)

fc %>%
  autoplot(ire_economy) +
  geom_line(aes(y = .fitted), col=2,
            data = augment(fit)) +
  theme_bw() +
  labs(y="Millions", title="Irish Population") +
  guides(colour = "none")

## Damped trend
ire_economy %>%
  model(
    `Holt's method` = ETS(Pop ~ error("A") + trend("A") + season("N")),
    `Damped Holt's method` = ETS(Pop ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  ) %>%
  forecast(h = 15) %>%
  autoplot(ire_economy, level = NULL) +
  theme_bw() +
  labs(title = "Irish population", y = "Millions") +
  guides(colour = guide_legend(title = "Forecast"))

## example: internet usage
www_usage <- as_tsibble(WWWusage)
www_usage %>% autoplot(value) +
  theme_bw() +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")

www_usage %>%
  model(SES = ETS(value ~ error("A") + trend("N") + season("N")),
        Holt = ETS(value ~ error("A") + trend("A") + season("N")),
        Damped = ETS(value ~ error("A") + trend("Ad") + season("N"))) %>%
  tidy

## which one is the best for 1-step ahead forecasting?
## stretch_tsibble creates sub-time series where we can evaluate forecasting accuracy
www_usage %>%
  stretch_tsibble(.init = 10) %>% ## first 10 points, then 11, then 12, ...
  model(SES = ETS(value ~ error("A") + trend("N") + season("N")),
        Holt = ETS(value ~ error("A") + trend("A") + season("N")),
        Damped = ETS(value ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 1) %>%
  accuracy(www_usage) %>%
  arrange(RMSE)

fit <- www_usage %>%
  model(Damped = ETS(value ~ error("A") + trend("Ad") + season("N")))

tidy(fit)

fit %>%
  forecast(h = 10) %>%
  autoplot(www_usage) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute") +
  theme_bw()

fit %>%
  gg_tsresiduals()

fit %>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 5)

fit %>%
  augment() %>%
  pull(.innov) %>%
  hnp(half = FALSE, scale = TRUE)

## Holt-Winters's seasonal method

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  dplyr::summarise(Trips = sum(Trips)/1e3)

aus_holidays %>%
  autoplot()

fit <- aus_holidays %>%
  model(add = ETS(Trips ~ error("A") + trend("A") + season("A")),
        mult = ETS(Trips ~ error("M") + trend("A") + season("M")))

fc <- fit %>% forecast(h = "3 years")

fc %>%
  autoplot(aus_holidays, level = NULL) +
  labs(title="Australian domestic tourism",
       y="Overnight trips (millions)") +
  theme_bw() +
  guides(colour = guide_legend(title = "Forecast"))

tidy(fit)