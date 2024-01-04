library(tidyverse)
library(fpp3)

# Benchmark forecasting methods -------------------------------------------

## GDP per capita
## Tidy the data
gdppc <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population)

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

fit %>%
  filter(Country == "Ireland") %>%
  report

## Forecast
fit %>% forecast(h = "3 years")

fit %>%
  filter(Country == "Ireland") %>%
  forecast(h = "3 years") %>%
  autoplot(gdppc) +
  theme_bw() +
  labs(y = "$US", title = "GDP per capita for Ireland")

## House registrations data
house_reg <- read.csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_time_series/data/house_reg.csv") %>%
  filter(County == "Dublin") %>%
  dplyr::select(Quarter, VALUE) %>%
  mutate(Year = as.numeric(substr(Quarter, 1, 4)),
         Quarter = substr(Quarter, 5, 6),
         YearQuarter = yearquarter(paste(Year, Quarter))) %>%
  dplyr::select(Year, Quarter, YearQuarter, VALUE) %>%
  as_tibble
names(house_reg)[4] <- c("Registrations")

house_reg <- house_reg %>%
  as_tsibble(index = YearQuarter)

house_reg %>%
  autoplot(Registrations) +
  theme_bw()

## Mean method
house_reg %>%
  model(MEAN(Registrations)) %>%
  forecast(h = "3 years") %>%
  autoplot(house_reg, level = NULL) + theme_bw()

## Naive method
house_reg %>%
  model(NAIVE(Registrations)) %>%
  forecast(h = "3 years") %>%
  autoplot(house_reg, level = NULL) + theme_bw()

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

## Comparing methods -- example 1: beer production
autoplot(aus_production)

## Set training data from 1992 to 2006
train <- aus_production %>%
  filter_index("1992 Q1" ~ "2006 Q4") ## filter_index is very useful! (start ~ end)

## Fit the models
beer_fit <- train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )
## Generate forecasts for 14 quarters
beer_fc <- beer_fit %>% forecast(h = 14)
## Plot forecasts against actual values
autoplot(beer_fc)
autoplot(beer_fc, data = beer)
autoplot(beer_fc, data = train)

beer_fc %>%
  autoplot(data = train, level = NULL) +
  autolayer(filter_index(aus_production, "2007 Q1" ~ .),
            colour = "black", lty = 2) +
  labs(y = "Megalitres",
       title = "Forecasts for quarterly beer production") +
  guides(colour = guide_legend(title = "Forecast")) +
  theme_bw()

## Comparing methods -- example 2: google stock
## Re-index based on trading days (market closed on weekends)
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  dplyr::mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)
## Filter the year of interest
google_2015 <- google_stock %>% filter(year(Date) == 2015)
# Fit the models
google_fit <- google_2015 %>%
  model(Mean = MEAN(Close),
        `Naïve` = NAIVE(Close),
        Drift = NAIVE(Close ~ drift()))
## Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock %>%
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit %>%
  forecast(new_data = google_jan_2016)
# Plot the forecasts
google_fc %>%
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black", lty = 2) +
  labs(y = "$US",
       title = "Google daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast")) +
  theme_bw()

aug <- google_2015 %>%
  model(NAIVE(Close)) %>%
  augment()

autoplot(aug, .innov) +
  labs(y = "$US",
       title = "Residuals from the naïve method")

aug %>%
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")

aug %>%
  ACF(.innov) %>%
  autoplot() +
  labs(title = "Residuals from the naïve method")

google_2015 %>%
  model(NAIVE(Close)) %>%
  gg_tsresiduals()

## Portmanteau tests (Box-Pierce and Ljung-Box)
aug %>% features(.innov, box_pierce, lag = 10, dof = 0)
aug %>% features(.innov, ljung_box, lag = 10, dof = 0)

## (Hyndman et al., 2021): use l = 10 (non-seasonal) or l = 2 * m (seasonal)
## iffy for l > T / 5

fit <- google_2015 %>% model(RW(Close ~ drift()))
tidy(fit)

augment(fit) %>% features(.innov, ljung_box, lag = 10, dof = 1)

## Prediction intervals
google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  hilo(.95)

google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  autoplot(google_2015) +
  theme_bw() +
  labs(title = "Google daily closing stock price", y = "$US")

## Bootstrapped intervals
fit <- google_2015 %>%
  model(NAIVE(Close))

sim <- fit %>% generate(h = 3, times = 5, bootstrap = TRUE)
sim

sim <- fit %>% generate(h = 30, times = 5, bootstrap = TRUE)

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
  select(-.model)

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

## which one is the best for 1-step ahead forecasting?
## stretch_tsibble creates sub-time series where we can evaluate forecasting accuracy
www_usage %>%
  stretch_tsibble(.init = 10) %>% ## first 10 points, then 11, then 12, ...
  model(SES = ETS(value ~ error("A") + trend("N") + season("N")),
        Holt = ETS(value ~ error("A") + trend("A") + season("N")),
        Damped = ETS(value ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 1) %>%
  accuracy(www_usage)

fit <- www_usage %>%
  model(Damped = ETS(value ~ error("A") + trend("Ad") + season("N")))

tidy(fit)

fit %>%
  forecast(h = 10) %>%
  autoplot(www_usage) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute") +
  theme_bw()

## Holt-Winters's seasonal method

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  dplyr::summarise(Trips = sum(Trips)/1e3)

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