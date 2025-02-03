library(tidyverse)

# Introduction and terminology --------------------------------------------

## a white noise process
set.seed(3022025)

wn <- rnorm(100)

plot(wn, type = "l")
plot.ts(wn)

plot.ts(rnorm(100))
## we want residuals from a time series model to look like this

## a model with a trend
beta0 <- 5
beta1 <- 2.5
tt <- 1:100

e <- rnorm(100, 0, 20)
y <- beta0 + beta1 * tt + e

plot.ts(y)
## not stationary

## a moving average (MA(1))
e <- rnorm(100, 0, 2)
y <- e[2:100] - .5 * e[1:99]

plot.ts(y)
## it can be theoretically proven that all MA(q) series **are** stationary

## a cumulative product of white noise variables
e <- rnorm(100, 0, 2)
y <- cumprod(e)

plot.ts(y)
## not stationary: the variance changes


# A tidy forecasting workflow ---------------------------------------------

library(fpp3)

## a dataset is loaded in base R as a "data.frame"
## the tidyverse version of a data.frame is called a "tibble"
## the tidy dataset for a time series is called "tsibble"

my_tibble <- tibble(Year = 2015:2019,
                    Observation = c(123, 30, 40, 78, 100))

## creating a tsibble directly
my_tsibble <- tsibble(Year = 2015:2019,
                      Observation = c(123, 30, 40, 78, 100),
                      index = Year)

## converting a tibble/data.frame into a tsibble
## %>% or |> are "pipes"; they are read as "and then"
my_tibble %>%
  as_tsibble(index = Year)

## a second example
monthly_tibble <- tibble(Month = c("2024 Jan", "2024 Feb", "2024 Mar", "2024 Apr"),
                         Observation = c(1, 40, 50, 3))

monthly_tsibble <- monthly_tibble %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

## filling gaps in a time series
monthly_tibble <- tibble(Month = c("2024 Jan", "2024 Feb", "2024 Apr", "2024 Jun"),
                         Observation = c(1, 40, 50, 3))

monthly_tsibble <- monthly_tibble %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month) %>%
  fill_gaps()

## an example with quarterly data, and reversing the order of the string (quarter first, then year)
## (you can also mix)
quarterly <- tibble(Quarter = c("Q1 2024", "Q2 2024", "Q3 2024", "Q4 2024"),
                    Observation = c(1, 40, 50, 3))

## also works
quarterly %>%
  mutate(Quarter = yearquarter(Quarter)) %>%
  as_tsibble(index = Quarter)

## example dataset: prison data
prison <- read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")
prison

prison <- prison %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(- Date) %>%
  as_tsibble(index = Quarter,
             key = c(State, Gender, Legal, Indigenous))
## warning: sometimes if the index argument is omitted, as_tsibble will
##          try and figure out which variable to use automatically
##          always make sure it's using the correct one

autoplot(prison) +
  theme_bw() +
  theme(legend.position = "none")

## example: ansett data
ansett

melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers / 1000)

melsyd_economy %>%
  autoplot(Passengers)

## you can customise further using ggplot functions
melsyd_economy %>%
  autoplot(Passengers) +
  ylab("Passengers (thousands)") +
  ggtitle("Ansett Melbourne-Sydney economy class")

## exactly the same as...
melsyd_economy %>%
  ggplot(aes(y = Passengers, x = Week)) +
  geom_line() +
  ylab("Passengers (thousands)") +
  ggtitle("Ansett Melbourne-Sydney economy class") +
  xlab("Week [1W]")

## seasonal plots
## aimsir17 data
library(aimsir17)

demand <- eirgrid17 %>%
  distinct(date, .keep_all = TRUE) %>%
  as_tsibble(index = date)

demand %>%
  autoplot(IEDemand)

demand %>%
  fill_gaps() %>%
  gg_season(IEDemand, period = "day")

demand %>%
  fill_gaps() %>%
  gg_season(IEDemand, period = "week")

demand %>%
  fill_gaps() %>%
  gg_season(IEDemand, period = "month")

## Mauna Loa CO2 data

new_co2 <- as_tsibble(co2)

new_co2 %>%
  autoplot()

new_co2 %>%
  gg_season(value, period = "year")

year(new_co2$index)
month(new_co2$index)

## plotting y_t versus y_{t-1} (lag 1)
yt <- new_co2$value
yt_lag1 <- yt[1:467]
plot(yt[-1], yt_lag1)
abline(0, 1, col = 2, lwd = 2)
cor(yt[-1], yt_lag1)

## use gg_lag to do these plots automatically for us
new_co2 %>%
  gg_lag(value, geom = "point", lags = 1:12,
         alpha = .3, size = 1)
## alpha = alpha-blending = adding transparency

## empirical autocorrelation function plot
## empirical ACF

acf(co2)
## we see humps at multiples of 12
## clear signature of yearly seasonal behaviour for monthly data


# Time series decomposition -----------------------------------------------

## example: US retail employment
retail <- us_employment %>%
  filter(Title == "Retail Trade", year(Month) >= 1990) %>%
  select(- Series_ID, - Title)

retail %>%
  autoplot()

dcmp <- retail %>%
  model(my_decomposition = STL(Employed))

components(dcmp)

components(dcmp) %>%
  autoplot()

## seasonally adjusted component = trend + remainder

components(dcmp) %>%
  ggplot(aes(x = Month, y = season_adjust)) +
  theme_bw() +
  geom_line()


# Moving averages (the smoother!) -----------------------------------------

## moving averages in time series can have different meaning
## moving average smoothing
## moving average as in ARIMA modelling

## example: Irish exports of goods and services
irish_exports <- global_economy %>%
  filter(Country == "Ireland")

irish_exports %>%
  autoplot(Exports)

exports <- irish_exports %>%
  pull(Exports)

forecast::ma(exports, order = 5)

slider::slide_dbl(exports, mean,
                  .before = 2,
                  .after = 1,
                  .complete = TRUE)

irish_exports <- irish_exports %>%
  select(Year, Exports) %>%
  mutate("3-MA" = forecast::ma(exports, order = 3),
         "5-MA" = forecast::ma(exports, order = 5),
         "7-MA" = forecast::ma(exports, order = 7),
         "9-MA" = forecast::ma(exports, order = 9)) %>%
  pivot_longer(3:6,
               names_to = "MA_order",
               values_to = "MA")

irish_exports %>%
  ggplot(aes(x = Year, y = Exports)) +
  theme_bw() +
  geom_line() +
  geom_line(aes(y = MA), col = 2) +
  facet_wrap(~ MA_order)

## example: Australian quarterly beer production
beer <- aus_production %>%
  filter(year(Quarter) >= 1990) %>%
  select(Quarter, Beer)

beer %>%
  autoplot()

## Classical decomposition -- additive
retail %>%
  model(classical_decomposition(Employed, type = "additive")) %>%
  components() %>%
  autoplot()

## Classical decomposition -- multiplicative
retail %>%
  model(classical_decomposition(Employed, type = "multiplicative")) %>%
  components() %>%
  autoplot()

## X-11 decomposition
library(seasonal)

retail %>%
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) %>%
  components() %>%
  autoplot()

## SEATS decomposition

retail %>%
  model(x11 = X_13ARIMA_SEATS(Employed ~ seats())) %>%
  components() %>%
  autoplot()

## STL

retail %>%
  model(my_decomposition = STL(Employed)) %>%
  components() %>%
  autoplot()


# Forecasting workflow ----------------------------------------------------

## example: GDP per capita data

## tidy the data
gdppc <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population)

## visualise the data
gdppc %>%
  filter(Country == "Ireland") %>%
  autoplot(GDP_per_capita)

gdppc %>%
  filter(Country == "United States") %>%
  autoplot(GDP_per_capita)

gdppc %>%
  filter(Country %in% c("Ireland", "United States",
                        "Canada", "United Kingdom")) %>%
  autoplot(GDP_per_capita) +
  facet_wrap(~ Country)

## define/specify a model
fit <- gdppc %>%
  filter(Country %in% c("Ireland", "United States",
                        "Canada", "United Kingdom")) %>%
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

fit

glance(fit)

report(fit)

fit %>%
  filter(Country == "Canada") %>%
  report()

fit %>%
  filter(Country == "Canada") %>%
  tidy()

## forecast
fit %>%
  forecast(h = "3 years")

fit %>%
  forecast(h = "3 years") %>%
  autoplot(gdppc)

my_plot <- fit %>%
  filter(Country == "Canada") %>%
  forecast(h = "3 years") %>%
  autoplot(gdppc) +
  theme_minimal() +
  ylab("$US") +
  ggtitle("GDP per capita in Canada")

## saving with high resolution
png("figure1.png", res = 800, units = "in", w = 6, h = 4)
print(my_plot)
dev.off()


# Benchmarking forecast methods -------------------------------------------

## reading the household registrations data locally
house_reg <- read_csv("house_reg_dublin.csv")

## reading directly from GitHub
house_reg <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/time_series_forecasting/data/house_reg_dublin.csv")

## converting to a tsibble
house_reg <- house_reg %>%
  mutate(YearQuarter = yearquarter(YearQuarter)) %>%
  as_tsibble(index = YearQuarter)

house_reg %>%
  autoplot(Registrations)

house_reg %>%
  gg_season(Registrations, period = "year")
## there doesn't seem to be any clear seasonal behaviour here

## Mean method
house_reg %>%
  model(MEAN(Registrations)) %>%
  forecast(h = "3 years") %>%
  autoplot(house_reg, level = NULL)

## Naive (Random Walk) method
house_reg %>%
  model(NAIVE(Registrations)) %>%
  forecast(h = "3 years") %>%
  autoplot(house_reg, level = NULL)

## Seasonal Naive method
house_reg %>%
  model(SNAIVE(Registrations)) %>%
  forecast(h = "3 years") %>%
  autoplot(house_reg, level = NULL)

## Drift method
house_reg %>%
  model(RW(Registrations ~ drift())) %>%
  forecast(h = "3 years") %>%
  autoplot(house_reg, level = NULL)

## example: beer production
## comparing the 4 benchmark forecasting methods

train <- aus_production %>%
  filter_index("1992 Q1" ~ "2006 Q4")
## you can use filter_index("start" ~ "end")

## fit the models
beer_fit <- train %>%
  model(Mean = MEAN(Beer),
        Naive = NAIVE(Beer),
        Seasonal_naive = SNAIVE(Beer),
        Drift = RW(Beer ~ drift()))

beer_forecast <- beer_fit %>%
  forecast(h = 14)

beer_forecast %>%
  autoplot(aus_production %>%
             filter_index("1990 Q1" ~ "2010 Q2"),
           level = NULL) +
  theme_minimal()