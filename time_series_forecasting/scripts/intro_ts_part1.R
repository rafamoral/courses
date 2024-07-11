library(tidyverse)

# Introduction and terminology --------------------------------------------

## white noise
plot.ts(rnorm(100), xlab = "time", ylab = expression(epsilon[t]))

## a model with a trend
beta0 <- 2
beta1 <- 1
sigma <- 20

y <- rnorm(100, beta0 + beta1 * 1:100, sigma)
plot.ts(y, xlab = "time", ylab = "y")
## the mean depends on t, so not stationary

## a moving average
epsilon <- rnorm(100, sd = 1)
y <- epsilon[2:100] - .5 * epsilon[1:99]

plot.ts(y, xlab = "time", ylab = "y")

## a product of white noise variables
epsilon <- rnorm(100, sd = 1)
y <- cumprod(epsilon)

plot.ts(y, xlab = "time", ylab = "y")

# Working with tsibbles ---------------------------------------------------

library(fpp3)

y <- tsibble(Year = 2015:2019,
             Observation = c(123, 39, 78, 52, 110),
             index = Year)
y

z <- tibble(Month = c("2019 Jan","2019 Feb","2019 Mar","2019 Apr","2019 May"),
            Observation = c(50,23,34,30,25))

z %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

## prison data
prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")
prison

prison <- prison %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(- Date) %>%
  as_tsibble(key = c(State, Gender, Legal, Indigenous),
             index = Quarter)
head(prison)

## Time plots

## ansett data
ansett

melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers/1000)

autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

## same as
ggplot(melsyd_economy, aes(x = Week, y = Passengers)) +
  geom_line() +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

## aimsir17 data
library(aimsir17)

demand <- eirgrid17 %>%
  distinct(date, .keep_all = TRUE) %>%
  as_tsibble(index = date)

demand %>%
  autoplot(IEDemand) +
  ggtitle("Energy demand in Ireland")

demand %>%
  fill_gaps() %>%
  gg_season(IEDemand, period = "day") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Daily energy demand in 2017: Ireland")

demand %>%
  fill_gaps() %>%
  gg_season(IEDemand, period = "week") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Weekly energy demand in 2017: Ireland")

demand %>%
  fill_gaps() %>%
  gg_season(IEDemand, period = "month") +
  labs(y = "MWh", title = "Monthly energy demand in 2017: Ireland")

## Mauna Loa CO2 data
new_co2 <- as_tsibble(co2)
new_co2

autoplot(new_co2) +
  ylab("CO2 concentration (ppm)")

new_co2 %>%
  mutate(month = month(index),
         year = year(index)) %>%
  gg_season(value, period = "year") +
  ylab("CO2 concentration (ppm)")

## lag plots -- helpful to uncover seasonal patterns
new_co2 %>%
  mutate(month = month(index),
         year = year(index)) %>%
  gg_lag(value, geom = "point", lags = 1:12, alpha = .4) +
  labs(x = "lag(CO2, k)",
       y = "CO2 concentration (ppm)")

# Time series decomposition -----------------------------------------------

## US retail employment data
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))

components(dcmp) %>% head

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Employed, colour="gray") +
  theme_bw() +
  geom_line(aes(y = trend), colour = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

components(dcmp) %>% autoplot

## seasonally adjusted
components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail") +
  theme_bw()

## Moving averages

## Irish exports data
gdp_ire <- global_economy %>%
  filter(Country == "Ireland") %>%
  mutate(GDPpercap = GDP/Population)

gdp_ire %>%
  autoplot(Exports) +
  theme_bw() +
  ylab("% of GDP") +
  ggtitle("Total Irish Exports of Goods and Services: 1960 -- 2017")

exp_ire <- gdp_ire %>% pull(Exports)
ma5 <- forecast::ma(exp_ire, order = 5) %>% as.numeric

ma5table <- tibble("Year" = 1960:2017, "Exports" = exp_ire, "5-MA" = ma5) %>%
  round(2)

head(ma5table)
tail(ma5table)

## we can use the slide_dbl function to create sliding windows
ma5 <- slider::slide_dbl(exp_ire, mean,
                         .before = 2, .after = 2,
                         .complete = TRUE)

gdp_ire %>%
  autoplot(Exports) +
  theme_bw() +
  ylab("% of GDP") +
  ggtitle("Total Irish Exports of Goods and Services: 1960 -- 2017") +
  geom_line(data = ma5table, aes(y = `5-MA`), col = 2, lwd = 1)

gdp_ire_long <- gdp_ire %>%
  mutate("3-MA" = forecast::ma(Exports, order = 3),
         "5-MA" = forecast::ma(Exports, order = 5),
         "7-MA" = forecast::ma(Exports, order = 7),
         "9-MA" = forecast::ma(Exports, order = 9)) %>%
  dplyr::select(Year, Exports, `3-MA`, `5-MA`, `7-MA`, `9-MA`) %>%
  pivot_longer(3:6,
               names_to = "MA_order",
               values_to = "MA")

gdp_ire_long %>%
  ggplot(aes(x = Year, y = Exports)) +
  theme_bw() +
  geom_line() +
  geom_line(aes(y = MA), col = 2, lwd = 1) +
  facet_wrap(~ MA_order) +
  ylab("% of GDP") +
  ggtitle("Total Irish Exports of Goods and Services: 1960 -- 2017")

## Australian beer production data
beer <- aus_production %>%
  filter(year(Quarter) >= 1992) %>%
  dplyr::select(Quarter, Beer)

beer_ma <- beer %>%
  mutate(
    `4-MA` = slider::slide_dbl(Beer, mean,
                               .before = 1, .after = 2, .complete = TRUE),
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE)
  )

beer_ma %>%
  autoplot(Beer, col = "gray") +
  theme_bw() +
  geom_line(aes(y = `2x4-MA`), colour = 2, lwd = 1)

## US retail employment data
us_retail_employment_ma <- us_retail_employment %>%
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )

us_retail_employment_ma %>%
  autoplot(Employed, col = "gray") +
  theme_bw() +
  geom_line(aes(y = `2x12-MA`), colour = 2, lwd = 1) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail",
       subtitle = "2x12-MA in red")

## Classical decomposition -- additive
us_retail_employment %>%
  model(
    classical_decomposition(Employed, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of total US retail employment") +
  theme_bw()

## Classical decomposition -- multiplicative
us_retail_employment %>%
  model(
    classical_decomposition(Employed, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of total US retail employment") +
  theme_bw()

## x11 and SEATS
library(seasonal)

x11_dcmp <- us_retail_employment %>%
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) %>%
  components()

autoplot(x11_dcmp) +
  theme_bw() +
  labs(title = "Decomposition of total US retail employment using X-11.")

seats_dcmp <- us_retail_employment %>%
  model(x11 = X_13ARIMA_SEATS(Employed ~ seats())) %>%
  components()

autoplot(seats_dcmp) +
  theme_bw() +
  labs(title = "Decomposition of total US retail employment using SEATS.")

## STL decomposition
us_retail_employment %>%
  model(
    STL(Employed)
  ) %>%
  components() %>%
  autoplot() +
  theme_bw()