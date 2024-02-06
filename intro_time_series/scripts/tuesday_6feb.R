library(tidyverse)

# Introduction and terminology --------------------------------------------

## white noise (normal)
epsilon_t <- rnorm(100)
epsilon_t

plot(epsilon_t, type = "l")
plot.ts(epsilon_t)

## a model with a trend
beta0 <- 2
beta1 <- 1
sigma <- 20

y <- beta0 + beta1 * (1:100) + epsilon_t * sigma
plot.ts(y)
## the mean depends on the time t, therefore not stationary

## a moving average
epsilon_t <- rnorm(100)
y <- epsilon_t[2:100] - 1/2 * epsilon_t[1:99]
plot.ts(y)
## NB: MA models are always stationary

## a product of white noise
epsilon_t <- rnorm(100)
y <- cumprod(epsilon_t)
plot.ts(y)

# tsibbles ----------------------------------------------------------------

library(fpp3)

y <- tsibble(Year = 2015:2019,
             Observation = c(123, 12, 30, 100, 50),
             index = Year)

y <- tibble(Month = c("2019 Jan","2019 Feb","2019 Mar","2019 Apr","2019 May"),
            Observation = c(50,23,34,30,25))

y %>%
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

## Time plots
?ansett
ansett

melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers/1000)

ggplot(melsyd_economy, aes(x = Week, y = Passengers)) +
  geom_line()

autoplot(melsyd_economy)
autoplot(melsyd_economy, Passengers)

autoplot(melsyd_economy, Passengers) +
  theme_bw() +
  xlab("Week") +
  ylab("Passengers in thousands")

## aimsir17 data
library(aimsir17)

?eirgrid17
eirgrid17

demand <- eirgrid17 %>%
  as_tsibble(index = date) ## gives error due to duplicated entries
  
duplicates(eirgrid17)

eirgrid17 %>%
  group_by(date) %>%
  summarise(IEDemand = mean(IEDemand))

demand <- eirgrid17 %>%
  distinct(date, .keep_all = TRUE) %>% ## keeping first occurrence
  as_tsibble(index = date)

autoplot(demand, IEDemand)

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
?co2
plot(co2)

new_co2 <- as_tsibble(co2)

autoplot(new_co2)

new_co2 %>%
  gg_season(value, period = "year")

month(new_co2$index)
year(new_co2$index) ## using lubridate

substr(new_co2$index, 1, 4) %>% as.numeric ## roundabout way using string manipulation

new_co2 %>%
  gg_lag(value, geom = "point", alpha = .3, lag = 1:12) +
  xlab("lag(CO2, k)") +
  ylab("CO2 concentration in ppm")

new_co2 %>%
  ACF %>%
  autoplot

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

components(dcmp) %>%
  autoplot()

## seasonally adjusted
components(dcmp) %>%
  autoplot(season_adjust)

## Moving averages
gdp_ire <- global_economy %>%
  filter(Country == "Ireland") %>%
  mutate(GDPpercap = GDP/Population)

autoplot(gdp_ire, Exports)

exp_ire <- gdp_ire %>% pull(Exports) ## same as gdp_ire$Exports

forecast::ma(exp_ire, order = 5)
slider::slide_dbl(exp_ire, mean, .before = 2, .after = 2,
                  .complete = TRUE)

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
  geom_line() +
  geom_line(aes(y = MA), col = 2) +
  facet_wrap(~ MA_order)

## Classical decomposition -- additive
us_retail_employment %>%
  model(cd = classical_decomposition(Employed, type = "additive")) %>%
  components() %>%
  autoplot()

## Classical decomposition -- multiplicative
us_retail_employment %>%
  model(cd = classical_decomposition(Employed, type = "multiplicative")) %>%
  components() %>%
  autoplot()

## alternative decomposition methods
library(seasonal)

x11_dcmp <- us_retail_employment %>%
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) %>%
  components()

autoplot(x11_dcmp)

seats_dcmp <- us_retail_employment %>%
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) %>%
  components

autoplot(seats_dcmp)

us_retail_employment %>%
  model(STL(Employed ~ trend(window = 12))) %>%
  components() %>%
  autoplot()