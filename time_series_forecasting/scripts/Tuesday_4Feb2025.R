library(tidyverse)
library(fpp3)

## are methods with smaller residuals "better" than methods with larger residuals?
## a toy example
beta0 <- 2
beta1 <- -1
beta2 <- .5

set.seed(422025)
x <- runif(100, -3, 10)
y <- rnorm(100, beta0 + beta1 * x + beta2 * x^2, 5)

plot(x, y)

fit1 <- lm(y ~ x)
fit2 <- lm(y ~ poly(x, 2))
fit3 <- lm(y ~ poly(x, 3))
fit4 <- lm(y ~ poly(x, 4))
fit10 <- lm(y ~ poly(x, 10))

abline(fit1, col = 2)
sum(resid(fit1)^2)

x_grid <- seq(-3, 10, length = 200)
lines(x_grid, predict(fit2, data.frame(x = x_grid)), col = 4)
sum(resid(fit2)^2)

lines(x_grid, predict(fit3, data.frame(x = x_grid)), col = 3)
sum(resid(fit3)^2)

lines(x_grid, predict(fit4, data.frame(x = x_grid)), col = 5)
sum(resid(fit4)^2)

lines(x_grid, predict(fit10, data.frame(x = x_grid)), col = 2, lwd = 2, lty = 2)
sum(resid(fit10)^2)


# Evaluating a time series model fit --------------------------------------

## example: google stock prices
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)

## filter for the year 2015
google_2015 <- google_stock %>%
  filter(year(Date) == 2015)

## fit the benchmarking forecast methods
google_fit <- google_2015 %>%
  model(Mean = MEAN(Close),
        Naive = NAIVE(Close),
        Drift = RW(Close ~ drift()))

## produce forecasts for January/2016
google_jan_2016 <- google_stock %>%
  filter(yearmonth(Date) == yearmonth("2016 Jan"))

google_fc <- google_fit %>%
  forecast(new_data = google_jan_2016)

## plot the forecasts
google_fc %>%
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, col = "gray") +
  theme_bw()

## "augment" the model
aug <- google_2015 %>%
  model(NAIVE(Close)) %>%
  augment()
## when using fable always work with ".innov", the "innovation residuals"

aug %>%
  ggplot(aes(x = .innov)) +
  geom_histogram()

hist(aug$.innov)

shapiro.test(aug$.innov) ## assessing normality
qqnorm(aug$.innov); qqline(aug$.innov)
## using a simulated envelope in a qq-plot
hnp::hnp(na.omit(aug$.innov), half = FALSE, scale = TRUE,
         paint = TRUE)

## assessing autocorrelation
aug %>%
  ACF(.innov) %>%
  autoplot()

## doing everything in one go
google_2015 %>%
  model(NAIVE(Close)) %>%
  gg_tsresiduals()

## portmanteau tests for autocorrelation
## Box-Pierce and Ljung-Box
aug %>%
  features(.innov, box_pierce, lag = 10, dof = 0)
aug %>%
  features(.innov, ljung_box, lag = 10, dof = 0)

## (Hyndman et al., 2021): use 10 lags for non-seasonal time series
## and 2 * m lags for seasonal time series with seasonal period m

## let's calculate some prediction intervals
google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10)

curve(dnorm(x, 759, sqrt(125)), xlim = c(700, 820))
qnorm(c(.025, .975), mean = 759, sd = sqrt(125))

google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  hilo(.95)

google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 100) %>%
  autoplot(google_2015)

## let's calculate bootstrapped prediction intervals (distribution agnostic)
fit <- google_2015 %>%
  model(NAIVE(Close))
  
fit %>%
  generate(h = 3, times = 2, bootstrap = TRUE)

sim <- fit %>%
  generate(h = 30, times = 5, bootstrap = TRUE)

google_2015 %>%
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(data = sim,
            aes(y = .sim, col = .rep))

boot_fit <- fit %>%
  forecast(h = 30, times = 10000, bootstrap = TRUE)

boot_fit %>%
  autoplot(google_2015)

google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 30) %>%
  autoplot(google_2015)

## forecasting with decomposition
retail <- us_employment %>%
  filter(Title == "Retail Trade", year(Month) >= 1990) %>%
  select(- Series_ID, - Title)

decomp <- retail %>%
  model(STL(Employed)) %>%
  components() %>%
  select(- .model)

## here we are only using the naive method for the seasonally adjusted series
decomp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(decomp)

## to forecast with decomposition, fable will use
## the seasonal naive method automatically for the seasonal component
## and we can choose whatever we want for the seasonally ajusted component
## to do that we use the function "decomposition_model"
fit_decomp <- retail %>%
  model(decomposition_model(STL(Employed),
                            NAIVE(season_adjust)))

fit_decomp %>%
  forecast() %>%
  autoplot(retail)

fit_decomp %>%
  gg_tsresiduals()

## evaluating point forecast accuracy
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

train <- aus_production %>%
  filter_index("1956 Q1" ~ "1991 Q4")

beer_fit <- train %>%
  model(Mean = MEAN(Beer),
        Naive = NAIVE(Beer),
        Seasonal_naive = SNAIVE(Beer),
        Drift = RW(Beer ~ drift()))

beer_forecast <- beer_fit %>%
  forecast(h = 14)

accuracy(beer_forecast, recent_production) %>%
  arrange(RMSE)


# Exponential smoothing ---------------------------------------------------

## Simple Exponential Smoothing (SES)

## example: Algerian exports
algeria_economy <- global_economy %>%
  filter(Country == "Algeria")

algeria_economy %>%
  autoplot(Exports)

## estimate the model parameters (alpha and l_0) and produce forecasts
fit <- algeria_economy %>%
  model(SES = ETS(Exports ~ error("A") + trend("N") + season("N")))

fit %>%
  report()

fit %>%
  tidy()

fit %>%
  gg_tsresiduals()

fit %>%
  forecast(h = 5) %>%
  autoplot(algeria_economy)

fit %>%
  forecast(h = 5, times = 10000, bootstrap = TRUE) %>%
  autoplot(algeria_economy)

fit %>%
  forecast(h = 5) %>%
  autoplot(algeria_economy) +
  geom_line(data = augment(fit),
            aes(y = .fitted),
            col = 2) +
  theme_bw()