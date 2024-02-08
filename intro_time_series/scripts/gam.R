library(fpp3)
library(tidyverse)
library(mgcv)

queensland_bulls <- aus_livestock %>%
  filter(Animal == "Bulls, bullocks and steers",
         State == "Queensland") %>%
  select(- Animal, - State) %>%
  mutate(year = year(Month),
         time = 1:n())

queensland_bulls %>%
  gg_season(Count, period = "year")

queensland_bulls_train <- queensland_bulls %>%
  filter(year(Month) < 2018)
queensland_bulls_test <- queensland_bulls %>%
  filter(year(Month) == 2018)

fit <- gam(Count ~ s(time, k = 30) + s(year, bs = "cc"),
           family = poisson,
           data = queensland_bulls_train)
summary(fit)

library(gratia)
draw(fit)

queensland_bulls %>%
  mutate(pred = predict(fit, newdata = queensland_bulls, type = "response")) %>%
  autoplot(Count) +
  theme_bw() +
  geom_line(data = queensland_bulls_test, col = 4) +
  geom_line(aes(y = pred), col = 2)
  
fit2 <- gam(Count ~ te(time, year, bs = "cc"),
            family = poisson,
            data = queensland_bulls_train)
summary(fit2)

AIC(fit)
AIC(fit2)

library(gratia)
draw(fit2)

queensland_bulls %>%
  mutate(pred = predict(fit2, newdata = queensland_bulls, type = "response")) %>%
  autoplot(Count) +
  theme_bw() +
  geom_line(data = queensland_bulls_test, col = 4) +
  geom_line(aes(y = pred), col = 2)

fit3 <- gamm(Count ~ te(time, year, bs = "cc"),
             family = poisson,
             data = queensland_bulls_train,
             correlation = corARMA(form = ~ 1 | year, p = 1))
summary(fit3$lme)
summary(fit3$gam)

AIC(fit3$lme)

queensland_bulls %>%
  mutate(pred = predict(fit3$gam, newdata = queensland_bulls, type = "response")) %>%
  autoplot(Count) +
  theme_bw() +
  geom_line(data = queensland_bulls_test, col = 4) +
  geom_line(aes(y = pred), col = 2)

fit4 <- gamm(Count ~ te(time, year, bs = "cc"),
             family = poisson,
             data = queensland_bulls_train,
             correlation = corARMA(form = ~ 1 | year, p = 2))
AIC(fit4$lme)
summary(fit4$lme)

queensland_bulls %>%
  mutate(pred = predict(fit4$gam, newdata = queensland_bulls, type = "response")) %>%
  autoplot(Count) +
  theme_bw() +
  geom_line(data = queensland_bulls_test, col = 4) +
  geom_line(aes(y = pred), col = 2)