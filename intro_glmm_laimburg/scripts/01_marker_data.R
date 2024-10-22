library(tidyverse)
library(readxl)
library(hnp)
library(gamlss)

marker_df <- read_csv("../data/01_marker_data.csv")
marker_df <- marker_df %>%
  mutate(milk_type = `Milk type`) %>%
  filter(milk_type != "H") %>%
  dplyr::select(- `Milk type`)

marker_df %>%
  group_by(milk_type) %>%
  summarise(mean = mean(Marker),
            sd = sd(Marker))

fit0 <- glm(Marker ~ milk_type,
            data = marker_df)
hnp(fit1)
anova(fit1)

fit1 <- glm(log(Marker) ~ milk_type,
            data = marker_df)
hnp(fit1)
anova(fit1)

fit2 <- glm(Marker ~ milk_type,
            family = Gamma,
            data = marker_df)
hnp(fit2)
anova(fit2)

fit3 <- glm(Marker ~ milk_type,
            family = inverse.gaussian,
            data = marker_df)
hnp(fit3)
anova(fit3)

fit4 <- gamlss(Marker ~ milk_type,
               sigma.formula = ~ milk_type,
               family = NO,
               data = marker_df)
wp(fit4)