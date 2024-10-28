# effect of milk type on a particular marker
# continuous data

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

marker_df %>%
  ggplot(aes(x = milk_type, y = Marker)) +
  theme_bw() +
  geom_boxplot()

fit0 <- glm(Marker ~ milk_type,
            data = marker_df)
hnp(fit0)
anova(fit0, test = "F")

fit1 <- glm(log(Marker) ~ milk_type,
            data = marker_df)
hnp(fit1)
anova(fit1, test = "F")

fit2 <- glm(Marker ~ milk_type,
            family = Gamma,
            data = marker_df)
hnp(fit2)
anova(fit2, test = "F")

fit3 <- glm(Marker ~ milk_type,
            family = inverse.gaussian,
            data = marker_df)
hnp(fit3)
anova(fit3, test = "F")

fit4 <- gamlss(Marker ~ milk_type,
               sigma.formula = ~ milk_type,
               family = NO,
               data = marker_df)
wp(fit4)
drop1(fit4)
drop1(fit4, what = "sigma")