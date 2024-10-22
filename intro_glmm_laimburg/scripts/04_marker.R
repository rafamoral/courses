library(tidyverse)
library(readxl)
library(hnp)
library(gamlss)
library(lme4)
library(lmerTest)

marker_df <- read_csv("../data/04_marker_input-output_data.csv")
marker_df <- marker_df %>%
  mutate(season = as.factor(season),
         silagetype = as.factor(silagetype),
         farmcode = as.factor(farmcode))

marker_df %>%
  ggplot(aes(x = markerin, y = markerout)) +
  theme_bw() +
  facet_grid(season ~ silagetype) +
  geom_point(alpha = .2)

fit1 <- lmer(sqrt(markerout) ~ (1 | farmcode) +
               season * poly(markerin, 2) * silagetype,
             data = marker_df)
summary(fit1)
anova(fit1)
hnp(fit1)

fit2 <- gamlss(markerout + .5 ~ random(farmcode) +
                 season * poly(markerin, 2) * silagetype,
               sigma.formula = ~ season * poly(markerin, 2) * silagetype,
               family = GA,
               data = marker_df)
wp(fit2)

fit_pred <- gamlss(markerout + .5 ~ season * poly(markerin, 2) * silagetype,
                   sigma.formula = ~ season * poly(markerin, 2) * silagetype,
                   family = GA,
                   data = marker_df)
marker_pred <- expand.grid(season = levels(marker_df$season),
                           markerin = seq(0, 250, length = 200),
                           silagetype = levels(marker_df$silagetype))
marker_pred$markerout <- predict(fit_pred, newdata = marker_pred,
                                 what = "mu", type = "response")

marker_df %>%
  ggplot(aes(x = markerin, y = markerout)) +
  theme_bw() +
  facet_grid(season ~ silagetype) +
  geom_point(alpha = .2) +
  geom_line(data = marker_pred)