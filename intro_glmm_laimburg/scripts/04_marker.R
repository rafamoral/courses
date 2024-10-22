library(tidyverse)
library(readxl)
library(hnp)
library(gamlss)
library(lme4)
library(lmerTest)

marker_df <- read_xlsx("04_marker_input-output_data.xlsx")
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