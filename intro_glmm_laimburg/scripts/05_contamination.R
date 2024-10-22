library(tidyverse)
library(readxl)
library(hnp)
library(gamlss)
library(lme4)
library(lmerTest)

contamination_df <- read_xlsx("05_milk_contamination_data.xlsx")
contamination_df <- contamination_df %>%
  mutate(obs_ID = as.factor(obs_ID),
         farmcode = as.factor(farmcode))

contamination_df %>%
  ggplot(aes(x = doy, y = contamination)) +
  theme_bw() +
  geom_line(aes(groups = farmcode),
            alpha = .5) +
  geom_smooth(se = FALSE) +
  facet_grid(overseed + rain ~ stubbleheight + springfym)

fit1 <- lmer(contamination ~ (doy | farmcode) +
               (doy + rain + stubbleheight + springfym + overseed) ^ 3,
             data = contamination_df)
summary(fit1)
anova(fit1)
hnp(fit1, verb = TRUE)

fit2 <- gamlss(contamination ~ random(farmcode) +
                 (doy + rain + stubbleheight + springfym + overseed) ^ 3,
               sigma.formula = ~ (doy + rain + stubbleheight + springfym + overseed) ^ 3,
               family = NBI,
               data = contamination_df)
wp(fit2)
plot(fit2)

contamination_df %>%
  mutate(yhat = predict(fit2, what = "mu", type = "response")) %>%
  ggplot(aes(x = contamination, y = yhat)) +
  theme_bw() +
  geom_point(alpha = .2) +
  geom_abline(intercept = 0, slope = 1, lty = 2)