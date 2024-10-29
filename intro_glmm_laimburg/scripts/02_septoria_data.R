# Experimental question: Effect of preceding crop,
# wheat cultivar, dose splitting and fertilisation level
# on the infection with Septoria.
# Design: split-plot design with four blocks.
# Within each block, the preceding crop is the main plot;
# wheat cultivar, dose splitting and fertilisation level
# (all treated as categorial) are randomised within the
# preceding crop.


library(tidyverse)
library(readxl)
library(hnp)
library(gamlss)
library(lme4)
library(lmerTest)

septoria_df <- read_csv("../data/02_Septoria_data.csv")
septoria_df <- septoria_df %>%
  mutate(across(where(is.character), as.factor),
         Block = as.factor(Block),
         Plot = as.factor(Plot))

septoria_df %>%
  ggplot(aes(x = Fertilisation_level, y = Infection/100, fill = Dose_splitting)) +
  theme_bw() +
  facet_grid(Preceding_crop ~ Wheat_cultivar) +
  geom_boxplot()

fit1 <- lmer(sqrt(Infection) ~ (1 | Block) + (1 | Block:Preceding_crop) +
               Fertilisation_level * Preceding_crop * Dose_splitting * Wheat_cultivar,
             data = septoria_df)
fit1 <- lmer(sqrt(Infection) ~ (1 | Block/Preceding_crop) +
               Fertilisation_level * Preceding_crop * Dose_splitting * Wheat_cultivar,
             data = septoria_df)
anova(fit1)
summary(fit1)

hnp(fit1, verb = TRUE, print = TRUE, paint = TRUE)

septoria_df %>%
  ggplot(aes(x = Wheat_cultivar, y = Infection/100)) +
  theme_bw() +
  facet_grid(~ Preceding_crop) +
  geom_boxplot()

## create the std error function
se <- function(x) sd(x)/sqrt(length(x))

septoria_df %>%
  group_by(Fertilisation_level) %>%
  summarise(mean = mean(Infection),
            std_err = se(Infection))

septoria_df %>%
  group_by(Dose_splitting) %>%
  summarise(mean = mean(Infection),
            std_err = se(Infection))

septoria_df %>%
  group_by(Preceding_crop, Wheat_cultivar) %>%
  summarise(mean = mean(Infection),
            std_err = se(Infection))

library(glmmTMB)
fit2 <- glmmTMB(Infection/100 + .001 ~ (1 | Block/Preceding_crop) +
                  Fertilisation_level * Preceding_crop * Dose_splitting * Wheat_cultivar,
                family = beta_family,
                data = septoria_df)
drop1(fit2, test = "Chisq")

fit3 <- glmmTMB(Infection/100 + .001 ~ (1 | Block/Preceding_crop) +
                  (Fertilisation_level + Preceding_crop + Dose_splitting + Wheat_cultivar)^3,
                family = beta_family,
                data = septoria_df)
drop1(fit3, test = "Chisq")

fit4 <- glmmTMB(Infection/100 + .001 ~ (1 | Block/Preceding_crop) +
                  (Fertilisation_level + Preceding_crop + Dose_splitting + Wheat_cultivar)^2 +
                  Fertilisation_level : Preceding_crop : Dose_splitting,
                family = beta_family,
                data = septoria_df)
drop1(fit4, test = "Chisq")

## there is a significant 3-way interaction between fertilisation level, preceding crop and dose splitting (LR = 10.4, d.f. = 3, p = 0.015)
## there is a significane 2-way interaction between preceding crop and wheat cultivar (LR = 14.1, d.f. = 2, p < 0.001)

fit5 <- glmmTMB(Infection/100 + .001 ~ (1 | Block/Preceding_crop) +
                  Preceding_crop * Wheat_cultivar +
                  Fertilisation_level * Preceding_crop * Dose_splitting,
                family = beta_family,
                data = septoria_df)
## global test
anova(fit2, fit5, test = "Chisq")

hnp(fit2) ## not implemented yet, can implement by hand or use alternative software

fit6 <- gamlss(Infection/100 + .001 ~ re(random = ~ 1 | Block/Preceding_crop) +
                 Fertilisation_level * Preceding_crop * Dose_splitting * Wheat_cultivar,
               family = BE,
               data = septoria_df)
wp(fit6)
summary(fit6)
getSmo(fit6) ## to obtain variances of the random effects

drop1(fit6)
## and then you may keep going the same way we did for glmmTMB