library(tidyverse)
library(readxl)
library(hnp)
library(gamlss)
library(lme4)
library(lmerTest)

septoria_df <- read_excel("02_Septoria_data.xls")
septoria_df <- septoria_df %>%
  mutate(across(where(is.character), as.factor),
         Block = as.factor(Block))

septoria_df %>%
  ggplot(aes(x = Fertilisation_level, y = Infection/100)) +
  theme_bw() +
  facet_grid(Preceding_crop ~ Wheat_cultivar) +
  geom_boxplot()

fit1 <- lmer(sqrt(Infection) ~ (1|Block/Preceding_crop) +
               Fertilisation_level * Preceding_crop * Dose_splitting * Wheat_cultivar,
             data = septoria_df)
anova(fit1)
summary(fit1)

fit2 <- gamlss(Infection/100 + .001 ~ re(random = ~ 1 | Block/Preceding_crop) +
                 Fertilisation_level * Preceding_crop * Dose_splitting * Wheat_cultivar,
               family = BE,
               data = septoria_df)
wp(fit2)
summary(fit2)
getSmo(fit2)

fit3 <- gamlss(Infection/100 + .001 ~ re(random = ~ 1 | Block/Preceding_crop) +
                 (Fertilisation_level + Preceding_crop + Dose_splitting + Wheat_cultivar) ^ 3,
               family = BE,
               data = septoria_df)
LR.test(fit3, fit2)

fit4 <- gamlss(Infection/100 + .001 ~ re(random = ~ 1 | Block/Preceding_crop) +
                 (Fertilisation_level + Preceding_crop + Dose_splitting + Wheat_cultivar) ^ 2,
               family = BE,
               data = septoria_df)
LR.test(fit4, fit3)

drop1(fit4)

fit5 <- gamlss(Infection/100 + .001 ~ re(random = ~ 1 | Block/Preceding_crop) +
                 Fertilisation_level + Dose_splitting + Preceding_crop * Wheat_cultivar,
               family = BE,
               data = septoria_df)
LR.test(fit5, fit4)

drop1(fit5)
wp(fit5)