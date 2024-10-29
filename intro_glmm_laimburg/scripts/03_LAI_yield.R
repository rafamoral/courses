library(tidyverse)
library(readxl)
library(hnp)
library(gamlss)
library(lme4)
library(lmerTest)
library(mgcv)
library(gratia)

lai_df <- read_csv("../data/03_LAI-yield_data.csv")[,1:10]
lai_df <- lai_df %>%
  mutate(parcel = as.factor(parcel),
         replicatewithinparcel = as.factor(replicatewithinparcel),
         growthcycle = as.factor(growthcycle),
         swardtype = as.factor(swardtype))

lai_df %>%
  ggplot(aes(x = LAI, y = yield)) +
  theme_bw() +
  geom_point(alpha = .2) +
  facet_wrap(~ swardtype)

lai_df %>%
  ggplot(aes(x = phenologicalstage, y = yield)) +
  theme_bw() +
  geom_point(alpha = .2) +
  facet_wrap(~ swardtype)

fit1 <- lmer(sqrt(yield) ~ (1 | parcel) +
               poly(LAI, 2) * swardtype +
               growthcycle + year + phenologicalstage + lodging,
             data = lai_df)
anova(fit1)
summary(fit1)
hnp(fit1, paint = TRUE)

library(mgcv)
fit2 <- gam(yield + .01 ~ s(LAI, k = 5),
            family = Gamma(link = "log"),
            data = lai_df)
summary(fit2)

library(gratia)
draw(fit2)

fit2 <- gam(yield + .01 ~ s(parcel, bs = "re") +
              s(LAI, by = swardtype, k = 20) +
              growthcycle + year + s(phenologicalstage) + lodging,
            family = Gamma(link = log),
            data = lai_df)
summary(fit2)
draw(fit2)
appraise(fit2, method = "simulate")

gam.check(fit2)

lai_df %>%
  mutate(yhat = predict(update(fit2, . ~ s(LAI, by = swardtype, k = 4)), type = "response")) %>%
  ggplot(aes(x = LAI, y = yield)) +
  theme_bw() +
  geom_point(alpha = .2) +
  geom_line(aes(y = yhat),
            col = 2, lwd = 1) +
  facet_wrap(~ swardtype)

fit2 <- gam(yield + .01 ~ s(parcel, bs = "re") +
              s(LAI, by = swardtype, k = 20) +
              growthcycle + year + s(phenologicalstage) + lodging,
            family = Gamma(link = log),
            data = lai_df)

fit3 <- gamlss(yield + .01 ~ random(parcel) +
                 pb(LAI) * swardtype +
                 growthcycle + year + phenologicalstage + lodging,
               sigma.formula = ~ pb(LAI) * swardtype +
                 growthcycle + year + phenologicalstage + lodging,
               family = GA,
               data = lai_df)
wp(fit3)

drop1(fit3, what = "mu")
drop1(fit3, what = "sigma")

stepGAICAll.A(fit3)
