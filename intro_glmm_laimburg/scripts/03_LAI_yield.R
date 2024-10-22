library(tidyverse)
library(readxl)
library(hnp)
library(gamlss)
library(lme4)
library(lmerTest)
library(mgcv)
library(gratia)

lai_df <- read_xlsx("03_LAI-yield_data.xlsx")
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

fit1 <- lmer(sqrt(yield) ~ (1 | parcel) +
               poly(LAI, 2) * swardtype +
               growthcycle + year + phenologicalstage + lodging,
             data = lai_df)
anova(fit1)
summary(fit1)
hnp(fit1, paint = TRUE)

fit2 <- gamlss(yield + .01 ~ random(parcel) +
                 pb(LAI) * swardtype +
                 growthcycle + year + phenologicalstage + lodging,
               sigma.formula = ~ pb(LAI) * swardtype +
                 growthcycle + year + phenologicalstage + lodging,
               family = GA,
               data = lai_df)
wp(fit2)

fit3 <- gam(yield + .01 ~ s(parcel, bs = "re") +
              s(LAI, by = swardtype, k = 5) +
              growthcycle + year + phenologicalstage + lodging,
            family = Gamma(link = log),
            data = lai_df)
draw(fit3)
summary(fit3)
appraise(fit3)

lai_df %>%
  mutate(yhat = predict(update(fit3, . ~ s(LAI, by = swardtype, k = 5)), type = "response")) %>%
  ggplot(aes(x = LAI, y = yield)) +
  theme_bw() +
  geom_point(alpha = .2) +
  geom_line(aes(y = yhat),
            col = 2, lwd = 1) +
  facet_wrap(~ swardtype)