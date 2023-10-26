library(tidyverse)
library(lme4)
library(lmerTest)

setwd("/Users/rafamoral/Dropbox/ONLINE_COURSES/courses/intro_mixed_models/scripts")

otolith_df <- read_csv("../data/ChumOtolith_SampleData.csv")
otolith_df <- otolith_df %>%
  mutate(FishID = factor(FishID),
         Sex = factor(Sex),
         StreamCategory = factor(StreamCategory),
         StreamName = factor(StreamName))

otolith_df %>%
  ggplot(aes(x = SurveyDate, y = HatcheryOrigin, colour = Sex)) +
  theme_bw() +
  geom_jitter(width = .2, height = .1) +
  facet_wrap(~ StreamName)

otolith_df %>%
  ggplot(aes(x = `Length(cm)`, y = HatcheryOrigin, colour = Sex)) +
  theme_bw() +
  geom_jitter(width = .2, height = .1) +
  facet_wrap(~ StreamName)

fit1 <- glmer(HatcheryOrigin ~ Sex + `Length(cm)` + SurveyDate + StreamCategory +
                (SurveyDate | StreamName),
              family = binomial,
              data = otolith_df)
summary(fit1)

drop1(fit1, test = "Chisq")

fit1 <- glmer(HatcheryOrigin ~ Sex + (1 | StreamName),
              family = binomial,
              data = otolith_df)

ranef(fit1)
summary(fit1)

library(sjPlot)
plot_model(fit1, type = "re", sort.est = "(Intercept)")