library(tidyverse)
library(lme4)

sleep2 <- sleepstudy %>%
  mutate(stress_level = floor(runif(n(), 0, 11)))

sleep2 <- sleep2 %>%
  group_by(Subject) %>%
  summarise(average_stress_level = mean(stress_level)) %>%
  left_join(., sleep2)

fit1 <- lmer(Reaction ~ Days + (1 | Subject),
             data = sleep2)
fit2 <- lmer(Reaction ~ Days + average_stress_level + (1 | Subject),
             data = sleep2)
anova(fit1, fit2)