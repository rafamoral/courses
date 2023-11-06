library(tidyverse)
library(lme4)
library(lmerTest)

# Crossed Random Effects --------------------------------------------------

blp_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_mixed_models/data/blp-short2.csv")
blp_df <- blp_df %>%
  mutate(participant = factor(participant),
         spelling = factor(spelling))

blp_df %>%
  filter(spelling == "herb")

blp_df %>%
  filter(participant == 18)

blp_df %>%
  ggplot(aes(x = reorder(participant, rt), y = rt)) +
  theme_bw() +
  geom_boxplot()

blp_df %>%
  ggplot(aes(x = reorder(spelling, rt), y = rt)) +
  theme_bw() +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

blp_df %>%
  group_by(spelling) %>%
  summarise(mean = mean(rt)) %>%
  arrange(mean)

fit17 <- lmer(rt ~ 1 + (1 | participant) + (1 | spelling),
              data = blp_df)
summary(fit17)

p_blup <- ranef(fit17)$participant$`(Intercept)`
w_blup <- ranef(fit17)$spelling$`(Intercept)`

## fastest participant
which.min(p_blup)
tibble(p_blup, levels(blp_df$participant))[75,]

## slowest participant
which.max(p_blup)
tibble(p_blup, levels(blp_df$participant))[67,]

## "easiest" word
which.min(w_blup)
tibble(w_blup, levels(blp_df$spelling))[12,]

## "hardest" word
which.max(w_blup)
tibble(w_blup, levels(blp_df$spelling))[1,]

blp_df %>%
  ggplot(aes(x = nletters, y = rt)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

fit18 <- lmer(rt ~ nletters + (1 | participant) + (1 | spelling),
              data = blp_df)
summary(fit18)
anova(fit18)

# Group-Level Predictors --------------------------------------------------

mathach_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_mixed_models/data/mathachieve.csv")
mathach_school_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_mixed_models/data/mathachieveschool.csv")

## in principle it is fine to fit this model, but in practice
## it is overparametersised (convergence problems)
fit19 <- lmer(mathach ~ ses + sex + minority +
                (ses | school) + (sex | school) + (minority | school),
              data = mathach_df)
summary(fit19)
head(ranef(fit19)$school) ## b0, b1, c0, c1, d0, d1

mathach_df2 <- mathach_df %>%
  mutate(sex_numeric = as.numeric(factor(sex)),
         minority_numeric = as.numeric(factor(minority)))

fit20 <- lmer(mathach ~ ses + sex + minority +
                (ses || school) +
                (0 + sex_numeric | school) + (0 + minority_numeric | school),
              data = mathach_df2)
summary(fit20)

fit21 <- lmer(mathach ~ ses + sex + minority + (ses || school),
              data = mathach_df2)
summary(fit21)
anova(fit20, fit21)

anova(fit20)

## merging both datasets
mathach_join_df <- inner_join(mathach_df, mathach_school_df, by = "school")

fit22 <- lmer(mathach ~ ses + sex + himinty + sector + pracad + disclim +
                (ses | school),
              data = mathach_join_df)
summary(fit22)
anova(fit22)

## non-sensical model, because himinty is at school-level, cannot vary
## by school (i.e., there is no range of himinty values per each school)
fit23 <- lmer(mathach ~ ses + himinty + (ses | school) + (himinty | school),
              data = mathach_join_df)

## school-level predictors have only 1 value per school
mathach_join_df %>%
  filter(school %in% unique(school)[1:9]) %>%
  ggplot(aes(x = himinty, y = mathach)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~ school)

mathach_join_df %>%
  filter(school %in% unique(school)[1:9]) %>%
  ggplot(aes(x = pracad, y = mathach)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~ school)

## student-level predictors have more than 1
mathach_join_df %>%
  filter(school %in% unique(school)[1:9]) %>%
  ggplot(aes(x = ses, y = mathach)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~ school)

# Generalized Linear Mixed Models (GLMMs) ---------------------------------

?glmer
?family
?glmer.nb

data("Arabidopsis")
?Arabidopsis

arabidopsis_df <- Arabidopsis %>% as_tibble
arabidopsis_df <- arabidopsis_df %>%
  mutate(gen = factor(gen),
         rack = factor(rack),
         nutrient = factor(nutrient))

## nutrient: fertilisation treatment with 2 levels
## amd: simulated herbivory treatment with 2 levels
## rack: nuisance factor with 2 levels
## status: nuisance factor with 3 levels

## fitting the Poisson GLMM
## nutrient * amd = nutrient + amd + nutrient : amd
fit24 <- glmer(total.fruits ~ rack + status + nutrient * amd +
                 (1 | reg) + (1 | popu) + (1 | gen),
               family = "poisson",
               data = arabidopsis_df)
summary(fit24)

drop1(fit24, test = "Chisq")

arabidopsis_df % >%
  ggplot(aes(x = nutrient, y = total.fruits, fill = amd)) +
  theme_bw() +
  geom_boxplot()

arabidopsis_df %>%
  ggplot(aes(x = gen, y = total.fruits, fill = amd)) +
  theme_bw() +
  geom_boxplot() +
  facet_wrap(~ nutrient, ncol = 1)

library(sjPlot)
plot_model(fit24, type = "re", sort.est = "(Intercept)")

# Individual-Level Random Effects -----------------------------------------

library(hnp)
data(cbb)
cbb_df <- cbb %>% as_tibble
cbb_df

## fitting a standard Poisson model
fit25 <- glm(count ~ block + trap + week,
             family = poisson,
             data = cbb_df)
summary(fit25)
anova(fit25, test = "Chisq")

cbb_df %>%
  group_by(trap, week) %>%
  summarise(mean = mean(count),
            var = var(count)) %>%
  ggplot(aes(x = mean, y = var)) +
  theme_bw() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2)

hnp(fit25) ## the standard Poisson model doesn't fit the data well

## trying a negative binomial model
fit26 <- glm.nb(count ~ block + trap + week,
                data = cbb_df)
drop1(fit26, test = "F")

hnp(fit26, paint = TRUE, print = TRUE) ## fit improves, not perfect

## let's try using individual-level random effects
cbb_df$z <- factor(1 : nrow(cbb_df))

## this is also called the "Poisson-Normal model"
fit27 <- glmer(count ~ block + trap + week + (1 | z),
               family = poisson,
               data = cbb_df)
summary(fit27)
drop1(fit27, test = "Chisq")

hnp(fit27, paint = TRUE, print = TRUE, verb = TRUE) ## good model fit

## what about the model for the Arabidopsis data?
hnp(fit24, paint = TRUE, print = TRUE, verb = TRUE) ## terrible fit

arabidopsis_df$z <- factor(1 : nrow(arabidopsis_df))
fit28 <- glmer(total.fruits ~ rack + status + nutrient * amd +
                  (1 | reg) + (1 | popu) + (1 | gen) + (1 | z),
                family = "poisson",
                data = arabidopsis_df)
hnp(fit28, paint = TRUE, print = TRUE, verb = TRUE) ## improved, not perfect

# Bayesian Mixed Models ---------------------------------------------------

library(brms)

fit29 <- lm(Reaction ~ Days, data = sleepstudy) # linear regression
fit30 <- brm(Reaction ~ Days, data = sleepstudy) # Bayesian linear regression

summary(fit29)$coefficients
fit30

plot(fit30)

fit31 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
fit32 <- brm(Reaction ~ Days + (Days | Subject), data = sleepstudy)

fit32
plot(fit32)

fit33 <- brm(Reaction ~ Days + (Days || Subject), data = sleepstudy)