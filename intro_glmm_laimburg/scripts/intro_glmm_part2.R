## loading packages
library(hnp) ## loading MASS package before dplyr so select is not masked
library(lme4)
library(tidyverse)

theme_set(theme_bw()) ## setting the theme for all ggplots



# Crossed Random Effects --------------------------------------------------

blp_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/blp-short2.csv")
blp_df %>%
  filter(spelling == "herb")
blp_df %>%
  filter(participant == 18)

blp_df <- blp_df %>%
  mutate(participant = as.factor(participant),
         spelling = as.factor(spelling))

blp_df %>%
  ggplot(aes(x = reorder(participant, rt),
             y = rt)) +
  geom_boxplot()

blp_df %>%
  ggplot(aes(x = reorder(spelling, rt),
             y = rt)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

blp_df %>%
  drop_na() %>%
  group_by(spelling) %>%
  summarise(mean = mean(rt),
            var = var(rt)) %>%
  arrange((mean))

## fit a mixed model with crossed random effects
## for participant and word (spelling)
fit12 <- lmer(rt ~ (1 | participant) + (1 | spelling),
              data = blp_df %>%
                drop_na())
summary(fit12)

ranef(fit12)

## same way to specify, but explicitly including intercept term
## and using na.omit (base R) rather than drop_na (dplyr)
fit12b <- lmer(rt ~ 1 + (1 | participant) + (1 | spelling),
               data = blp_df %>%
                 na.omit)
logLik(fit12)
logLik(fit12b)



# GLMMs -------------------------------------------------------------------

## main syntax
# glmer(y ~ x + (1 | group),
#       family = poisson,
#       data = my_data,
#       nAGQ = 1) ## default approximation of integral: adaptive Laplace approx.

## extra example: Arabidopsis fruit production when subject to simulated herbivory
data(Arabidopsis)
?Arabidopsis

arabidopsis_df <- as_tibble(Arabidopsis)

## nutrient: fertilisation treatment with 2 levels
## amd: simulated herbivory treatment with 2 levels
## rack: nuisanse factor (2 greenhouse racks)
## status: nuisance factor (3 germination methods)
## reg: region
## popu: population within region
## gen: genotype within population within region
## total.fruits: response variable (count)

fit17 <- glmer(total.fruits ~ rack + status +
                 nutrient * amd +
                 (1 | reg) + (1 | popu) + (1 | gen),
               family = poisson,
               data = arabidopsis_df)
summary(fit17)

drop1(fit17, test = "Chisq")

arabidopsis_df %>%
  ggplot(aes(x = factor(nutrient), y = total.fruits,
             fill = amd)) +
  geom_boxplot()

arabidopsis_df %>%
  ggplot(aes(x = amd, y = total.fruits,
             fill = factor(nutrient))) +
  geom_boxplot()

arabidopsis_df %>%
  group_by(nutrient, amd) %>%
  summarise(mean = mean(total.fruits),
            sd = sd(total.fruits))

library(sjPlot)
plot_model(fit17) ## fixed effects
plot_model(fit17, type = "re")

## checking adequacy
hnp(fit17, verb = TRUE) ## clearly there is overdispersion
                        ## drop1 analysis unreliable

## one way to account for overdispersion is to
## use a negbin distribution instead of the Poisson
## we will show an alternative way here, see below

# Individual-level Random Effects -----------------------------------------

library(hnp)
data(cbb)
cbb_df <- cbb %>% as_tibble
cbb_df

fit27 <- glm(count ~ block + trap + week,
             family = poisson,
             data = cbb_df)
summary(fit27)
anova(fit27, test = "Chisq")

cbb_df %>%
  group_by(trap, week) %>%
  summarise(mean = mean(count),
            var = var(count)) %>%
  ggplot(aes(x = mean, y = var)) +
  theme_bw() +
  geom_point()

hnp(fit27)

fit28 <- glm.nb(count ~ block + trap + week,
                data = cbb_df)
drop1(fit28, test = "F")

hnp(fit28, paint = TRUE)

cbb_df$z <- factor(1 : nrow(cbb_df))

fit29 <- glmer(count ~ block + trap + week + (1 | z),
               family = poisson,
               data = cbb_df)
summary(fit29)
drop1(fit29, test = "Chisq")

hnp(fit29, paint = TRUE, verb = TRUE)