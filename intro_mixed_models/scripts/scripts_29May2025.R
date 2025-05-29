library(tidyverse)

# Crossed random effects --------------------------------------------------

blp_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_mixed_models/data/blp-short2.csv")
blp_df <- blp_df %>%
  mutate(spelling = as.factor(spelling),
         participant = as.factor(participant))

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
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  coord_flip()

blp_df %>%
  group_by(spelling) %>%
  summarise(mean = mean(rt)) %>%
  arrange(mean)

blp_df %>%
  group_by(spelling) %>%
  summarise(mean = mean(rt)) %>%
  arrange(desc(mean))

## we could fit a fixed effects model (additive)
fit1 <- lm(rt ~ participant + spelling,
           data = blp_df)
summary(fit1)
## not a very useful model, especially since our research
## question is not exactly about the systematic differences
## between participants and words

## let's, instead, fit a mixed model and consider
## participant and spelling to be crossed random effects
library(lme4)
library(lmerTest)

fit2 <- lmer(rt ~ (1 | participant) + (1 | spelling),
             data = blp_df)
summary(fit2)

## BLUP: "best linear unbiased predictor" -- random effects
## BLUE: "best linear unbiased estimator" -- fixed effects

p_blup <- ranef(fit2)$participant[,1]
s_blup <- ranef(fit2)$spelling[,1]

library(sjPlot)
plot_model(fit2, type = "re", sort.est = "(Intercept)")

## let's check model adequacy
library(hnp)
hist(p_blup)
hist(s_blup)
## checking normality of random effects
hnp(p_blup, scale = TRUE, paint = TRUE)
hnp(s_blup, scale = TRUE, paint = TRUE)

## check overall adequacy of the fitted model
fit2 <- lmer(rt ~ (1 | participant) + (1 | spelling),
             data = na.omit(blp_df))
hnp(fit2, paint = TRUE)
## beware: model doesn't fit the data very well (is inadequate for inferential purposes)
## in this case transformations could help (although we tried the log transform and it didn't really)
## we can also try other distributions other than the normal
## we can also try other fixed effect structures (maybe some other covariates will help improve model fit)

blp_df %>%
  ggplot(aes(x = nletters, y = rt)) +
  theme_bw() +
  geom_point() +
  geom_smooth(se = FALSE, method = lm)

fit3 <- lmer(rt ~ nletters + (1 | participant) + (1 | spelling),
             data = na.omit(blp_df))
summary(fit3)
anova(fit3)
hnp(fit3, paint = TRUE)
## advanced: gamlss R package allows for the implementation of
## random intercepts and also a plethora of GLM families


# Group-level predictors --------------------------------------------------

mathach_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_mixed_models/data/mathachieve.csv")
mathach_school_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_mixed_models/data/mathachieveschool.csv")

mathach_df %>%
  group_by(school) %>%
  summarise(n = n()) %>%
  pull(n) %>%
  hist

## in principle we could fit this model, but in practice
## this is overparameterised and yields convergence problems
fit4 <- lmer(mathach ~ ses + sex + minority +
               (ses | school) + (sex | school) + (minority | school),
             data = mathach_df)
summary(fit4)
head(ranef(fit4)$school)
## we don't need these 3 random intercepts per school... one would do!

## let's remove the extra 2 random intercepts
## the code below won't work, however, because sex and minority are treated as factors
fit5 <- lmer(mathach ~ ses + sex + minority +
               (ses | school) + (0 + sex | school) + (0 + minority | school),
             data = mathach_df)
summary(fit5)

## instead, we need to convert them to dummy variables
fit6 <- lmer(mathach ~ ses + sex + minority +
               (ses | school) + (0 + sex_dummy | school) + (0 + minority_dummy | school),
             data = mathach_df %>%
               mutate(sex_dummy = as.numeric(factor(sex)) - 1,
                      minority_dummy = as.numeric(factor(minority)) - 1))
summary(fit6)
## this model converges and it is our "full" model that can be reduced and tested
## to check the need for the inclusion of specific random effects

## remove the correlation
fit7 <- lmer(mathach ~ ses + sex + minority +
               (ses || school) + (0 + sex_dummy | school) + (0 + minority_dummy | school),
             data = mathach_df %>%
               mutate(sex_dummy = as.numeric(factor(sex)) - 1,
                      minority_dummy = as.numeric(factor(minority)) - 1))
anova(fit6, fit7) ## LR test is not significant: the simpler model is just as good as the more complex one
                  ## therefore, we keep fit7 (correlation = 0)

fit8 <- lmer(mathach ~ ses + sex + minority +
               (ses || school) + (0 + minority_dummy | school),
             data = mathach_df %>%
               mutate(sex_dummy = as.numeric(factor(sex)) - 1,
                      minority_dummy = as.numeric(factor(minority)) - 1))
anova(fit7, fit8) ## the random effect of the difference between males vs females within school
                  ## is important to keep in the model
                  ## note: we should halve the p-value here because the hypothesis test
                  ## is on the boundary of the parametric space
                  ## H0: sigma^2 = 0 vs. H1: sigma^2 > 0, LRT ~ 1/2 * chisq_0 + 1/2 * chisq_1
                  ## therefore, the correct p-value for this test is 0.008122 / 2 = 0.004061

fit9 <- lmer(mathach ~ ses + sex + minority +
               (ses || school) + (0 + sex_dummy | school),
             data = mathach_df %>%
               mutate(sex_dummy = as.numeric(factor(sex)) - 1,
                      minority_dummy = as.numeric(factor(minority)) - 1))
anova(fit7, fit9) ## same conclusion for the minority random effect

fit10 <- lmer(mathach ~ ses + sex + minority +
                (1 | school) + (0 + sex_dummy | school) + (0 + minority_dummy | school),
              data = mathach_df %>%
                mutate(sex_dummy = as.numeric(factor(sex)) - 1,
                       minority_dummy = as.numeric(factor(minority)) - 1))
anova(fit7, fit10) ## we can drop the random slopes per school over ses

## now that we selected a structure for our random effects,
## we will now do selection of fixed effects
anova(fit10)
detach("package:lmerTest", unload = TRUE)
fit10 <- lmer(mathach ~ ses + sex + minority +
                (1 | school) + (0 + sex_dummy | school) + (0 + minority_dummy | school),
              data = mathach_df %>%
                mutate(sex_dummy = as.numeric(factor(sex)) - 1,
                       minority_dummy = as.numeric(factor(minority)) - 1))
drop1(fit10, test = "Chisq") ## likelihood-ratio tests dropping one fixed effect at a time

## final model: fit10 (for now!)

## now, let's merge both datasets and work with group-level predictors as well
mathach_join_df <- inner_join(mathach_df, mathach_school_df, by = "school")

library(lmerTest)
fit11 <- lmer(mathach ~ ses + sex + minority + ## covariates at level 0
                scale(size) + sector + pracad + disclim + himinty + meanses + ## covariates at level 1
                (ses | school),
              data = mathach_join_df)
summary(fit11)
anova(fit11)

detach("package:lmerTest", unload = TRUE)
fit11 <- lmer(mathach ~ ses + sex + minority + ## covariates at level 0
                scale(size) + sector + pracad + disclim + himinty + meanses + ## covariates at level 1
                (ses | school),
              data = mathach_join_df)
drop1(fit11, test = "Chisq")

## let's fit a simpler model with a new random effect
fit12 <- lmer(mathach ~ ses + size_scaled + (size_scaled | school),
              data = mathach_join_df %>%
                mutate(size_scaled = size/1000))
summary(fit12)

mathach_join_df %>%
  filter(school %in% unique(school)[1:9]) %>%
  ggplot(aes(x = size, y = mathach)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~ school)
## group-level predictors have only one value per group

## although the model converges, it wouldn't be a good idea to use it
## because as seen in the plot above, size does not vary per school
## and therefore we can't estimate random slopes over size within school
ranef(fit12)$school$size_scaled[1:9]

## student-level predictors, on the other hand, have multiple
## and allow for better inference across their values
mathach_join_df %>%
  filter(school %in% unique(school)[1:9]) %>%
  ggplot(aes(x = ses, y = mathach)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~ school)


# GLMMs -------------------------------------------------------------------

?glmer

data("Arabidopsis")
?Arabidopsis

arabidopsis_df <- as_tibble(Arabidopsis)
arabidopsis_df <- arabidopsis_df %>%
  mutate(gen = as.factor(gen),
         rack = as.factor(rack),
         nutrient = as.factor(nutrient))

## let's fit the Poisson GLMM

fit13 <- glmer(total.fruits ~ rack + status + # these are nuisance control variables (reflect design)
                              amd * nutrient + # this is what we are interested in investigating
                              (1 | reg) + (1 | popu) + (1 | gen),
               family = poisson,
               data = arabidopsis_df)
summary(fit13)
anova(fit13) ## don't look at F tests any more!!!
drop1(fit13, test = "Chisq") ## interaction is significant, we stop here

ranef(fit13)

plot_model(fit13, type = "re", sort.est = "(Intercept)")

arabidopsis_df %>%
  ggplot(aes(x = nutrient, y = total.fruits, fill = amd)) +
  theme_bw() +
  geom_boxplot()

arabidopsis_df %>%
  ggplot(aes(x = amd, y = total.fruits, fill = nutrient)) +
  theme_bw() +
  geom_boxplot()

arabidopsis_df %>%
  group_by(amd, nutrient) %>%
  summarise(mean = mean(total.fruits))

## changing integration methods
fit14 <- glmer(total.fruits ~ rack + status +
                 amd * nutrient +
                 (1 | reg) + (1 | popu) + (1 | gen),
               family = poisson,
               data = arabidopsis_df,
               nAGQ = 0)

logLik(fit13)
logLik(fit14)

summary(fit13)
summary(fit14)

## check adequacy of the Poisson GLMM
hnp(fit13, verb = TRUE, paint = TRUE)
## model does not fit well, data is overdispersed

## we can fix this in different ways
## 1. choose another distribution that accommodates overdispersion
## 2. use an individual-level random effect

## option 1: using the negative binomial
fit15 <- glmer.nb(total.fruits ~ rack + status +
                  amd * nutrient +
                  (1 | reg) + (1 | popu) + (1 | gen),
                  data = arabidopsis_df)
summary(fit15)
drop1(fit15, test = "Chisq") ## takes longer

## option 2: using an individual-level random effect
arabidopsis_df <- arabidopsis_df %>%
  mutate(id = factor(1:nrow(arabidopsis_df)))

fit16 <- glmer(total.fruits ~ rack + status +
                 amd * nutrient +
                 (1 | reg) + (1 | popu) + (1 | gen) +
                 (1 | id), # this is the individual-level random effect that accommodates overdispersion
               family = poisson,
               data = arabidopsis_df,
               nAGQ = 0)
summary(fit16)
hnp(fit16, sim = 19, conf = 1, verb = TRUE, paint = TRUE)


# Individual-level random effects -----------------------------------------

library(hnp)
data(cbb)
cbb_df <- as_tibble(cbb)

## start by fitting a standard Poisson GLM
fit17 <- glm(count ~ block + trap + week,
             family = poisson,
             data = cbb_df)
summary(fit17)
## for Poisson and binomial GLMs only, we can look at the residual deviance
## and compare it with the number of d.f. of the residual
## for a well-fitted model, these two values will be similar
## if, however, residual deviance >> residual d.f., we have evidence of overdispersion
## conversely, if residual deviance << residual d.f., we have evidence of underdispersion
anova(fit17, test = "Chisq")

hnp(fit17, paint = TRUE, print = TRUE) ## terrible fit!

## let's have a look at the mean-variance relationship in the data
cbb_df %>%
  group_by(trap, week) %>%
  summarise(mean = mean(count),
            var = var(count)) %>%
  ggplot(aes(x = mean, y = var)) +
  theme_bw() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2)
## variance function looks quadratic

## let's try a negative binomial model
fit18 <- glm.nb(count ~ block + trap + week,
                data = cbb_df)
summary(fit18)
drop1(fit18, test = "F") ## for the negbin GLM, use F-tests

hnp(fit18, paint = TRUE, print = TRUE) ## much better fit when compared to the Poisson GLM

## let's try now using an individual-level random effect

cbb_df <- cbb_df %>%
  mutate(z = factor(1:nrow(cbb_df)))

fit19 <- glmer(count ~ block + trap + scale(week) + (1 | z),
               family = poisson,
               data = cbb_df)
summary(fit19)
drop1(fit19, test = "Chisq")

hnp(fit19, paint = TRUE, print = TRUE, verb = TRUE) ## improved model adequacy


# Bayesian mixed models ---------------------------------------------------

# number of iterations (no. MCMC samples)
# burn-in period (initial iterations to be discarded)
# thinning rate (only keep MCMC samples every k iterations)

library(brms)

fit20 <- lm(Reaction ~ Days, data = sleepstudy) # linear regression
fit21 <- brm(Reaction ~ Days, data = sleepstudy) # Bayesian linear regression

summary(fit20)$coefficients
fit21

plot(fit21)

fit22 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
fit23 <- brm(Reaction ~ Days + (Days | Subject), data = sleepstudy)

summary(fit22)
fit23

plot(fit23)