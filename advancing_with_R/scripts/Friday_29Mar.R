## loading packages
library(hnp) ## loading MASS package before dplyr so select is not masked
library(lme4)
library(tidyverse)

theme_set(theme_bw()) ## setting the theme for all ggplots


# Linear Mixed Models -----------------------------------------------------


## load the data
data(sleepstudy)
sleepstudy_df <- as_tibble(sleepstudy)

## exploratory plot
sleepstudy_df %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Subject)

## predicted regression line for each participant
## (estimating 18 intercepts and 18 slopes)
sleepstudy_df %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Subject) +
  geom_smooth(method = lm,
              se = FALSE)
    
## a model for subject 334
sleepstudy_334 <- sleepstudy_df %>%
  filter(Subject == 334)

fit1 <- lm(Reaction ~ Days,
           data = sleepstudy_334)
coef(fit1)
sigma(fit1)

## a model for all subjects (not a mixed model)
fit2 <- lm(Reaction ~ 0 + Subject + Subject : Days,
           data = sleepstudy_df)
coef(fit2)
sigma(fit2)

## fit the linear mixed model
fit3 <- lmer(Reaction ~ Days + (Days | Subject),
             data = sleepstudy_df)
summary(fit3)

ranef(fit3) ## gives "b_i"
coef(fit3)  ## gives "beta_i = beta + b_i"

cbind(coef(fit3)$Subject,
      matrix(coef(fit2), ncol = 2, byrow = FALSE))

## Random intercepts only
## (all slopes assumed to be identical across subjects)
fit4 <- lmer(Reaction ~ Days + (1 | Subject),
             data = sleepstudy_df)
summary(fit4)

## Random slopes only
## (all intercepts assumed to be identical across subjects)
fit5 <- lmer(Reaction ~ Days + (0 + Days | Subject),
             data = sleepstudy_df)
summary(fit5)

## comparing
library(modelr)

sleepstudy_df <- sleepstudy_df %>%
  add_predictions(model = fit3,
                  var = "random_int_slope") %>%
  add_predictions(model = fit4,
                  var = "random_int") %>%
  add_predictions(model = fit5,
                  var = "random_slope")

sleepstudy_df %>%
  pivot_longer(4:6,
               names_to = "model",
               values_to = "pred") %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point(size = .5) +
  geom_line(aes(y = pred, col = model)) +
  facet_wrap(~ Subject)

## Random intercepts and slopes but no correlation
fit6 <- lmer(Reaction ~ Days + (Days || Subject),
             data = sleepstudy_df)
summary(fit6)

## let's test to see whether rho is significantly different from 0
anova(fit6, fit3)

## null hypothesis: fit6 and fit3 are equivalent
## we do not reject the null, conclusion: rho isn't significantly different from 0
## we select fit6 (rand int, slopes, no corr)

summary(fit6)
anova(fit6)

system.time(
  lmer(Reaction ~ Days + (Days || Subject),
       data = sleepstudy_df)
)

## load a companion library to get the p-values and approx. d.f.
library(lmerTest)
summary(fit6)

## need to re-fit to be able to use the p-values
fit6 <- lmer(Reaction ~ Days + (Days || Subject),
             data = sleepstudy_df)
summary(fit6)
anova(fit6)

## finalise the plot for publication
add_predictions(data = sleepstudy,
                model = fit6) %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point(size = .5) +
  geom_line(aes(y = pred)) +
  facet_wrap(~ Subject) +
  scale_x_continuous(breaks = 0:9)

add_predictions(data = sleepstudy,
                model = fit6) %>%
  ggplot(aes(x = Days, y = Reaction,
             group = Subject)) +
  geom_point(size = .5) +
  geom_line(aes(y = pred)) +
  scale_x_continuous(breaks = 0:9)

sleepstudy_df %>%
  mutate(pred = predict(fit6,
                        re.form = NA)) %>%
  ggplot(aes(x = Days, y = Reaction,
             group = Subject)) +
  geom_point(size = .5) +
  geom_line(aes(y = pred)) +
  scale_x_continuous(breaks = 0:9)


# Nested Random Effects ---------------------------------------------------

classroom_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/classroom.csv")
classroom_df <- classroom_df %>%
  mutate(schoolid = factor(schoolid),
         classid = factor(classid),
         classid2 = factor(classid2))

classroom_df %>%
  ggplot(aes(x = ses, y = mathscore)) +
  geom_point(size = .2) +
  facet_wrap(~ schoolid, ncol = 16)

classroom_df %>%
  filter(schoolid == "11") %>%
  ggplot(aes(x = ses, y = mathscore,
             col = classid)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

## fit the random intercepts and slopes for class within school
fit7 <- lmer(mathscore ~ ses + (ses | schoolid) + (ses | classid),
             data = classroom_df)
summary(fit7)

## remove the correlation between class random effects
fit8 <- lmer(mathscore ~ ses + (ses | schoolid) + (ses || classid),
             data = classroom_df)
summary(fit8)

## remove the random slope for class within school
fit9 <- lmer(mathscore ~ ses + (ses | schoolid) + (1 | classid),
             data = classroom_df)
summary(fit9)

## continuing to fit nested models (to select later on via LRT / AIC)
fit10 <- lmer(mathscore ~ ses + (ses || schoolid) + (1 | classid),
              data = classroom_df)
summary(fit10)

fit11 <- lmer(mathscore ~ ses + (1 | schoolid) + (1 | classid),
              data = classroom_df)
summary(fit11)

## let's compare the models
anova(fit11, fit10, fit9, fit8, fit7) ## local tests
anova(fit11, fit7) ## global test

## recall
## classid: unique identifier for class
## classid2: identifier for class within school
## using classid2 to fit the exact same model as fit11:
fit11 <- lmer(mathscore ~ ses + (1 | schoolid) + (1 | classid),
              data = classroom_df)
fit11b <- lmer(mathscore ~ ses + (1 | schoolid / classid2),
               data = classroom_df)
fit11c <- lmer(mathscore ~ ses + (1 | schoolid) + (1 | schoolid : classid2),
               data = classroom_df)
logLik(fit11)
logLik(fit11b)
logLik(fit11c)

## also identical
fit7 <- lmer(mathscore ~ ses + (ses | schoolid) + (ses | classid),
             data = classroom_df)
fit7b <- lmer(mathscore ~ ses + (ses | schoolid / classid2),
              data = classroom_df)
logLik(fit7)
logLik(fit7b)

## Reporting:
## There was no significant effect of the inclusion of random slopes
## per class (LR = 2.52, d.f. = 1, p = 0.113)

## The AICs were 11896, 11896, and 11895 for (fit11), (fit10), and (fit9),
## respectively. We selected (fit11) on the basis of parsimony, since
## it is the simplest model, and these three models are essentially
## equivalent based on AIC.

AIC(fit11) ## this gives AIC based on REML (we don't want that!)
deviance(fit11, REML = FALSE) + 2 * 5 ## this is the AIC based on the likelihood
## AIC = deviance + 2 * k

fit11_ML <- lmer(mathscore ~ ses + (1 | schoolid) + (1 | classid),
                 data = classroom_df,
                 REML = FALSE)
AIC(fit11_ML)


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


# Group-Level Predictors --------------------------------------------------

mathach_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/mathachieve.csv")
mathach_school_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/mathachieveschool.csv")

mathach_df %>%
  ggplot(aes(x = ses, y = mathach)) +
  geom_point(size = .1) +
  facet_wrap(~ school, ncol = 16)

mathach_df %>%
  filter(school %in% c("s1224","s2030","s7734","s1433")) %>%
  ggplot(aes(x = ses, y = mathach)) +
  geom_point(size = .5) +
  facet_wrap(~ school)

mathach_df %>%
  filter(school %in% c("s1224","s2030","s7734","s1433")) %>%
  ggplot(aes(x = sex, y = mathach)) +
  geom_boxplot() +
  facet_wrap(~ school)


fit13 <- lmer(mathach ~ ses + sex + minority +
                (ses | school) + (sex | school) + (minority | school),
              data = mathach_df)
summary(fit13)

fit14 <- lmer(mathach ~ ses + sex + minority + (ses | school),
              data = mathach_df)
summary(fit14)

## joining the datasets
mathach_join_df <- inner_join(mathach_df,
                              mathach_school_df)

## fit the mixed model also including group-level predictors
mathach_join_df <- mathach_join_df %>%
  mutate(size_scaled = as.numeric(scale(size)))

fit15 <- lmer(mathach ~ ses + sex + minority +
                size_scaled + sector + pracad + disclim + himinty + meanses +
                (ses | school),
              data = mathach_join_df)
summary(fit15)

## model selection
drop1(fit15)

## checking adequacy
hnp(fit15, verb = TRUE, paint = TRUE)
## model is not adequate
## needs to be made more complex / structures need to change
## e.g. model variance with covariates (gamlss / glmmTMB)
##      introduce nonlinearities for continuous predictors (splines / gamms)
##      include higher-order interactions between variables

## let's include effects in the variance
library(glmmTMB)

fit16 <- glmmTMB(mathach ~ ses + sex + minority +
                   size_scaled + sector + pracad + disclim + himinty + meanses +
                   (ses | school),
                 dispformula = ~ ses + sex + minority,
                 data = mathach_join_df)
summary(fit16)

fit15_ML <- lmer(mathach ~ ses + sex + minority +
                  size_scaled + sector + pracad + disclim + himinty + meanses +
                  (ses | school),
                 data = mathach_join_df,
                 REML = FALSE)

fit16 <- glmmTMB(mathach ~ ses + sex + minority +
                   size_scaled + sector + pracad + disclim + himinty + meanses +
                   (ses | school),
                 dispformula = ~ ses + sex + minority,
                 data = mathach_join_df,
                 REML = FALSE)

AIC(fit15)
AIC(fit16)


# GLMMs -------------------------------------------------------------------

## main syntax
# glmer(y ~ x + (1 | group),
#       family = poisson,
#       data = my_data,
#       nAGQ = 1) ## default approximation of integral: adaptive Laplace approx.

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
## we will show an alternative way here


# Individual-level Random Effects -----------------------------------------

## first, create a factor variable that has a unique value
## per observation
arabidopsis_df <- arabidopsis_df %>%
  mutate(ind = factor(1:n()))

## re-fit the model and add (1 | ind) as the individual-level
## random effect to account for extra variability

fit18 <- glmer(total.fruits ~ rack + status +
                 nutrient * amd +
                 (1 | reg) + (1 | popu) + (1 | gen) +
                 (1 | ind),
               family = poisson,
               data = arabidopsis_df)
summary(fit18)

drop1(fit18, test = "Chisq")

fit19 <- glmer(total.fruits ~ rack + status +
                 nutrient + amd +
                 (1 | reg) + (1 | popu) + (1 | gen) +
                 (1 | ind),
               family = poisson,
               data = arabidopsis_df)
drop1(fit19, test = "Chisq")

arabidopsis_df %>%
  group_by(nutrient) %>%
  summarise(mean = mean(total.fruits),
            sd = sd(total.fruits))

arabidopsis_df %>%
  group_by(amd) %>%
  summarise(mean = mean(total.fruits),
            sd = sd(total.fruits))

## checking adequacy
hnp(fit18, verb = TRUE)

## let's add a zero-inflation parameter to this model
fit20 <- glmmTMB(total.fruits ~ rack + status +
                   nutrient + amd +
                   (1 | reg) + (1 | popu) + (1 | gen) +
                   (1 | ind),
                 ziformula = ~ 1,
                 family = poisson,
                 data = arabidopsis_df)
summary(fit20) ## some estimates not available
## model is potentially over-parameterised
## or maybe we should use another paradigm
## for estimation, such as Bayesian inference

AIC(fit19) ## without zero-inflation
AIC(fit20) ## with zero-inflation - not available 
           ## due to problems when estimating the model