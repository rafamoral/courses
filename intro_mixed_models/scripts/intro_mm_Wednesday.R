library(tidyverse)

# Linear Mixed Effects Models ---------------------------------------------

## loading the data
library(lme4)
data("sleepstudy")
sleepstudy_df <- as_tibble(sleepstudy)
sleepstudy_df

## fitting the linear mixed effects model (multilevel model, ...)
fit8 <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject),
             data = sleepstudy_df)
summary(fit8)

## distribution of random intercepts (population)
curve(dnorm(x, mean = 251.405, sd = 24.741), xlim = c(120, 380))
abline(v = 251.405 + c(-1, 1) * 1.96 * 24.741, lty = 2)

## distribution of random slopes (population)
curve(dnorm(x, mean = 10.467, sd = 5.922), xlim = c(-10, 30))
abline(v = 10.467 + c(-1, 1) * 1.96 * 5.922, lty = 2)

## what is the probability we would draw a subject from the
## population that performed better under sleep deprivation?
pnorm(0, mean = 10.467, sd = 5.922)

fixef(fit8) ## betas (fixed effects)
ranef(fit8) ## these are the b's (BLUPs)

fit8 <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject),
             data = sleepstudy_df)
## is identical to
fit9 <- lmer(Reaction ~ Days + (Days | Subject),
             data = sleepstudy_df)

logLik(fit8) ## same as...
logLik(fit9)

model.matrix(~ 1 + Days, data = sleepstudy_df) ## declaring intercept
model.matrix(~ Days, data = sleepstudy_df)     ## but this is the same
model.matrix(~ 0 + Days, data = sleepstudy_df) ## removing intercept

## Random intercepts only = all slopes assumed identical across subjects
fit10 <- lmer(Reaction ~ Days + (1 | Subject),
              data = sleepstudy_df)
summary(fit10)

## Random slopes only = all intercepts assumed identical across subjects
fit11 <- lmer(Reaction ~ Days + (0 + Days | Subject),
              data = sleepstudy_df)
summary(fit11)

## comparing predictions from fit10 and fit11
predict(fit10, re.form = NA) ## marginal predictions (averaging out the random effects)
predict(fit10) ## conditional predictions (including the random effects)

library(modelr)
sleepstudy_df <- add_predictions(data = sleepstudy_df, model = fit10, var = "random_intercepts")
sleepstudy_df <- add_predictions(data = sleepstudy_df, model = fit11, var = "random_slopes")

sleepstudy_df %>%
  pivot_longer(cols = 4:5,
               names_to = "model",
               values_to = "predictions") %>%
  ggplot(aes(x = Days, y = predictions, group = Subject)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~ model)

## Random slopes and random intercepts, but no correlation
## between the random slopes and random intercepts
fit12 <- lmer(Reaction ~ Days + (Days || Subject),
              data = sleepstudy_df)
summary(fit12)
logLik(fit12) ## 5 d.f. = 5 estimated parameters (2 fixed effects, 3 variance components)
logLik(fit9) ## 6 d.f. = 6 estimated parameters (2 fixed effects, 3 variance components, 1 correlation)

## equivalent to...
fit12 <- lmer(Reaction ~ Days + (1 | Subject) + (0 + Days | Subject),
              data = sleepstudy_df)

## model comparison
logLik(fit9)
deviance(fit9)
deviance(fit9, REML = FALSE)

anova(fit9, fit12) ## comparing fit9 (estimates rho) vs fit12 (assumes rho = 0)
curve(dchisq(x, df = 1))
abline(v = 0.0639, lty = 2)
## does it look like 0.0639 comes from a Chi-squared distribution with 1 d.f.?
## hypothesis: H_0: rho = 0 vs. H_a = rho != 0
## or H_0: fit9 and fit12 are equivalent in terms of goodness-of-fit vs. H_a: they aren't

## using the Akaike Information Criterion (smaller = better)
AIC(fit12)
AIC(fit9)
## fit12's AIC is the smallest -> AIC favours fit12 (rho = 0)

## using the Bayesian Information Criterion (smaller = better)
BIC(fit12)
BIC(fit9)
## fit12's BIC is the smallest -> BIC favours fit12 (rho = 0)

- 2 * logLik(fit12) + 2 * 5
deviance(fit12) + 2 * 5

fit12b <- lmer(Reaction ~ Days + (Days || Subject),
               data = sleepstudy_df,
               REML = FALSE)
- 2 * logLik(fit12b) + 2 * 5
deviance(fit12b) + 2 * 5

summary(fit12)
summary(fit12b)

deviance(fit12, REML = FALSE)
deviance(fit9, REML = FALSE)
deviance(fit12, REML = FALSE) - deviance(fit9, REML = FALSE) ## LRT statistic
pchisq(deviance(fit12, REML = FALSE) - deviance(fit9, REML = FALSE),
       df = 1, lower.tail = FALSE)
1 - pchisq(deviance(fit12, REML = FALSE) - deviance(fit9, REML = FALSE),
           df = 1)

anova(fit9, fit10) ## testing H_0: sigma2_1 = 0 and rho = 0 vs. H_a: at least one of them is not zero
curve(dchisq(x, df = 2), xlim = c(0, 50))
abline(v = 42.139, lty = 2)

anova(fit9, fit11) ## testing H_0: sigma2_0 = 0 and rho = 0 vs. H_a: at least one of them is not zero
curve(dchisq(x, df = 2), xlim = c(0, 50))
abline(v = 22.141, lty = 2)

anova(fit9) ## F-test, but no p-value
drop1(fit9, test = "Chisq") ## Likelihood-ratio test, includes p-value

library(lmerTest)
summary(fit9) ## lme4 doesn't give us p-values for the fixed effects
fit9_refit <- lmer(Reaction ~ Days + (Days | Subject),
                   data = sleepstudy_df)
summary(fit9_refit)
anova(fit9_refit)

## producing predicted lines for each subject
add_predictions(data = sleepstudy_df, model = fit12) %>%
  ggplot(aes(x = Days, y = Reaction)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred),
            col = 4) +
  facet_wrap(~ Subject)

add_predictions(data = sleepstudy_df, model = fit12) %>%
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred))

# Nested Random Effects ---------------------------------------------------

classroom_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_mixed_models/data/classroom.csv")
classroom_df

classroom_df %>%
  ggplot(aes(x = ses, y = mathscore)) +
  theme_bw() +
  geom_point(size = .5) +
  facet_wrap(~ schoolid) +
  geom_smooth(method = "lm", se = FALSE, lwd = .3)

classroom_df %>%
  group_by(schoolid) %>%
  summarise(n_children = n()) %>%
  filter(n_children == 2)

classroom_df %>%
  filter(schoolid == 11) %>%
  ggplot(aes(x = ses, y = mathscore, col = factor(classid))) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## fitting a mixed model with random intercepts and slopes per school,
## as well as random intercepts and slopes per class within school
fit13 <- lmer(mathscore ~ ses + (ses | schoolid) + (ses | classid),
              data = classroom_df)
summary(fit13)

## equivalent model using a different indexing for classid (classid2)
fit13b <- lmer(mathscore ~ ses + (ses | schoolid / classid2),
               data = classroom_df)
logLik(fit13)
logLik(fit13b)

a <- factor(rep(c("a1","a2","a3"), 2))
b <- factor(rep(c("b1","b2"), each = 3))

tibble(schoolid = b, classid2 = a, a:b)

classroom_df$classid %>% unique %>% length
unique(factor(classroom_df$schoolid) : factor(classroom_df$classid2)) %>% length

## creating a vector of unique classroom IDs
classroom_df2 <- classroom_df %>%
  mutate(classid2 = factor(classid2),
         schoolid = factor(schoolid),
         unique_classid = schoolid : classid2)
fit13c <- lmer(mathscore ~ ses + (ses | schoolid) + (ses | unique_classid),
               data = classroom_df2)
logLik(fit13)
logLik(fit13c)
## fit13, fit13b, fit13c are all equivalent

## assuming rho_c = 0 (no correlation between random intercepts and slopes
## for classroom within school)
fit14 <- lmer(mathscore ~ ses + (ses | schoolid) + (ses || classid),
              data = classroom_df)
summary(fit14)

## fitting only random intercepts per classroom within school
fit15 <- lmer(mathscore ~ ses + (ses | schoolid) + (1 | classid),
              data = classroom_df)
summary(fit15)

anova(fit13, fit14, fit15) ## beware! fit13 estimated a perfect correlation,
                           ## and fit14 has an estimate at the boundary of 
                           ## the parametric space (sigma2_1(c) = 0)
