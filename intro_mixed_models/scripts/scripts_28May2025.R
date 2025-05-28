library(tidyverse)
library(lme4)


# Linear mixed effects models ---------------------------------------------

## loading the sleepstudy data
sleepstudy_df <- as_tibble(sleepstudy)

## let's create an exploratory plot
sleepstudy_df %>%
  ggplot(aes(x = Days, y = Reaction, col = Subject)) +
  theme_bw() +
  geom_line()
## (a "spaghetti" plot)

## we can also look at each individual in more detail
sleepstudy_df %>%
  ggplot(aes(x = Days, y = Reaction)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~ Subject)

## let's create a model for subject 308
sleepstudy_308 <- sleepstudy_df %>%
  filter(Subject == 308)

fit1 <- lm(Reaction ~ Days,
           data = sleepstudy_308)
coef(fit1) ## the estimates for beta0 and beta1 (intercept and slope for subject 308)
sigma(fit1)^2 ## the estimate for sigma^2

## "flat" / non-mixed model for all subjects in the data
## 18 * 2 + 1 = 37 parameters (18 intercepts, 18 slopes, 1 variance)
fit2 <- lm(Reaction ~ Days * Subject,
           data = sleepstudy_df)
coef(fit2)
## NB: the "*" syntax in an R formula means:
## a * b = a + b + a:b
## a : b = that is just the interaction a:b

## using a reparameterisation of the formula to obtain all coefficients directly
fit2_repar <- lm(Reaction ~ 0 + Subject + Subject : Days,
                 data = sleepstudy_df)
coef(fit2_repar)

logLik(fit2)
logLik(fit2_repar)

sleepstudy_df %>%
  ggplot(aes(x = Days, y = Reaction)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~ Subject) +
  geom_smooth(se = FALSE, method = "lm")

## let's fit the mixed model
fit3 <- lmer(Reaction ~ Days + (Days | Subject),
             data = sleepstudy_df)
summary(fit3)

## distribution for the random intercepts
curve(dnorm(x, mean = 251.405, sd = 24.741), xlim = c(120, 380))
abline(v = 251.405 + c(-1, 1) * 1.96 * 24.741, lty = 2)

## distribution for the random slopes
curve(dnorm(x, mean = 10.467, sd = 5.922), xlim = c(-10, 30))
abline(v = 10.467 + c(-1, 1) * 1.96 * 5.922, lty = 2)

## how likely would it be that a new randomly chosen person from this population
## would thrive under sleep deprivation (i.e. having a negative slope)
pnorm(0, mean = 10.467, sd = 5.922)

fixef(fit3) ## these are the betas
ranef(fit3) ## these are the b's

library(ggrepel)

ranef(fit3)$Subject %>%
  mutate(subj_ID = rownames(ranef(fit3)$Subject)) %>%
  ggplot(aes(x = `(Intercept)`, y = Days)) +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_text_repel(aes(label = subj_ID))

## let's calculate predictions and compare with the predictions from the "flat" model
predict(fit3) ## these are conditional predictions (include the random effects)
predict(fit3, re.form = NA) ## these are marginal predictions (averaging out the random effects)

library(modelr)
sleepstudy_df %>%
  add_predictions(model = fit2, var = "flat") %>%
  add_predictions(model = fit3, var = "mixed") %>%
  pivot_longer(4:5,
               names_to = "model",
               values_to = "pred") %>%
  ggplot(aes(x = Days, y = Reaction)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred, color = model)) +
  facet_wrap(~ Subject)

## let's experiment with reduced models
## random intercepts only (no random slopes)
fit4 <- lmer(Reaction ~ Days + (1 | Subject),
             data = sleepstudy_df)
summary(fit4)

fit5 <- lmer(Reaction ~ Days + (0 + Days | Subject),
             data = sleepstudy_df)
summary(fit5)

sleepstudy_df %>%
  add_predictions(model = fit3, var = "random_int_slo") %>%
  add_predictions(model = fit4, var = "random_int") %>%
  add_predictions(model = fit5, var = "random_slo") %>%
  pivot_longer(4:6,
               names_to = "model",
               values_to = "pred") %>%
  ggplot(aes(x = Days, y = Reaction)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred, color = model)) +
  facet_wrap(~ Subject)

## random slopes and random intercept, but assuming the correlation
## between them is zero (rho = 0)
fit6 <- lmer(Reaction ~ Days + (Days || Subject),
             data = sleepstudy_df)
summary(fit6)

## (model selection)
## let's compare our model fits and conclude which one should be used to make inference
## fit3: random intercepts and slopes and correlation
fit3 <- lmer(Reaction ~ Days + (Days | Subject),
             data = sleepstudy_df)
## fit6: random intercepts and slopes (no correlation)
fit6 <- lmer(Reaction ~ Days + (Days || Subject),
             data = sleepstudy_df)
## fit4: random intercepts only
fit4 <- lmer(Reaction ~ Days + (1 | Subject),
             data = sleepstudy_df)
## fit5: random slopes only
fit5 <- lmer(Reaction ~ Days + (0 + Days | Subject),
             data = sleepstudy_df)

anova(fit3, fit6)
curve(dchisq(x, 1), xlim = c(0,5))
abline(v = qchisq(.95, df = 1), lty = 2)
abline(v = 0.0639, col = 2)
## the hypothesis we are testing here can be written as
## H0: the two models are equivalence in terms of fit
## H1: the more complex model fits the data better than the less complex model

## our test results tell us that there is no evidence that the null hypothesis (H0)
## should be rejected
## therefore, we may discard the more complex model (fit3) in favour of the
## more parsimonious one (fit6), which assumes rho to be zero

anova(fit6, fit4)
anova(fit6, fit5)
## these tests tell us that the more complex model (fit6) provides a significantly
## better fit to the data when compared to the less complex ones
## in other words, the random intercepts AND random slopes are important to
## provide a good approximation to the data generating mechanism

extractAIC(fit3)
extractAIC(fit6)
extractAIC(fit4)
extractAIC(fit5)

## tests for fixed effects
anova(fit6)   ## F statistic, but no p-value
summary(fit6) ## Wald-t statistic, but no p-value

drop1(fit6, test = "Chisq")

library(lmerTest)
## first re-fit your model
fit6 <- lmer(Reaction ~ Days + (Days || Subject),
             data = sleepstudy_df)
summary(fit6)
anova(fit6)

## extra:
## Durbin-Wu-Hausman test
## function phtest within package plm
## this is a test to check whether you should include a covariate as random or fixed


# Nested random effects ---------------------------------------------------

classroom_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_mixed_models/data/classroom.csv")
classroom_df

## some exploratory visualisation
classroom_df %>%
  ggplot(aes(x = ses, y = mathscore)) +
  theme_bw() +
  geom_point(size = .5) +
  facet_wrap(~ schoolid) +
  geom_smooth(se = FALSE, method = "lm", lwd = .3)

## sample size per school
classroom_df %>%
  group_by(schoolid) %>%
  summarise(n_children = n()) %>%
  arrange(n_children)

## histogram of the predictor variable "ses"
hist(classroom_df$ses) ## a bit skewed to the right

classroom_df %>%
  filter(schoolid == 60) %>%
  ggplot(aes(x = ses, y = mathscore)) +
  theme_bw() +
  geom_point(size = .5) +
  geom_vline(xintercept = mean(classroom_df$ses))

## let's have a look at classes within a school
classroom_df %>%
  group_by(schoolid) %>%
  summarise(n_classes = length(unique(classid))) %>%
  arrange(desc(n_classes))

classroom_df %>%
  filter(schoolid == 11) %>%
  ggplot(aes(x = ses, y = mathscore, col = factor(classid))) +
  theme_bw() +
  geom_point() +
  geom_smooth(se = FALSE, method = lm)

## let's fit some mixed models incorporating the nested data structure
fit7 <- lmer(mathscore ~ ses + (ses | schoolid) + (ses | classid),
             data = classroom_df)
## note that we are using "classid", which has unique identifiers for class within school
## this is the syntax when we have unique IDs
summary(fit7)

## what if we don't have unique identifiers for classid?
## this is the variable "classid2", which goes from 1 until the total number of classes for each school
fit7b <- lmer(mathscore ~ ses + (ses | schoolid / classid2),
              data = classroom_df)
summary(fit7b)
logLik(fit7)
logLik(fit7b)

# this formula
# ~ ses + (ses | schoolid / classid2)
# tells us: we have fixed intercept and slope over ses
#           and random intercepts and slopes over ses per schoolid
#           and per classid2 within schoolid

a <- factor(rep(c("a1","a2","a3"), 2))
b <- factor(rep(c("b1","b2"), each = 3))

tibble(schoolid = b, classid2 = a, a : b)

fit7c <- lmer(mathscore ~ ses + (ses | schoolid) + (ses | my_classid),
              data = classroom_df %>%
                mutate(my_classid = factor(schoolid) : factor(classid2)))
logLik(fit7c)

## fitting a reduced model, assuming rho_c = 0 (no correlation between random
## intercepts and slopes between classes within school)
fit8 <- lmer(mathscore ~ ses + (ses | schoolid) + (ses || classid),
             data = classroom_df)
summary(fit8)

anova(fit7, fit8)

## further reducing our model, now removing random slopes per class within school
fit9 <- lmer(mathscore ~ ses + (ses | schoolid) + (1 | classid),
             data = classroom_df)
summary(fit9)

## aside: degenerate distributions
curve(dnorm(x, mean = 0, sd = 1), xlim = c(-2,2), ylim = c(0,10))

for(i in c(.5, .2, .1, .05, .01, .001, .0001)) {
  curve(dnorm(x, mean = 0, sd = i), add = TRUE)
  Sys.sleep(.5)
}
## end: aside

## further reducing, removing correlation for the school random effects
fit10 <- lmer(mathscore ~ ses + (ses || schoolid) + (1 | classid),
              data = classroom_df)

## further reducing our model, now removing random slopes per school
fit11 <- lmer(mathscore ~ ses + (1 | schoolid) + (1 | classid),
              data = classroom_df)
summary(fit11)

anova(fit11)

## comparing models
## fit7: random int + slopes + corr for schools and classes within school
## fit8: removing corr for classes random int and slopes
## fit9: removing random slopes per class within school
## fit10: removing correlation for school random effects
## fit11: removing random slopes per school
anova(fit7, fit8, fit9, fit10, fit11)
## from this test, we conclude that the simplest model that only has
## random intercepts per school, and per class within school
## fits the data just as well as the more complex ones
## and, therefore, could be selected for inference