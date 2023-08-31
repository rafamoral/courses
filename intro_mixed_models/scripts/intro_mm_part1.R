library(tidyverse)

## Random Effect Models --------------------------------------------------------

## loading the cbpp dataset
library(lme4)

cbpp_df <- as_tibble(lme4::cbpp)
cbpp_df

cbpp_df %>%
  ggplot(aes(x = herd, y = incidence/size)) +
  theme_bw() +
  geom_boxplot()

## look at batch 4
cbpp_df_4 <- filter(cbpp_df, herd == "4")

fit1 <- glm(cbind(incidence, size - incidence) ~ 1,
            family = binomial,
            data = cbpp_df_4)

## estimated value of beta, i.e. the log odds of disease incidence in herd 4
coef(fit1)
## estimated value of pi, i.e. the probability of disease incidence in herd 4
plogis(coef(fit1))
## 95% confidence interval on the probability of disease incidence in herd 4
plogis(confint.default(fit1))

## a single model of all herds
fit2 <- glm(cbind(incidence, size - incidence) ~ 0 + herd,
            family = binomial,
            data = cbpp_df)
round(plogis(coef(fit2)), 2)

logits_glm <- coef(fit2)
pi_glm <- plogis(coef(fit2))

# multilevel version of the multi herd model
library(lme4)
fit3 <- glmer(cbind(incidence, size - incidence) ~ 1 + (1 | herd),
              family = binomial,
              data = cbpp_df)
summary(fit3)

fixef(fit3)
VarCorr(fit3)$herd[1]

fixef(fit3) + c(-1, 1) * 1.96 * VarCorr(fit3)$herd[1]
plogis(fixef(fit3) + c(-1, 1) * 1.96 * VarCorr(fit3)$herd[1])
## if we repeated this study with an infinite number of herds, we expect
## that 95% of the time the incidence will be between these probabilities

## random effect, or random differences, from the average log odds
ranef(fit3)
coef(fit2)

## shrinkage
comp <- tibble("herd" = 1:15,
               "flat" = plogis(coef(fit2)),
               "mixed" = plogis(fixef(fit3) + ranef(fit3)$herd$`(Intercept)`))

comp %>%
  pivot_longer(2:3,
               names_to = "model type",
               values_to = "pi") %>%
  ggplot(aes(x = `model type`, y = pi, group = herd)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  ylab(expression(pi))

## Linear Mixed Effect Models --------------------------------------------------

alcohol_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_mixed_models/data/alcohol.csv")
alcohol_df

alcohol_df %>%
  ggplot(aes(x = reorder(country, alcohol), y = alcohol)) +
  theme_bw() +
  geom_boxplot() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  xlab("country")

fit4 <- lmer(alcohol ~ 1 + (1|country),
             data = alcohol_df)
summary(fit4)
## ranef gives the "b" values (BLUPs)
ranef(fit4)

## the mu's themselves are from beta_0 + b or coef(fit4)
## and beta_0 is from fixef(fit4)
fixef(fit4)
coef(fit4)

## Intraclass correlation
22.2 / (22.2 + 1.1)
VarCorr(fit4)
VarCorr(fit4) %>% as.data.frame
vars <- (VarCorr(fit4) %>% as.data.frame)[,"vcov"]
vars[1] / sum(vars)

## displaying shrinkage
comp <- tibble("country" = unique(alcohol_df$country),
               "flat" = coef(lm(alcohol ~ 0 + country, data = alcohol_df)),
               "mixed" = coef(fit4)$country$`(Intercept)`)

comp %>%
  pivot_longer(2:3,
               names_to = "model type",
               values_to = "mu") %>%
  ggplot(aes(x = `model type`, y = mu, group = country)) +
  theme_bw() +
  geom_point() +
  geom_line(alpha = .3) +
  ylab(expression(mu))

## shrinkage when all variation is due to random noise
beta0 <- 5
sigma2 <- 4

set.seed(2023)
simulated_df <- tibble(y = rnorm(100, mean = beta0, sd = sqrt(sigma2)),
                       country = gl(20, 5))

fit5 <- lmer(y ~ 1 + (1 | country),
             data = simulated_df)
summary(fit5)
ranef(fit5)

comp <- tibble("country" = unique(simulated_df$country),
               "flat" = coef(lm(y ~ 0 + country, data = simulated_df)),
               "mixed" = coef(fit5)$country$`(Intercept)`)

comp %>%
  pivot_longer(2:3,
               names_to = "model type",
               values_to = "mu") %>%
  ggplot(aes(x = `model type`, y = mu, group = country)) +
  theme_bw() +
  geom_point() +
  geom_line(alpha = .3) +
  ylab(expression(mu))

## Linear Mixed Effects Models -------------------------------------------------

## loading the data
library(lme4)
data(sleepstudy)
sleepstudy_df <- as_tibble(sleepstudy)
sleepstudy_df

sleepstudy_df %>%
  ggplot(aes(x = Days, y = Reaction)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  facet_wrap(~ Subject)

sleepstudy_df %>%
  ggplot(aes(x = Days, y = Reaction)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~ Subject) +
  geom_smooth(se = FALSE, method = "lm")

## a model for subject 334
sleepstudy_334 <- filter(sleepstudy, Subject == 334)
fit6 <- lm(Reaction ~ Days, data = sleepstudy_334)
coef(fit6) # the beta coefficients
sigma(fit6) # for sigma

fit6a <- lm(Reaction ~ 1 + Days, data = sleepstudy_334)
coef(fit6a)
fit6b <- lm(Reaction ~ 0 + Days, data = sleepstudy_334)
coef(fit6b)

## non-multilevel model but of all subjects in the data
fit7 <- lm(Reaction ~ 0 + Subject + Subject:Days, data = sleepstudy_df)
summary(fit7)
sigma(fit7)

## multilevel linear model aka linear mixed effects model
fit8 <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), data = sleepstudy_df)
ranef(fit8) # these are b
coef(fit8)  # these are beta

## This is identical to fit8
fit9 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy_df)

## The bivariate normal distribution
VarCorr(fit9) %>% as_tibble
Sigma <- matrix(c(612, 9.6, 9.6, 35.1), ncol = 2)

library(mvtnorm)
x_grid <- seq(-60, 60, length = 100)
y_grid <- seq(-20, 20, length = 100)
f <- function(x, y) dmvnorm(cbind(x, y), sigma = Sigma)
z <- outer(x_grid, y_grid, f)

persp(z)
persp(z, theta = 30, phi = 25)

contour(x_grid, y_grid, z, asp = 1)

Sigma <- matrix(c(612, sqrt(612 * 35.1) * .8, sqrt(612 * 35.1) * .8, 35.1), ncol = 2)
z2 <- outer(x_grid, y_grid, f)
persp(z2, theta = 30, phi = 25)
contour(x_grid, y_grid, z2, asp = 1)

Sigma <- matrix(c(612, sqrt(612 * 35.1) * -.8, sqrt(612 * 35.1) * -.8, 35.1), ncol = 2)
z3 <- outer(x_grid, y_grid, f)
persp(z3, theta = 30, phi = 25)
contour(x_grid, y_grid, z3, asp = 1)

## looking at 2d shrinkage
flat_coef <- coef(lm(Reaction ~ 0 + Subject + Subject:Days, data = sleepstudy_df))
comp <- tibble("subject" = unique(sleepstudy_df$Subject),
               "flat_intercept" = flat_coef[1:18],
               "flat_slope" = flat_coef[19:36],
               "mixed_intercept" = coef(fit8)$Subject$`(Intercept)`,
               "mixed_slope" = coef(fit8)$Subject$Days)

grid_all <- expand.grid(x = fixef(fit8)[1] + seq(-60, 60, length = 100),
                        y = fixef(fit8)[2] + seq(-20, 20, length = 100))
Sigma <- matrix(c(612, 9.6, 9.6, 35.1), ncol = 2)
grid_all$density <- apply(grid_all, 1, dmvnorm, mean = fixef(fit8), sigma = Sigma)

comp %>%
  ggplot(aes(x = flat_intercept, y = flat_slope)) +
  theme_bw() +
  geom_point(size = 1) +
  geom_segment(aes(xend = mixed_intercept, yend = mixed_slope),
               arrow = arrow(length = unit(.1, "cm"))) +
  geom_text(aes(label = subject), size = 3,
            nudge_y = 1) +
  xlab(expression(hat(beta)[0])) +
  ylab(expression(hat(beta)[1])) +
  geom_vline(xintercept = fixef(fit8)[1], lty = 2) +
  geom_hline(yintercept = fixef(fit8)[2], lty = 2) +
  geom_contour(data = grid_all,
               aes(x = x, y = y, z = density),
               col = 1, alpha = .2)

## Random intercepts only
## So all slopes assumed to be identical across subjects
fit10 <- lmer(Reaction ~ 1 + Days + (1 | Subject), data = sleepstudy_df)
summary(fit10)

## Random slopes only
fit11 <- lmer(Reaction ~ 1 + Days + (0 + Days | Subject), data = sleepstudy_df)
summary(fit11)

## comparing
library(modelr)
sleepstudy_df <- add_predictions(data = sleepstudy_df, model = fit10, var = "random_intercepts")
sleepstudy_df <- add_predictions(data = sleepstudy_df, model = fit11, var = "random_slopes")

sleepstudy_df %>%
  pivot_longer(4:5,
               names_to = "model",
               values_to = "predictions") %>%
  ggplot(aes(x = Days, y = predictions, group = Subject)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~ model)

## Random slopes and random intercepts but no correlation
fit12 <- lmer(Reaction ~ Days + (Days || Subject), data = sleepstudy_df)

anova(fit12, fit8)

deviance(fit12)
deviance(fit12, REML = FALSE)

drop1(fit12)
AIC(fit12)
- 2 * logLik(fit12) + 12
deviance(fit12, REML = FALSE) + 12

drop1(fit12, test = "Chisq")

deviance(fit12, REML = FALSE)
deviance(fit8, REML = FALSE)
deviance(fit12, REML = FALSE) - deviance(fit8, REML = FALSE)
anova(fit8, fit12)

pchisq(deviance(fit12, REML = FALSE) - deviance(fit8, REML = FALSE),
       1, lower.tail = FALSE)

anova(fit8, fit10)
anova(fit8, fit11)

library(lmerTest)
summary(fit9)
fit9 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy_df)
summary(fit9)

add_predictions(sleepstudy, fit9) %>% 
  ggplot(aes(x = Days, y = Reaction)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~ Subject)

add_predictions(sleepstudy, fit9) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred))

## Nested Random Effects -------------------------------------------------------

classroom_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_mixed_models/data/classroom.csv")
classroom_df

classroom_df %>%
  ggplot(aes(x = ses, y = mathscore)) +
  theme_bw() +
  geom_point(size = 1) + 
  facet_wrap(~ schoolid) + 
  stat_smooth(method = "lm", se = FALSE, lwd = .5)

classroom_df %>%
  filter(schoolid == "11") %>%
  ggplot(aes(x = ses, y = mathscore, col = factor(classid))) +
  theme_bw() +
  geom_point(size = 1) +
  stat_smooth(method = "lm", se = FALSE, lwd = .5)

fit13 <- lmer(mathscore ~ ses + (ses | schoolid) + (ses | classid),
             data = classroom_df)
summary(fit13)

fit14 <- lmer(mathscore ~ ses + (ses | schoolid) + (ses || classid),
             data = classroom_df)
summary(fit14)

fit15 <- lmer(mathscore ~ ses + (ses | schoolid) + (1 | classid),
             data = classroom_df)

summary(fit15)

## does not work in the same way as fit13 does not work
fit16 <- lmer(mathscore ~ ses + (ses | schoolid / classid2),
             data = classroom_df)

summary(fit16)

## for now, we will simplify to random intercepts only at BOTH schools and classes levels
## fit17 and fit18 and fit19 are all the same
fit17 <- lmer(mathscore ~ ses + (1 | schoolid / classid2),
             data = classroom_df)
fit18 <- lmer(mathscore ~ ses + (1 | schoolid) + (1 | classid),
             data = classroom_df)
fit19 <- lmer(mathscore ~ ses + (1 | schoolid) + (1 | schoolid:classid2),
             data = classroom_df)
logLik(fit17); logLik(fit18); logLik(fit19)

anova(fit15, fit18)
summary(fit18)
anova(fit18)

## we can use classid2 to make the fit15 model as follows
fit20 <- lmer(mathscore ~ ses + (ses | schoolid) + (1 | schoolid:classid2),
             data = classroom_df)
logLik(fit15); logLik(fit20)

## Crossed Random Effects ------------------------------------------------------

blp_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_mixed_models/data/blp-short2.csv")
blp_df %>% filter(spelling == "herb")
blp_df %>% filter(participant == 18)
blp_df <- blp_df %>%
  mutate(participant = as.factor(participant),
         spelling = as.factor(spelling))

blp_df %>%
  ggplot(aes(x = reorder(participant, rt), y = rt)) +
  theme_bw() +
  geom_boxplot()

blp_df %>%
  ggplot(aes(x = reorder(spelling, rt), y = rt)) +
  theme_bw() +
  geom_boxplot()

blp_df %>%
  group_by(spelling) %>%
  summarise(mean = mean(na.omit(rt)),
            var = var(na.omit(rt))) %>%
  arrange(desc(mean)) %>%
  View

fit21 <- lmer(rt ~ 1 + (1 | participant) + (1 | spelling),
              data = blp_df %>%
                na.omit)
summary(fit21)

## Group-Level Predictors ------------------------------------------------------

mathach_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_mixed_models/data/mathachieve.csv")
mathach_school_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_mixed_models/data/mathachieveschool.csv")

## in principle, just fine, but in practice not so much... convergence errors
M_22 <- lmer(mathach ~ ses + sex + minority + (ses|school) + (sex|school) + (minority|school),
             REML = F,
             data = mathach_df)

M_23 <- lmer(mathach ~ ses + sex + minority + (1 + ses|school), 
             REML = F,
             data = mathach_df)

mathach_join_df <- inner_join(mathach_df, mathach_school_df, by = 'school')

M_24 <- lmer(mathach ~ ses + himinty + (1 + ses|school), 
             REML = F,
             data = mathach_join_df)

M_25 <- lmer(mathach ~ ses + himinty + (1 + ses|school) + (himinty|school), 
             REML = F,
             data = mathach_join_df)

# does not work ...
# M_26 <- lmer(mathach ~ ses + pracad + (ses|school) + (pracad|school)) 



## Generalized Linear Mixed Models (GLMMs) -------------------------------------



## Individual-Level Random Effects ---------------------------------------------



## Bayesian Mixed Models -------------------------------------------------------

library(brms)

M_27 <- lm(Reaction ~ Days, data = sleepstudy)  # linear regression
M_28 <- brm(Reaction ~ Days, data = sleepstudy) # Bayesian linear regression

summary(M_27)$coefficients
M_28

## Bayesian mixed effects model

M_29 <- brm(Reaction ~ Days + (Days|Subject), data = sleepstudy)
M_8 <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)

M_30 <- brm(Reaction ~ Days + (Days|Subject), 
            data = sleepstudy)

M_31 <- brm(Reaction ~ Days + (Days||Subject), 
            data = sleepstudy)

waic_30 <- waic(M_30)
waic_31 <- waic(M_31)
loo_compare(waic_30, waic_31)