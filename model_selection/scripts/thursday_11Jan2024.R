library(tidyverse)

# Variable selection ------------------------------------------------------

student_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/data/student.csv")

## we have 26 covariates, how do we select the best ones to predict maths scores?

fit18 <- lm(math ~ ., data = student_df)

fit18_step_back <- step(fit18,
                        direction = "backward")
AIC(fit18_step_back) ## notice how this is different to the AIC reported in step
                     ## this is because step() is dropping the constants in the
                     ## log-likelihood when calculating the AIC = 2K - 2logLik

fit19 <- lm(math ~ 1, data = student_df)

fit19_step_forward <- step(fit19,
                           direction = "forward",
                           scope = formula(fit18))

sort(names(coef(fit18_step_back)))
sort(names(coef(fit19_step_forward)))

AIC(fit18_step_back)
AIC(fit19_step_forward)

fit20_step_both <- step(fit18,
                        direction = "both")

fit21_step_both <- step(fit19,
                        direction = "both",
                        scope = formula(fit18))

AIC(fit18_step_back)
AIC(fit19_step_forward)
AIC(fit20_step_both)
AIC(fit21_step_both)

2 ^ 26 ## total number of models to consider

2 ^ (26 + choose(26, 2))

## all subsets regression
library(MuMIn)

fit22 <- lm(Fertility ~ .,
            data = swiss,
            na.action = "na.fail")

fit22_all_subsets <- dredge(fit22)
fit22_all_subsets

final_set <- get.models(fit22_all_subsets, cumsum(weight) < .95)

## regularisation

## avoid overfitting of noise, e.g.
tibble(age = seq(18, 89, length = 200)) %>%
  add_predictions(model = lm(vocab ~ ns(age, 27),
                             data = vocab_df),
                  var = "vocab") %>%
  ggplot(aes(x = age, y = vocab)) +
  theme_bw() +
  geom_point(data = vocab_df) +
  geom_line(col = 4)

library(glmnet)

## LASSO
## need to create the matrix of predictors and
## the vector of responses separately
y <- student_df$math
X <- student_df %>% select(- math) %>% as.matrix

fit23_lasso <- glmnet(x = X, y = y, alpha = 1) ## alpha = 1 for LASSO
plot(fit23_lasso, xvar = "lambda", label = TRUE)

fit23_lasso_cv <- cv.glmnet(x = X, y = y, alpha = 1)
plot(fit23_lasso_cv)

## first dashed line = best cv score
## second = more regularised that is within 1 sd of the smallest MSE
coef(fit23_lasso,
     s = c(fit23_lasso_cv$lambda.min,
           fit23_lasso_cv$lambda.1se))

formula(fit20_step_both)

## Ridge regression
fit24_ridge <- glmnet(x = X, y = y, alpha = 0) ## alpha = 0 for ridge regression
plot(fit24_ridge, xvar = "lambda", label = TRUE)

fit24_ridge_cv <- cv.glmnet(x = X, y = y, alpha = 0)
plot(fit24_ridge_cv)

coef(fit24_ridge,
     s = c(fit24_ridge_cv$lambda.min,
           fit24_ridge_cv$lambda.1se))

## careful with scaling vs. non-scaling
apply(student_df, 2, range)
apply(student_df, 2, var)

fit25_lasso <- glmnet(x = scale(X), y = y, alpha = 1)
fit25_lasso_cv <- cv.glmnet(x = scale(X), y = y, alpha = 1)
plot(fit25_lasso_cv)

cbind(
  coef(fit23_lasso,
       s = fit23_lasso_cv$lambda.1se),
  coef(fit25_lasso,
       s = fit25_lasso_cv$lambda.1se)
)

# Bayesian regularised variable selection ---------------------------------

student_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/data/student.csv")

library(brms)
fit26 <- brm(math ~ age + school,
             data = student_df)
fit26

## check posterior distributions and trace plots
plot(fit26)

fit27 <- brm(math ~ .,
             data = student_df,
             prior = set_prior("horseshoe(df = 3)"))
fit27

library(bayesplot)
mcmc_areas(fit27)
mcmc_areas(fit27,
           regex_pars = "^b_[safPMFtpnhirg]",
           prob = 0.95)

## another example

sleep_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/data/sleepstudy.csv")

sleep_df %>%
  ggplot(aes(x = Days, y = Reaction)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Subject)

## Bayesian linear regression
fit28 <- brm(Reaction ~ Days, data = sleep_df)

## normal linear mixed effects
fit29 <- brm(Reaction ~ Days + (Days | Subject), data = sleep_df)

## robust linear mixed effects (t-distributed errors)
fit30 <- brm(Reaction ~ Days + (Days | Subject),
             family = student(),
             data = sleep_df)

## Watanabe AIC (Widely Applicable IC)
waic(fit28)
waic(fit29)
waic(fit30)

loo_compare(waic(fit28), waic(fit29), waic(fit30))

# Distributional regression using gamlss ----------------------------------

library(gamlss)

insect_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/data/insect_diet.csv")
insect_df <- na.omit(insect_df)

insect_df %>%
  ggplot(aes(x = day, y = weight)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ diet)

## comparing weights at day 14

day14_df <- insect_df %>%
  filter(day == 14)

day14_df %>%
  ggplot(aes(x = diet, y = weight)) +
  theme_bw() +
  geom_boxplot()

fit31 <- lm(weight ~ diet, data = day14_df)
anova(fit31)
summary(fit31)

fit32 <- gamlss(weight ~ diet, data = day14_df)
logLik(fit31)
logLik(fit32)

## fit a model with heterogeneous variances
fit32 <- gamlss(weight ~ diet,
                sigma.formula = ~ diet,
                data = day14_df)
summary(fit32)

AIC(fit31)
AIC(fit32)

## comparing ag with sf (checking whether they are the same)
day14_df$diet2 <- factor(day14_df$diet)
levels(day14_df$diet2)[c(1,3)] <- c("ag + sf")

fit33 <- gamlss(weight ~ diet,
                sigma.formula = ~ diet2,
                data = day14_df)
AIC(fit32)
AIC(fit33)

fit34 <- gamlss(weight ~ diet,
                sigma.formula = ~ diet2,
                family = GA,
                data = day14_df)
AIC(fit33)
AIC(fit34)

## modelling heterogeneous variances over time
anticarsia_df <- insect_df %>%
  filter(diet == "ag")

anticarsia_df %>%
  ggplot(aes(x = day, y = weight)) +
  theme_bw() +
  geom_point()

anticarsia_df %>%
  mutate(pred = predict(gamlss(weight ~ ns(day, 4)))) %>%
  ggplot(aes(x = day, y = weight)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred), col = 2)

fit35 <- gamlss(weight ~ ns(day, 4),
                data = anticarsia_df)
plot(fit35)
wp(fit35) ## worm plot

fit36 <- gamlss(weight ~ ns(day, 4),
                sigma.formula = ~ day,
                data = anticarsia_df)
plot(fit36)
wp(fit36)

AIC(fit35)
AIC(fit36)

## illustrating stepGAICAll.A
?usair
names(usair) <- c("SO2","temperature","manufacturers",
                  "population","wind_speed","rainfall","rain_days")

fit37 <- gamlss(SO2 ~ .,
                sigma.formula = ~ .,
                family = NO,
                data = usair)
plot(fit37)
wp(fit37)

fit38 <- stepGAICAll.A(fit37)

AIC(fit37)
AIC(fit38)