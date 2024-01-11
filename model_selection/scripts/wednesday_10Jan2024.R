library(tidyverse)

## Nested model comparisons in GLMs (using differences between deviances)

swiss_df <- swiss %>%
  mutate(z = Fertility > median(Fertility))

fit8 <- glm(z ~ Agriculture + Education,
            family = binomial,
            data = swiss_df)

fit9 <- glm(z ~ Agriculture + Education + Catholic,
            family = binomial,
            data = swiss_df)

logLik(fit8)
logLik(fit9)
logLik(fit9) - logLik(fit8)

deviance(fit8)
deviance(fit9)
deviance(fit8) - deviance(fit9)

## deviance comparison in glms using a chi-squared test
anova(fit8, fit9, test = "Chisq")
curve(dchisq(x, df = 1), xlim = c(0,15))
abline(v = deviance(fit8) - deviance(fit9), lty = 2)

## p-value computed manually
pchisq(deviance(fit8) - deviance(fit9), df = 1, lower.tail = FALSE)

## one continuous and one categorical predictor

samara_df <- read_table("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/data/samara.txt")
samara_df <- samara_df %>%
  mutate(Tree = factor(Tree))

samara_df %>%
  ggplot(aes(x = Load, y = Velocity, col = Tree)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## null model
fit10 <- lm(Velocity ~ 1, data = samara_df)
## lines parallel to the x-axis
fit11 <- lm(Velocity ~ Tree, data = samara_df)
## equal lines model
fit12 <- lm(Velocity ~ Load, data = samara_df)
## parallel lines model
fit13 <- lm(Velocity ~ Tree + Load, data = samara_df)
## common intercepts model
fit14 <- lm(Velocity ~ Tree : Load, data = samara_df)
## separate lines model
fit15 <- lm(Velocity ~ Tree * Load, data = samara_df)

library(modelr)
samara_df %>%
  add_predictions(model = fit10, var = "M0") %>%
  add_predictions(model = fit11, var = "M1") %>%
  add_predictions(model = fit12, var = "M2") %>%
  add_predictions(model = fit13, var = "M3") %>%
  add_predictions(model = fit14, var = "M4") %>%
  add_predictions(model = fit15, var = "M5") %>%
  pivot_longer(cols = 4:9,
               names_to = "model",
               values_to = "pred") %>%
  ggplot(aes(x = Load, y = Velocity, col = Tree)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~ model)

anova(fit10, fit11, fit13, fit15)
anova(fit10, fit12, fit13, fit15)
anova(fit10, fit12, fit14, fit15)

anova(fit12, fit15) ## another nested comparison

summary(fit10)$adj.r.squared
summary(fit11)$adj.r.squared
summary(fit12)$adj.r.squared
summary(fit13)$adj.r.squared
summary(fit14)$adj.r.squared
summary(fit15)$adj.r.squared

# Out-of-sample predictive performance ------------------------------------

## loading some helper functions
source("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/scripts/helper_functions.R")

## read in housing data
housing_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/data/housing.csv")

housing_df %>%
  ggplot(aes(x = price)) +
  theme_bw() +
  geom_histogram(bins = 50, col = "white", lwd = .2)

housing_df %>%
  ggplot(aes(x = log(price))) +
  theme_bw() +
  geom_histogram(bins = 50, col = "white", lwd = .2)

## normal model
fit16 <- glm(price ~ 1, data = housing_df)
## gamma model
fit17 <- glm(price ~ 1, family = Gamma(link = "log"), data = housing_df)

coef(fit16)
coef(fit17)

logLik(fit16) ## in-sample
logLik(fit17) ## in-sample

glm_loo_cv(fit16) ## elpd of fit16 (normal)
glm_loo_cv(fit17) ## elpd of fit17 (gamma)

## deviance scale
glm_loo_cv(fit16, deviance_scale = TRUE) ## - 2 * elpd of fit16 (normal)
glm_loo_cv(fit17, deviance_scale = TRUE) ## - 2 * elpd of fit17 (gamma)

glm_loo_cv(fit16, deviance_scale = TRUE) - glm_loo_cv(fit17, deviance_scale = TRUE)

## AIC approx. -2 * elpd (approx doesn't hold well for more complex models)

logLik(fit16)      ## log-likelihood
-2 * logLik(fit16) ## deviance
2 * 2 - 2 * logLik(fit16) ## AIC = 2K + deviance
AIC(fit16) ## built-in function AIC()

AIC(fit17)

## estimate standard errors for the elpd
glm_loo_cv(fit16, deviance_scale = TRUE, se = TRUE)
glm_loo_cv(fit17, deviance_scale = TRUE, se = TRUE)
## we could do the same for AIC using bootstrap, for instance
## important to take into account uncertainty when using threshold rules

delta_AIC <- AIC(fit16) - AIC(fit17)

boot_diff <- function() {
  y_boot <- sample(housing_df$price, nrow(housing_df), replace = TRUE)
  fit1_boot <- glm(y_boot ~ 1)
  fit2_boot <- glm(y_boot ~ 1, family = Gamma(link = "log"))
  AIC_diff <- AIC(fit1_boot) - AIC(fit2_boot)
  return(AIC_diff)
}

boot_diff()

all_diffs <- replicate(5000, boot_diff())
mean(all_diffs)
sd(all_diffs)
quantile(all_diffs, c(.025, .975))

## Small sample correction

AIC(fit16)
AICc(fit16)

AIC(fit17)
AICc(fit17)

## Nonlinear regression with polynomials and splines
library(splines)
library(modelr)

vocab_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/model_selection/data/vocab.csv")
vocab_df

vocab_df %>%
  ggplot(aes(x = age, y = vocab)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = "lm")

stats_vocab <- tibble(df = 1:20,
                      RSS = rep(NA, 20),
                      R2 = rep(NA, 20),
                      AICc = rep(NA, 20))

for(i in 1:20) {
  model <- lm(vocab ~ poly(age, i),
              data = vocab_df)
  stats_vocab$RSS[i] <- sum(residuals(model)^2)
  stats_vocab$R2[i] <- summary(model)$r.squared
  stats_vocab$AICc[i] <- AICc(model)
}

stats_vocab %>%
  pivot_longer(2:4,
               names_to = "gof_measure",
               values_to = "value") %>%
  ggplot(aes(x = df, y = value)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~ gof_measure,
             scales = "free_y")

## AICc selected polynomial of the third order
vocab_df %>%
  add_predictions(model = lm(vocab ~ poly(age, 3),
                             data = vocab_df),
                  var = "pred") %>%
  ggplot(aes(x = age, y = vocab)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred), col = 4)

## this fits noise (completely overfitted)
vocab_df %>%
  add_predictions(model = lm(vocab ~ poly(age, 10),
                             data = vocab_df),
                  var = "pred") %>%
  ggplot(aes(x = age, y = vocab)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred), col = 4)

## now with splines (natural splines = ns())
stats_vocab_splines <- tibble(df = 1:20,
                              RSS = rep(NA, 20),
                              R2 = rep(NA, 20),
                              AICc = rep(NA, 20))

for(i in 1:20) {
  model <- lm(vocab ~ ns(age, i),
              data = vocab_df)
  stats_vocab_splines$RSS[i] <- sum(residuals(model)^2)
  stats_vocab_splines$R2[i] <- summary(model)$r.squared
  stats_vocab_splines$AICc[i] <- AICc(model)
}

stats_vocab_splines %>%
  pivot_longer(2:4,
               names_to = "gof_measure",
               values_to = "value") %>%
  ggplot(aes(x = df, y = value)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~ gof_measure,
             scales = "free_y")

## AICc selected ns with df = 6
vocab_df %>%
  add_predictions(model = lm(vocab ~ ns(age, 6),
                             data = vocab_df),
                  var = "pred") %>%
  ggplot(aes(x = age, y = vocab)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred), col = 4)

## this fits noise (completely overfitted)
vocab_df %>%
  add_predictions(model = lm(vocab ~ ns(age, 20),
                             data = vocab_df),
                  var = "pred") %>%
  ggplot(aes(x = age, y = vocab)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred), col = 4)

# Model averaging ---------------------------------------------------------

stats_vocab_splines$AICc
stats_vocab_splines %>%
  mutate(akaike_weights = akaike_weights(AICc) %>% round(2))

sum(akaike_weights(stats_vocab_splines$AICc))

vocab_df_pred <- vocab_df

for(i in 1:20) {
  vocab_df_pred <- vocab_df_pred %>%
    add_predictions(model = lm(vocab ~ ns(age, i),
                               data = vocab_df),
                    var = paste("df =", i))
}

all_pred <- vocab_df_pred %>%
  select(- age, - vocab) %>%
  as.matrix

vocab_df$wpred <- (t(all_pred) * akaike_weights(stats_vocab_splines$AICc)) %>%
  colSums

vocab_df %>%
  add_predictions(model = lm(vocab ~ ns(age, 6),
                             data = vocab_df),
                  var = "pred") %>%
  ggplot(aes(x = age, y = vocab)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = pred), col = 4) +
  geom_line(aes(y = wpred), col = 2)

## we average the predictions, not the coefficients!