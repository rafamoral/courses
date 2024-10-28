library(tidyverse)

# loading data directly from github

# Example 1: weight data
weight_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_glmm_laimburg/data/weight.csv")
weight_df

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  theme_bw() +
  geom_point(alpha = .3) +
  geom_smooth()

## fit a simple linear regression model
fit1 <- lm(weight ~ height, data = weight_df)
summary(fit1)
confint(fit1, parm = "height")

# Example 2:
# effect of milk type on a particular marker
# continuous data

marker_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_glmm_laimburg/data/01_marker_data.csv")

marker_df <- marker_df %>%
  mutate(milk_type = `Milk type`) %>%
  filter(milk_type != "H") %>%
  dplyr::select(- `Milk type`) %>%
  mutate(milk_type = as.factor(milk_type))

marker_df %>%
  group_by(milk_type) %>%
  summarise(mean = mean(Marker),
            variance = var(Marker))

marker_df %>%
  ggplot(aes(x = milk_type, y = Marker)) +
  geom_boxplot()

fit0a <- glm(Marker ~ milk_type,
             family = gaussian,
             data = marker_df)
summary(fit0a)

fit0b <- glm(Marker ~ 0 + milk_type,
             family = gaussian,
             data = marker_df)
summary(fit0b)

logLik(fit0a)
logLik(fit0b)

summary(fit0a)$coefficients
summary(fit0b)$coefficients

## how can we check whether fit0a fits the data well?
## check for normality of residuals
shapiro.test(rstudent(fit0a))
## check for homogeneity of variances
bartlett.test(Marker ~ milk_type,
              data = marker_df)

## producing a half-normal plot with a simulated envelope
## the envelope is such that if the model is well-fitted (or
## the data is a plausible realisation of the fitted model)
## we expect most points to lie within the bands of the envelope

# install.packages("hnp")
library(hnp)
hnp(fit0a)
hnp(fit0a, print = TRUE, paint = TRUE)

## attempt a variance-stabilising transformation
fit1 <- glm(log(Marker) ~ milk_type,
            family = gaussian,
            data = marker_df)
summary(fit1)

hnp(fit1, print = TRUE, paint = TRUE)

## other alternatives

## a Gamma model
fit2 <- glm(Marker ~ milk_type,
            family = Gamma(link = "log"),
            data = marker_df)
summary(fit2)

hnp(fit2, print = TRUE, paint = TRUE)

## an inverse-Gaussian model
fit3 <- glm(Marker ~ milk_type,
            family = inverse.gaussian(link = "log"),
            data = marker_df)
summary(fit3)

hnp(fit3, print = TRUE, paint = TRUE)

## fitting a double GLM (modelling the variance heterogeneity)
## a.k.a. generalized additive models for location, scale and shape
## (GAMLSS)

# install.packages("gamlss")
library(gamlss)

fit4 <- gamlss(Marker ~ milk_type,
               family = NO,
               data = marker_df)
logLik(fit4)
logLik(fit0a)

fit5 <- gamlss(Marker ~ milk_type,
               sigma.formula = ~ milk_type,
               family = NO,
               data = marker_df)
summary(fit5)

## hnp doesn't automatically work for all gamlss objects
## but we can use worm-plots (which are similar to hnp)
wp(fit4) ## assuming homogeneity of variances
wp(fit5) ## accommodating the heterogeneity of variances



# Bernoulli GLMs ----------------------------------------------------------

## loading the affairs dataset
affairs_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_glmm_laimburg/data/affairs.csv")

affairs_df <- affairs_df %>%
  mutate(had_affair = affairs > 0)

## probability of having an affair as a function of number of years married
affairs_df %>%
  ggplot(aes(x = yearsmarried, y = had_affair %>% as.numeric)) +
  theme_bw() +
  geom_jitter(width = .2, height = .1, alpha = .5) +
  geom_smooth(se = FALSE)

## fitting the Bernoulli logistic regression model
fit6 <- glm(had_affair ~ poly(yearsmarried, 2),
            family = binomial(link = "logit"),
            data = affairs_df)
summary(fit6)

exp(coef(fit6)[2]) ## this is the estimated odds-ratio for a 1-unit increase in x

confint(fit6)

exp(confint.default(fit6, parm = "yearsmarried"))

## creating predictions
library(modelr)

## create a grid to get a smoother curve

affairs_pred <- tibble(yearsmarried = seq(.125, 15, length = 200))

affairs_pred <- affairs_pred %>%
  add_predictions(model = fit6, type = "response")

affairs_df %>%
  ggplot(aes(x = yearsmarried, y = as.numeric(had_affair))) +
  theme_bw() +
  geom_jitter(width = .5, height = .05,
              alpha = .4) +
  geom_line(data = affairs_pred,
            aes(y = pred))

fit_linear <- glm(had_affair ~ yearsmarried,
                  family = binomial(link = "logit"),
                  data = affairs_df)

fit_quad <- glm(had_affair ~ poly(yearsmarried, 2),
                family = binomial(link = "logit"),
                data = affairs_df)

anova(fit_linear, fit_quad, test = "Chisq")

# comparing with reference distribution
curve(dchisq(x, df = 1), xlim = c(0,10))
abline(v = 5.6452, lty = 2)


# Binomial GLMs -----------------------------------------------------------

## loading the citrus psyllid mortality data
library(hnp)
data(fungi)

fungi$species <- relevel(fungi$species, "beauveria")

fungi %>%
  ggplot(aes(x = lconc, y = y/m, colour = species)) +
  theme_bw() +
  geom_point() +
  ylab("proportion of dead insects") +
  xlab("log10(concentration)")

## fitting the binomial GLM
## response: matrix of 2 columns, 1st = successes, 2nd = failures
fit7 <- glm(cbind(y, m - y) ~ lconc,
            family = binomial,
            data = fungi)

## assess goodness-of-fit
hnp(fit7) ## terrible fit, do not continue

anova(fit7, test = "Chisq")

## fitting possible models
M1 <- glm(cbind(y, m - y) ~ 1,
          family = binomial,
          data = fungi)

M2 <- glm(cbind(y, m - y) ~ lconc,
          family = binomial,
          data = fungi)

M3 <- glm(cbind(y, m - y) ~ species,
          family = binomial,
          data = fungi)

M4 <- glm(cbind(y, m - y) ~ lconc + species,
          family = binomial,
          data = fungi)

M5 <- glm(cbind(y, m - y) ~ lconc : species,
          family = binomial,
          data = fungi)

M6 <- glm(cbind(y, m - y) ~ lconc * species,
          family = binomial,
          data = fungi)

anova(M1, M2, M5, M6, test = "Chisq")
anova(M1, M2, M4, M6, test = "Chisq")
anova(M1, M3, M4, M6, test = "Chisq")

AIC(M1)
AIC(M2)
AIC(M3)
AIC(M4)
AIC(M5)
AIC(M6)

BIC(M1)
BIC(M2)
BIC(M3)
BIC(M4)
BIC(M5)
BIC(M6)

M6_repar <- glm(cbind(y, m - y) ~ 0 + species + lconc : species,
                family = binomial,
                data = fungi)
exp(confint.default(M6_repar))

## golf dataset: using alternative link functions to the logit

golf_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_glm/data/golf_putts.csv")
golf_df

fit11 <- glm(cbind(success, attempts - success) ~ poly(distance, 2),
             family = binomial(link = "probit"),
             data = golf_df)

hnp(fit11, print = TRUE, paint = TRUE)

newdata <- tibble(distance = seq(2, 20, length = 200))
pred <- predict(fit11, newdata = newdata, se.fit = TRUE)

newdata$pred <- pnorm(pred$fit)
newdata$lower <- pnorm(pred$fit - 1.96 * pred$se.fit)
newdata$upper <- pnorm(pred$fit + 1.96 * pred$se.fit)

panel_a <- newdata %>%
  ggplot(aes(x = distance, y = pred)) +
  theme_bw() +
  geom_point(data = golf_df,
             aes(y = success/attempts)) +
  geom_line(col = 4) + 
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = 4,
              alpha = .35) +
  ggtitle("(a) probit link")


fit11 <- glm(cbind(success, attempts - success) ~ poly(distance, 2),
             family = binomial(link = "logit"),
             data = golf_df)

hnp(fit11, print = TRUE, paint = TRUE)

newdata <- tibble(distance = seq(2, 20, length = 200))
pred <- predict(fit11, newdata = newdata, se.fit = TRUE)

newdata$pred <- plogis(pred$fit)
newdata$lower <- plogis(pred$fit - 1.96 * pred$se.fit)
newdata$upper <- plogis(pred$fit + 1.96 * pred$se.fit)

panel_b <- newdata %>%
  ggplot(aes(x = distance, y = pred)) +
  theme_bw() +
  geom_point(data = golf_df,
             aes(y = success/attempts)) +
  geom_line(col = 4) + 
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = 4,
              alpha = .35) +
  ggtitle("(b) logit link")


fit11 <- glm(cbind(success, attempts - success) ~ poly(distance, 2),
             family = binomial(link = "cloglog"),
             data = golf_df)

hnp(fit11, print = TRUE, paint = TRUE)

newdata <- tibble(distance = seq(2, 20, length = 200))
pred <- predict(fit11, newdata = newdata, se.fit = TRUE)

newdata$pred <- 1 - exp(- exp(pred$fit))
newdata$lower <- 1 - exp(- exp(pred$fit - 1.96 * pred$se.fit))
newdata$upper <- 1 - exp(- exp(pred$fit + 1.96 * pred$se.fit))

panel_c <- newdata %>%
  ggplot(aes(x = distance, y = pred)) +
  theme_bw() +
  geom_point(data = golf_df,
             aes(y = success/attempts)) +
  geom_line(col = 4) + 
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = 4,
              alpha = .35) +
  ggtitle("(c) cloglog link")

# install.packages("ggpubr")
library(ggpubr)

ggarrange(panel_a, panel_b, panel_c, ncol = 3)

png("figure1.png", res = 800, units = "in", w = 14, h = 4)
ggarrange(panel_a, panel_b, panel_c, ncol = 3)
dev.off()

png("figure2.png", res = 800, units = "in", w = 6, h = 4)
print(panel_c)
dev.off()