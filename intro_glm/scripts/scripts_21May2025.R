library(tidyverse)

# Multinomial GLMs --------------------------------------------------------

## load the elephant grass grazing dataset
grazing_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_glm/data/grazing.csv")
grazing_df

## using package nnet
## other packages can fit multinomial models, e.g. VGAM
## ordinal models can be fitted using package MASS, function polr

library(nnet)

fit1 <- multinom(resp ~ months,
                 data = grazing_df)
summary(fit1)

## logit 2: log(pi2/pi1) =  0.55 - 0.015 * month
## logit 3: log(pi3/pi1) = -1.22 - 0.047 * month

## take month 3 as an example, let's calculate pi1, pi2, pi3
den <- 1 + exp(0.5475287 - 0.01535834 * 3) + exp(-1.2238692 - 0.04701409 * 3)
pi1 <- 1 / den
pi2 <- pi1 * exp(0.5475287 - 0.01535834 * 3)
pi3 <- pi1 * exp(-1.2238692 - 0.04701409 * 3)
pi1 + pi2 + pi3

predict(fit1, newdata = data.frame(months = 3), type = "prob")
c(pi1, pi2, pi3)

obs_month3 <- grazing_df %>%
  filter(months == 3) %>%
  pull(resp) %>%
  table

obs_month3 / sum(obs_month3)

## generating fitted curves over time
library(modelr)

newdata <- tibble(months = seq(3, 16, length = 200))
pred_data <- bind_cols(newdata,
                       add_predictions(newdata, model = fit1, type = "prob")$pred)

wrong_format <- add_predictions(newdata, model = fit1, type = "prob")

pred_data %>%
  pivot_longer(2:4,
               names_to = "category",
               values_to = "pred") %>%
  ggplot(aes(x = months, y = pred, color = category)) +
  theme_bw() +
  geom_line() +
  ylim(0, 1)

## does not work!
wrong_format %>%
  pivot_longer(2:4,
               names_to = "category",
               values_to = "pred") %>%
  ggplot(aes(x = months, y = pred, color = category)) +
  theme_bw() +
  geom_line() +
  ylim(0, 1)

## let's do a hypothesis test -- is the "months" predictor significant?
fit2 <- multinom(resp ~ 1,
                 data = grazing_df)
anova(fit1, fit2)


# Poisson GLMs ------------------------------------------------------------

## binomial approximation
dbinom(10, size = 10000, prob = 0.00075)
dpois(10, lambda = 10000 * 0.00075)

## properties: equi-dispersion
y <- rpois(1e6, lambda = 10)
mean(y)
var(y)

## if Var > mean = overdispersion
## if Var < mean = underdispersion

## example: number of GP visits in the past year
doctor_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_glm/data/doctor_visits.csv")
doctor_df$sex <- factor(doctor_df$sex, labels = c("male","female"))
doctor_df$age <- doctor_df$age * 100

## exploratory plot
doctor_df %>%
  ggplot(aes(x = age, y = gp_visits, color = sex)) +
  theme_bw() +
  geom_jitter(height = .2, width = .5, alpha = .2) +
  geom_smooth(se = FALSE)

## let's fit some Poisson models to this dataset
fit3 <- glm(gp_visits ~ sex + age,
            family = poisson,
            data = doctor_df)
summary(fit3)
estimates <- coef(fit3)

## what is the rate of GP visits (lambda / mean) of the Poisson distribution
## for a female aged 50?
log_rate_female_50 <- estimates[1] + estimates[2] + estimates[3] * 50
rate_female_50 <- exp(log_rate_female_50)

## draw samples from the Poisson distribution with a rate of 0.3658379
table(rpois(1000, lambda = rate_female_50))
floor(dpois(0:5, lambda = rate_female_50) * 1000)
round(dpois(0:5, lambda = rate_female_50), 4)

## what is the rate of GP visits (lambda / mean) of the Poisson distribution
## for a male aged 50?
log_rate_male_50 <- estimates[1] + estimates[3] * 50
rate_male_50 <- exp(log_rate_male_50)

## draw samples from the Poisson distribution with a rate of 0.2825715
table(rpois(1000, lambda = rate_male_50))
floor(dpois(0:5, lambda = rate_male_50) * 1000)
round(dpois(0:5, lambda = rate_male_50), 4)

rbind(floor(dpois(0:5, lambda = rate_female_50) * 1000),
      floor(dpois(0:5, lambda = rate_male_50) * 1000))

log_rate_female_50 - log_rate_male_50
estimates

c(rate_female_50, rate_male_50)
rate_female_50 / rate_male_50

exp(estimates[2])
exp(estimates[3])

## 95% CIs
exp(confint.default(fit3, parm = "sexfemale"))
## with 95% confidence, the rate of gp visits increases, on average, from
## 16% to 44% for females when compared to males, for a fixed age

## let's plot the fitted curves
newdata <- expand.grid(age = seq(19, 72, length = 200),
                       sex = levels(doctor_df$sex))

my_plot <- newdata %>%
  add_predictions(model = fit3, type = "response", var = "gp_visits") %>%
  ggplot(aes(x = age, y = gp_visits, color = sex)) +
  theme_bw() +
  geom_jitter(data = doctor_df,
              height = .2, width = .5, alpha = .2) +
  geom_line() +
  ylab("Number of GP visits") +
  xlab("Age") +
  scale_y_continuous(breaks = seq(0, 10, 2))

ggsave(filename = "gp_visits.png", plot = my_plot, dpi = 800,
       width = 7, height = 5)

png(filename = "gp_visits2.png", width = 7, height = 5, res = 800, units = "in")
print(my_plot)
dev.off()

## model comparison
fit4 <- glm(gp_visits ~ sex + age + insurance,
            family = poisson,
            data = doctor_df)
deviance(fit3)
deviance(fit4)

anova(fit3, fit4, test = "Chisq")

drop1(fit4, test = "Chisq")

summary(fit4)

## verifying equidispersion assumption
doctor_df %>%
  mutate(age_cat = cut(age, breaks = c(0,40,60,80))) %>%
  group_by(age_cat, sex, insurance) %>%
  summarise(mean = mean(gp_visits),
            var = var(gp_visits),
            n = n()) %>%
  ggplot(aes(x = mean, y = var, size = n)) +
  theme_bw() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2)

## loading the insurance dataset (example of the use of offset variables)
insurance_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_glm/data/insurance.csv")
insurance_df$District <- as.factor(insurance_df$District)
insurance_df$Age <- factor(insurance_df$Age,
                           levels = c("<25","25-29","30-35",">35"))

## exploratory plots
insurance_df %>%
  ggplot(aes(x = Holders, y = Claims)) +
  theme_bw() +
  geom_point()

insurance_df %>%
  ggplot(aes(x = Age, y = Claims)) +
  theme_bw() +
  geom_boxplot()

insurance_df %>%
  ggplot(aes(x = Age, y = Claims / Holders)) +
  theme_bw() +
  geom_boxplot()

## let's fit the Poisson model ignoring the offset variable
fit5 <- glm(Claims ~ Age,
            family = poisson,
            data = insurance_df)
summary(fit5)

exp(2.19916) ## according to the misspecified model, people older than 35
             ## make 9 times more claims than people younger than 25

## now let's accommodate the different exposures, with the offset term
fit6 <- glm(Claims ~ Age + offset(log(Holders)),
            family = poisson,
            data = insurance_df)
summary(fit6)

exp(-0.49758) ## this model estimates a 40% reduction in claims
              ## for people older than 35 compared to the youngest stratum

newdata <- tibble(Age = unique(insurance_df$Age),
                  Holders = 1000)

add_predictions(data = newdata, model = fit6, type = "response")
add_predictions(data = newdata, model = fit5, type = "response")

anova(fit5, fit6, test = "Chisq") ## can't compare using LRT given both models have the same number of parameters

AIC(fit5)
AIC(fit6)

## checking goodness-of-fit based on residual deviances
deviance(fit5)
df.residual(fit5)

curve(dchisq(x, df = df.residual(fit5)), xlim = c(0,200))

## we will work with the progeny data for this example
library(hnp)
data("progeny")
?progeny

progeny %>%
  ggplot(aes(x = extract, y = y)) +
  theme_bw() +
  geom_boxplot()

fit7 <- glm(y ~ extract,
            family = poisson,
            data = progeny)
summary(fit7)

curve(dchisq(x, df = 36), xlim = c(0,100))
abline(v = deviance(fit7), lty = 2)

qchisq(.95, df = 36) ## critical chisq value
abline(v = qchisq(.95, df = 36), lty = 2, col = 2)

pchisq(deviance(fit6), df = 36, lower.tail = FALSE) ## evidence of lack-of-fit
## evidence that the Poisson model is not adequate
## evidence that the observed data is not a plausible realisation of the fitted Poisson model

progeny %>%
  group_by(extract) %>%
  summarise(mean = mean(y),
            var = var(y)) %>%
  mutate(var_over_mean = var / mean)
## clear display of overdispersion (i.e. variance is larger than the mean)
## in conclusion: the Poisson model is inadequate and should not be relied on
##                for inferential purposes

## Q-Q plots
y <- rnorm(100)

qqnorm(y)
qqline(y)

set.seed(2025)
hnp(y, half = FALSE, print = TRUE, paint = TRUE)
hnp(y, print = TRUE, paint = TRUE) ## half-normal version

## let's produce the hnp for our fitted model
hnp(fit7)
hnp(fit7, paint = TRUE, print = TRUE)

## for Bernoulli or Multinoulli data, there is no adequacy check available
## due to the nature of the random variable
## for all other distributions, you can use hnp (as long as you can
## simulate new samples from the distribution = parametric bootstrapping)

anova(fit7, test = "Chisq")

## let's do an ANOVA
fit8 <- lm(log(y+.5) ~ extract,
           data = progeny)
anova(fit8)

hnp(fit8)

shapiro.test(rstudent(fit8))