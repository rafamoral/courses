## loading required packages
library(tidyverse)
library(lme4)
library(gamlss)
library(gamlss.add)
library(mgcv)
library(gratia)
library(hnp)

## loading datasets
ecrf <- read_table("eCRF.txt")
facs <- read_table("facs_counts_renamed.txt")
nanostring <- read_table("nanostring.txt")

cell_expression <- full_join(ecrf, facs)
gene_expression <- full_join(ecrf, nanostring)

## Gene expression data
## How IFNG expression changes when infected with influenza (vs. control)
## Topics: multiple linear regression
##         distributional regression
##         diagnostic checking
##         model selection
##         regression trees in the linear predictor
##         generalized additive models

IFNG_expression <- gene_expression %>%
  dplyr::select(1:41, IFNG) %>%
  na.omit %>%
  pivot_wider(names_from = Stimulus.Name,
              values_from = IFNG) %>%
  mutate(expression = Influenza - Null) %>%
  dplyr::select(1:40, expression) %>%
  dplyr::select(- SUBJID)

fmla_rhs <- paste("~", paste(names(IFNG_expression)[1:39], collapse = "+"))

fit <- lm(expression ~ .,
          data = IFNG_expression)
summary(fit)
anova(fit, test = "F")
drop1(fit, test = "F")
plot(fit, which = 1)
plot(fit, which = 2)
hnp(fit, paint = TRUE, print = TRUE, half = FALSE)

## jointly modelling mean and variance
fit1 <- gamlss(expression ~ .,
               family = NO,
               data = IFNG_expression)
logLik(fit)
logLik(fit1)
wp(fit1)

fit2 <- gamlss(expression ~ .,
               sigma.formula = ~ .,
               family = NO,
               data = IFNG_expression)

wp(fit2)
summary(fit2)
drop1(fit2, what = "mu")
drop1(fit2, what = "sigma")

fit2b <- stepGAICAll.A(fit2)

AIC(fit1)
AIC(fit2)
AIC(fit2b)
summary(fit2b)
## adding a regression tree to the linear predictor for the mean
fit3 <- gamlss(expression ~ nn(~ Age + OwnsHouse + Sex + BornInCity +
                                 CMVPositiveSerology + RecentPersonalCrisis +
                                 Smoking + HadChickenPox + VaccineMMR +
                                 VaccineFlu + DepressionScore + HeartRate +
                                 Temperature + HourOfSampling + DayOfSampling),
               sigma.formula = ~ Age + PhysicalActivity + LowAppetite +
                 Education + DustExposure + HadAppendicectomy + VaccineTyphoid +
                 VaccineWhoopingCough + DayOfSampling,
               family = NO,
               data = IFNG_expression)
wp(fit3)
plot(fit3)
plot(getSmo(fit3))
text(getSmo(fit3), cex = .9)

## modelling continuous predictors using thin plate splines
fit4 <- gam(expression ~ s(Age) + OwnsHouse + Sex + LivesWithKids +
              BornInCity + CMVPositiveSerology + Listless +
              RecentPersonalCrisis + Smoking + HadChickenPox +  
              VaccineMMR + VaccineFlu + s(DepressionScore) + s(HeartRate) +  
              s(Temperature) + s(HourOfSampling) + s(DayOfSampling),
            data = IFNG_expression)
summary(fit4)
gam.check(fit4)

draw(fit4)
appraise(fit4, method = "simulate")
worm_plot(fit4, method = "simulate")

## Cell expression
## How covariates affect N_CD16 frequencies
## Topics: non-normal GLMs
##         distributional regression
##         regression trees

cell_expression %>%
  ggplot(aes(y = N_CD16hi.panel4, x = Age)) +
  theme_bw() +
  geom_point()

NCD16 <- full_join(ecrf, facs %>% dplyr::select(N_CD16hi.panel4, SUBJID)) %>%
  dplyr::select(- SUBJID)

fmla_rhs <- paste("~", paste(names(NCD16)[-40], collapse = "+"))

fit5 <- gamlss(N_CD16hi.panel4 ~ .,
               family = NO,
               data = na.omit(NCD16))
wp(fit5)

fit6 <- gamlss(N_CD16hi.panel4 ~ .,
               sigma.formula = ~ .,
               family = NO,
               data = na.omit(NCD16))
wp(fit6)

fit7 <- gamlss(N_CD16hi.panel4 ~ .,
               family = NBI,
               data = na.omit(NCD16))
wp(fit7)

fit8 <- gamlss(N_CD16hi.panel4 ~ .,
              sigma.formula = ~ .,
              family = NBI,
              data = na.omit(NCD16))
wp(fit8)
summary(fit8)
plot(fit8)

fit9 <- gamlss(N_CD16hi.panel4 ~ Sex + OwnsHouse + PhysicalActivity + 
                 LivesWithPartner + LivesWithKids + BornInCity +
                 CMVPositiveSerology + Smoking + UsesCannabis +
                 RecentPersonalCrisis + Education +
                 DustExposure + Income + HadMeasles +
                 HadChickenPox + HadMumps + HadTonsillectomy +
                 HadAppendicectomy + VaccineHepA +
                 VaccineMMR + VaccineTyphoid + VaccineWhoopingCough +
                 VaccineYellowFever + VaccineHepB + VaccineFlu +
                 tr(~ Age + PhysicalActivity + BMI +
                      MetabolicScore + LowAppetite +
                      TroubleConcentrating + HoursOfSleep +
                      Listless + DepressionScore + HeartRate +
                      Temperature + HourOfSampling + DayOfSampling),
               sigma.formula = ~ .,
               family = NBI,
               data = na.omit(NCD16))
wp(fit9)
plot(fit9)
plot(getSmo(fit9))
text(getSmo(fit9), cex = .9)
summary(fit9)

y <- na.omit(NCD16)$N_CD16hi.panel4
y_hat_tree <- exp(predict(fit9))
y_hat_linear <- exp(predict(fit8))

sqrt(sum((y - y_hat_tree)^2))
sqrt(sum((y - y_hat_linear)^2))

cor(y, y_hat_tree)
cor(y, y_hat_linear)

plot(y, y_hat_tree, asp = 1); abline(0, 1, lty = 2)
plot(y, y_hat_linear, asp = 1); abline(0, 1, lty = 2)

## Gene expression data
## Studying multiple genes
## Topics: mixed models
##         group-level predictors

gene_expression_long <- gene_expression %>%
  na.omit %>%
  pivot_longer(42:601,
               names_to = "gene",
               values_to = "expression") %>%
  mutate(SUBJID = factor(SUBJID))

## a subset of 50 genes
gene_expression_subset <- gene_expression %>%
  na.omit %>%
  pivot_longer(42:91,
               names_to = "gene",
               values_to = "expression") %>%
  mutate(SUBJID = factor(SUBJID))

## a single model of all genes
fit10 <- lm(expression ~ 0 + gene,
            data = gene_expression_subset)
round(coef(fit10), 2)

## multilevel version
fit11 <- lmer(expression ~ 1 + (1 | gene),
              data = gene_expression_subset)
summary(fit11)

fixef(fit11)
VarCorr(fit11)$gene[1]

## intraclass correlation coefficient
VarCorr(fit11)$gene[1] / (VarCorr(fit11)$gene[1] + sigma(fit11)^2)

## shrinkage
comp <- tibble("gene" = 1:50,
               "flat" = coef(fit10),
               "mixed" = fixef(fit11) + ranef(fit11)$gene$`(Intercept)`)

comp %>%
  pivot_longer(2:3,
               names_to = "model type",
               values_to = "estimate") %>%
  ggplot(aes(x = `model type`, y = estimate, group = gene)) +
  theme_bw() +
  geom_point() +
  geom_line()

## shrinkage when all variation is due to random noise
beta0 <- 5
sigma2 <- 4

set.seed(2023)
simulated_df <- tibble(y = rnorm(100, mean = beta0, sd = sqrt(sigma2)),
                       gene = gl(20, 5))

fit_sim <- lmer(y ~ 1 + (1 | gene),
                data = simulated_df)
summary(fit_sim)
ranef(fit_sim)
.02 / 3.94 ## intraclass correlation coefficient

comp <- tibble("gene" = unique(simulated_df$gene),
               "flat" = coef(lm(y ~ 0 + gene, data = simulated_df)),
               "mixed" = coef(fit_sim)$gene$`(Intercept)`)

comp %>%
  pivot_longer(2:3,
               names_to = "model type",
               values_to = "mu") %>%
  ggplot(aes(x = `model type`, y = mu, group = gene)) +
  theme_bw() +
  geom_point() +
  geom_line(alpha = .3) +
  ylab(expression(mu))

## using all genes
fit12 <- lmer(expression ~ 1 + (1 | gene),
              data = gene_expression_long)
summary(fit12)

## including more covariates
fit13 <- lmer(expression ~ Stimulus.Name + (1 | gene),
             data = gene_expression_long)
summary(fit13)

AIC(fit12)
AIC(fit13)

drop1(fit13, test = "Chisq")

fit13_ranef <- tibble(gene = rownames(ranef(fit13)$gene),
                      eblup = ranef(fit13)$gene)
fit13_ranef %>%
  arrange(desc(eblup))
fit13_ranef %>%
  arrange(eblup)

## this model takes 2-5 min to fit
fit14 <- lmer(expression ~ Stimulus.Name +
                (0 + dummy(Stimulus.Name, "Null") | gene) +
                (0 + dummy(Stimulus.Name, "BCG") | gene) +
                (0 + dummy(Stimulus.Name, "C.albicans") | gene) +
                (0 + dummy(Stimulus.Name, "E.coli") | gene) +
                (0 + dummy(Stimulus.Name, "Influenza") | gene) +
                (0 + dummy(Stimulus.Name, "S.aureus") | gene) +
                (0 + dummy(Stimulus.Name, "SEB") | gene),
              data = gene_expression_long)
## alternatively...
## https://www.dropbox.com/scl/fi/o9u361r52w58is2m8qzs6/fit14.RData?rlkey=tcbjtxdmszrbk69zkphyvkm5i&dl=1
## load("../../../fit14.RData")

summary(fit14)

AIC(fit13)
AIC(fit14)

fit14_ranef <- tibble(gene = rownames(ranef(fit14)$gene),
                      Control = ranef(fit14)$gene[,1],
                      BCG = ranef(fit14)$gene[,2],
                      C.albicans = ranef(fit14)$gene[,3],
                      E.coli = ranef(fit14)$gene[,4],
                      Influenza = ranef(fit14)$gene[,5],
                      S.aureus = ranef(fit14)$gene[,6],
                      SEB = ranef(fit14)$gene[,7])
fit14_ranef %>%
  arrange(desc(Control))
fit14_ranef %>%
  arrange(desc(Influenza))

fit14_ranef %>%
  sample_n(10) %>%
  pivot_longer(2:8,
               names_to = "challenge",
               values_to = "expression") %>%
  ggplot(aes(x = reorder(gene, - expression), y = expression)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  facet_wrap(~ challenge) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

ranef_plot <- fit14_ranef %>%
  pivot_longer(2:8,
               names_to = "challenge",
               values_to = "expression") %>%
  mutate(sign = factor(sign(expression))) %>%
  ggplot(aes(x = reorder(gene, - expression), y = expression, fill = sign)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  facet_wrap(~ challenge, ncol = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        legend.position = "none")

ggsave(filename = "ranef_plot.png",
       plot = ranef_plot,
       width = 32, height = 30)
