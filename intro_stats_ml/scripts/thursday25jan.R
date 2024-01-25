library(tidyverse)

# Regression --------------------------------------------------------------

## Body fat dataset
bfat <- read.table("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/data/bodyfat.txt",
                   header = TRUE)
head(bfat)

pairs(bfat)
source("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/scripts/opairs.R")
opairs(bfat)

fit <- lm(bfat ~ ., data = bfat)
coef(fit)

y_hat <- fitted(fit)
y <- bfat$bfat

y - y_hat
residuals(fit)

mse_fit <- mean(residuals(fit)^2)

fit2 <- lm(bfat ~ abdomen, data = bfat)
mse_fit2 <- mean(residuals(fit2)^2)

system.time(lm(bfat ~ ., data = bfat))

library(leaps)
allfits <- regsubsets(bfat ~ ., data = bfat)
summary(allfits)
plot(allfits, scale = "r2", col = "blue")

sum_allfits <- summary(allfits)
plot(1:7, sum_allfits$cp, type = "o")
plot(1:7, 1 - sum_allfits$rsq, type = "o")
plot(1:7, sum_allfits$bic, type = "o")

## Shrinkage
library(glmnet)

## creating matrix of predictors
X <- bfat %>%
  select(- bfat) %>%
  as.matrix
y <- bfat$bfat

ridge_fit <- glmnet(X, y, alpha = 0) ## alpha = 0 for ridge
plot(ridge_fit, xvar = "lambda", label = TRUE)

cv_ridge <- cv.glmnet(X, y, alpha = 0)
plot(cv_ridge)
cv_ridge$lambda.min
cv_ridge$lambda.1se

cbind(coef(fit),
      coef(ridge_fit, s = cv_ridge$lambda.min),
      coef(ridge_fit, s = cv_ridge$lambda.1se))

lasso_fit <- glmnet(X, y, alpha = 1) ## alpha = 1 for lasso
plot(lasso_fit, xvar = "lambda", label = TRUE)

cv_lasso <- cv.glmnet(X, y, alpha = 1)
plot(cv_lasso)
cv_lasso$lambda.min
cv_lasso$lambda.1se

cbind(coef(fit),
      coef(lasso_fit, s = cv_lasso$lambda.min),
      coef(lasso_fit, s = cv_lasso$lambda.1se))

# Smoothing ---------------------------------------------------------------

## loading some fake data
fake_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/data/fake_data.csv")

cor(fake_df)

fake_df %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

fit <- lm(y ~ poly(x, 4), data = fake_df)

fake_df %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = fitted(fit)),
            col = 2)

fit3 <- lm(y ~ poly(x, 3), data = fake_df)
fit4 <- lm(y ~ poly(x, 4), data = fake_df)

anova(fit3, fit4)

fit15 <- lm(y ~ poly(x, 15), data = fake_df)
anova(fit4, fit15)

mean(residuals(fit3)^2)
mean(residuals(fit4)^2)
mean(residuals(fit15)^2)

## Splines
library(splines)

f1 <- lm(y ~ bs(x, df = 6), data = fake_df)

fake_df %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = fitted(f1)),
            col = 2)

## 5-fold CV to select number of knots
k <- 5
fold <- sample(k, nrow(fake_df), replace = TRUE)
fold_size <- table(fold)

mse <- vector(length = k)
knots <- 1:5
cv <- vector(length = length(knots))

for(j in 1:length(knots)) {
  for (i in 1:k) {
    fold_i <- fake_df[fold == i,]
    fold_other <- fake_df[fold != i,]
    f <- lm(y ~ bs(x, df = knots[j] + 4), data = fold_other) 
    pred <- predict(f, fold_i)
    mse[i] <- mean((pred - fold_i$y)^2, na.rm = TRUE)
  }
  cv[j] <- weighted.mean(mse, fold_size)
  # weighted as each fold not the same size.
}

plot(knots, cv, ty = "o")
knots[which.min(cv)]

## Smoothing splines
f2 <- smooth.spline(fake_df$x, fake_df$y, cv = TRUE)
f2

fake_df %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = fitted(f1)),
            col = 2) +
  geom_line(aes(y = fitted(f2)),
            col = 4)

my_seq <- rev(c(50,30,10,5,2,1,.9,.8,.7,.6,.5,.4,.3,.2,
                .1,.05,.025,.01,.001,.0001,.00001,.000001,
                1e-7,1e-8,1e-9,1e-10))
xpred <- seq(min(fake_df$x), max(fake_df$x), length = 200)

for(i in my_seq) {
  plot(y ~ x, col = "gray50", data = fake_df)
  lines(xpred, predict(smooth.spline(fake_df$x, fake_df$y, lambda = i, cv = TRUE),
                       data.frame(x = xpred))$y$x, col = 4, lwd = 2)
  legend("topleft", paste("lambda =", i))
  legend("topright", paste("PRESS =", round(smooth.spline(fake_df$x, fake_df$y, lambda = i)$cv.crit, 2)))
  Sys.sleep(time = .5)
}

# Generalized Additive Models ---------------------------------------------

## Salary data
sal <- read.table("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/data/salary.txt",
                  header = TRUE)
sal$gender <- factor(ifelse(sal$gender == 1, "F", "M"))

head(sal)

sal %>%
  group_by(gender) %>%
  summarise(m = mean(salary))

fit1 <- lm(log(salary) ~ ., data = sal)
coef(fit1)
exp(confint(fit1, "genderM"))

pairs(sal %>%
        select(- gender))

## fitting a GAM
fit2 <- lm(log(salary) ~ educ + ns(pexp, 4) + ns(time, 4) + gender,
           data = sal)
anova(fit1, fit2)

AIC(fit1)
AIC(fit2)

exp(confint(fit2, "genderM"))

library(mgcv)
fit3 <- gam(log(salary) ~ educ + s(pexp) + s(time) + gender,
            data = sal)
summary(fit3)
plot(fit3)

AIC(fit3)

# Tree-based methods ------------------------------------------------------

library(rpart) # rpart function fits the tree
library(rpart.plot) # rpart.plot function does neat visualisations

## loading the spam data
data(spam, package = "kernlab")
dim(spam)

## loading the Hitters data
data(Hitters, package = "ISLR")

## selecting mtry via cross-validation
library(caret)

fit_control <- trainControl(method = "cv",
                            number = 5,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = twoClassSummary)

## for the spam dataset
set.seed(2024)
fit_rf <- train(type ~ .,
                data = spam,
                method = "rf",
                tuneGrid = data.frame(mtry = c(3,5,7,10,15,20)),
                trControl = fit_control,
                metric = "ROC")

load("cv_rf.RData")
fit_rf

library(MLeval)
rf_roc <- evalm(fit_rf, plots = "r")

## for the Hitters dataset
fit_control <- trainControl(method = "cv",
                            number = 5)

set.seed(2024)
fit_rf_hitters <- train(log(Salary) ~ .,
                        data = na.omit(Hitters),
                        method = "rf",
                        tuneGrid = data.frame(mtry = c(3,6,9,12)),
                        trControl = fit_control,
                        metric = "RMSE")
fit_rf_hitters
plot(varImp(fit_rf_hitters))

# BART --------------------------------------------------------------------

library(dbarts)

Hitters_complete <- na.omit(Hitters)

fit_bart <- bart2(log(Salary) ~ .,
                  keepTrees = TRUE,
                  data = Hitters_complete[-1,])
plot(fit_bart)

pred <- predict(fit_bart, Hitters_complete[1,], type = "ev")

plot(density(exp(pred)))
quantile(exp(pred), probs =  c(.025, .5, .975))

hist(residuals(fit_bart))
shapiro.test(residuals(fit_bart))

qqnorm(residuals(fit_bart))
qqline(residuals(fit_bart))

library(hnp)
hnp(residuals(fit_bart), scale = TRUE, half = FALSE, paint = TRUE)

# GAMLSS ------------------------------------------------------------------

library(gamlss)
library(gamlss.add)

library(randomForest)
set.seed(2024)
varImpPlot(randomForest(log(Salary) ~ ., data = Hitters_complete))

fit_gamlss <- gamlss(log(Salary) ~ tr(~ CAtBat + CRuns + CHits + CRBI + CWalks) +
                       AtBat + Hits + HmRun + Runs + RBI + Walks + Years +
                       League + Division + PutOuts + Assists + Errors + NewLeague,
                     data = Hitters_complete)
wp(fit_gamlss)
plot(fit_gamlss)
plot(getSmo(fit_gamlss))
text(getSmo(fit_gamlss))

fit_gamlss3 <- gamlss(log(Salary) ~ tr(~ CAtBat + CRuns + CHits + CRBI + CWalks) +
                        AtBat + Hits + HmRun + RBI + Walks + Years +
                        League + Division + Assists,
                      sigma.formula = ~ tr(~ CAtBat + CRuns + CHits + CRBI + CWalks) +
                        AtBat + Runs + Walks + Years +
                        League + Division + PutOuts + Assists,
                      data = Hitters_complete,
                      control = gamlss.control(n.cyc = 50))
wp(fit_gamlss3)
plot(fit_gamlss3)
plot(getSmo(fit_gamlss3))
text(getSmo(fit_gamlss3))

AIC(fit_gamlss)
AIC(fit_gamlss3)

set.seed(2024)
k <- 5
fold <- sample(k, nrow(Hitters_complete), replace = TRUE)
fsize <- table(fold)

mse <- matrix(NA, nrow = k, ncol = 4)
colnames(mse) <- c("CART" ,"Random Forests", "BART", "GAMLSS")

for(i in 1:k) {
  foldi <- Hitters_complete[fold == i,]
  foldOther <- Hitters_complete[fold != i,]
  
  fit_cart <- rpart(log(Salary) ~ ., data = foldOther, cp = 0.042)
  pred_cart <- predict(fit_cart, foldi)
  
  fit_rf <- randomForest(log(Salary) ~ ., data = foldOther)
  pred_rf <- predict(fit_rf, foldi)
  
  fit_bart <- bart2(log(Salary) ~ ., data = foldOther, keepTrees = TRUE)
  pred_bart <- colMeans(predict(fit_bart, foldi, type = "ev"))
  
  fit_gamlss <- update(fit_gamlss3, data = foldOther)
  pred_gamlss <- predict(fit_gamlss, newdata = foldi, what = "mu")
  
  mse[i,1] <- mean((pred_cart - log(foldi$Salary))^2)
  mse[i,2] <- mean((pred_rf - log(foldi$Salary))^2)
  mse[i,3] <- mean((pred_bart - log(foldi$Salary))^2)
  mse[i,4] <- mean((pred_gamlss - log(foldi$Salary))^2)
}

mse

cv <- apply(mse, 2, function(x) weighted.mean(x, fsize))
cv

# Boruta ------------------------------------------------------------------

library(Boruta)
boruta_fit <- Boruta(Salary ~ ., data = Hitters_complete)
table(boruta_fit$finalDecision)
boruta_fit$finalDecision[boruta_fit$finalDecision == "Confirmed"]