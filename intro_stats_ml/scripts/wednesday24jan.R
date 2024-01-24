library(tidyverse)

# Principal components analysis -------------------------------------------

## Example: USArrests data
USArrests
plot(USArrests)
cor(USArrests) %>% round(2)

## check variances for each variable (i.e. need for scaling)
apply(USArrests, 2, var)
## good idea to scale, variance of the 'Assault' variable
## is much higher than the other variables

p <- prcomp(USArrests, scale = TRUE)
p
summary(p)

## screeplot
source("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/scripts/screeplot.R")
screeplot(p)

## biplot of PC1 and PC2
biplot(p, cex = .5, cex.axis = .5, scale = 0)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

install.packages("remotes")
remotes::install_github("vqv/ggbiplot")

library(ggbiplot)
biplot(p)

ggbiplot(p,
         labels = state.abb,
         groups = state.region,
         ellipse = TRUE) +
  theme_bw()

# Supervised learning - Classification ------------------------------------

## Logistic regression
library(ISLR)
data(Default)

Default %>%
  ggplot(aes(x = balance, y = default)) +
  theme_bw() +
  geom_point()

Default <- Default %>%
  mutate(default01 = (Default$default %>% as.numeric) - 1)

Default %>%
  ggplot(aes(x = balance, y = default01)) +
  theme_bw() +
  geom_jitter(width = 0, height = .1, size = .2,
              alpha = .2)

## fit the logistic regression model / "train the ML algorithm"
fit1 <- glm(default ~ balance,
            family = binomial,
            data = Default)
coef(fit1) ## beta0 and beta1

## predicting the probability of defaulting if a client
## has a credit card balance of 1000
predict(fit1, data.frame(balance = 1000),
        type = "response") ## to give us a prediction in the
                           ## probability scale

## what about a balance of 2000?
predict(fit1, data.frame(balance = 2000), type = "response")

Default %>%
  ggplot(aes(x = balance, y = default01)) +
  theme_bw() +
  geom_jitter(width = 0, height = .1, size = .2,
              alpha = .2) +
  geom_smooth(method = "glm", method.args = list(family = binomial),
              se = FALSE)

## one categorical predictor
fit2 <- glm(default ~ student,
            family = binomial,
            data = Default)
coef(fit2)
exp(coef(fit2)[2]) ## students have a 50% higher odds of defaulting

## two continuous predictors
fit3 <- glm(default ~ balance + income,
            family = binomial,
            data = Default)
coef(fit3) %>% round(6)

## creating a grid for prediction
grid <- expand.grid(balance = seq(0, 2700, length = 300),
                    income = seq(700, 75000, length = 300))
grid$prob <- predict(fit3, grid, type = "response")
grid$pred <- factor(ifelse(grid$prob < .5, "No", "Yes"))

grid %>%
  ggplot(aes(x = balance, y = income, fill = pred)) +
  theme_bw() +
  geom_raster()

grid %>%
  ggplot(aes(x = balance, y = income, fill = prob)) +
  theme_bw() +
  geom_raster() +
  scale_fill_distiller(type = "seq", palette = 5, direction = 0)

## now we must evaluate predictive performance

## Confusion matrices
fit4 <- glm(default ~ balance + income + student,
            family = binomial,
            data = Default)

prob <- predict(fit4, type = "response")
pred <- ifelse(prob < .5, "No", "Yes")

table(Default$default, pred)

(9627 + 105)/10000 ## accuracy (% predicted correctly overall)
(40 + 228)/10000 ## overall misclassification rate (1 - accuracy)

## ROC curves
source("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/scripts/calcERR.R")
e <-calcERR(prob, Default$default,
            thresh = seq(0, 1, length = 50))

plot(e$type2, 1 - e$type1, type = "o")
abline(0, 1, lty = 2)

auroc <- approxfun(x = e$type2, y = 1 - e$type1)

## approximating using trapezoidal rule
x_grid <- seq(0, 1, length = 500)
y_grid <- auroc(x_grid)
sum((y_grid[-1] + y_grid[-length(y_grid)]) * diff(x_grid)[1] / 2)

## approximating using adaptive quadrature
integrate(auroc, lower = 0, upper = 1)

# K-nearest neighbours ----------------------------------------------------

library(class)

xdata <- scale(Default[,3:4])

pred <- knn(xdata, xdata, Default$default, k = 3)

grid <- expand.grid(
  balance = seq(0, 2700, length = 400),
  income = seq(700, 75000, length = 400)
)
means <- attr(xdata, "scaled:center")
sds <- attr(xdata, "scaled:scale")
grids <- scale(grid, center = means, scale = sds)
grid$pred <- knn(xdata, grids, Default[,1], k = 30)

grid %>%
  ggplot(aes(x = balance, y = income, fill = pred)) +
  geom_raster()

training_set <- Default[1:8000,3:4]
test_set <- Default[8001:10000,3:4]

knn_5 <- knn(train = training_set, test = test_set,
             cl = Default[1:8000,1], k = 5)
knn_10 <- knn(train = training_set, test = test_set,
              cl = Default[1:8000,1], k = 10)

table(Default[8001:10000,1], knn_5)
table(Default[8001:10000,1], knn_10)
## out-of-sample predictive performance
## validation / using a test/validation set
## 80:20 split for training:test data

## Cross-validation
library(caret)

fit_control <- trainControl(method = "cv",
                            number = 10,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            savePredictions = TRUE)

set.seed(20)
fit_knn <- train(default ~ balance + income,
                 data = Default,
                 method = "knn",
                 tuneGrid = data.frame(k = c(1,3,10,30,60)),
                 trControl = fit_control,
                 metric = "ROC")
fit_knn

fit_logistic <- train(default ~ balance + income,
                      data = Default,
                      method = "bayesglm",
                      family = "binomial",
                      trControl = fit_control,
                      metric = "ROC")
fit_logistic

## Getting a ROC curve from a caret model fit
library(MLeval)
eval_knn <- evalm(fit_knn, plots = "r")
eval_logistic <- evalm(fit_logistic, plots = "r")

library(pROC)
selected_indices <- fit_knn$pred$k == 3
plot.roc(fit_knn$pred$obs[selected_indices],
         fit_knn$pred$Yes[selected_indices])
plot.roc(fit_logistic$pred$obs,
         fit_logistic$pred$Yes)