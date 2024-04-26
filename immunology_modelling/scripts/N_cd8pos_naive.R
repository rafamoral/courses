## loading required packages
library(tidyverse)
library(lme4)
library(gamlss)
library(caret)
library(Boruta)

## loading datasets
ecrf <- read_table("eCRF.txt")
facs <- read_table("facs_counts_renamed.txt")

cell_expression <- full_join(ecrf, facs)

## cd8pos_naivecytes

N_cd8pos_naive <- full_join(ecrf, facs %>% dplyr::select(N_CD8bpos_naive.panel1, SUBJID)) %>%
  dplyr::select(- SUBJID)

fmla_rhs <- paste("~", paste(names(N_cd8pos_naive)[-40], collapse = "+"))

fit <- gamlss(N_CD8bpos_naive.panel1 ~ .,
              sigma.formula = ~ .,
              family = NBI,
              data = na.omit(N_cd8pos_naive))
wp(fit)
summary(fit)

## 5-fold cross-validation
fold <- gl(5, 156)
rmse <- numeric(5)

for(i in 1:5) {
  training_data <- na.omit(N_cd8pos_naive)[- which(fold == i),]
  fit_fold <- gamlss(N_CD8bpos_naive.panel1 ~ .,
                     sigma.formula = ~ .,
                     family = NBI,
                     data = training_data)
  y <- training_data$N_CD8bpos_naive.panel1
  y_hat <- predict(fit_fold, type = "response")
  rmse[i] <- sqrt(sum((y - y_hat)^2))
}

rmse_gamlss <- mean(rmse)

## random forests

fit_control <- trainControl(method = "cv",
                            number = 5)

set.seed(2024)
rf_fit <- train(N_CD8bpos_naive.panel1 ~ .,
                data = na.omit(N_cd8pos_naive),
                method = "rf",
                tuneGrid = data.frame(mtry = c(2,5,10,15,20)),
                trControl = fit_control,
                metric = "RMSE")

rf_fit
varImp(rf_fit)

## comparing predictions on training set
y <- na.omit(N_cd8pos_naive)$N_CD8bpos_naive.panel1
y_hat_nb <- predict(fit, type = "response")
y_hat_rf <- predict(rf_fit, na.omit(N_cd8pos_naive))

cor(y, y_hat_nb)
cor(y, y_hat_rf)

par(mfrow = c(1,2))
plot(y, y_hat_nb, main = "NB"); abline(0, 1, lty = 2)
plot(y, y_hat_rf, main = "RF"); abline(0, 1, lty = 2)
