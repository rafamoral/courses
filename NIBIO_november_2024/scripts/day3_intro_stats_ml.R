library(tidyverse)

# Introduction: Example datasets ------------------------------------------

## Wage data
wage_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml2/data/wage.csv")

wage_df %>%
  ggplot(aes(x = age, y = wage)) +
  theme_bw() +
  geom_point()

wage_df %>%
  ggplot(aes(x = wage)) +
  theme_bw() +
  geom_histogram()

wage_df %>%
  ggplot(aes(x = education, y = wage)) +
  theme_bw() +
  geom_boxplot()

## Stock market data
stock_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml2/data/stock_market.csv")

stock_df_long <- stock_df %>%
  mutate(id = 1:nrow(stock_df)) %>%
  pivot_longer(cols = c(2:6, 8),
               names_to = "lag",
               values_to = "perc_return") %>%
  mutate(lag = as.factor(lag))

levels(stock_df_long$lag) <- c(-1,-2,-3,-4,-5,0)
stock_df_long$lag <- as.numeric(as.character(stock_df_long$lag))

stock_df_long %>%
  filter(id %in% 1:10) %>%
  ggplot(aes(x = lag, y = perc_return, group = id, col = Direction)) +
  theme_bw() +
  geom_line()

stock_df_long %>%
  ggplot(aes(x = lag, y = perc_return, group = id, col = Direction)) +
  theme_bw() +
  geom_line(alpha = .2)

stock_df %>%
  ggplot(aes(x = Direction, y = Lag5)) +
  theme_bw() +
  geom_boxplot()

stock_df %>%
  ggplot(aes(x = Lag5, y = Direction)) +
  theme_bw() +
  geom_jitter(width = 0, height = .05, size = .2)

## Gene expression data
library(ISLR)
?NCI60

NCI60$labs
image(t(NCI60$data), yaxt = "n", xaxt = "n", xlab = "Gene")
axis(2, at = seq(0, 1, length = 64), labels = NCI60$labs,
     las = 1, cex.axis = .4)
axis(1, at = seq(0, 1, length = 20),
     labels = floor(seq(1, ncol(NCI60$data), length = 20)),
     cex.axis = .6, las = 2)

# Unsupervised Learning ---------------------------------------------------

## Distances
x <- matrix(c(3, 0, -1,
              5, 4, 0,
              3, 1, 2,
              3, 4, 1,
              2, 1, 4),nrow=5)
rownames(x)<- letters[1:5]
colnames(x)<- c("V1", "V2", "V3")
x

D1 <- as.matrix(dist(x, method = "euclidean"))
D1

D2 <- D1 * D1
D2

D3 <- as.matrix(dist(x, method = "manhattan"))
D3

## Correlations

## Pearson correlation
cor(x)

## Spearman correlation
cor(x, method = "spearman")

## Example: EuroStat
educ_df <- read.table("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml2/data/eurostat.txt",
                      row.names = 1, header = TRUE)
educ_df <- educ_df[, -19]
educ_df <- t(educ_df)
head(educ_df)

library(PairViz)
pcp(educ_df, lwd = 2, scale = FALSE, col = 1:18)
axis(2)

stars(educ_df, nrow = 3, col.stars = 1:18)

d <- dist(educ_df, "euclidean")
h <- hclust(d, "single")

d1 <- as.dendrogram(h)
dev.off()
plot(d1)

## for a fancier plot with colors
library(dendextend)

## auto-coloring 4 clusters of branches
d2 <- color_branches(d1, k = 4, col = c(2,3,5,4))
plot(d2)

cutree(h, 4)

pcp(educ_df, lwd = 2, scale = FALSE, col = cutree(h,4) + 1)
axis(2)

stars(educ_df[h$order,], nrow = 3,
      col.stars = cutree(h,4)[h$order] + 1)

## Example: Music data
music <- read.csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml2/data/music.csv", stringsAsFactors = TRUE)
music.feat <- music[, 4:8]
pairs(music.feat, col = music$Artist)

pcp(music.feat, col = music$Type)

d <- dist(music.feat, "euclidean")
h <- hclust(d, "single")
labl.single <- cutree(h,3)
d1 <- as.dendrogram(h)

par(mar = c(10,4,3,2))
d2 <- color_branches(d1, k = 3, col = c(4,3,2))
plot(d2)

h <- hclust(d, "average")
labl.Ave <- cutree(h, 3)
d3 <- as.dendrogram(h)

d4 <- color_branches(d3, k = 3, col = c(4,2,3)) # auto-coloring 4 clusters of branches.
plot(d4)

dl <- dendlist(d2, d4)


tanglegram(dl, sort = TRUE, common_subtrees_color_lines = FALSE, 
           highlight_distinct_edges = FALSE, highlight_branches_lwd = FALSE)

pairs(music.feat, col = labl.Ave + 1)
pcp(music.feat, col = labl.Ave + 1)

## k-means example: NCI60 data

nci_data <- NCI60$data
nci_labs <- NCI60$labs
nci_data[1:4,1:6]
nci_labs[1:6]

nci_data1 <- scale(nci_data)

twss <- numeric(15)
for(k in 1:15) {
  twss[k] <- kmeans(nci_data1, centers = k, nstart = 25)$tot.withinss
}

plot(1:15, twss, type = "b", xlab = "Number of Clusters",
     ylab = "Total within groups sum of squares")

km <- kmeans(nci_data1, 5, nstart = 25)
tab <- table(nci_labs, km$cluster)
tab

par(mar = c(2,8,2,2))
barplot(t(tab), horiz = TRUE, las = 2, col = 2:6)
dev.off()

# Classification ----------------------------------------------------------

## Logistic regression
library(ISLR)
data(Default)
Default

Default %>%
  ggplot(aes(x = balance, y = default)) +
  theme_bw() +
  geom_jitter(width = 0, height = .1, size = .2, alpha = .5)

Default <- Default %>%
  mutate(default01 = default %>% as.factor %>% as.numeric %>% `-`(1))

Default %>%
  ggplot(aes(x = balance, y = default01)) +
  theme_bw() +
  geom_jitter(width = 0, height = .1, size = .2, alpha = .5) +
  geom_smooth(method = glm, method.args = list(family = binomial))

## fitting the logistic regression model
fit1 <- glm(default ~ balance, family = "binomial", data = Default)

## log-odds of defaulting if client has a credit card balance of 1000
predict(fit1, data.frame(balance = 1000))

## probability of defaulting if client has a credit card balance of 1000
predict(fit1, data.frame(balance = 1000), type = "response")

## odds-ratio
exp(coef(fit1)[2])
## for every extra dollar in the credit card balance, the odds of
## defaulting increase bby 0.55%

## one categorical predictor
fit2 <- glm(default ~ student, family = "binomial", data = Default)
summary(fit2)
exp(coef(fit2)[2])

## two continuous predictors
fit3 <- glm(default ~ balance + income,
            family = "binomial",
            data = Default)
summary(fit3)

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
  scale_fill_distiller(type = "seq", palette = 7, direction = 1)

## show how interactions, polynomial and spline effects change the boundary

## Confusion matrices
fit4 <- glm(default ~ balance + income + student,
            family = "binomial",
            data = Default)

prob <- predict(fit4, type = "response")
pred <- factor(ifelse(prob < .5, "No", "Yes"))

table(Default$default, pred)

## ROC curve
source("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml2/scripts/calcERR.R")
e <- calcERR(prob, Default$default,
             thresh = seq(0, 1, length.out = 500))

auroc <- approxfun(x = e$type2, y = 1 - e$type1)

## approximating using trapezoidal rule
x_grid <- seq(0, 1, length = 500)
y_grid <- auroc(x_grid)
sum((y_grid[-1] + y_grid[-length(y_grid)]) * diff(x_grid)[1] / 2)

## approximating using adaptive quadrature
integrate(auroc, lower = 0, upper = 1)

# KNN ---------------------------------------------------------------------

library(class)
head(Default)
xdata <- scale(Default[,3:4])

Default %>%
  ggplot(aes(x = balance, y = income, color = default)) +
  theme_bw() +
  geom_point(alpha = 0.3)

pred <- knn(xdata, xdata, Default[,1], k = 3)

grid <- expand.grid(
    balance = seq(0, 2700, length = 400),
    income = seq(700, 75000, length = 400)
  )
means <- attr(xdata, "scaled:center")
sds <- attr(xdata, "scaled:scale")
grids <- scale(grid, center = means, scale = sds)
grid$pred <- knn(xdata, grids, Default[,1], k = 3)

grid %>%
  ggplot(aes(x = balance, y = income, fill = pred)) +
  geom_raster()

grid$pred <- knn(xdata, grids, Default[,1], k = 30)

grid %>%
  ggplot(aes(x = balance, y = income, fill = pred)) +
  geom_raster()

table(Default$default, pred)

set.seed(2024)
indTrain <- sample(nrow(Default), round(.8 * nrow(Default)))
indTest <- (1:nrow(Default))[-indTrain]

f1 <- glm(default ~ balance + income + student,
          family = "binomial",
          data = Default[indTrain,])
pred1 <- predict(f1, Default[indTest,], type = "response")
pred1 <- factor(ifelse(pred1 < .5, "No", "Yes"))
tab1 <- table(Default$default[indTest], pred1)
tab1

xdata <- scale(Default[indTrain,3:4])
means <- attr(xdata,"scaled:center")
sds <- attr(xdata,"scaled:scale")
xdataTest <- scale(Default[indTest,3:4], center=means, scale=sds)

pred2 <- knn(xdata, xdataTest, Default[indTrain,1], k = 3)
tab2 <- table(Default$default[indTest], pred2)
tab2

get_accuracy <- Vectorize(function(k) {
  pred <- knn(xdata, xdataTest, Default[indTrain,1], k = k)
  tab <- table(Default$default[indTest], pred)
  accuracy <- (tab[1,1] + tab[2,2]) / sum(tab)
  type1 <- tab[2,1] / sum(tab[2,])
  return(data.frame(accuracy = accuracy, type1 = type1))
})

acc <- get_accuracy(1:50)

plot(1:50, acc[1,], xlab = "k", ylab = "accuracy", ty = "l")
plot(1:50, acc[2,], xlab = "k", ylab = "type-1 error", ty = "l")

which.max(acc[1,])
which.min(acc[2,])

## Cross-validation
library(caret)

fit_control <- trainControl(method = "cv",
                            number = 10,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = twoClassSummary)

set.seed(2024)
fit_knn <- train(default ~ balance + income + student,
                 data = Default,
                 method = "knn",
                 tuneGrid = data.frame(k = c(1,3,10,30,60,90,120,150)),
                 trControl = fit_control,
                 metric = "ROC")
fit_knn

set.seed(2024)
fit_logistic <- train(default ~ balance + income + student,
                      data = Default,
                      method = "bayesglm",
                      family = "binomial",
                      trControl = fit_control,
                      metric = "ROC")
fit_logistic

## ROC curve (two ways)
library(MLeval)
eval_knn <- evalm(fit_knn, plots = "r")
eval_logistic <- evalm(fit_logistic, plots = "r")

library(pROC)
selectedIndices <- fit_knn$pred$k == 90
plot.roc(fit_knn$pred$obs[selectedIndices],
         fit_knn$pred$Yes[selectedIndices])
plot.roc(fit_logistic$pred$obs,
         fit_logistic$pred$Yes)

# Tree-based methods ------------------------------------------------------

## Regression trees - Hitters example
data(Hitters)
?Hitters

library(rpart)
library(rpart.plot)

fit_tree <- rpart(log(Salary) ~ ., data = Hitters)
rpart.plot(fit_tree)
plotcp(fit_tree)

fit_tree2 <- rpart(log(Salary) ~ ., data = Hitters, cp = 0.042)
rpart.plot(fit_tree2)

## Classification trees - spam example
data(spam, package = "kernlab")
set.seed(1234)
select <- sample(1:nrow(spam), 1000)
spam_training <- spam[-select,]
spam_validation <- spam[select,]

set.seed(123)
treeSpam <- rpart(type ~ ., data = spam_training, cp = 0)
plotcp(treeSpam)

treeSpam2 <- prune(treeSpam, cp = .0045)
rpart.plot(treeSpam2)

predmat <- predict(treeSpam, newdata = spam_validation)
head(predmat)

pred <- predict(treeSpam, newdata = spam_validation, type = "class")

ctable <- table(pred, spam_validation$type)
ctable

pred09 <- ifelse(predmat[,2] > 0.9, 'spam', 'nonspam')
ctable2 <- table(pred09, spam_validation$type)
ctable2

## Random forests
library(randomForest)
set.seed(2019)
rf_spam <- randomForest(type ~ ., data = spam_training)
plot(rf_spam)

predmat <- predict(rf_spam, newdata=spam_validation)
pred <- predict(rf_spam, newdata=spam_validation, type = "class")

ctable <- table(pred, spam_validation$type)
ctable

varImpPlot(rf_spam)

## selecting mtry via cross-validation
fit_control <- trainControl(method = "cv",
                            number = 5,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = twoClassSummary)

set.seed(2024)
fit_rf <- train(type ~ .,
                data = spam,
                method = "rf",
                tuneGrid = data.frame(mtry = c(3,5,7,10,15,20)),
                trControl = fit_control,
                metric = "ROC")

load("intro_stats_ml2/scripts/cv_rf.RData")
fit_rf
plot(varImp(fit_rf))

selectedIndices <- fit_rf$pred$mtry == 7
plot.roc(fit_rf$pred$obs[selectedIndices],
         fit_rf$pred$spam[selectedIndices])

## for Hitters dataset
fit_control <- trainControl(method = "cv",
                            number = 5)

set.seed(2024)
fit_rf <- train(log(Salary) ~ .,
                data = na.omit(Hitters),
                method = "rf",
                tuneGrid = data.frame(mtry = c(3,5,7,10)),
                trControl = fit_control,
                metric = "RMSE")
fit_rf
plot(varImp(fit_rf))

# BART --------------------------------------------------------------------

library(dbarts)

## Hitters dataset
Hitters_complete <- na.omit(Hitters)

fit_bart <- bart2(log(Salary) ~ .,
                  data = Hitters_complete,
                  keepTrees = TRUE,
                  power = 2,
                  base = .95) # experiment to show more/less penalisation
plot(fit_bart)
dev.off()

fit_bart$fit$plotTree(chainNum = 1, sampleNum = 500, treeNum = 1)
fit_bart$fit$plotTree(chainNum = 1, sampleNum = 500, treeNum = 2)
dev.off()

hist(residuals(fit_bart))
shapiro.test(residuals(fit_bart))

qqnorm(residuals(fit_bart))
qqline(residuals(fit_bart))

library(hnp)
hnp(residuals(fit_bart), scale = TRUE, half = FALSE, paint = TRUE)

## tuning k
## default k = 2
cv_bart <- xbart(log(Salary) ~ .,
                 data = Hitters_complete,
                 k = c(1,2,3,4,5),
                 seed = 2024)
apply(cv_bart, 2, mean)

cv_bart <- xbart(log(Salary) ~ .,
                 data = Hitters_complete,
                 k = seq(0.5, 3, .5),
                 seed = 2024)
apply(cv_bart, 2, mean)

cv_bart <- xbart(log(Salary) ~ .,
                 data = Hitters_complete,
                 k = seq(0.5, 1.5, .1),
                 seed = 2024)
apply(cv_bart, 2, mean)

set.seed(123)
random_rows <- sample(1:nrow(Hitters_complete))

Hitters_train <- Hitters_complete[random_rows[1:200], ]
Hitters_test <- Hitters_complete[random_rows[201:263], ]

fit_bart <- bart2(log(Salary) ~ .,
                  data = Hitters_train,
                  test = Hitters_test,
                  k = 1.4, # experiment with different values of k
                  verbose = FALSE) #

sqrt(sum((fit_bart$yhat.test.mean - log(Hitters_test$Salary))^2/63))

## modelling the mean using BART and decomposing the variance using random effects
library(stan4bart)
library(lme4)

fit_stan4bart <- stan4bart(TICKS ~ bart(HEIGHT + YEAR) + (1 | BROOD) + (1 | LOCATION),
                           data = grouseticks,
                           bart_args = list(keepTrees = TRUE),
                           verbose = -1)

predictions <- stan4bart:::extract.stan4bartFit(object = fit_stan4bart, sample = "train", type = "ppd")
predictions <- t(predictions)

plot(grouseticks$TICKS, apply(predictions,2,mean), cex = .7, pch = 16, col = "#00000040",
     xlab = "Observed", ylab = "Predicted"); abline(0, 1, lty = 2)

locations <- list()

for(i in seq_along(unique(grouseticks$LOCATION))) {
  locations[[i]] <- apply(as.matrix(predictions[, grouseticks$LOCATION == i]), 1, mean)
}

means <- lapply(locations, mean)
lower <- lapply(locations, quantile, .025)
upper <- lapply(locations, quantile, .975)

tibble(location = seq_along(unique(grouseticks$LOCATION)),
       lower = unlist(lower),
       upper = unlist(upper),
       estimate = unlist(means)) %>%
  ggplot(aes(x = reorder(location, estimate), y = estimate)) +
  theme_bw() +
  geom_point() +
  geom_segment(aes(x = reorder(location, estimate),
                   xend = reorder(location, estimate),
                   y = upper,
                   yend = lower)) +
  coord_flip()

## allows for inference
## probability that there are more ticks in location 4 than location 5
sum(locations[[4]] > locations[[5]]) / nrow(predictions)

## accounts for unseen future groups
## non-independent data
## more regularization
## improved predictions

# Boruta ------------------------------------------------------------------

library(Boruta)

boruta_fit <- Boruta(Salary ~ ., data = Hitters_complete)
table(boruta_fit$finalDecision)
boruta_fit$finalDecision[boruta_fit$finalDecision == "Confirmed"]

# GAMLSS ------------------------------------------------------------------

library(gamlss)
library(gamlss.add)

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

fit_gamlss2 <- gamlss(log(Salary) ~ tr(~ CAtBat + CRuns + CHits + CRBI + CWalks) +
                        AtBat + Hits + HmRun + Runs + RBI + Walks + Years +
                        League + Division + PutOuts + Assists + Errors + NewLeague,
                      sigma.formula = ~ tr(~ CAtBat + CRuns + CHits + CRBI + CWalks) +
                        AtBat + Hits + HmRun + Runs + RBI + Walks + Years +
                        League + Division + PutOuts + Assists + Errors + NewLeague,
                      data = Hitters_complete,
                      control = gamlss.control(n.cyc = 50))
wp(fit_gamlss2)
plot(fit_gamlss2)
plot(getSmo(fit_gamlss2))
text(getSmo(fit_gamlss2))

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

fit_gamlss4 <- gamlss(log(Salary) ~ nn(~ CAtBat + CRuns + CHits + CRBI + CWalks) +
                        AtBat + Hits + HmRun + RBI + Walks + Years +
                        League + Division + Assists,
                      sigma.formula = ~ nn(~ CAtBat + CRuns + CHits + CRBI + CWalks) +
                        AtBat + Runs + Walks + Years +
                        League + Division + PutOuts + Assists,
                      data = Hitters_complete,
                      control = gamlss.control(n.cyc = 50))
wp(fit_gamlss4)
plot(fit_gamlss4)
plot(getSmo(fit_gamlss4))

AIC(fit_gamlss)
AIC(fit_gamlss2)
AIC(fit_gamlss3)
AIC(fit_gamlss4)

## comparing
set.seed(12345)
k <- 5
fold <- sample(k, nrow(Hitters_complete), replace = TRUE)
fsize <- table(fold)

mse <- matrix(NA, nrow = k, ncol = 5)
colnames(mse) <- c("CART" ,"Random Forests", "BART", "GAMLSS_RF", "GAMLSS_NN")

for(i in 1:k) {
  foldi <- Hitters_complete[fold == i,]
  foldOther <- Hitters_complete[fold != i,]
  
  fit_cart <- rpart(log(Salary) ~ ., data = foldOther, cp = 0.042)
  pred_cart <- predict(fit_cart, foldi)
  
  fit_rf <- randomForest(log(Salary) ~ ., data = foldOther)
  pred_rf <- predict(fit_rf, foldi)
  
  fit_bart <- bart2(log(Salary) ~ ., data = foldOther, keepTrees = TRUE)
  pred_bart <- colMeans(predict(fit_bart, foldi, type = "ev"))
  
  fit_gamlss_rf <- update(fit_gamlss3, data = foldOther)
  pred_gamlss_rf <- predict(fit_gamlss3, newdata = foldi, what = "mu")
  
  fit_gamlss_nn <- update(fit_gamlss4, data = foldOther)
  pred_gamlss_nn <- predict(fit_gamlss4, newdata = foldi, what = "mu")
  
  mse[i,1] <- mean((pred_cart - log(foldi$Salary))^2)
  mse[i,2] <- mean((pred_rf - log(foldi$Salary))^2)
  mse[i,3] <- mean((pred_bart - log(foldi$Salary))^2)
  mse[i,4] <- mean((pred_gamlss_rf - log(foldi$Salary))^2)
  mse[i,5] <- mean((pred_gamlss_nn - log(foldi$Salary))^2)
}

mse

cv <- apply(mse, 2, function(x) weighted.mean(x, fsize))
sort(cv)