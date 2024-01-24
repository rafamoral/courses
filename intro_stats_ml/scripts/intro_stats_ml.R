library(tidyverse)

# Introduction: Example datasets ------------------------------------------

## Wage data
wage_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/data/wage.csv")

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
stock_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/data/stock_market.csv")

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
educ_df <- read.table("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/data/eurostat.txt",
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
music <- read.csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/data/music.csv", stringsAsFactors = TRUE)
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

# Principal Components Analysis -------------------------------------------

## Example: USArrests Data
head(USArrests)
plot(USArrests)
apply(USArrests, 2, var) ## good idea to scale the data

p <- prcomp(USArrests, scale = TRUE)
p

biplot(p, scale = 0, cex = c(.5,.5), cex.axis = .5)

plot(p)
summary(p)

source("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/scripts/screeplot.R")
screeplot(p)

# remotes::install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(p,
         labels = state.abb,
         groups = state.region,
         ellipse = TRUE) +
  theme_bw()

## Example: Olympic Decathlon Data
olympic <- read.table("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/data/olympic88.txt",
                      header = TRUE, check.names = FALSE)
head(olympic)
pairs(olympic, gap = .2, cex = .5)

round(apply(olympic, 2, var), 2)
p <- prcomp(olympic[,-11], scale = TRUE)

p$rotation[,1:2]
summary(p)
screeplot(p)

biplot(p, scale = 0, cex = c(.5,.5), cex.axis = .5)

p <- prcomp(olympic[-34,-11], scale = TRUE)
screeplot(p)
p$rotation[,1:2]

biplot(p, scale = 0, cex = c(.5,.5), cex.axis = .5)

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
source("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/scripts/calcERR.R")
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

# Regression --------------------------------------------------------------

## Bodyfat data
bfat <- read.table("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/data/bodyfat.txt", header = TRUE)
head(bfat)

source("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/scripts/opairs.R")
opairs(bfat)

f <- lm(bfat ~ ., data = bfat)
summary(f)

library(leaps)
allfits <- regsubsets(bfat ~ ., data = bfat)
summary(allfits)$which
par(mar = c(3,3,0,0))
plot(allfits, scale = "r2", col = "blue", main = "Best")
dev.off()

fwfits <- regsubsets(bfat ~ ., data = bfat, method = "forward")
bwfits <- regsubsets(bfat ~ ., data = bfat, method = "backward")
par(mfrow=c(1,2))
par(mar=c(3,3,0,0))
plot(fwfits, scale = "r2", col = "blue", main = "Forwards")
plot(bwfits, scale = "r2", col = "blue", main = "Backwards")
dev.off()

sum.allfits <- summary(allfits)
names(sum.allfits)

par(mfrow = c(1,2))
npred <- 1:7
plot(npred, sum.allfits$cp, pch = 20, xlab = "number of predictors")
lines(npred, sum.allfits$cp)
plot(npred, sum.allfits$rsq, pch = 20, xlab = "number of predictors")
lines(npred, sum.allfits$rsq)
dev.off()

## Validation via a test set
set.seed(123)
s <- sample(nrow(bfat), 21)
bfatTrain <- bfat[-s,]
bfatTest <- bfat[s,]
f1 <- lm(bfat ~ age + height + neck + abdomen, data = bfatTrain)
mean(residuals(f1)^2) # train
pred1 <- predict(f1, bfatTest)
mean((pred1 - bfatTest$bfat)^2) # test

f2 <- lm(bfat ~ weight + height + abdomen, data = bfatTrain)
mean(residuals(f2)^2) # train
pred2 <- predict(f2, bfatTest)
mean((pred2 - bfatTest$bfat)^2) # test

## 5-fold CV
set.seed(123)
k <- 5
fold <- sample(k, nrow(bfat), replace=T)
fold
fsize <- table(fold)

mse <- vector(length = k)
for(i in 1:k) {
  foldi <- bfat[fold == i,]
  foldOther <- bfat[fold != i,]
  f <- lm(bfat ~ age + height + neck + abdomen, foldOther)
  pred <- predict(f, foldi)
  mse[i] <- mean((pred - foldi$bfat)^2) # MSEi
}
mse
cv <- weighted.mean(mse, fsize)
cv

mse <- vector(length = k)
for(i in 1:k) {
  foldi <- bfat[fold == i,]
  foldOther <- bfat[fold != i,]
  f <- lm(bfat ~ weight + height + abdomen, foldOther)
  pred <- predict(f, foldi)
  mse[i] <-mean((pred - foldi$bfat)^2) # MSEi
}
mse
cv <- weighted.mean(mse, fsize)
cv

## Shrinkage
library(glmnet)
x <- model.matrix(bfat ~ . - 1, data = bfatTrain)
y <- bfatTrain$bfat
grid <- 10^seq(-3, 5, length = 100)
ridge_fit <- glmnet(x, y, alpha = 0, lambda = grid) # alpha = 0 for ridge

matplot(t(coef(ridge_fit)[-1,100:1]),
        type = "l", ylab = expression(hat(beta)[lambda]),
        xaxt="n", xlab = expression(lambda)) 
z <- round(seq(1, 100, length = 6))
axis(1, at = z,labels = round(grid[z], 3), cex.axis = 0.7)

set.seed(2024)
cv_out <- cv.glmnet(x, y, alpha = 0)
cv_out$lambda.min

ridge_fit2 <- glmnet(x, y, alpha = 0, 
                     lambda = cv_out$lambda.min)
coef(ridge_fit2)
coef(lm(bfat ~ ., data = bfatTrain))

lasso_fit <- glmnet(x, y, alpha = 1, lambda = grid) # alpha = 1 for lasso
plot(lasso_fit)

cv_out <- cv.glmnet(x, y, alpha = 1)
cv_out$lambda.min

lasso_fit2 <- glmnet(x, y, alpha = 1, 
                     lambda = cv_out$lambda.min)
coef(lasso_fit)


ridge_pred <- predict(ridge_fit2, 
                      newx = model.matrix(bfat ~ . - 1, data = bfatTest))
lasso_pred <- predict(lasso_fit2, 
                      newx = model.matrix(bfat ~ . - 1, data = bfatTest))

calcMSE <- function(y, yhat) mean((yhat - y)^2)

calcMSE(bfatTest$bfat, ridge_pred)
calcMSE(bfatTest$bfat, lasso_pred)

par(mfrow = c(1,2))
plot(bfatTest$bfat, ridge_pred); abline(0, 1, lty = 2)
plot(bfatTest$bfat, lasso_pred); abline(0, 1, lty = 2)
dev.off()

# Smoothing ---------------------------------------------------------------

## Polynomial regression
set.seed(123)
n <- 200
x <- runif(n,0,1.75)^3
fx <- sin(x)
y <- fx + rnorm(100, sd = .15)
d <- data.frame(y = y, x = x)

f1 <- lm(y ~ x, data=d)

d %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() + 
  geom_line(aes(y = fitted(f1)),
            color = "red")

f3 <- lm(y ~ x + I(x^2) + I(x^3), data = d)

d %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() + 
  geom_line(aes(y = fitted(f3)),
            color = "red")

f4 <- lm(y ~ x + I(x ^ 2) + I(x ^ 3)+I(x ^ 4), data = d)

d %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = fitted(f3)),
            color = "red") +
  geom_line(aes(y = fitted(f4)),
            color = "blue")

mean(residuals(f3)^2)
mean(residuals(f4)^2)

anova(f3, f4)

f10 <- lm(y ~ poly(x, 10), data = d)

d %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = fitted(f10)),
            color = "red") +
  geom_line(aes(y = fitted(f4)),
            color = "blue")

mean(residuals(f10)^2)

anova(f4, f10)

## loess
flo <- loess(y ~  x, data = d)

d %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = fitted(flo)),
            color = "red") +
  geom_line(aes(y = fitted(f4)),
            color = "blue")

mean(residuals(flo)^2)

flo1 <- loess(y ~  x, data = d, span = .2)

d %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = fitted(flo)),
            color = "red") +
  geom_line(aes(y = fitted(flo1)),
            color = "blue")

mean(residuals(flo1)^2)

k <- 5
fold <- sample(k, nrow(d), replace = TRUE)

mse <- vector(length = k)
span <- seq(.1, .9, by = .05)
cv <- vector(length = length(span))

for(j in 1:length(span)) {
  for(i in 1:k) {
    foldi <- d[fold == i,]
    foldOther <- d[fold != i,]
    f <- loess(y ~ x, data = foldOther, span = span[j])
    pred <- predict(f, foldi)
    mse[i] <- mean((pred - foldi$y)^2, na.rm = TRUE) # MSEi
  }
  cv[j]<- mean(mse)
}

plot(span, cv)
lines(span,cv)
span[which.min(cv)] # produces the lowest CV

flo <- loess(y ~  x, data = d, span=span[which.min(cv)])

d %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = fitted(flo)),
            color = "red") 

mean(residuals(flo)^2)

## Splines
library(splines)
f1 <- lm(y ~ bs(x, df = 6), data = d) # df is the number of basis functions

d %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = fitted(f1)),
            color = "red")

f2 <- lm(y ~ bs(x, knots = 1:3), data = d)
d %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = fitted(f1)),
            color = "red") +
  geom_line(aes(y = fitted(f2)),
            color = "blue")

k <- 5
fold <- sample(k, nrow(d), replace = TRUE)
fsize <- table(fold)

mse <- vector(length = k)
knots <- 1:5
cv <- vector(length = length(knots))

for(j in 1:length(knots)) {
  for (i in 1:k) {
    foldi <- d[fold == i,]
    foldOther <- d[fold != i,]
    f <- lm(y ~ bs(x, df = knots[j] + 3), data = foldOther) 
    pred <- predict(f, foldi)
    mse[i] <- mean((pred - foldi$y)^2, na.rm = TRUE) # MSEi
  }
  cv[j] <- weighted.mean(mse, fsize)
  # weighted as each fold not the same size.
}

plot(knots, cv)
lines(knots,cv)
knots[which.min(cv)] # produces the lowest CV

f2 <- lm(y ~ bs(x, df = 5), data = d)
d %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = fitted(f1)),
            color = "red") + 
  geom_line(aes(y = fitted(f2)),
            color = "blue")

## Example: Wage data
Wage$wage250 <- as.numeric(Wage$wage > 250)

Wage %>%
  ggplot(aes(x = age, y = wage, colour = as.factor(wage250))) +
  theme_bw() +
  geom_point()

f1 <- glm(wage250 ~ age,
          family = "binomial",
          data = Wage)

pred1 <- predict(f1, Wage, type = "response")

Wage %>%
  ggplot(aes(x = age, y = wage250)) +
  theme_bw() +
  geom_jitter(width = 0, height = 0.1, size = .2) +
  geom_line(aes(x = age, y = pred1),
            color = "red")

f2 <- glm(wage250 ~ bs(age, 4),
          family = "binomial",
          data = Wage)

pred2 <- predict(f2, Wage, type = "response")

Wage %>%
  ggplot(aes(x = age, y = wage250 * .2)) +
  theme_bw() +
  geom_jitter(width = 0, height = 0.01, size = .2) +
  geom_line(aes(x = age, y = pred1),
            color = "red") +
  geom_line(aes(x = age, y = pred2),
            color = "blue")

f3 <- glm(wage250 ~ poly(age, 4), 
          family = "binomial",
          data = Wage)
pred3 <- predict(f3, Wage, type = "response")

Wage %>%
  ggplot(aes(x = age, y = wage250 * .2)) +
  theme_bw() +
  geom_jitter(width = 0, height = 0.01, size=.2) +
  geom_line(aes(x = age, y = pred2),
            color = "blue") +
  geom_line(aes(x = age, y = pred3),
            color = "green")

## Smoothing splines
my_seq <- rev(c(50,30,10,5,2,1,.9,.8,.7,.6,.5,.4,.3,.2,
                .1,.05,.025,.01,.001,.0001,.00001,.000001,
                1e-7,1e-8,1e-9,1e-10))
xpred <- seq(min(x), max(x), length = 200)

for(i in my_seq) {
  plot(y ~ x, col = "gray50")
  lines(xpred, predict(smooth.spline(x, y, lambda = i, cv = TRUE),
                       data.frame(x = xpred))$y$x, col = 4, lwd = 2)
  legend("topleft", paste("lambda =", i))
  legend("topright", paste("PRESS =", round(smooth.spline(x, y, lambda = i)$cv.crit, 2)))
  Sys.sleep(time = .2)
}

# GAMs --------------------------------------------------------------------

## Salary data
sal <- read.table("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/data/salary.txt",
                  header = TRUE)
sal$gender <- factor(ifelse(sal$gender == 1, "F", "M"))
head(sal)

fit1 <- lm(log(salary) ~ . , data = sal)
summary(fit1)

confint(fit1, "genderM") # for log(sal)
exp(confint(fit1, "genderM")) # sal

fit2 <- lm(log(salary) ~ educ + ns(pexp, 4) + ns(time, 4) + gender, 
           data = sal)
anova(fit1, fit2)

fit3 <- lm(log(salary) ~ educ + ns(pexp, 4) + time + gender,
           data = sal)
anova(fit3, fit2)

exp(confint(fit3, "genderM")) # sal

library(gam)
plot.Gam(fit3, terms = "ns(pexp, 4)")

## Wage data
fit_gam <- gam(wage250 ~ year + s(age, df = 5) + education,
               family = "binomial",
               data = Wage)

par(mfrow = c(1,3))
plot(fit_gam, se = TRUE, col = 4)
table(Wage$education, Wage$wage250)

fit_gam <- gam(wage250 ~ year + s(age, df = 5) + education,
               family = "binomial",
               data = Wage %>%
                 filter(education !="1. < HS Grad"))

par(mfrow = c(1,3))
plot(fit_gam, se = TRUE, col = 4)

grid <- expand.grid(
  year = seq(2003, 2009, length = 100),
  age = seq(18, 80, length = 100)
)

grid$education <- levels(Wage$education)[5]

grid$pred <- matrix(predict(fit_gam,
                            newdata = grid,
                            type = "response"),
                    dim(grid)[1], 1)

grid %>%
  ggplot(aes(x = year, y = age, z = pred)) +
  theme_bw() +
  geom_raster(aes(fill = pred)) +
  scale_fill_distiller(type = "seq", palette = 1)

grid$pred2 <- grid$pred > 0.12

grid %>%
  ggplot(aes(x = year, y = age, fill = pred2)) + 
  theme_bw() +
  geom_raster()

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

load("intro_stats_ml/scripts/cv_rf.RData")
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
                  data = Hitters_complete)
plot(fit_bart)
dev.off()

hist(residuals(fit_bart))
shapiro.test(residuals(fit_bart))

qqnorm(residuals(fit_bart))
qqline(residuals(fit_bart))

library(hnp)
hnp(residuals(fit_bart), scale = TRUE, half = FALSE, paint = TRUE)

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

## comparing
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