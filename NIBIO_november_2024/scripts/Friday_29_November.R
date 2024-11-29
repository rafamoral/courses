library(tidyverse)


# Introduction: example datasets ------------------------------------------

# wage data

wage_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_stats_ml2/data/wage.csv")

wage_df %>%
  ggplot(aes(x = age, y = wage)) +
  theme_bw() +
  geom_point()

wage_df %>%
  ggplot(aes(x = wage)) +
  theme_bw() +
  geom_histogram()

# stock market data
stock_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_stats_ml2/data/stock_market.csv")
stock_df

stock_df_long <- stock_df %>%
  mutate(id = 1:nrow(stock_df)) %>%
  pivot_longer(c(2:6, 8),
               names_to = "lag",
               values_to = "perc_return") %>%
  mutate(lag = as.factor(lag))

levels(stock_df_long$lag) <- c(-1,-2,-3,-4,-5,0)
stock_df_long$lag <- as.numeric(as.character(stock_df_long$lag))

stock_df_long %>%
  ggplot(aes(x = lag, y = perc_return, group = id, colour = Direction)) +
  theme_bw() +
  geom_line()

stock_df_long %>%
  filter(id %in% 1:10) %>%
  ggplot(aes(x = lag, y = perc_return, group = id, colour = Direction)) +
  theme_bw() +
  geom_line()

# gene expression data
library(ISLR)
?NCI60

names(NCI60)
NCI60$labs

image(t(NCI60$data), yaxt = "n", xaxt = "n", xlab = "Gene")


# Clustering --------------------------------------------------------------

?dist

# Example: Eurostat
educ_df <- read.table("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/intro_stats_ml2/data/eurostat.txt",
                      row.names = 1, header = TRUE)

educ_df <- educ_df %>%
  select(- age)

educ_df <- t(educ_df)

library(MASS)
parcoord(educ_df, lwd = 2, col = 1:18)

stars(educ_df)

# let's do hierarchical clustering
d <- dist(educ_df, method = "euclidean")
h <- hclust(d, method = "ward.D2")

dendrogram1 <- as.dendrogram(h)
plot(dendrogram1)

# let's make it fancier
library(dendextend)

dendrogram2 <- color_branches(dendrogram1, k = 4,
                              col = c(2,3,4,5))
plot(dendrogram2)


# k-means clustering
# example: NCI60

nci_data <- NCI60$data
nci_labs <- NCI60$labs

nci_data_scaled <- scale(nci_data)

my_kmeans <- kmeans(nci_data_scaled, centers = 5, nstart = 25)
my_kmeans$tot.withinss

my_kmeans <- kmeans(nci_data_scaled, centers = 6, nstart = 25)
my_kmeans$tot.withinss

twss <- numeric(15)

for(i in 1:15) {
  my_kmeans <- kmeans(nci_data_scaled, centers = i, nstart = 25)
  twss[i] <- my_kmeans$tot.withinss
}

twss

plot(1:15, twss, ty = "b",
     xlab = "number of clusters",
     ylab = "Total within-cluster sum of squares")

# model-based clustering
# package mclust can be very helpful!


# Tree-based methods ------------------------------------------------------

# regression example: hitters data
data(Hitters)

library(rpart)
library(rpart.plot)

fit_tree <- rpart(log(Salary) ~ ., data = Hitters)
rpart.plot(fit_tree)
plotcp(fit_tree)

fit_tree2 <- rpart(log(Salary) ~ .,
                   data = Hitters,
                   cp = 0.06)
rpart.plot(fit_tree2)

# classification trees example: spam dataset
data(spam, package = "kernlab")

set.seed(1234)
my_selection <- sample(1:nrow(spam), 1000)
spam_training <- spam[- my_selection,]
spam_test <- spam[my_selection,]

fit_tree1 <- rpart(type ~ .,
                   data = spam_training,
                   cp = 0)
plotcp(fit_tree1)

fit_tree2 <- rpart(type ~ .,
                   data = spam_training,
                   cp = 0.0046)
rpart.plot(fit_tree2)

# let's compute some predictions and compare performances

pred1 <- predict(fit_tree1, newdata = spam_test,
                 type = "class")
pred2 <- predict(fit_tree2, newdata = spam_test,
                 type = "class")

conf_mat1 <- table(spam_test$type, pred1)
conf_mat2 <- table(spam_test$type, pred2)

TPR1 <- 346 / (346 + 44)
TPR2 <- 337 / (337 + 53)

ACC1 <- (570 + 346) / 1000
ACC2 <- (574 + 337) / 1000

pred_p1 <- predict(fit_tree1, newdata = spam_test)[,2]
pred_p2 <- predict(fit_tree2, newdata = spam_test)[,2]

conf_mat1b <- table(spam_test$type,
                    ifelse(pred_p1 > .8, "spam", "nonspam"))
conf_mat2b <- table(spam_test$type,
                    ifelse(pred_p2 > .8, "spam", "nonspam"))

conf_mat1b
conf_mat2b

# fitting random forests to the spam dataset

library(randomForest)
set.seed(2024)
fit_rf <- randomForest(type ~ ., data = spam_training)
fit_rf

plot(fit_rf)

fit_rf <- randomForest(type ~ ., data = spam_training, mtry = 15)
fit_rf

# selecting mtry via cross-validation
library(caret)

fit_control <- trainControl(method = "cv",
                            number = 5,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = twoClassSummary)

set.seed(2024)
fit_rf_caret <- train(type ~ .,
                      data = spam,
                      method = "rf",
                      tuneGrid = data.frame(mtry = c(5,8,15)),
                      trControl = fit_control,
                      metric = "ROC")
fit_rf_caret

# plotting the ROC curve
library(pROC)
selected_index <- fit_rf_caret$pred$mtry == 8
plot.roc(fit_rf_caret$pred$obs[selected_index],
         fit_rf_caret$pred$spam[selected_index])


# BART --------------------------------------------------------------------

library(dbarts)

# hitters dataset
Hitters_complete <- na.omit(Hitters)

fit_bart <- bart2(log(Salary) ~ .,
                  data = Hitters_complete,
                  keepTrees = TRUE,
                  power = 2,
                  base = .95)
plot(fit_bart)

fit_bart$fit$plotTree(chainNum = 1, sampleNum = 1, treeNum = 1)
fit_bart$fit$plotTree(chainNum = 1, sampleNum = 1, treeNum = 2)

dev.off()
hist(residuals(fit_bart))
shapiro.test(residuals(fit_bart))

qqnorm(residuals(fit_bart))
qqline(residuals(fit_bart))

library(hnp)
hnp(residuals(fit_bart), scale = TRUE, half = FALSE,
    paint = TRUE, print = TRUE)