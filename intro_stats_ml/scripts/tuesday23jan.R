library(tidyverse)

# Introduction: Example datasets ------------------------------------------

## Wage data
wage_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/data/wage.csv")

wage_df %>%
  ggplot(aes(x = age, y = wage)) +
  geom_point() +
  geom_smooth(se = FALSE)

wage_df %>%
  ggplot(aes(x = wage)) +
  geom_histogram()

wage_df %>%
  ggplot(aes(x = education, y = wage)) +
  geom_boxplot()

wage_df %>%
  ggplot(aes(x = education, y = wage)) +
  geom_violin()

wage_df %>%
  ggplot(aes(x = education, y = wage)) +
  geom_violin() +
  geom_jitter(height = 0, width = .1,
              alpha = .2)


## Stock market data
stock_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/data/stock_market.csv")
stock_df

stock_df_long <- stock_df %>%
  mutate(id = 1:nrow(stock_df)) %>%
  pivot_longer(cols = c(2,3,4,5,6,8),
               names_to = "lag",
               values_to = "perc_return") %>%
  mutate(lag = as.factor(lag))

levels(stock_df_long$lag) <- c(-1,-2,-3,-4,-5,0)
stock_df_long$lag <- stock_df_long$lag %>% as.character %>% as.numeric

stock_df_long %>%
  ggplot(aes(x = lag, y = perc_return, group = id, col = Direction)) +
  theme_bw() +
  geom_line(alpha = .1)

stock_df_long %>%
  filter(id %in% 1:10) %>%
  ggplot(aes(x = lag, y = perc_return, group = id, col = Direction)) +
  theme_bw() +
  geom_line()

stock_df %>%
  ggplot(aes(x = Direction, y = Lag1)) +
  geom_boxplot()

## Gene expression data
library(ISLR)
?NCI60

dim(NCI60$data)
NCI60$labs
image(t(NCI60$data), yaxt = "n", xaxt = "n", xlab = "Gene")
axis(2, at = seq(0, 1, length = 64), labels = NCI60$labs,
     las = 1, cex.axis = .4)
axis(1, at = seq(0, 1, length = 20),
     labels = floor(seq(1, ncol(NCI60$data), length = 20)),
     cex.axis = .6, las = 2)

# Unsupervised learning ---------------------------------------------------

## calculating distances
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

D2 <- D1 * D1 ## `*` = element-wise product
              ## %*% = matrix cross-product
D2

h <- hclust(as.dist(D2), method = "single")
plot(as.dendrogram(h))

## Example: EuroStat
educ_df <- read.table("https://raw.githubusercontent.com/rafamoral/courses/main/intro_stats_ml/data/eurostat.txt",
                      header = TRUE, row.names = 1)
educ_df <- educ_df %>%
  select(- age) %>%
  t

library(PairViz)
pcp(educ_df, scale = FALSE, col = 1:18)

stars(educ_df, nrow = 3, col.stars = 1:18)

## calculate distances
d <- dist(educ_df)
h <- hclust(d, method = "ward.D2")
d1 <- as.dendrogram(h)
plot(d1)

## coloring k clusters
library(dendextend)
plot(color_branches(d1, k = 2, col = 1:2))

cutree(h, 4)

pcp(educ_df, scale = FALSE, col = cutree(h, 4))

stars(educ_df, nrow = 3, col.stars = cutree(h, 4))

## k-means example: NCI60 data
nci_data <- NCI60$data
nci_labs <- NCI60$labs
nci_labs[1:4]

apply(nci_data, 2, var)

nci_data1 <- scale(nci_data) ## scaling to have mean 0 and variance 1
apply(nci_data1, 2, var)
colMeans(nci_data1)

nci_clust <- kmeans(nci_data1, centers = 4, nstart = 25)
nci_clust$cluster
nci_clust$tot.withinss

twss <- numeric(15)
for(k in 1:15) {
  twss[k] <- kmeans(nci_data1, centers = k, nstart = 25)$tot.withinss
}

plot(1:15, twss, type = "o", xlab = "k (no. clusters)",
     ylab = "TWSS")

## take the 5-cluster solution
nci_clust <- kmeans(nci_data1, centers = 5, nstart = 25)
table(nci_labs, nci_clust$cluster)
