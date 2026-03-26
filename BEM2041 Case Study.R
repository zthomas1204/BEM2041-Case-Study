install.packages(c("readxl", "dplyr", "factoextra"))

library(readxl)
library(dplyr)
library(factoextra)

df <- read_excel("360buy_SurveyData.xlsx")

# Quick check
head(df)
str(df)
summary(df)

#select cluster variables
cluster_vars <- df %>%
  select(CusChoice, ConstUp, ReplacReminder, ProdReturn, ProInsuCov)

#Elbow Method
set.seed(123)
fviz_nbclust(cluster_vars, kmeans, method = "wss")

#Silouhette Method
fviz_nbclust(cluster_vars, kmeans, method = "silhouette")

#k means clustering
set.seed(123)
k <- 3
km <- kmeans(cluster_vars, centers = k, nstart = 25)

df$segment <- km$cluster

#Euclidean distance
dist_mat <- dist(cluster_vars, method = "euclidean")

hc <- hclust(dist_mat, method = "ward.D2")

plot(hc, labels = FALSE, main = "Hierarchical clustering (Euclidean distance)")

k <- 3
df$segment_hc <- cutree(hc, k = k)
table(df$segment_hc)

#profiling segements
km$centers
aggregate(cluster_vars, by = list(Segment = df$segment), mean)

#add demographics
aggregate(df[, c("CusAgeYr", "CusGen", "LevEdn", "LevIncome", "CusAcct")],
          by = list(Segment = df$segment),
          FUN = mean)
aggregate(df[, c("CusAgeYr", "LevIncome")],
          by = list(Segment = df$segment),
          FUN = median)

#visualize clusters
fviz_cluster(km, data = cluster_vars,
             geom = "point",
             ellipse.type = "norm",
             palette = "jco")